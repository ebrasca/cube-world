;;;; cube-world.lisp

(in-package #:cube-world)

;;; "cube-world" goes here. Hacks and glory await!

(defparameter *width* 1920)
(defparameter *height* 1080)

(defparameter *verts* nil)
(defparameter *index* nil)
(defparameter *chunks-manager* nil)

(defparameter *texture* nil)
(defparameter *sampler* nil)

(defvar *camera* nil)

;;--------------------------------------------------------------
;; GPU
(defstruct-g g-pct
  (position :vec4 :accessor pos)
  (color :vec4 :accessor col)
  (texture :vec3 :accessor tex))

(defun-g vert ((g-pct g-pct) (offset :vec3) &uniform (model->world :mat4) (world->cam :mat4) (cam->clip :mat4))
  (values (* cam->clip
             world->cam
             model->world
             (+ (pos g-pct) (v! offset 0)))
          (col g-pct)
          (:smooth (tex g-pct))))

(defun-g frag ((col :vec4) (tex-coord :vec3) &uniform (tex :sampler-3d))
  col ;;(texture tex tex-coord)
  )

(defpipeline-g prog-1 ()
  (vert g-pct :vec3)
  (frag :vec4 :vec3))

;;--------------------------------------------------------------
;; CPU
(defclass chunk ()
  ((offset :accessor chunk-offset :initarg :offset)
   (blocks :accessor chunk-blocks :initarg :blocks)
   (n-blocks :accessor chunk-n-blocks :initarg :n-blocks)
   (gpu-blocks :accessor chunk-gpu-blocks :initarg :gpu-blocks)
   (gpu-stream :accessor chunk-gpu-stream :initarg :gpu-stream))
  (:default-initargs
   :gpu-blocks nil
   :gpu-stream nil))

(defun make-chunk (ox oy oz)
  (let* ((blocks (loop :for z :below 64 :append
                       (loop :for y :below 64 :append
                             (loop :for x :below 64
                                   :for noise := (loop :for i :from 1 :to 9 :sum
                                                       (* (ash 1 i)
                                                          (noise-3d (/ (+ ox x) (ash 1 (1- i)) 1.0d0)
                                                                    (/ (+ oy y) (ash 1 (1- i)) 1.0d0)
                                                                    (/ (+ oz z) (ash 1 (1- i)) 1.0d0))))
                                   :when (and (< noise 0))
                                   :collect (v! x y (- z))))))
         (blocks-length (length blocks)))
    (unless (zerop blocks-length)
      (make-instance 'chunk
                     :offset (v! ox oy oz)
                     :blocks blocks
                     :n-blocks blocks-length))))

;; WIP
(defclass chunk-manager ()
  ((chunks :accessor chunk-manager-chunks :initarg :chunks))
  (:default-initargs
   :chunks nil))

(defmethod loading ((cm chunk-manager))
  (let ((pos (v! (map 'vector #'floor (v3:* (v! 1/64 1/64 1/64) (camera-pos *camera*))))))
    (progn (loop :for chunk :in (chunk-manager-chunks *chunks-manager*)
                 :for gpu-stream := (chunk-gpu-stream chunk)
                 :for gpu-blocks := (chunk-gpu-blocks chunk)
                 :do (progn (free gpu-stream)
                            (free gpu-blocks)))
           (setf (chunk-manager-chunks *chunks-manager*) nil)
           (let ((chunk (make-chunk (* (aref pos 0) 64.0) (* (aref pos 1) 64.0) (* (aref pos 2) 64.0))))
             (when chunk
               (let* ((gpu-blocks (make-gpu-array (chunk-blocks chunk)
                                                  :dimensions (chunk-n-blocks chunk) :element-type :vec3))
                      (gpu-stream (make-buffer-stream (list *verts* (cons gpu-blocks 1))
                                                      :index-array *index*)))
                 (setf (chunk-gpu-blocks chunk) gpu-blocks
                       (chunk-gpu-stream chunk) gpu-stream)
                 (push chunk (chunk-manager-chunks cm))))))))

(defmethod setup ((cm chunk-manager)))
(defmethod unloading ((cm chunk-manager)))
(defmethod rebuilding ((cm chunk-manager)))
(defmethod visibility ((cm chunk-manager)))

(defmethod render ((cm chunk-manager))
  (loop :for chunk :in (chunk-manager-chunks *chunks-manager*)
        :for gpu-stream := (chunk-gpu-stream chunk)
        :when gpu-stream :do
        (with-instances (chunk-n-blocks chunk)
          (map-g #'prog-1 gpu-stream
                 :cam->clip (cepl.camera:cam->clip *camera*)
                 :world->cam (m4:* (q:to-mat4 (camera-rot *camera*))
                                   (m4:translation (v4:* (v! -1 -1 -1 1) (camera-pos *camera*))))
                 :model->world (m4:translation (v! (chunk-offset chunk) 1.0))
                 :tex *sampler*))))

(defun now ()
  (* 0.01 (get-internal-real-time)))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  ;; keyboard events
  (when (keyboard-button (keyboard) key.c)
    (loading *chunks-manager*))
  (when (keyboard-button (keyboard) key.r)
    (setf (camera-pos *camera*) (v! 0 0 0)
          (camera-rot *camera*) (q! 1.0 0.0 0.0 0.0)))
  (when (keyboard-button (keyboard) key.w)
    (v3:decf (camera-pos *camera*)
             (m3:mrow*vec3 (v! 0.0 0.0 1.0) (q:to-mat3 (camera-rot *camera*)))))
  (when (keyboard-button (keyboard) key.s)
    (v3:incf (camera-pos *camera*)
             (m3:mrow*vec3 (v! 0.0 0.0 1.0) (q:to-mat3 (camera-rot *camera*)))))
  (when (keyboard-button (keyboard) key.a)
    (setf (camera-rot *camera*)
          (q:normalize
           (q:* (camera-rot *camera*)
                (q! 1.0 0.0 -0.1 0.0)))))
  (when (keyboard-button (keyboard) key.d)
    (setf (camera-rot *camera*)
          (q:normalize
           (q:* (camera-rot *camera*)
                (q! 1.0 0.0 0.1 0.0)))))
  (when (keyboard-button (keyboard) key.q)
    (v3:decf (camera-pos *camera*)
             (v! 0.0 1.0 0.0)))
  (when (keyboard-button (keyboard) key.e)
    (v3:incf (camera-pos *camera*)
             (v! 0.0 1.0 0.0)))
  (render *chunks-manager*)
  (swap)
  (decay-events))

(let ((running nil))
  (defun run-loop ()
    (setf running t
          *camera* (make-camera)
          ;; create cube
          *verts* (make-gpu-array `((,(v! -0.5 -0.5 -0.5 1.0) ,(v! -0.5 -0.5 -0.5 1.0) ,(v! 0.0 0.0 0.0))
                                    (,(v!  0.5 -0.5 -0.5 1.0) ,(v!  0.5 -0.5 -0.5 1.0) ,(v! 1.0 0.0 0.0))
                                    (,(v!  0.5  0.5 -0.5 1.0) ,(v!  0.5  0.5 -0.5 1.0) ,(v! 1.0 1.0 0.0))
                                    (,(v! -0.5  0.5 -0.5 1.0) ,(v! -0.5  0.5 -0.5 1.0) ,(v! 0.0 1.0 0.0))
                                    (,(v! -0.5 -0.5  0.5 1.0) ,(v! -0.5 -0.5  0.5 1.0) ,(v! 0.0 0.0 1.0))
                                    (,(v!  0.5 -0.5  0.5 1.0) ,(v!  0.5 -0.5  0.5 1.0) ,(v! 1.0 0.0 1.0))
                                    (,(v!  0.5  0.5  0.5 1.0) ,(v!  0.5  0.5  0.5 1.0) ,(v! 1.0 1.0 1.0))
                                    (,(v! -0.5  0.5  0.5 1.0) ,(v! -0.5  0.5  0.5 1.0) ,(v! 0.0 1.0 1.0)))
                                  :dimensions 8 :element-type 'g-pct)
          *index* (make-gpu-array '(0 3 2  2 1 0
                                    0 4 7  7 3 0
                                    3 7 6  6 2 3
                                    2 6 5  5 1 2
                                    1 5 4  4 0 1
                                    4 5 6  6 7 4)
                                  :dimensions 36 :element-type :unsigned-short))
    ;; ;; create texture
    ;; (setf *texture* (with-c-array-freed
    ;;                     (temp (make-c-array (loop :for i :below 8 :collect
    ;;                                               (loop :for j :below 8 :collect
    ;;                                                     (loop :for k :below 8 :collect
    ;;                                                           (v! 0.0 1.0 0.0
    ;;                                                               1.0))))
    ;;                                         :dimensions '(8 8 8) :element-type :uint8-vec4))
    ;;                   (make-texture temp))
    ;;       *sampler* (sample *texture*))
    (setf *chunks-manager* (make-instance 'chunk-manager))
    (loading *chunks-manager*)
    (loop :while (and running (not (shutting-down-p))) :do
          (continuable (step-demo))))
  (defun stop-loop () (setf running nil)))

;;;; cube-world.lisp

(in-package #:cube-world)

;;; "cube-world" goes here. Hacks and glory await!

(defparameter *width* 1920)
(defparameter *height* 1080)

(defparameter *verts* nil)
(defparameter *index* nil)
(defparameter *chunk* nil)
(defparameter *chunks* nil)

(defparameter *texture* nil)
(defparameter *sampler* nil)

(defvar *camera* nil)
(defparameter *old* (v! 0 0 0))

;;--------------------------------------------------------------
;; GPU
(defstruct-g g-pct
  (position :vec4 :accessor pos)
  (color :vec4 :accessor col)
  (texture :vec2 :accessor tex))

(defun-g vert ((g-pct g-pct) (offset :vec3) &uniform (model->world :mat4) (world->cam :mat4) (cam->clip :mat4))
  (values (* cam->clip
             world->cam
             model->world
             (+ (pos g-pct) (v! offset 0)))
          (tan (col g-pct))
          (:smooth (tex g-pct))))

(defun-g frag ((pos :vec4) (tex-coord :vec2) &uniform (tex :sampler-2d))
  (/ (+ (abs pos)
        (texture tex tex-coord))
     2))

(defpipeline-g prog-1 ()
  (vert g-pct :vec3)
  (frag :vec4 :vec2))

;;--------------------------------------------------------------
;; CPU
(defstruct chunk
  (offset)
  (array-offsets)
  (array-offsets-length)
  (stream))

(defun generate-chunk (ox oy oz)
  (time
   (flet ((temp (x y z)
            (loop :for i :from 1 :to 7 :sum
                  (* (ash 1 i)
                     (noise-3d (/ (+ ox x)
                                  (ash 1 (1- i)) 1.0d0)
                               (/ (+ oy y)
                                  (ash 1 (1- i)) 1.0d0)
                               (/ (+ oz z)
                                  (ash 1 (1- i)) 1.0d0))))))
     (dotimes (z 10))
     (let ((positions (loop :for z :below 64 :append
                            (loop :for y :below 64 :append
                                  (loop :for x :below 64
                                        :for noise := (temp x y z)
                                        :when (and (< noise 0)
                                                   ;; filtra cubos no visibles testing purpose
                                                   ;; (not
                                                   ;;  (and
                                                   ;;   (< (temp (1- x) y z) 0)
                                                   ;;   (< (temp x (1- y) z) 0)
                                                   ;;   (< (temp x y (1- z)) 0)
                                                   ;;   (< (temp (1+ x) y z) 0)
                                                   ;;   (< (temp x (1+ y) z) 0)
                                                   ;;   (< (temp x y (1+ z)) 0)))
                                                   )
                                        :collect (v! x y z))))))
       (unless (zerop (length positions))
         (let ((gpu-positions (make-gpu-array positions :dimensions (length positions) :element-type :vec3)))
           (make-chunk :offset (v! ox oy oz 1.0)
                       :array-offsets gpu-positions
                       :array-offsets-length (length positions)
                       :stream (make-buffer-stream (list *verts* (cons gpu-positions 1))
                                                   :index-array *index*))))))))

(defun free-chunk (chunk)
  (free (chunk-array-offsets chunk))
  (free (chunk-stream chunk)))

(defun now ()
  (* 0.01 (get-internal-real-time)))

(defun mouse-units ()
  (v3:- (v! (map 'list #'round
                 (v2:* (v! (/ 32.0 0.5625) -32.0)
                       (v2:- (v2:/ (mouse-pos (mouse))
                                   (v! *width* *height*))
                             (v! 0.5 0.5))))
            0)
        (camera-pos *camera*)))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)

  (when (keyboard-button (keyboard) key.c)
    (dolist (chunk *chunks*)
      (free-chunk chunk))
    (setf *chunks* nil)
    (dotimes (z 2)
      (dotimes (y 2)
        (dotimes (x 2)
          (let ((chunk (generate-chunk (* x 64.0)
                                       (* y 64.0)
                                       (* z 64.0))))
            (when chunk
              (push chunk
                    *chunks*)))))))

  (when (keyboard-button (keyboard) key.r)
    (setf (camera-pos *camera*) (v! 0 0 -6)
          (camera-rot *camera*) (q! 1.0 0.0 0.0 0.0)))
  
  (when (keyboard-button (keyboard) key.w)
    (v3:decf (camera-pos *camera*)
             (m3:mrow*vec3 (v! 0.0 0.0 -1.0) (q:to-mat3 (camera-rot *camera*)))))
  (when (keyboard-button (keyboard) key.s)
    (v3:incf (camera-pos *camera*)
             (m3:mrow*vec3 (v! 0.0 0.0 -1.0) (q:to-mat3 (camera-rot *camera*)))))
  
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

  (dolist (chunk *chunks*)
    (with-instances (chunk-array-offsets-length chunk)
      (map-g #'prog-1 (chunk-stream chunk)
             :model->world (m4:translation (chunk-offset chunk))
             :world->cam (m4:* (q:to-mat4 (camera-rot *camera*))
                               (m4:translation (camera-pos *camera*)))
             :cam->clip (cepl.camera:cam->clip *camera*)
             :tex *sampler*)))
  
  (swap)
  (decay-events))

(let ((running nil))
  (defun run-loop ()
    (setf running t
          *camera* (make-camera :pos (v! 0 0 0))
          ;; cube
          *verts* (make-gpu-array `((,(v! -0.5 -0.5 -0.5 1.0) ,(v! -0.5 -0.5 -0.5 1.0) ,(v! 0.0 0.0))
                                    (,(v!  0.5 -0.5 -0.5 1.0) ,(v!  0.5 -0.5 -0.5 1.0) ,(v! 1.0 0.0))
                                    (,(v!  0.5  0.5 -0.5 1.0) ,(v!  0.5  0.5 -0.5 1.0) ,(v! 1.0 1.0))
                                    (,(v! -0.5  0.5 -0.5 1.0) ,(v! -0.5  0.5 -0.5 1.0) ,(v! 0.0 1.0))
                                    (,(v! -0.5 -0.5  0.5 1.0) ,(v! -0.5 -0.5  0.5 1.0) ,(v! 0.0 0.0))
                                    (,(v!  0.5 -0.5  0.5 1.0) ,(v!  0.5 -0.5  0.5 1.0) ,(v! 1.0 0.0))
                                    (,(v!  0.5  0.5  0.5 1.0) ,(v!  0.5  0.5  0.5 1.0) ,(v! 1.0 1.0))
                                    (,(v! -0.5  0.5  0.5 1.0) ,(v! -0.5  0.5  0.5 1.0) ,(v! 0.0 1.0)))
                                  :dimensions 8 :element-type 'g-pct)
          *index* (make-gpu-array '(0 3 2  2 1 0
                                    0 4 7  7 3 0
                                    3 7 6  6 2 3
                                    2 6 5  5 1 2
                                    1 5 4  4 0 1
                                    4 5 6  6 7 4)
                                  :dimensions 36 :element-type :unsigned-short))
    ;; texture
    (setf *texture* (with-c-array-freed
                        (temp (make-c-array (loop :for i :below 16 :collect
                                                  (loop :for j :below 16 :collect
                                                        (v! (noise-3d (/ i 1.0d0)
                                                                      (/ j 1.0d0)
                                                                      0.0d0)
                                                            (noise-3d (/ i 1.0d0)
                                                                      (/ j 1.0d0)
                                                                      1.0d0)
                                                            (noise-3d (/ i 1.0d0)
                                                                      (/ j 1.0d0)
                                                                      2.0d0)
                                                            1.0)))
                                            :dimensions '(16 16) :element-type :uint8-vec4))
                      (make-texture temp))
          *sampler* (sample *texture*))
    ;; clean chunks
    (dolist (chunk *chunks*)
      (free-chunk chunk))
    (setf *chunks* nil)
    ;; create chunks
    (dotimes (z 2)
      (dotimes (y 2)
        (dotimes (x 2)
          (let ((chunk (generate-chunk (* x 64.0)
                                       (* y 64.0)
                                       (* z 64.0))))
            (when chunk
              (push chunk
                    *chunks*))))))
    (loop :while (and running (not (shutting-down-p))) :do
          (continuable (step-demo))))
  (defun stop-loop () (setf running nil)))

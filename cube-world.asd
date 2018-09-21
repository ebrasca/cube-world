;;;; cube-world.asd

(asdf:defsystem #:cube-world
  :description "Describe cube-world here"
  :author "Bruno Cichon <ebrasca.ebrasca@openmailbox.org>"
  :license "Specify license here"
  :depends-on (#:cepl.sdl2 #:cepl.skitter.sdl2 #:cepl.camera
	       #:varjo
               #:livesupport
               #:rtg-math.vari

               #:noise
               #:lparallel
               ;; #:l-system
               ;; #:turtle-graphics
               )
  :serial t
  :components ((:file "package")
               (:file "cube-world")))

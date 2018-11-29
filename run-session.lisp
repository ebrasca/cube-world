(ql:quickload :cube-world)
(in-package :cube-world)
(progn (repl *width* *height* 4.5) ; start
       (run-loop))
(run-loop) ; refresh




(in-package :wedf)

(defun load-files ()
  (load "/home/rett/dev/clim-ed/package.lisp")
  (load "/home/rett/dev/clim-ed/ed.lisp")
  (load "/home/rett/dev/clim-ed/ed-commands.lisp")
  (load "/home/rett/dev/clim-ed/wed.lisp")
  (load "/home/rett/dev/clim-ed/ed-stream.lisp")
  (load "/home/rett/dev/clim-ed/wed-stream.lisp")
  (load "/home/rett/dev/clim-ed/wed-frame.lisp")
  (load "/home/rett/dev/clim-ed/wed-frame-commands.lisp")
  (load "/home/rett/dev/clim-ed/site-specific.lisp"))

(defun run-ed ()
  (load-files)
  (activate-wed-frame *wed-frame*)
  (clim:run-frame-top-level *wed-frame*))

(defun get-text-style ()
  *wed-frame*
  (clim:medium-text-style (frame-top-level-sheet *wed-frame*)))

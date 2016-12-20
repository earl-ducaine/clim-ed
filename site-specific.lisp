(in-package :wedf)

;;; **********************************************************************
;;; Copyright (c) 92, 93 Hallvard Traetteberg, Claudio Massucho.
;;; All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice.  Suggestions, comments and bug reports are welcome.  Please
;;; address email to: Hallvard.Tretteberg@si.sintef.no
;;; **********************************************************************

(defparameter *root* (clim:find-port))
;;      #+:mcl     (clim:open-root-window :mcl)
;;      #+:allegro (clim:open-root-window :clx)
;;      )

(setf *halwed-tool-command-alist*
      `(
        (#-:allegro #\UpArrow      #+:allegro ,(control-char #\1) . ed:previous-line)
        (#-:allegro #\DownArrow    #+:allegro ,(control-char #\4) . ed:next-line)
        (#-:allegro #\BackArrow    #+:allegro ,(control-char #\2) . ed:backward-char)
        (#-:allegro #\ForwardArrow #+:allegro ,(control-char #\3) . ed:forward-char)

        (,(control-char #\p) . ed:previous-line) (,(control-char #\n) . ed:next-line)
        (,(control-char #\b) . ed:backward-char) (,(control-char #\f) . ed:forward-char)

        (,(control-char #\a) . ed:beginning-of-line)
        (,(control-char #\e) . ed:end-of-line)
        (,(meta-char    #\<) . ed:beginning-of-text)
        (,(make-char #\> :shift :meta) . ed:end-of-text)

        (#\Tab . ed:tabulate)

        (,(control-char #\w) . ed:delete-region)
        (#\Rubout            . ed:delete-char-backward)
        (,(control-char #\d) . ed:delete-char)
        (,(control-char #\Rubout) . ed:delete-to-beginning)
        (,(control-char #\k)      . ed:delete-to-end)

        (,(control-char #\x)     . ed:swap-point-mark)
        (,(control-char #\Space) . ed:set-mark)
        (,(meta-char #\Space)    . ed:unset-mark)

        (,(control-char #\y)                . ed:yank)
        (,(meta-char    #\y)                . ed:undo)
        (,(make-char    #\y :control :meta) . ed:knay)
        ))

;;(setf *wed-frame* (clim:make-application-frame 'wed-frame :parent *root*))
(defparameter *wed-frame* (make-application-frame 'wed-frame))


;; sheets can have a text style this returns it.
;; (clim:medium-text-style (frame-top-level-sheet *wed-frame*))


#|
(flet ((print-fun ()
         (format t "This is a great text editor"))
       )
  (activate-wed-with-print-fun #'print-fun *wed-frame*))

(progn
  (activate-wed-frame *wed-frame*)
  (clim:run-frame-top-level *wed-frame*)
  )
|#

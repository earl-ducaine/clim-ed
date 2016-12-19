(in-package :ed)

;;; **********************************************************************
;;; Copyright (c) 92, 93 Hallvard Traetteberg.  All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as 
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice.  Suggestions, comments and bug reports are welcome.  Please 
;;; address email to: Hallvard.Tretteberg@si.sintef.no
;;; **********************************************************************

(define-ed-motion previous-line (point) (&optional (number 1)) (forward-line ed point (- number)))
(define-ed-motion next-line     (point) (&optional (number 1)) (forward-line ed point number))

(define-ed-motion backward-char (point) (&optional (number 1)) (forward-char ed point (- number)))
(define-ed-motion forward-char  (point) (&optional (number 1)))

(define-ed-motion beginning-of-line (point))
(define-ed-motion end-of-line       (point))

(define-ed-motion beginning-of-text (point))
(define-ed-motion end-of-text       (point))

(define-ed-mover search-backward (point) (string) (search-for ed point string t))
(define-ed-mover search-forward  (point) (string) (search-for ed point string nil))

(define-ed-changer insert (point) (args) (insert ed point args t t))

(define-ed-changer replace-region (region) (arg))
(define-ed-command-duplicates replace-region transform-region)

(define-ed-changer tabulate (point) ())

(define-ed-changer delete-region (region) () (replace-region ed region nil))
(define-ed-changer delete-char          (point) () (motion-delete ed point '(forward-char)))
(define-ed-changer delete-char-backward (point) () (motion-delete ed point '(backward-char)))
(define-ed-changer delete-to-beginning  (point) () (motion-delete ed point '(beginning-of-line)))
(define-ed-changer delete-to-end        (point) () (motion-delete ed point '(end-of-line)))

(define-ed-mover swap-point-mark (point mark) () (setf (ed-point ed) mark (ed-mark ed) point))

(define-ed-mover set-mark (point) () (setf (ed-mark ed) point))
(define-ed-mover unset-mark ()    () (setf (ed-mark ed) nil))

(define-ed-changer undo-replace ())

(define-ed-command-duplicates undo-replace knay undo)

(define-ed-changer yank (point) () (insert ed point (ed-replaced ed) t t))

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

;;; wed-tool

(defclass wed-tool ()
  ((name :initarg :name :initform "WED" :accessor name)))

(defmethod reset-tool ((tool wed-tool)) (declare (ignore tool)))
(defgeneric tool-handler (tool frame))

;;; comtabs

(define-command-table text-editor-comtab)
(define-command-table lisp-editor-comtab :inherit-from (text-editor-comtab))

;;;

(define-application-frame wed-frame ()
  ((wed :initform (make-instance 'halwed) :initarg :wed :accessor wed-frame-wed)
   (comtab :initform 'lisp-editor-comtab :initarg :comtab :accessor wed-frame-comtab)
   (tool :initform (make-instance 'wed-tool) :initarg :tool :accessor wed-frame-tool))
  (:panes (title :title :display-string "Window editor frame")
	  (wed :application :scroll-bars :vertical
	       :default-text-style (make-text-style :fix :roman :small)
	       :display-after-commands nil
	       :display-function 'draw-wed-pane)
	  (minibuffer :interactor))
  (:layouts
   (default
       (vertically ()
	 title wed minibuffer))))

(defun draw-wed-pane (frame pane)
  ;; (setf (medium-text-style (frame-top-level-sheet frame))
  ;; 	(medium-merged-text-style (frame-top-level-sheet frame)))
  (setf (medium-text-style pane)
	(medium-merged-text-style pane))
  (with-output-recording-options (pane :draw t)
    (wed:refresh-wed (wed-frame-wed frame) t))
  )

(defun redisplay-wed-frame-wed-pane (frame &key (force-p nil))
  (let ((pane (get-frame-pane frame 'wed)))
    (with-output-recording-options (pane :record nil :draw t)
      (wed:wed-command (wed-frame-wed frame) (if force-p 'wed:layout 'wed:refresh)))
    ))

(defun current-wed-pane (&optional (frame *application-frame*))
  (values (get-frame-pane frame 'wed)))

(defun current-wed (&optional (frame *application-frame*))
  (wed-frame-wed frame))

(defmethod activate-wed-frame ((frame wed-frame))
  (let ((wed (wed-frame-wed frame)))
    (setf (wed::wed-win wed) (current-wed-pane frame))
    (wed:update-wed-layout wed t)
    (wed:refresh-wed wed t)
    ))

(defmethod activate-wed-frame :around ((frame wed-frame))
  (setf (frame-command-table frame) (wed-frame-comtab frame))
  (call-next-method)
  (redisplay-frame-pane frame 'wed :force-p t))

;;; Special throwing function

(defun tool-return (&key (new-tool nil) (command nil) (reuse-event nil))
  (throw 'tracking-pointer (values new-tool command reuse-event)))

;;; General event handler

;; Ugly trick that works!

#+:allegro (defmethod clim-utils:queue-empty-p ((win null)) (values t))

(defmethod tool-handler ((tool wed-tool) frame)
  (let ((pane (current-wed-pane frame))
        (button-pressed-p nil))
    (catch 'tracking-pointer
      (unwind-protect
        (with-output-recording-options (pane :draw t :record nil)
          (tracking-pointer (pane :multiple-window t :context-type nil)
             (:pointer-motion (window x y)
               (when button-pressed-p
                 (pointer-motion tool window x y)))
             (:pointer-button-press (event x y)
               (setf button-pressed-p t)
               (pointer-button-press tool event x y))
             (:pointer-button-release (event x y)
              (setf button-pressed-p nil)
              (pointer-button-release tool event x y))
             (:keyboard     (character)
              (keyboard tool character))
             ))
        (reset-tool tool))
        )))

(defmethod read-frame-command ((frame wed-frame) &key stream)
  (declare (ignore stream))
  (let ((tool (wed-frame-tool frame)))
    (if (not tool)
      (call-next-method)
      (multiple-value-bind (new-tool command reuse-event) (tool-handler tool frame)
        (when new-tool
          (setf (wed-frame-tool frame) (if (eq new-tool t) nil new-tool)))
        (if reuse-event
          (progn
            (if (consp reuse-event)
              (unread-gesture (car reuse-event) :stream (cdr reuse-event))
              (unless (eq reuse-event t)
                (unread-gesture reuse-event :stream (current-wed-pane frame))))
            (call-next-method))
          (when command
            (values command)))
        ))))

(defmethod execute-frame-command ((frame wed-frame) command)
  (declare (ignore command))
  (let ((pane (current-wed-pane frame)))
    (multiple-value-bind (redisplay)
                         (with-output-recording-options (pane :record nil :draw t)
                           (call-next-method))
        (when redisplay
          (redisplay-frame-pane frame 'wed :force-p t)))
    ))

;;; Event handlers

(defmethod pointer-button-press ((tool wed-tool) event x y)
  (declare (ignore tool))
  (let ((wed (wed-frame-wed *application-frame*)))
    (if (eq (wed:wed-win wed) (event-sheet event))
      (wed:position-set-marker wed x y ed:ed-point)
      (tool-return :reuse-event event))
    ))

(defmethod pointer-motion ((tool wed-tool) window x y)
  (declare (ignore tool window x y)))

(defmethod pointer-button-release ((tool wed-tool) event x y)
  (declare (ignore tool))
  (let ((wed (wed-frame-wed *application-frame*)))
    (when (eq (wed:wed-win wed) (event-sheet event))
      (let ((marker (wed:position-marker wed x y)))
        (unless (ed:marker-= marker (ed:ed-point wed))
          (wed:position-set-marker wed x y ed:ed-mark)))
      )))

(defun make-char (char &rest shifts)
  (if (member :shift shifts)
      (char-upcase char)
      char))

(defun test-char-bits (char &rest shifts)
  t)

(defun shift-char   (char) (make-char char :shift))
(defun control-char (char) (make-char char :control))
(defun meta-char    (char) (make-char char :meta))

(defun unmodified-char-p (char)
  (zerop (event-modifier-state char)))

(defun shift-char-p (char)
  (= 1 (event-modifier-state char)))

(defun control-char-p (char)
  (= 2 (event-modifier-state char)))

(defun meta-char-p (char)
  (= 3 (event-modifier-state char)))

;;; Mapping from character to ed-commands

(defvar *halwed-tool-command-alist* nil "Will be set to an alist of char and command")

(defun standard-process-keyboard (wed char)
  (let ((ed:*haled-command-alist* *halwed-tool-command-alist*))
    (cond ((eql char (meta-char #\x))
           (tool-return :reuse-event t))
          ((eql char (control-char #\l))
           (wed:wed-command wed 'wed:refresh))
          ((eql char (meta-char #\l))
           (wed:wed-command wed 'wed:layout))
          (t (multiple-value-bind (ignore error-p)
		 (if t
		     ;; (and (characterp char) (unmodified-char-p char)
		     ;;      (not (assoc char ed:*haled-command-alist*))
		     ;;      (funcall (ed:ed-valid-fun wed) char))
		     (ed:ed-command wed 'ed:insert char)
		     (ed:ed-command wed char))
               (declare (ignore ignore))
               (when error-p
                 (beep)))
             (let ((next-char (read-gesture :stream (wed:wed-win wed) :timeout 0)))
               (if (characterp next-char)
                 (standard-process-keyboard wed next-char)
                 (progn
                   (when next-char
                     (unread-gesture next-char :stream (wed:wed-win wed)))
                   (wed:update-wed-layout wed)
                   (wed:refresh-wed wed))))))
    ))

(defmethod keyboard ((tool wed-tool) char)
  (let ((wed (current-wed)))
    (handler-bind
               (
                (ed:unknown-haled-command
                 #'(lambda (condition)
                     (declare (ignore condition))
                     (tool-return :command `(com-beep-message "Unknown ED command!"))))
                (ed:haled-read-only-error
                 #'(lambda (condition)
                     (declare (ignore condition))
                     (tool-return :command `(com-beep-message "Read Only Region!"))))
                (ed:haled-mark-not-set-error
                 #'(lambda (condition)
                     (declare (ignore condition))
                     (tool-return :command `(com-beep-message "Mark not set!"))))
                )
      (standard-process-keyboard wed char))
    ))

(defun activate-wed-frame-with-init-fun (init-fun frame)
  (let ((wed (make-instance 'wed:halwed)))
    (setf (wed-frame-wed frame) wed)
    (activate-wed-frame frame)
    (funcall init-fun wed))
  (run-frame-top-level frame)
  )

(defun activate-wed-frame-with-stream-fun (stream-fun frame)
  (flet ((init-fun (wed)
           (let ((out-stream (make-instance 'halwed-output-stream :ed wed :marker :top)))
             (unwind-protect (progn
                               (funcall stream-fun out-stream)
                               (force-output out-stream))
               (close out-stream))))
         )
    (activate-wed-frame-with-init-fun #'init-fun frame)))

(defun activate-wed-with-print-fun (print-fun frame)
  (flet ((stream-fun (wed-stream)
           (let ((*standard-output* wed-stream))
             (funcall print-fun)))
         )
    (activate-wed-frame-with-stream-fun #'stream-fun frame)))

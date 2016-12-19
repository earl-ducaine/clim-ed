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
  (
   (name :initarg :name :initform "WED" :accessor name)
   )
  )

(defmethod reset-tool ((tool wed-tool)) (declare (ignore tool)))
(defgeneric tool-handler (tool frame))

;;; comtabs

(clim:define-command-table text-editor-comtab)
(clim:define-command-table lisp-editor-comtab :inherit-from (text-editor-comtab))

;;;

(clim:define-application-frame wed-frame ()
  (
   (wed    :initform (make-instance 'halwed)    :initarg :wed    :accessor wed-frame-wed)
   (comtab :initform 'lisp-editor-comtab        :initarg :comtab :accessor wed-frame-comtab)
   (tool   :initform (make-instance 'wed-tool)  :initarg :tool   :accessor wed-frame-tool)
   )
  (:panes ((title :title :display-string "Window editor frame")
           (wed :application :scroll-bars :vertical
                :default-text-style (clim:make-text-style :fix :roman :small)
                :display-after-commands nil
                :display-function 'draw-wed-pane)
           (minibuffer :interactor)
           ))
  (:layout ((default (:column 1 (title :compute) (wed :rest) (minibuffer 1/8)))))
  )

(defun draw-wed-pane (frame pane)
  (setf (slot-value pane 'clim::current-text-style)
        (slot-value pane 'clim::merged-text-style))
  (clim:with-output-recording-options (pane :record-p nil :draw-p t)
    (wed:refresh-wed (wed-frame-wed frame) t))
  )

(defun redisplay-wed-frame-wed-pane (frame &key (force-p nil))
  (let ((pane (clim:get-frame-pane frame 'wed)))
    (clim:with-output-recording-options (pane :record-p nil :draw-p t)
      (wed:wed-command (wed-frame-wed frame) (if force-p 'wed:layout 'wed:refresh)))
    ))

(defun current-wed-pane (&optional (frame clim:*application-frame*))
  (values (clim:get-frame-pane frame 'wed)))

(defun current-wed (&optional (frame clim:*application-frame*))
  (wed-frame-wed frame))

(defmethod activate-wed-frame ((frame wed-frame))
  (let ((wed (wed-frame-wed frame)))
    (setf (wed::wed-win wed) (current-wed-pane frame))
    (wed:update-wed-layout wed t)
    (wed:refresh-wed wed t)
    ))

(defmethod activate-wed-frame :around ((frame wed-frame))
  (setf (clim:frame-command-table frame) (wed-frame-comtab frame))
  (call-next-method)
  (clim:redisplay-frame-pane frame 'wed :force-p t))

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
        (clim:with-output-recording-options (pane :draw-p t :record-p nil)
          (clim:tracking-pointer (pane :multiple-window t :context-type nil)
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

(defmethod clim:read-frame-command ((frame wed-frame) &key stream)
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
              (clim:unread-gesture (car reuse-event) :stream (cdr reuse-event))
              (unless (eq reuse-event t)
                (clim:unread-gesture reuse-event :stream (current-wed-pane frame))))
            (call-next-method))
          (when command
            (values command)))
        ))))

(defmethod clim:execute-frame-command ((frame wed-frame) command)
  (declare (ignore command))
  (let ((pane (current-wed-pane frame)))
    (multiple-value-bind (redisplay)
                         (clim:with-output-recording-options (pane :record-p nil :draw-p t)
                           (call-next-method))
        (when redisplay
          (clim:redisplay-frame-pane frame 'wed :force-p t)))
    ))

;;; Event handlers

(defmethod pointer-button-press ((tool wed-tool) event x y)
  (declare (ignore tool))
  (let ((wed (wed-frame-wed clim:*application-frame*)))
    (if (eq (wed:wed-win wed) (clim:event-window event))
      (wed:position-set-marker wed x y ed:ed-point)
      (tool-return :reuse-event event))
    ))

(defmethod pointer-motion ((tool wed-tool) window x y)
  (declare (ignore tool window x y)))

(defmethod pointer-button-release ((tool wed-tool) event x y)
  (declare (ignore tool))
  (let ((wed (wed-frame-wed clim:*application-frame*)))
    (when (eq (wed:wed-win wed) (clim:event-window event))
      (let ((marker (wed:position-marker wed x y)))
        (unless (ed:marker-= marker (ed:ed-point wed))
          (wed:position-set-marker wed x y ed:ed-mark)))
      )))

(defun make-char (char &rest shifts)
  #-:allegro
  (clim-utils::%set-char-bits char (apply #'clim::make-shift-mask shifts))
  #+:allegro
  (flet ((make-char-bits (shifts)
           (+ (if (member :control shifts) cltl1::char-control-bit 0)
              (if (member :meta    shifts) cltl1::char-meta-bit    0)))
         )
    (when (member :shift shifts)
      (setf char (char-upcase char)))
    (cltl1::make-char char (make-char-bits shifts)))
  )

(defun test-char-bits (char &rest shifts)
  (let ((test-char-bits (apply #'clim::make-shift-mask shifts))
        (char-bits (clim-utils:char-bits char)))
    (= char-bits test-char-bits)))

(defun shift-char   (char) (make-char char :shift))
(defun control-char (char) (make-char char :control))
(defun meta-char    (char) (make-char char :meta))

(defun unmodified-char-p (char) (test-char-bits char))
(defun shift-char-p      (char) (test-char-bits char :shift))
(defun control-char-p    (char) (test-char-bits char :control))
(defun meta-char-p       (char) (test-char-bits char :meta))

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
                                  (if (and (characterp char) (unmodified-char-p char)
                                           (not (assoc char ed:*haled-command-alist*))
                                           (funcall (ed:ed-valid-fun wed) char))
                                    (ed:ed-command wed 'ed:insert char)
                                    (ed:ed-command wed char))
               (declare (ignore ignore))
               (when error-p
                 (clim:beep)))
             (let ((next-char (clim:read-gesture :stream (wed:wed-win wed) :timeout 0)))
               (if (characterp next-char)
                 (standard-process-keyboard wed next-char)
                 (progn
                   (when next-char
                     (clim:unread-gesture next-char :stream (wed:wed-win wed)))
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
  (clim:run-frame-top-level frame)
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

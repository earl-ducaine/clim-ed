(in-package :wedf)

;;; **********************************************************************
;;; Copyright (c) 92, 93 Hallvard Traetteberg.  All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as 
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice.  Suggestions, comments and bug reports are welcome.  Please 
;;; address email to: Hallvard.Tretteberg@si.sintef.no
;;; **********************************************************************

(clim:define-command (com-exit :command-table text-editor-comtab :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-exit-ed :command-table text-editor-comtab :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(defun execute-wed-command (wed-command-name)
  (let ((wed (current-wed)))
    (wed:wed-command wed wed-command-name)))

(defmacro define-window-editor-command (wed-command-name)
  (let ((command-fun-name (intern (format nil "COM-~a" wed-command-name))))
    `(clim:define-command (,command-fun-name :command-table text-editor-comtab
                                             :menu t :name t)
       ()
       (execute-wed-command ',wed-command-name))
    ))

(define-window-editor-command wed:refresh)
(define-window-editor-command wed:layout)

(clim:define-presentation-translator command-menu
  (clim:blank-area clim:command text-editor-comtab :gesture :select ; should be :menu
                   :priority 0
                   :tester ((window) (eq window (clim:get-frame-pane clim:*application-frame* 'minibuffer)))
                   :tester-definitive t
                   )
  ()
  (values (clim:menu-choose-command-from-command-table 'text-editor-comtab :label "Command")))

(defun print-message-in-minibuffer (object &optional (prefix t))
  (let ((use-prefix (cond ((eq prefix t) "Message: ")
                          ((not prefix) "")
                          (t prefix))))
    (let ((stream (clim:get-frame-pane clim:*application-frame* 'minibuffer)))
      (format stream "~a" use-prefix)
      (if (functionp object)
        (funcall object stream)
        (format stream "~a" (format nil (if prefix "~a" "~s") object)))
      (values object))))

(defun beep-message-in-minibuffer (string &optional (prefix t))
  (clim:beep)
  (print-message-in-minibuffer string prefix))

(clim:define-command (com-beep-message :command-table text-editor-comtab :name t)
  ((string 'string :prompt nil))
  (beep-message-in-minibuffer string))

(defun with-fun-error-as-beep-message (fun)
  (multiple-value-bind (value condition)
                       (ignore-errors (funcall fun))
    (when condition
      (beep-message-in-minibuffer condition "Error: "))
    (values value condition))
  )

(defmacro with-error-as-beep-message (&body body)
  `(with-fun-error-as-beep-message #'(lambda () . ,body))
  )

(defvar *execute-ed-command-error* nil)

(defun execute-ed-command (ed-command-name &rest args)
  (let ((wed (current-wed)))
    (multiple-value-bind (value condition)
                         (with-error-as-beep-message
                           (multiple-value-bind (ignore error-p)
                                                (ed:ed-command wed ed-command-name args)
                             (declare (ignore ignore))
                             (when error-p
                               (clim:beep))))
      (declare (ignore value))
      (if condition
        (setf *execute-ed-command-error* condition)
        (progn
          (wed:update-wed-layout wed)
          (wed:refresh-wed wed)))
      )))

(defmacro define-text-editor-command (ed-command-name &optional (arg-type nil))
  (let ((command-fun-name (intern (format nil "COM-~a" ed-command-name))))
    `(clim:define-command (,command-fun-name :command-table text-editor-comtab
                                             :menu t :name t)
       ,(if arg-type `((,arg-type ',arg-type :prompt nil)) nil)
       (execute-ed-command ',ed-command-name . ,(if arg-type `(,arg-type) nil)))
    ))

(define-text-editor-command ed:previous-line)
(define-text-editor-command ed:next-line)
(define-text-editor-command ed:backward-char)
(define-text-editor-command ed:forward-char)
(define-text-editor-command ed:beginning-of-line)
(define-text-editor-command ed:end-of-line)
(define-text-editor-command ed:beginning-of-text)
(define-text-editor-command ed:end-of-text)

(define-text-editor-command ed:transform-region symbol)
(define-text-editor-command ed:replace-region string)

(define-text-editor-command ed:tabulate)

(define-text-editor-command ed:delete-region)
(define-text-editor-command ed:delete-char)
(define-text-editor-command ed:delete-char-backward)
(define-text-editor-command ed:delete-to-beginning)
(define-text-editor-command ed:delete-to-end)

(define-text-editor-command ed:swap-point-mark)
(define-text-editor-command ed:set-mark)
(define-text-editor-command ed:unset-mark)

(define-text-editor-command ed:yank)
(define-text-editor-command ed:undo)
(define-text-editor-command ed:knay)
(define-text-editor-command ed:undo-replace)

(define-text-editor-command ed:insert string)
(define-text-editor-command ed:search-backward string)
(define-text-editor-command ed:search-forward  string)

(clim:define-command (com-find-file :command-table text-editor-comtab
                                    :menu t :name t)
  ((path 'clim:pathname :prompt nil))
  (with-error-as-beep-message
    (with-open-file (stream path :direction :input :element-type 'character)
      (execute-ed-command 'ed:insert stream))
    ))

(clim:define-command (com-save-file :command-table text-editor-comtab
                                    :menu t :name t)
  ((path 'clim:pathname :prompt nil))
  (with-open-file (stream path :direction :output :element-type 'character)
    (write-string (ed:ed-text-as-string (current-wed)) stream))
  )

;;;

(defun execute-ed-function (fun)
  (let ((wed (current-wed)))
    (prog1
      (funcall fun wed)
      (wed:update-wed-layout wed)
      (wed:refresh-wed wed))
    ))

(defun call-with-wed-standard-io (wed fun &optional (read-from :point))
  (let* ((in-stream  (make-instance 'weds:halwed-input-stream  :ed wed :marker read-from))
         (out-stream (make-instance 'weds:halwed-output-stream :ed wed :marker in-stream))
         )
    (unwind-protect
      (let ((*standard-input* in-stream) (*standard-output* out-stream))
        (funcall fun)
        (force-output))
      (close in-stream)
      (close out-stream))))

(defun wed-read-funcall (wed fun &optional (read-from :point))
  (call-with-wed-standard-io wed #'(lambda () (funcall fun (read))) read-from))

(defun wed-read-funcall-format (wed fun &optional (format nil) (read-from :point) (stream t))
  (let ((format (if (or (eq format t) (not format)) "~s" format)))
    (wed-read-funcall wed #'(lambda (obj) (format stream format (funcall fun obj))) read-from)))

(defmacro define-lisp-editor-funcall-print-result-command (fun-name &optional (com-name nil))
  (unless com-name
    (setf com-name (intern (format nil "COM-~a" fun-name))))
  `(clim:define-command (,com-name :command-table lisp-editor-comtab
                                   :menu t :name t)
                        ()
     (execute-ed-function
      #'(lambda (wed) (wed-read-funcall-format wed #',fun-name "~%~s"))))
  )

(define-lisp-editor-funcall-print-result-command identity)
(define-lisp-editor-funcall-print-result-command eval)
(define-lisp-editor-funcall-print-result-command eval com-eval-form)
(define-lisp-editor-funcall-print-result-command macroexpand-1)
(define-lisp-editor-funcall-print-result-command macroexpand)

(defun execute-ed-function-to-minibuffer (fun)
  (execute-ed-function
   #'(lambda (wed)
       (let ((result (funcall fun wed)))
         (print-message-in-minibuffer result nil)
         (values result))))
  )

(defmacro define-lisp-editor-funcall-print-result-to-minibuffer-command (fun-name &optional (com-name nil))
  (unless com-name
    (setf com-name (intern (format nil "COM-~a-TO-MINIBUFFER" fun-name))))
  `(clim:define-command (,com-name :command-table lisp-editor-comtab
                                   :menu t :name t)
                        ()
     (execute-ed-function-to-minibuffer
      #'(lambda (wed)
          (wed-read-funcall wed #',fun-name))))
  )

(define-lisp-editor-funcall-print-result-to-minibuffer-command identity)
(define-lisp-editor-funcall-print-result-to-minibuffer-command eval)
(define-lisp-editor-funcall-print-result-to-minibuffer-command macroexpand-1)
(define-lisp-editor-funcall-print-result-to-minibuffer-command macroexpand)

(clim:define-command (com-eval-form-and-exit :command-table lisp-editor-comtab
                                             :menu t :name t)
  ()
  (execute-ed-function
   #'(lambda (wed) (wed-read-funcall wed #'eval :top)))
  (com-exit))

(defun wed-read-funcall-buffer (wed fun &optional (read-from :top))
  (flet ((read-buffer-funcall ()
           (let ((eof (cons nil nil)))
             (loop
               (let ((form (read nil nil eof)))
                 (when (eq form eof)
                   (return-from read-buffer-funcall nil))
                 (funcall fun form))
               )))
         )
    (call-with-wed-standard-io wed #'read-buffer-funcall read-from)))

(defmacro define-lisp-editor-funcall-buffer-forms-command (fun-name &optional (com-name nil))
  (unless com-name
    (setf com-name (intern (format nil "COM-~a-BUFFER" fun-name))))
  `(clim:define-command (,com-name :command-table lisp-editor-comtab
                                   :menu t :name t)
                        ()
     (execute-ed-function
      #'(lambda (wed)
          (wed-read-funcall-buffer wed #',fun-name)))
     )
  )

(define-lisp-editor-funcall-buffer-forms-command eval)
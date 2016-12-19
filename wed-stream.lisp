(in-package :halwed-stream)

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

(defclass halwed-stream (haled-stream) ())
(defclass halwed-output-stream (haled-output-stream) ())
(defclass halwed-input-stream  (haled-input-stream)  ())

(defmethod
  #+:mcl     ccl:stream-tyo
  #+:allegro stream::stream-write-char
  :after
  ((stream halwed-output-stream) char)
  (when (member char '(#\Newline #\Return))
    (let ((wed (ed-stream-ed stream)))
      (wed:update-wed-layout wed)
      (wed:refresh-wed wed))
    ))

(defmethod 
  #+:mcl     ccl:stream-force-output
  #+:allegro stream::stream-force-output
  :after
  ((stream halwed-output-stream))
  (let ((wed (ed-stream-ed stream)))
    (wed:update-wed-layout wed)
    (wed:refresh-wed wed)
  ))

(defmethod
  #+:mcl     ccl:stream-line-length
  #+:allegro stream-line-length
  ((stream halwed-output-stream))
  (values (wed:line-length (ed-stream-ed stream))))

(defmethod
  #+:mcl     ccl:stream-write-string
  #+:allegro stream::stream-write-string
  :after
  ((stream halwed-output-stream) string .
   #+:mcl (start end)
   #+:allegro (&optional (start 0) (end (length string))))
  (let ((wed (ed-stream-ed stream)))
    (when (find-if #'(lambda (char)
                       (member char '(#\Newline #\Return)))
                   string :start start :end end)
      (wed:update-wed-layout wed)
      (wed:refresh-wed wed))
  ))

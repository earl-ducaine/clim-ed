(in-package :haled-stream)

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

(defclass haled-stream (stream)
  (
   (ed         :initarg :ed                      :accessor ed-stream-ed)
   (marker-num :initarg :marker :initform :point :accessor ed-stream-marker-num)
   )
  )

(defclass haled-output-stream (haled-stream
                            #+:mcl     ccl:output-stream
                            #+:allegro fundamental-character-output-stream
                            )
  ()
  )

(defclass haled-input-stream (haled-stream
                            #+:mcl     ccl:input-stream
                            #+:allegro fundamental-character-input-stream
                            )
  ((last-tyi  :initform nil :accessor ed-input-stream-last-tyi))
  )

(defmethod initialize-instance :after ((stream haled-stream) &rest initargs)
  (declare (ignore initargs))
  (setf (ed-stream-marker-num stream)
        (let ((marker (ed-stream-marker-num stream)))
          (if (typep marker 'haled-stream)
            (values (ed-stream-marker-num marker))
            (let* ((ed (ed-stream-ed stream))
                   (markers (ed:ed-stream-markers ed))
                   (marker-num (ed:allocate-ed-marker ed markers)))
              (unless (member marker '(:point :mark :top :bottom))
                (setf marker :point))
              (setf (ed:markers-marker markers marker-num)
                    (ed:initial-marker ed marker))
              (values marker-num)))
          ))
  )

;;; basic stream character output

(defmethod
  #+:mcl     ccl:stream-tyo
  #+:allegro stream-write-char
  ((stream haled-output-stream) char)
  (let ((ed (ed-stream-ed stream)))
    (ed:insert ed (ed:markers-marker (ed:ed-stream-markers ed)
                                     (ed-stream-marker-num stream))
               char)
    ))

;;; stream force output

(defmethod
  #+:mcl     ccl:stream-force-output
  #+:allegro stream-force-output
  ((stream haled-output-stream))
  (declare (ignore stream)))

;;; basic stream character input

(defmethod
  #+:mcl     ccl:stream-tyi
  #+:allegro stream-read-char
  ((stream haled-input-stream))
  (block stream-tyi
    (let ((last-char (ed-input-stream-last-tyi stream)))
      (when last-char
        (setf (ed-input-stream-last-tyi stream) nil)
        (return-from stream-tyi last-char)))
    (let* ((ed (ed-stream-ed stream))
           (markers (ed:ed-stream-markers ed))
           (marker-num (ed-stream-marker-num stream))
           (marker (ed:markers-marker markers marker-num))
           (char (ed:following-char ed marker)))
      (let ((new-marker (ed:forward-char ed (ed:markers-marker markers marker-num))))
        (when new-marker
          (setf (ed:markers-marker markers marker-num) new-marker)))
      (values
       #+:mcl     char
       #+:allegro (or char :eof)
       )
      )))

;;; stream character unread

(defmethod
  #+:mcl     ccl:stream-untyi
  #+:allegro stream-unread-char
  ((stream haled-input-stream) char)
  (setf (ed-input-stream-last-tyi stream) char)
  (values
   #+:mcl     char
   #+:allegro nil
   )
  )


(defmethod
  #+:mcl     ccl:stream-close #+:mcl :after
  #+:allegro close
  ((stream haled-stream) .
   #+:mcl nil
   #+:allegro (&key abort))
  #+:allegro (declare (ignore abort))
  (let* ((ed (ed-stream-ed stream))
         (marker-num (ed-stream-marker-num stream)))
    (ed:deallocate-ed-marker ed (ed:ed-stream-markers ed) marker-num)
    (setf (ed-stream-marker-num stream) nil))
  (values))

;;; stream fresh line

(defmethod
  #+:mcl     ccl:stream-fresh-line
  #+:allegro stream-fresh-line
  ((stream haled-output-stream))
  (let* ((ed (ed-stream-ed stream))
         (marker-num (ed-stream-marker-num stream))
         (marker (ed:markers-marker (ed:ed-stream-markers ed) marker-num)))
    (unless (zerop (ed:marker-col marker))
      (
       #+:mcl     ccl:stream-tyo
       #+:allegro stream-write-char
       stream #\Newline))
    ))

;;; stream clear input

(defmethod
  #+:mcl     ccl:stream-clear-input
  #+:allegro stream-clear-input
  ((stream haled-input-stream))
  (declare (ignore stream)))

;;; stream abort

#+:mcl
(defmethod ccl:stream-abort ((stream haled-stream))
  (ccl:stream-close stream))
#+:allegro
(defmethod stream-abort ((stream haled-stream))
  (stream-close stream :abort t))

;;; stream column

(defmethod
  #+:mcl     ccl:stream-column
  #+:allegro stream-column ; should this be stream-line-column?
  ((stream haled-output-stream))
  (let* ((ed (ed-stream-ed stream))
         (marker-num (ed-stream-marker-num stream))
         (marker (ed:markers-marker (ed:ed-stream-markers ed) marker-num)))
    (values (ed:marker-col marker))))

;;; stream line length

(defmethod
  #+:mcl     ccl:stream-line-length
  #+:allegro stream-line-length ; should this be stream-output-width?
  ((stream haled-output-stream))
  (values 80))

;;; stream write string

(defmethod
  #+:mcl     ccl:stream-write-string
  #+:allegro stream-write-string
  ((stream haled-output-stream) string . ; Note this dot!
   #+:mcl (start end)
   #+:allegro (&optional (start 0) (end (length string))))
  (let ((ed (ed-stream-ed stream)))
    (ed:insert ed (ed:markers-marker (ed:ed-stream-markers ed)
                                     (ed-stream-marker-num stream))
               (subseq string start end))
    ))

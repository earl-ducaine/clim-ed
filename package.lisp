;;; **********************************************************************
;;; Copyright (c) 92, 93 Hallvard Traetteberg.  All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice.  Suggestions, comments and bug reports are welcome.  Please
;;; address email to: Hallvard.Tretteberg@si.sintef.no
;;; **********************************************************************



(defpackage :haled
  (:nicknames :ed)
  (:use :clim :common-lisp)
  (:export

   "EDLIN" "EDLIN-STR" "EDLIN-MODIFIED-AT" "EDLIN-CONSTRUCTOR"

   "MAKE-MARKER" "MARKER-LINE" "MARKER-COL" "MARKER-VALUES"
   "MARKER-=" "MARKER-<=" "MARKER-<"

   "HALED" "ED-MODIFIED-LINES" "ED-LINES" "ED-LINE" "MAKE-STRING-FROM-ED"

   "MAKE-MARKERS" "MARKERS-MARKER" "MAP-INTO-MARKERS"
   "ED-MARKERS"

   "DEFINE-ED-MARKER"
   "ED-MARKER" "ED-POINT" "ED-MARK"
   "ED-REPLACED-MARKERS" "ED-READ-ONLY-MARKERS" "ED-STREAM-MARKERS"
   "INITIAL-MARKER" "ALLOCATE-ED-MARKER" "DEALLOCATE-ED-MARKER"

   "FORWARD-LINE" "FORWARD-CHAR"
   "BEGINNING-OF-LINE" "END-OF-LINE" "BEGINNING-OF-TEXT" "END-OF-TEXT"

   "FOLLOWING-CHAR" "PRECEDING-CHAR"

   "MAKE-REGION" "REGION-FROM" "REGION-TO" "MARKER-IN-REGION-P"

   "MARK-REGION-AS-READ-ONLY" "UNMARK-READ-ONLY-REGION"

   "ED-ERROR" "ED-READ-ONLY-ERROR"
   "ED-COMMAND-ERROR" "UNKNOWN-ED-COMMAND" "ILLEGAL-ED-COMMAND"

   "REGION-STRING"
   "ED-TEXT-AS-STRING"

   "ED-VALID-FUN"
   "MARK-AS-MODIFIED" "INSERT" "REPLACE-REGION" "UNDO-REPLACE" "TABULATE"

   "MOTION-REGION-FUNCALL" "MOTION-DELETE" "MOTION-REPLACE"

   "*HALED-COMMAND-ALIST*"

   "INSTALL-ED-COMMAND" "DUPLICATE-ED-COMMAND"
   "DEFINE-ED-COMMAND" "DEFINE-ED-COMMAND-DUPLICATES"
   "DEFINE-ED-MOTION" "DEFINE-ED-MOVER" "DEFINE-ED-CHANGER"
   "ED-COMMAND-LOOKUP" "ED-COMMAND"
   "PREVIOUS-LINE" "NEXT-LINE" "BACKWARD-CHAR" "FORWARD-CHAR"
   "BEGINNING-OF-LINE" "END-OF-LINE" "BEGINNING-OF-TEXT" "END-OF-TEXT"
   "SEARCH-BACKWARD" "SEARCH-FORWARD"
   "REPLACE-REGION" "TRANSFORM-REGION"
   "DELETE-REGION" "DELETE-CHAR" "DELETE-CHAR-BACKWARD"
   "DELETE-TO-BEGINNING" "DELETE-TO-END"

   "SWAP-POINT-MARK" "SET-MARK" "UNSET-MARK"
   "YANK" "UNDO" "KNAY" "UNDO-REPLACE"

   "UNKNOWN-HALED-COMMAND" "ILLEGAL-HALED-COMMAND"
   "HALED-READ-ONLY-ERROR" "HALED-MARK-NOT-SET-ERROR"

   "IGNORE-HALED-COMMAND-ERROR-RESTART"
   "USE-POINT-INSTEAD-OF-MARK-RESTART"
   )
  )

(defpackage :halwed
  (:nicknames :wed)
  (:use :common-lisp :haled)
  (:export
   "HALWED"
   "WED-WIN"
   "LINE-LENGTH"
   "LAYOUT-WED" "UPDATE-WED-LAYOUT" "REFRESH-WED"
   "POSITION-MARKER" "POSITION-SET-MARKER"

   "WED-COMMAND"
   "REFRESH" "LAYOUT"
   )
  )

(defpackage :haled-stream
  (:nicknames "EDS")
  (:use "COMMON-LISP")
  (:export
   "HALED-STREAM" "HALED-INPUT-STREAM" "HALED-OUTPUT-STREAM"
   "ED-STREAM-ED"
   ))

(defpackage :halwed-stream
  (:nicknames "WEDS")
  (:use "COMMON-LISP" "HALED-STREAM")
  (:export
   "HALWED-STREAM" "HALWED-INPUT-STREAM" "HALWED-OUTPUT-STREAM"
   ))

(defpackage :halwed-frame
  (:nicknames "WEDF")
  (:use :clim :common-lisp :halwed :halwed-stream)
  (:export
   "TEXT-EDITOR-COMTAB" "LISP-EDITOR-COMTAB"

   "WED-FRAME" "WED-TOOL"

   "*HALWED-TOOL-COMMAND-ALIST*"

   "ACTIVATE-WED-FRAME"
   "ACTIVATE-WED-FRAME-WITH-INIT-FUN"
   "ACTIVATE-WED-FRAME-WITH-STREAM-FUN"
   "ACTIVATE-WED-WITH-PRINT-FUN"
   ))


(defvar clim::+BACKGROUND+ nil)
(export 'clim::+BACKGROUND+ :clim)

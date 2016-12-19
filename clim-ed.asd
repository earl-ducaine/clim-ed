
(defvar clim::+BACKGROUND+ nil)
(export 'clim::+BACKGROUND+ :clim)

(asdf:defsystem :clim-ed
  :depends-on (:clim2 :alexandria)
  :components
  ((:file "ed-packages")
   (:file "ed")
   (:file "ed-commands")
   (:file "wed")
   (:file "ed-stream")
   (:file "wed-stream")
   (:file "wed-frame")
   (:file "wed-frame-commands")))


(asdf:defsystem :clim-ed
  :depends-on (:clim2 :alexandria)
  :components
  ((:file "package")
   (:file "ed")
   (:file "ed-commands")
   (:file "wed")
   (:file "ed-stream")
   (:file "wed-stream")
   (:file "wed-frame")
   (:file "wed-frame-commands")
   (:file "site-specific")))

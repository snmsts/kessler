;;don't edit
(defsystem "kessler"
  :depends-on("cl-ppcre")
  :class :package-inferred-system
  :components((:file "detect")
              (:file "main"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")

(uiop/package:define-package :kessler/main (:nicknames) (:use :cl) (:shadow)
                             (:export :main) (:intern))
(in-package :kessler/main)
;;don't edit above
(defun main (argv)
  ;; write optparse
  (let* ((in (first argv))
         (out (second argv))
         (import-language (kessler/detect:detect in))
         (in (or (uiop:directory-exists-p (pathname (format nil "~A/" in)))
                 (uiop:file-exists-p (pathname in))))
         (out (when out (if (uiop:directory-pathname-p out)
                            out
                            (format nil "~A~A"
                                    out
                                    (uiop:directory-separator-for-host)))))
         (export-language "cl")
         (syntax-tree
           (when import-language
             (format t "import ~A ~S~%" import-language in)
             (ql:quickload (format nil "kessler.import.~A" import-language) :silent t)
             (funcall (read-from-string (format nil "kessler.import.~A/main:main" import-language))
                      in
                      out))))
    (when syntax-tree
      (if (and out export-language)
          (progn 
            (format t "export ~A ~S~%" export-language out)
            (ql:quickload (format nil "kessler.export.~A" export-language) :silent t)
            (funcall (read-from-string (format nil "kessler.export.~A/main:main" export-language))
                     syntax-tree in out))
          (format t "exported syntax :~%~S ~%~S~%~S~%" syntax-tree in out)))))

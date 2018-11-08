(uiop/package:define-package :kessler.export.cl/main (:nicknames) (:use :cl)
                             (:shadow) (:export :main) (:intern))
(in-package :kessler.export.cl/main)
;;don't edit above
(defvar *mods* nil)

(defun ->lisp-type (elt)
  (cond ((equal elt "f64")
         :double)))

(defun generate-lisp-package (elt)
  (let ((str (format nil "~A:~A" (first elt) (second elt))))
    (if (find str *mods* :test 'equal)
        ""
        (progn
          (push str *mods*)
          (format nil "~A~%~A~%" 
                  `(defpackage ,(format nil ":~A_~A"
                                          (substitute #\_ #\/ (first elt))
                                          (string-downcase (second elt))
                                          ) (":use" ":cl"))
                  `(in-package ,(format nil ":~A_~A"
                                          (substitute #\_ #\/ (first elt))
                                          (string-downcase (second elt))
                                          )))))))
(defun generate-lisp-val (elt)
  (format nil "~A;;val~%~S~%~S~%" 
          (generate-lisp-package elt)
          `(export (cffi:defcfun ,(format nil "~A_~A__~A"
                                  (substitute #\_ #\/ (first elt))
                                  (string-downcase (second elt))
                                  (first (third elt)))
               ,(->lisp-type (second (third elt))) (p :pointer)))
          `(export (cffi:defcfun 
               ,(format nil "~A_set_~A_~A"
                        (substitute #\_ #\/ (first elt))
                        (string-downcase (second elt))
                        (first (third elt)))
               :void
             (p :pointer)
             (val ,(->lisp-type (second (third elt))))))))

(defun generate-lisp-fn (elt)
  (if (fourth (third elt)) ;; ignore complex function for now
      ""
      (format nil "~A;;fn~%~A~%" 
              (generate-lisp-package elt)
              `(export ("cffi:defcfun" ,(format nil "\"~A_~A_~A\""
                                                (substitute #\_ #\/ (first elt))
                                                (string-downcase (second elt))
                                                (first (third elt)))
                        ,(if (equal (first (third elt)) "new")
                             ":pointer"
                             ":void")
                        ,@(mapcar (lambda (x)
                                    (if (equal x "&mut self")
                                        '"(self :pointer)"
                                        (let ((a (mapcar (lambda (x) (string-trim " " x))
                                                         (uiop:split-string x :separator '(#\:)))))
                                          (list (first a) (format nil "~S" (->lisp-type (second a)))))))
                                  (second (third elt))))))))

(defun generate-lisp (elt)
  (funcall (read-from-string (format nil "generate-lisp-~A" (first elt)))
           (rest elt)))

(defun main (syntax origin dest)
  (let ((*package* (find-package :kessler.export.cl/main)))
    (with-open-file (o (merge-pathnames "i.lisp" dest)
                       :direction :output 
                       :if-exists :supersede)
      (format o "(let* ((rust-path (merge-pathnames \"export/\" (make-pathname :defaults (or #.*compile-file-pathname*
                                                                                *load-pathname*)
                                                                  :type nil
                                                                  :name nil)))
             (path (make-pathname :defaults (merge-pathnames \"target/release/\" rust-path)
                                  :name \"libexport\"
                                  :type (or #+darwin \"dylib\" \"so\"))))
(ignore-errors (cffi:load-foreign-library path)))~%")
      (dolist (i (mapcar #'generate-lisp syntax))
        (princ i o)))
    (with-open-file (o (make-pathname
                        :defaults dest
                        :name (first (last (pathname-directory dest)))
                        :type "asd")
                       :direction :output 
                       :if-exists :supersede)
      (format o "~S"
              `(defsystem ,(first (last (pathname-directory dest)))
                 :depends-on(:cffi)
                 :components((:file "i")))))))

(uiop/package:define-package :kessler/detect (:nicknames) (:use :cl) (:shadow)
                             (:export :detect) (:intern))
(in-package :kessler/detect)
;;don't edit above

(defun list-detect-files-1 ()
  (directory (merge-pathnames "*.lisp" (asdf:system-relative-pathname :kessler "detect/"))))

(defvar *list-detect-files* '(list-detect-files-1))

(defun list-detect-files ()
  (loop for fun in *list-detect-files*
         append (funcall fun)))

(defun detect-script-call (file input)
  (ignore-errors
   (funcall (progn
              (load file)
              (read-from-string (format nil "kessler/detect/~A::detect" (pathname-name file))))
            input)))

(defun detect (input)
  (let ((pathname (or (uiop:directory-exists-p (pathname (format nil "~A/" input)))
                      (uiop:file-exists-p (pathname input)))))
    (loop for file in (list-detect-files)
          when (detect-script-call file input)
          return it)))


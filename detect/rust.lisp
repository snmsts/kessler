(uiop/package:define-package :kessler/detect/rust (:use :cl))
(in-package :kessler/detect/rust)

(defun detect (input)
  (and 
   (uiop:directory-exists-p input)
   "rust"))

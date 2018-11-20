(uiop/package:define-package :kessler.import.rust/main (:nicknames) (:use :cl)
                             (:shadow) (:export :main) (:intern))
(in-package :kessler.import.rust/main)
;;don't edit above


(defvar *path* nil)
(defvar *libname* nil)

(defun parse-val (moudle struct val)
  (list :val moudle struct (cdr (remove "" (uiop:split-string (string-left-trim " " val) :separator  '(#\Space #\: #\,)) :test 'equal))))

(defun parse-fn (module impl fn)
  `(:fn ,module ,impl 
    ,(let ((split (uiop:split-string (aref fn 2) :separator '(#\) #\())))
       `(;;,fn
         ,(aref fn 0)
         ,(mapcar (lambda (x) (string-left-trim " " x))
                  (uiop:split-string (first split) :separator '(#\,)))
         ,(let ((i (nth-value 1 (cl-ppcre:scan-to-strings "-> ([^ ]*)" (aref fn 3)))))
            (and i (aref i 0)))
         ,(aref fn 1)))))

(defun parse-impl (module)
  (let ((file (or (probe-file (merge-pathnames (format nil "~A.rs"     module) *path*))
                  (probe-file (merge-pathnames (format nil "~A/mod.rs" module) *path*)))))
    (loop with impl_
          with struct_
          for line in (uiop:read-file-lines file)
          for struct = (nth-value 1 (cl-ppcre:scan-to-strings "pub struct ([^ ]*) {" line))
          for impl = (nth-value 1 (cl-ppcre:scan-to-strings "^impl ([^ ]*) {$" line))
          for fn = (nth-value 1 (cl-ppcre:scan-to-strings "^    pub fn ([^<(]+)(<.+>)?\\((.*)\\) (.*){$" line))
          for val = (cl-ppcre:scan-to-strings "    pub ([^:]*): ([^,]*),$" line)
          when (equal "}" line) do (setf impl_ nil struct nil)
          when impl do (setf impl_ (aref impl 0))
          when struct do (setf struct_ (aref struct 0))
          when (and struct_ val) collect (parse-val module struct_ val)
          when fn collect (parse-fn module impl_ fn))))

(defun list-module-in-dir (symbol dir &key path)
  (let ((file (make-pathname :defaults dir
                             :type "rs"
                             :name symbol)))
    (if (probe-file file)
        (list-module-files file :path path)
        (list-module-files (merge-pathnames (format nil "~A/mod.rs" symbol) dir) :path path))))

(defun parse-line-mod (line)
  (and (find #\; line)
       (let ((parsed (remove "" (uiop:split-string line :separator '(#\Space #\tab #\;))
                             :test #'equal)))
         (and (find "mod" parsed :test 'equal)
              (if (find "pub" parsed :test 'equal)
                  (first (last parsed))
                  :not)))))

(defun list-module-files (file &key path)
  (with-open-file (in file)
    (loop for line = (parse-line-mod (read-line in nil nil))
          while line
          when (stringp line)
          collect (format nil "~{~A~^/~}" `(,@path ,line))
          when (stringp line)
          append (list-module-in-dir line (make-pathname :defaults file :type nil :name nil)
                                     :path `(,@path ,line)))))

(defvar *mods* nil)

(defun generate-rust-use (elt)
  (let ((str (format nil "~A:~A" (first elt) (second elt))))
    (if (find str *mods* :test 'equal)
        ""
        (progn
          (push str *mods*)
          (format nil "use ffi::~A::~A as ~A;~%"
                  *libname*
                  (let ((a (first elt)))
                    (format nil "~{~A~^::~}" (uiop:split-string a :separator '(#\/))))
                  (substitute #\_ #\:
                              (substitute #\_ #\/ (string-downcase str))))))))

(defun generate-rust-val (elt)
  (format nil "~A~A~%"
          (format nil "~A#[no_mangle]
pub extern  fn ~A_~A__~A(
    _self: *mut ~A_~A::~A,
) -> ~A {
    unsafe { (*_self).~A }
}"
                  (generate-rust-use elt)
                  (substitute #\_ #\/ (first elt))
                  (string-downcase (second elt))
                  (first (third elt))
                  (substitute #\_ #\/ (first elt))
                  (string-downcase (second elt))
                  (second elt)
                  (second (third elt))
                  (first (third elt)))
          (format nil "
#[no_mangle]
pub extern fn ~A_set_~A_~A(
    _self: *mut ~A_~A::~A,
    val: ~A,
) {
    unsafe { (*_self).~A= val; }
}"
                  (substitute #\_ #\/ (first elt))
                  (string-downcase (second elt))
                  (first (third elt))
                  (substitute #\_ #\/ (first elt))
                  (string-downcase (second elt))
                  (second elt)
                  (second (third elt))
                  (first (third elt)))))

(defun generate-rust-fn (elt)
  (if (fourth (third elt)) ;; ignore complex function for now
      ""
      (format nil "~A#[no_mangle]
pub extern fn ~A_~A_~A(
~{    ~A,~^~%~}
) ~A{
    ~A
}~%"
              (generate-rust-use elt)
              (substitute #\_ #\/ (first elt))
              (string-downcase (second elt))
              (first (third elt))
              (mapcar (lambda (x)
                        (if (equal x "&mut self")
                            (format nil "mut _self: ~A_~A::~A"
                                    (substitute #\_ #\/ (first elt))
                                    (string-downcase (second elt))
                                    (second elt))
                            x))
                      (second (third elt)))
              (if (zerop (length (third (third elt))))
                  ""
                  (format nil "-> ~A~A_~A::~A "
                          (if (equal (first (third elt)) "new")
                              "*mut " "")
                          (substitute #\_ #\/ (first elt))
                          (string-downcase (second elt))
                          (third (third elt))))
              (if (equal (first (third elt)) "new")
                  (format nil "Box::into_raw(Box::new(~A_~A::~A::~A(~{~A~^, ~})))"
                          (substitute #\_ #\/ (first elt))
                          (string-downcase (second elt))
                          (second elt)
                          (first (third elt))
                          (mapcar (lambda (x) 
                                    (first (uiop:split-string x :separator '(#\:))))
                                  (second (third elt))))
                  (format nil "_self.~A(~{~A~^, ~})" 
                          (first (third elt))
                          (mapcar (lambda (x) 
                                    (first (uiop:split-string x :separator '(#\:))))
                                  (rest (second (third elt)))))))))

(defun generate-rust (elt)
  (funcall (read-from-string (format nil "generate-rust-~A" (first elt)))
           (rest elt)))

(defun toml-libname (path)
  (let ((toml (cl-toml:parse-file (merge-pathnames "Cargo.toml" path)))
         *)
    (or 
     (and (setf * (gethash "lib" toml))
          (setf * (gethash "name" *)))
     (and (setf * (gethash "package" toml))
          (setf * (gethash "name" *))))))

(defun main (origin dest)
  (uiop:run-program (format nil "cd ~A;rustfmt src/lib.rs" origin) :ignore-error-status t)
  (force-output)
  (ignore-errors
   (let* ((path (uiop:getcwd))
          (modules (list-module-files (merge-pathnames "src/lib.rs" origin)))
          (*libname* (toml-libname origin))
          (*path* (merge-pathnames "src/" origin))
          (parsed (apply #'append (remove nil (mapcar #'parse-impl modules)))))
     (when dest
       (let ((dest (merge-pathnames "export/" 
                                    (if (uiop:directory-pathname-p dest)
                                        dest
                                        (format nil "~A~A"
                                                dest
                                                (uiop:directory-separator-for-host))))))
         (ensure-directories-exist dest)
         (uiop:run-program (format nil "cd ~A;cargo init --lib" dest) :ignore-error-status t)
         (with-open-file (o (merge-pathnames "src/lib.rs" dest)
                            :direction :output
                            :if-exists :supersede)
           (format o "pub mod ffi;~%"))
         (with-open-file (o (merge-pathnames "src/ffi.rs" dest)
                            :direction :output
                            :if-exists :supersede)
           (format o "extern crate ~A;~%" *libname*)
           (let* ((*package* (find-package :kessler.import.rust/main)))
             (dolist (i (mapcar #'generate-rust parsed))
               (format o "~A" i))
             (format o "~%")))
         (let* ((lib (first (last (pathname-directory origin))))
                (toml-path (merge-pathnames "Cargo.toml" dest))
                (toml (cl-toml:parse-file toml-path))
                (* (setf (gethash "dependencies" toml)
                         (or (gethash "dependencies" toml)
                             (cl-toml::make-table))))
                (* (setf (gethash lib *)
                         (or (gethash lib *)
                             (cl-toml::make-table))))
                (* (setf (gethash "path" *)
                         (format nil "~A" origin)))
                (* (setf (gethash "lib" toml)
                         (or (gethash "lib" toml)
                             (cl-toml::make-table))))
                (* (setf (gethash "crate-type" *)
                         (cl-toml::make-toml-array))))
           (cl-toml::toml-array-push * "dylib")
           (setf * (setf (gethash "profile" toml)
                         (or (gethash "profile" toml)
                             (cl-toml::make-table))))
           (setf * (setf (gethash "dev" *)
                         (or (gethash "dev" *)
                             (cl-toml::make-table))))
           (setf (gethash "opt-level" *) 3)
           (setf (gethash "debug" *) 'cl-toml:false)
           (with-open-file (o (merge-pathnames "Cargo.toml" dest)
                              :direction :output
                              :if-exists :supersede)
             (cl-toml:encode toml o)))
         (uiop:run-program (format nil "cd ~A;cargo build --release" dest)
                           :output :interactive
                           :error-output :interactive)))
     parsed)))

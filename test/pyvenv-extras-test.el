;;; pyvenv-extras-test.el --- Tests for projectile and persp-mode-based Python virtual environment tracking, and more
;;
;; Copyright (c) 2018-2021 Brandon T. Willard
;;
;; Author: Brandon T. Willard
;; URL: https://github.com/brandonwillard/pyvenv-extras
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'pyvenv)
(require 'projectile)
(require 'persp-mode)
(require 'pyvenv-extras)

(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

;;; Test Utilities

(defmacro projectile-test-with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory.

Taken from `projectile's `projectile-test.el'.
"
  (declare (indent 0) (debug (&rest form)))
  `(let ((sandbox (expand-file-name
                   (convert-standard-filename "test/sandbox/")
                   (file-name-directory (locate-library "pyvenv-extras.el" t)))))
     (when (file-directory-p sandbox)
       (delete-directory sandbox t))
     (make-directory sandbox t)
     (let ((default-directory sandbox))
       ,@body)))

(defmacro projectile-test-with-files (files &rest body)
  "Evaluate BODY in the presence of FILES.
You'd normally combine this with `projectile-test-with-sandbox'.

Taken from `projectile's `projectile-test.el'.
"
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn
     ,@(mapcar (lambda (file)
                 (if (string-suffix-p "/" file)
                     `(make-directory ,file t)
                   `(with-temp-file ,file)))
               files)
     ,@body))

;;; Tests

(ert-deftest test-switch-pyvenv ()
  (projectile-test-with-sandbox
   (projectile-test-with-files
    ("project-1/"
     "project-1/.projectile"
     "project-1/.dir-locals.el"
     "project-1/file-2.py"
     "project-2/"
     "project-2/.projectile"
     "project-2/.dir-locals.el"
     "project-2/file-2.py")

    (let ((project-1-dir (file-name-as-directory (expand-file-name "project-1")))
          (project-2-dir (file-name-as-directory (expand-file-name "project-2"))))

      (with-temp-file (f-join project-1-dir ".dir-locals.el")
        "((nil . ((pyvenv-workon . \"project-env-1\"))))")

      (with-temp-file (f-join project-2-dir ".dir-locals.el")
        "((nil . ((pyvenv-workon . \"project-env-2\"))))")

      (pyvenv-mode +1)
      (pyvenv-extras-mode +1)
      (pyvenv-persp-tracking-mode +1)

      (projectile-add-known-project project-1-dir)
      (projectile-add-known-project project-2-dir)

      ;; Switch to the first project
      (cd "project-1")

      (should (equal (projectile-project-name) "project-1"))
      (should (equal (projectile-project-root) project-1-dir))

      ;; Switch to the second project
      (cd "../project-2")

      (should (equal (projectile-project-name) "project-2"))
      (should (equal (projectile-project-root) project-2-dir))

      ;; TODO: Add real tests!

      ))))

;;; pyvenv-extras-test.el ends here

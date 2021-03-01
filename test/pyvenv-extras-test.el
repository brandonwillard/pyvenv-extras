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

(defmacro with-file-buffer-hack-local (file &rest body)
  (declare (indent 1) (debug (&rest form)))
  `(let ((file-buffer (create-file-buffer ,file)))
     (with-current-buffer file-buffer
       (hack-dir-local-variables)
       (hack-local-variables-apply)
       ,@body)))

(cl-defun env-bin-paths-to-env-name (paths &optional (split-str "/envs/"))
  (mapcar #'(lambda (x) (s-chop-suffix "/bin" (cadr (s-split split-str x))))
          (seq-filter #'(lambda (x) (s-contains? split-str x)) paths)))

;;; Tests

(ert-deftest test-switch-pyvenv ()
  (projectile-test-with-sandbox
   (projectile-test-with-files
    ("envs/"
     "envs/project-1-env/"
     "envs/project-1-env/bin/"
     "envs/project-1-env/bin/activate"
     "envs/project-2-env/"
     "envs/project-2-env/bin/"
     "envs/project-2-env/bin/activate"
     "project-1/"
     "project-1/.projectile"
     "project-1/.dir-locals.el"
     "project-1/file-2.py"
     "project-2/"
     "project-2/.projectile"
     "project-2/.dir-locals.el"
     "project-2/file-2.py")

    (let ((project-1-dir (file-name-as-directory (expand-file-name "project-1")))
          (project-2-dir (file-name-as-directory (expand-file-name "project-2")))
          (venvs-dir (file-name-as-directory (expand-file-name "envs"))))

      ;; Debugging:
      ;; (advice-add #'pyvenv-workon :before #'(lambda (&rest r) (message "pyvenv-workon: %s" r)))
      ;; (advice-add #'pyvenv-deactivate :before #'(lambda () (message "pyvenv-deactivate: %s" pyvenv-virtual-env)))

      (setenv "WORKON_HOME" (expand-file-name "envs"))

      (should-not (equal pyvenv-virtual-env-name "project-1-env"))

      (with-temp-file (f-join project-1-dir ".dir-locals.el")
        (insert "((nil . ((pyvenv-workon . \"project-1-env\"))))"))

      (with-temp-file (f-join project-2-dir ".dir-locals.el")
        (insert "((nil . ((pyvenv-workon . \"project-2-env\"))))"))

      (persp-mode +1)
      (pyvenv-mode +1)
      (pyvenv-extras-mode +1)
      (pyvenv-projectile-tracking-mode +1)
      (pyvenv-persp-tracking-mode +1)

      (projectile-add-known-project project-1-dir)
      (projectile-add-known-project project-2-dir)

      ;; This will create a perspective and associate it with project-1
      (persp-switch project-1-dir)

      ;; Switch to the first project
      (cd "project-1")

      ;; Open a buffer for a file in project-2
      (with-file-buffer-hack-local (f-join project-2-dir "file-2.py")
        (pyvenv-extras//run-in-pyvenv
         ;; We're still in the project-1 directory, so the venv should reflect
         ;; that
         (should (equal pyvenv-virtual-env-name "project-1-env"))
         ;; There should only be one path entry for the venv
         (should (equal (env-bin-paths-to-env-name exec-path "/sandbox/envs/") '("project-1-env")))
         ;; (message "file-2: %s - %s - %s - %s" pyvenv-virtual-env-name (projectile-project-name)
         ;;          (safe-persp-name (get-frame-persp))
         ;;          (env-bin-paths-to-env-name exec-path "/sandbox/envs/"))
         ))

      ;; Open a buffer for a file in project-1
      (with-file-buffer-hack-local (f-join project-1-dir "file-1.py")
        (pyvenv-extras//run-in-pyvenv
         (should (equal pyvenv-virtual-env-name "project-1-env"))
         (should (equal (env-bin-paths-to-env-name exec-path "/sandbox/envs/") '("project-1-env")))
         ;; (message "file-1: %s - %s - %s - %s" pyvenv-virtual-env-name (projectile-project-name)
         ;;          (safe-persp-name (get-frame-persp))
         ;;          (env-bin-paths-to-env-name exec-path "/sandbox/envs/"))
         ))

      ;; (should (equal (safe-persp-name (get-frame-persp)) project-1-dir))
      (should (equal (projectile-project-name) "project-1"))
      (should (equal (projectile-project-root) project-1-dir))
      ;; (should (equal pyvenv-virtual-env-name "project-1-env"))

      ;; Switch to the second project
      (cd "../project-2")

      ;; This will create a second perspective and associate it with project-2
      (persp-switch project-2-dir)

      (with-file-buffer-hack-local (f-join project-2-dir "file-2.py")
        (pyvenv-extras//run-in-pyvenv
         (should (equal pyvenv-virtual-env-name "project-2-env"))
         ;; (message "file-2: %s - %s - %s - %s" pyvenv-virtual-env-name (projectile-project-name)
         ;;          (safe-persp-name (get-frame-persp))
         ;;          (env-bin-paths-to-env-name exec-path "/sandbox/envs/"))
         ))

      (with-file-buffer-hack-local (f-join project-1-dir "file-1.py")
        (pyvenv-extras//run-in-pyvenv
         (should (equal pyvenv-virtual-env-name "project-2-env"))
         ;; (message "file-1: %s - %s - %s - %s" pyvenv-virtual-env-name (projectile-project-name)
         ;;          (safe-persp-name (get-frame-persp))
         ;;          (env-bin-paths-to-env-name exec-path "/sandbox/envs/"))
         ))

      ;; (should (equal (safe-persp-name (get-frame-persp)) project-2-dir))
      (should (equal (projectile-project-name) "project-2"))
      (should (equal (projectile-project-root) project-2-dir))
      ;; (should (equal pyvenv-virtual-env-name "project-2-env"))

      ;; Switch back to the first project
      ;; (cd "../project-1")
      ;; (should (equal pyvenv-virtual-env-name "project-1-env"))

      ;; TODO: Add more tests!

      ))))

;;; pyvenv-extras-test.el ends here

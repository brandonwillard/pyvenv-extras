;;; pyvenv-extras.el --- Add projectile and persp-mode-based Python virtual environment tracking using pyvenv, and more
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
;; Add `projectile' and `persp-mode'-based Python virtual environment tracking using `pyvenv', and more

;;; Code:

(require 'cl-lib)
(require 's)
(require 'f)
(require 'python)
(require 'pyvenv)
(require 'projectile)


(defvar pyvenv-extras--pyvenv-virtual-env-name-prev
  nil
  "Name of the previously active virtual env; nil otherwise")

(defvar pyvenv-extras--pyvenv-virtual-env-prev
  nil
  "Path of the previously active virtual env; nil otherwise")

(defvar pyvenv-extras--pyvenv-last-scope
  nil
  "The last scope (e.g. buffer, project) considered by `pyvenv-track-virtualenv'.")

;; Make the `pyvenv' variables buffer-local
(defvar-local pyvenv-workon nil)

(defvar-local pyvenv-virtual-env nil)

(defvar-local pyvenv-virtual-env-name nil)

(with-eval-after-load 'pyvenv
  ;; Because we made the core `pyvenv' variables buffer-local, we
  ;; need update `pyvenv-deactivate'; otherwise, it won't be able to
  ;; deactivate environments in some situations.

  (defun pyvenv-extras//pyvenv-deactivate (orig-func)
    (if (and pyvenv-extras--pyvenv-virtual-env-prev (not pyvenv-virtual-env))
        ;; Make sure that the previous venv is deactivated.
        ;; We need to use `pyvenv-extras--pyvenv-virtual-env-prev',
        ;; because `pyvenv-virtual-env' could be buffer-local.
        (let ((pyvenv-virtual-env pyvenv-extras--pyvenv-virtual-env-prev))
          (funcall orig-func))
      (funcall orig-func)))

  (advice-add #'pyvenv-deactivate :around #'pyvenv-extras//pyvenv-deactivate))

(defun pyvenv-extras//get-project-local-venv ()
      "Returns projectile-specific `pyvenv-workon' and
 `python-shell-virtualenv-root' values obtained from a `projectile' project's
 `dir-locals-class-alist'."
      (when-let* ((project-locals (ignore-errors
                               (alist-get (intern
                                           (expand-file-name (projectile-project-root)))
                                          dir-locals-class-alist)))
             (project-locals-nil (alist-get nil project-locals))
             (pyvenv-workon (or (cdr (assoc 'pyvenv-workon project-locals-nil))
                                (bound-and-true-p pyvenv-workon)))
             (python-shell-virtualenv-root (or (format "%s/%s" (getenv "WORKON_HOME")
                                                       (bound-and-true-p pyvenv-workon))
                                               (bound-and-true-p python-shell-virtualenv-root))))
        (cons pyvenv-workon python-shell-virtualenv-root)))

(defun pyvenv-extras//project-process-name (proc-name)
  (let ((proj-name (with-demoted-errors "Error: %S" (projectile-project-name))))
    (if (and proj-name
             (not (s-suffix? (format "<%s>" proj-name) proc-name)))
        (format "%s<%s>" proc-name proj-name)
      proc-name)))

(defun pyvenv-extras//python-shell-get-process-name (orig-func dedicated)
  (pyvenv-extras//project-process-name (funcall orig-func dedicated)))

(defmacro pyvenv-extras//run-in-pyvenv (&rest forms)
  "Provides a projectile-specific `pyvenv-workon' environment via
`dir-locals-class-alist'.

This macro searches in dir-locals for a `pyvenv-workon' value."
  (declare (indent 1) (debug (&rest form)))
  `(let* ((env-info (pyvenv-extras//get-project-local-venv))
          (pyvenv-workon-res (car env-info))
          (python-shell-virtualenv-root (cdr env-info)))
     (pyvenv-workon pyvenv-workon-res)
     ,@forms))

(defun pyvenv-extras//pyvenv-track-projectile-virtualenv (oldfun &rest args)
  "Functions like `pyvenv-track-virtualenv', but only considers changing the
venv when projectile-associated venvs change.

When projectile is altered to have `persp-mode'-scoped projects, this
effectively enables `persp-mode' virtualenv scopes."
  (let ((proj-name (with-demoted-errors "Error: %S" (projectile-project-name))))
    (unless (string-equal proj-name
                          ;; Do this to avoid variable type/content switching issues.
                          (format "%s" pyvenv-extras--pyvenv-last-scope))
      ;; Make sure we use the venv given by the new project and not any
      ;; buffer-local value, since buffers from other projects may be visiting
      ;; in the current project.
      ;; XXX: This consideration is limited to only projects that set
      ;; `pyvenv-workon' via dir-local values.
      (pyvenv-extras//run-in-pyvenv
       (progn
         (setq pyvenv-extras--pyvenv-last-scope proj-name)
         (apply oldfun args))))))

(defun pyvenv-extras//pyvenv-conda-activate-additions ()
  (pyvenv-extras//run-in-pyvenv
    (setq pyvenv-extras--pyvenv-virtual-env-name-prev pyvenv-virtual-env-name)
    (setq pyvenv-extras--pyvenv-virtual-env-prev pyvenv-virtual-env)
    (when (and (bound-and-true-p pyvenv-virtual-env)
              (bound-and-true-p pyvenv-virtual-env-name))
      (setenv "CONDA_PREFIX"
              (string-remove-suffix "/" pyvenv-virtual-env))
      (setenv "CONDA_DEFAULT_ENV" pyvenv-virtual-env-name))))

(defun pyvenv-extras//pyvenv-conda-deactivate-additions ()
  (setenv "CONDA_PREFIX" nil)
  (setenv "CONDA_DEFAULT_ENV" nil))

(defun pyvenv-extras//pyvenv-conda-env-term-init (send-string-func)
  "Activate the current env in a newly opened shell PROCESS.

Inspired by https://github.com/necaris/conda.el/blob/master/conda.el#L339"
  (pyvenv-extras//run-in-pyvenv
    (-when-let* ((pyvenv-env-name (or (bound-and-true-p pyvenv-workon)
                                      (bound-and-true-p pyvenv-virtual-env-name)))
                (activate-command (if (eq system-type 'windows-nt)
                                      '("activate")
                                      ;'("source" "activate")
                                    '("conda" "activate")))
                (full-command (append activate-command
                                      `(,pyvenv-env-name "\n")))
                (command-string (combine-and-quote-strings full-command)))
      (progn
        (message "sending %s to %S" command-string (current-buffer))
        (funcall send-string-func command-string)))))

(defun pyvenv-extras//pyvenv-track-buffer-virtualenv (oldfun &rest args)
  "Functions like `pyvenv-track-virtualenv', but only checks when buffers are
changed."
  (pyvenv-extras//run-in-pyvenv
    (unless (eq (current-buffer)
                pyvenv-extras--pyvenv-last-scope)
      (setq pyvenv-extras--pyvenv-last-scope (current-buffer))
      ;; (message "(%s) setting local venv %s" "track-buffer" pyvenv-workon)
      (apply oldfun args))))

(cl-defun pyvenv-extras//pyvenv-mode-set-local-virtualenv (&optional (caller-name ""))
  "If the buffer-local `pyvenv-workon' and global `pyvenv-virtual-env-name'
values differ, [re]activate the buffer's `pyvenv-workon' env."
  (pyvenv-extras//run-in-pyvenv
    (when (and (boundp 'pyvenv-workon)
              (local-variable-p 'pyvenv-workon)
              (not (string-equal pyvenv-workon
                                  (or (ignore-errors (default-toplevel-value 'pyvenv-virtual-env-name))
                                      pyvenv-extras--pyvenv-virtual-env-name-prev))))
      (message "(%s) setting local venv %s" caller-name pyvenv-workon)
      (pyvenv-workon pyvenv-workon))))

(defun pyvenv-extras//persp-after-switch-set-venv (frame-or-window)
  ;; `persp-activate' calls `persp-restore-window-conf', which
  ;; switches/restores the window config for the perspective.  If we don't
  ;; work within the new window's buffer, then we're not making the changes
  ;; we want.
  (with-current-buffer (window-buffer)
    (pyvenv-extras//pyvenv-mode-set-local-virtualenv "persp-switch")))

(defun pyvenv-extras//filter-venvwrapper-supported-anaconda-hooks (pyvenv-res &rest r)
  "If we're using Anaconda envs, do not run virtualenvwrapper hooks."
  (and pyvenv-res
       (not (s-contains? (concat (f-path-separator) "anaconda")
                         pyvenv-res
                         t))))

(defun pyvenv-extras//run-in-pyvenv-wrapper (oldfun &rest args)
  (pyvenv-extras//run-in-pyvenv
   (apply oldfun args)))

(defun pyvenv-extras//python-adjust-adaptive-fill-regexp ()
  "Don't allow `adaptive-fill-regexp' to strip '%'s from lines send to a
 python process "
  (setq-local adaptive-fill-regexp
              (s-replace "%" "" adaptive-fill-regexp)))

(defun pyvenv-extras//pyvenv-restart-python (&rest _)
    "Restart Python inferior processes with virtual environment awareness and
 without cursor jumps."
    ;; TODO: `pyvenv-restart-python' checks `pyvenv-virtual-env-name' and
    ;; `pyvenv-virtual-env' *within* each inferior Python buffer, so we need to
    ;; [re]set those values there (e.g. using the caller's venv values).
    (interactive)
    (save-window-excursion
      (dolist (buf (persp-buffer-list))
        (set-buffer buf)
        (pyvenv-extras//run-in-pyvenv
         (when (and (eq major-mode 'inferior-python-mode)
                    (get-buffer-process buf))
           (let ((cmd (combine-and-quote-strings (process-command
                                                  (get-buffer-process buf))))
                 (dedicated (if (string-match "\\[.*\\]$" (buffer-name buf))
                                t
                              nil))
                 (show nil))
             (delete-process (get-buffer-process buf))
             (insert "\n\n"
                     "###\n"
                     (format "### Restarting in virtualenv %s (%s)\n"
                             pyvenv-virtual-env-name
                             pyvenv-workon
                             ;; pyvenv-virtual-env
                             )
                     "###\n"
                     "\n\n")
             (run-python cmd dedicated show)))))))

(defun pyvenv-extras//set-project-root (func &rest args)
  "Run the wrapped function in the project root directory."
  (let ((default-directory (expand-file-name (or (projectile-project-root) default-directory))))
    (apply func args)))

(defun pyvenv-extras//vterm-init-pyvenv ()
  (pyvenv-extras//pyvenv-conda-env-term-init #'vterm-send-string))

(defun pyvenv-extras//term-init-pyvenv ()
  (pyvenv-extras//pyvenv-conda-env-term-init
   #'(lambda (command-string) (term-send-string (current-buffer) command-string))))

(defun pyvenv-extras//flycheck-python-find-project-root (checker)
  (projectile-project-root))

(defun pyvenv-extras//flycheck-virtualenv-executable-find (executable &rest find-any)
  "Find an EXECUTABLE in the current virtualenv (exclusively) if any."
  ;; TODO: Use `pyvenv-extras//run-in-pyvenv'?
  (if (bound-and-true-p python-mode)
      (if (bound-and-true-p python-shell-virtualenv-root)
          (let ((exec-path (nth 0 (python-shell-calculate-exec-path))))
            (executable-find executable))
        (when find-any
          (executable-find executable)))
    (executable-find executable)))

;;;###autoload
(define-minor-mode pyvenv-buffer-tracking-mode
  "Activate `pyvenv' tracking only on buffer changes."
  :group 'pyvenv-extras
  :require 'pyvenv
  :init-value nil
  :global t
  ;; (advice-member-p 'pyvenv-extras//pyvenv-track-buffer-virtualenv
  ;;                          #'pyvenv-track-virtualenv)
  (if pyvenv-buffer-tracking-mode
      (progn
        (pyvenv-tracking-mode +1)
        (advice-add #'pyvenv-track-virtualenv :around
                    #'pyvenv-extras//pyvenv-track-buffer-virtualenv))
    (progn
      (pyvenv-tracking-mode -1)
      (advice-remove #'pyvenv-track-virtualenv
                     #'pyvenv-extras//pyvenv-track-buffer-virtualenv))))

;;;###autoload
(define-minor-mode pyvenv-projectile-tracking-mode
  "Activate `pyvenv' tracking only on `projectile' project changes."
  :group 'pyvenv-extras
  :require 'pyvenv
  :init-value nil
  :global t
  (if pyvenv-projectile-tracking-mode
      (progn
        (pyvenv-tracking-mode +1)
        (advice-add #'pyvenv-track-virtualenv :around
                    #'pyvenv-extras//pyvenv-track-projectile-virtualenv))
    (progn
      (pyvenv-tracking-mode -1)
      (advice-remove #'pyvenv-track-virtualenv
                     #'pyvenv-extras//pyvenv-track-projectile-virtualenv))))

;;;###autoload
(define-minor-mode pyvenv-persp-tracking-mode
  "Activate `pyvenv' tracking on `persp-mode' perspective changes."
  :group 'pyvenv-extras
  :require 'pyvenv
  :init-value nil
  :global t
  (if pyvenv-persp-tracking-mode
      (add-hook 'persp-activated-functions #'pyvenv-extras//persp-after-switch-set-venv)
    (remove-hook 'persp-activated-functions #'pyvenv-extras//persp-after-switch-set-venv)))

;;;###autoload
(define-minor-mode pyvenv-extras-mode
  "Activate `pyvenv-extras' mode."
  :group 'pyvenv-extras
  :require 'python
  :init-value nil
  :global t
  (if pyvenv-extras-mode
      (progn
        ;; Make interactive Python sessions project-specific by adding
        ;; `projectile' project names to `python-mode' process names.
        (advice-add #'python-shell-get-process-name :around
                    #'pyvenv-extras//python-shell-get-process-name)

        (add-hook 'term-exec-hook #'pyvenv-extras//term-init-pyvenv)
        (add-hook 'vterm-mode-hook #'pyvenv-extras//vterm-init-pyvenv)

        (add-hook 'pyvenv-post-activate-hooks #'pyvenv-extras//pyvenv-conda-activate-additions)
        (add-hook 'pyvenv-post-deactivate-hooks #'pyvenv-extras//pyvenv-conda-deactivate-additions)

        (advice-add #'python-shell-make-comint :around #'pyvenv-extras//set-project-root)

        (advice-add #'pyvenv-restart-python :override #'pyvenv-extras//pyvenv-restart-python)

        (advice-add #'run-python :around #'pyvenv-extras//run-in-pyvenv-wrapper)

        (advice-add #'pyvenv-virtualenvwrapper-supported
                    :filter-return #'pyvenv-extras//filter-venvwrapper-supported-anaconda-hooks)

        (add-hook 'python-mode-hook #'pyvenv-extras//python-adjust-adaptive-fill-regexp)

        (when (featurep 'flycheck)
          (setq flycheck-executable-find
                #'pyvenv-extras//flycheck-virtualenv-executable-find))

        (when (fboundp 'flycheck-python-find-project-root)
          ;; Make sure `flycheck' uses the `projectile' project
          (advice-add #'flycheck-python-find-project-root :override #'pyvenv-extras//flycheck-python-find-project-root)))
    (progn
      (advice-remove #'python-shell-get-process-name #'pyvenv-extras//python-shell-get-process-name)

      (remove-hook 'term-exec-hook #'pyvenv-extras//term-init-pyvenv)
      (remove-hook 'vterm-mode-hook #'pyvenv-extras//vterm-init-pyvenv)

      (remove-hook 'pyvenv-post-activate-hooks #'pyvenv-extras//pyvenv-conda-activate-additions)
      (remove-hook 'pyvenv-post-deactivate-hooks #'pyvenv-extras//pyvenv-conda-deactivate-additions)

      (advice-remove #'python-shell-make-comint #'pyvenv-extras//set-project-root)

      (advice-remove #'pyvenv-restart-python #'pyvenv-extras//pyvenv-restart-python)

      (advice-remove #'run-python #'pyvenv-extras//run-in-pyvenv-wrapper)

      (advice-remove #'pyvenv-virtualenvwrapper-supported
                     #'pyvenv-extras//filter-venvwrapper-supported-anaconda-hooks)
      (remove-hook 'python-mode-hook #'pyvenv-extras//python-adjust-adaptive-fill-regexp)

      (when (featurep 'flycheck)
        (setq flycheck-executable-find #'flycheck-default-executable-find))

      (when (fboundp 'flycheck-python-find-project-root)
        (advice-remove #'flycheck-python-find-project-root #'pyvenv-extras//flycheck-python-find-project-root)))))

(provide 'pyvenv-extras)

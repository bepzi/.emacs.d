;; -*- lexical-binding: t; -*-

;;; init.el --- Entry point for Emacs configuration
;;; Commentary:
;;
;; Loads the literate config file, then runs all of the included
;; elisp.

;;; Code:

(when (version< emacs-version "26.1")
  (warn "You're using %s, which is not supported" (emacs-version)))

;; Automatically follow symlinks without prompting. Necessary if you
;; use symlinks to manage the config files.
(setq vc-follow-symlinks t)

(let ((config-file-org (concat user-emacs-directory "emacs-config.org")))
  (if (file-exists-p config-file-org)
      (org-babel-load-file config-file-org)
    (error "No such config file '%s'" config-file-org)))

(message "init.el loaded in %s\n" (emacs-init-time))

;;; init.el ends here

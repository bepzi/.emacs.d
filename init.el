;; -*- lexical-binding: t; -*-

;;; init.el --- Entry point for Emacs configuration
;;; Commentary:
;;
;; Loads the literate config file, then runs all of the included
;; elisp.

;;; Code:

;; Automatically follow symlinks without prompting. Necessary if you
;; use symlinks to manage the config files.
(setq vc-follow-symlinks t)

;; Some older Emacsen don't have this function.
(unless (fboundp 'org-babel-load-file)
  (error "%s" "No such function 'org-babel-load-file'"))

(let ((config-file-org (concat user-emacs-directory "emacs-config.org"))
      (config-file-el (concat user-emacs-directory "emacs-config.el")))
  (unless (file-exists-p config-file-org)
    (error "No such file '%s'" config-file-org))

  ;; Minor optimization: avoid invoking org-babel if we don't have to.
  (if (file-newer-than-file-p config-file-el config-file-org)
      (load config-file-el)
    (org-babel-load-file config-file-org)))

(message "init.el loaded in %s\n" (emacs-init-time))

;;; init.el ends here

;;; init.el --- Entry point for Emacs configuration
;;; Commentary:
;;
;; Loads the literate config file, then runs all of the included
;; elisp.

;;; Code:

;; Automatically follow symlinks without prompting
;; Necessary since we use symlinks to manage our config files
(setq vc-follow-symlinks t)

;; Only tangle the literate config file if a recent version of Org is
;; installed (some older Emacs don't have this function)
(when (fboundp 'org-babel-load-file)
  (org-babel-load-file "~/.emacs.d/emacs-config.org"))

;;; init.el ends here

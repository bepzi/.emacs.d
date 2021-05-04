;;; init.el --- Entry point for Emacs configuration
;;; Commentary:
;;
;; Loads the literate config file, then runs all of the included
;; elisp.

;;; Code:

;; Automatically follow symlinks without prompting. Necessary if you
;; use symlinks to manage the config files.
(setq vc-follow-symlinks t)

;; Only tangle the literate config file if a recent version of Org is
;; installed, since some older Emacs don't have this function.
(when (fboundp 'org-babel-load-file)
    (let ((config-files (directory-files user-emacs-directory 'FULL "config\.org$")))
      (if (= (length config-files) 1)
          (org-babel-load-file (car config-files))

        (error "%s" "Couldn't find exactly one 'config.org' \
file in the same directory as 'init.el'"))))

;;; init.el ends here

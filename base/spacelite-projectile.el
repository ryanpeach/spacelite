;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacelite/init-projectile ()
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache")
	projectile-known-projects-file (concat user-emacs-directory
					       ".cache/projectile-bookmarks.eld"))
  (use-package 
    recentf) 
  (add-hook 'find-file-hook (lambda () 
			      (unless recentf-mode (recentf-mode) 
				      (recentf-track-opened-file)))) 
  (setq recentf-save-file (concat user-emacs-directory ".cache/recentf") recentf-max-saved-items
	1000 recentf-auto-cleanup 'never recentf-auto-save-timer (run-with-idle-timer 600 t
										      'recentf-save-list))
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir)) 
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'") 
  (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory ".cache/")))
  (use-package 
    projectile 
    :diminish 'projectile-mode) 
  (use-package 
    counsel-projectile
    :diminish 'ivy-mode) 
  (setq projectile-completion-system 'ivy projectile-sort-order 'recentf
	projectile-globally-ignored-directories (append '(".cache")))
  (spacelite/set-leader-keys
    "pbd" 'projectile-kill-buffers
    "pf" 'counsel-projectile-find-file
    "ph" 'counsel-projectile
    "pl" 'counsel-projectile-switch-project
    "p//" 'counsel-projectile-ag
  )
  (projectile-global-mode))

(provide 'spacelite-projectile)

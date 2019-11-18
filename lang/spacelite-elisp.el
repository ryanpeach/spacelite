(defun spacelite/init-elisp () 
  (use-package 
    elisp-format 
    :defer t)

  ;; Auto format elisp on save
  (add-hook 'before-save-hook (lambda () 
				(when (buffer-file-name) 
				  (elisp-format-file (buffer-file-name))))))

(provide 'spacelite-elisp)

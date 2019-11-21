(defun spacelite/init-org ()
  (use-package
    org
    :defer t)
  (use-package htmlize)
  (use-package
    evil-org
    :ensure t
    :after org
    :config (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook (lambda ()
                                    (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(provide 'spacelite-org)

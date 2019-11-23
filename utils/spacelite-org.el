(defun spacelite/init-org ()
  (use-package
    org
    :defer t
    :init
    (spacelite/set-leader-keys-for-major-mode 'org-mode "a" 'org-agenda)
    )
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

(defun spacelite/init-org ()
  (add-hook 'org-mode-hook #'lazy-yas-minor-mode)
  (use-package
    org
    :defer t
    :init
    (spacelite/set-leader-keys-for-major-mode 'org-mode "a" 'org-agenda)
    :config
    (setq org-modules '(org-bbdb
                        org-bibtex
                        org-crypt
                        org-docview
                        org-gnus
                        org-habit
                        org-info
                        org-mhe
                        org-w3m))

    (org-babel-load-languages (quote ((emacs-lisp . t) (ledger . t) (python . t))))
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

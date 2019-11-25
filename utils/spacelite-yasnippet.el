(defun spacelite/init-yasnippet ()
  (use-package yasnippet
    :defer t
    :init
    (use-package yasnippet-snippets
      :defer t)
    (yas-global-mode 1)
    (add-hook 'after-init-hook 'global-company-mode)
    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets") ;; personal snippets
          )
    )

  )

(provide 'spacelite-yasnippet)

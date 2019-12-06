(defun spacelite/init-multi-cursor ()
  (use-package evil-mc
    :ensure t
    :defer t
    :init
    (global-evil-mc-mode 1)
    )
  )

(provide 'spacelite-multi-cursor)

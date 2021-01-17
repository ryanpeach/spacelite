(defun spacelite/init-multi-cursor ()
  (use-package evil-mc
    :ensure t
    :defer t
    :after evil
    :init
    (global-evil-mc-mode 1)
    :config
    (progn
      (evil-global-set-key 'normal [escape] 'evil-mc-undo-all-cursors)
      (evil-global-set-key 'normal (kbd "C-S-<mouse-1>") 'evil-mc-make-cursor-on-click)
      )
    )
  )

(defun evil-mc-make-cursor-on-click (event)
  "Used by C-S-<mouse-1> to make a cursor wherever you click."
  (interactive "e")
  (progn
    (evil-mc-make-cursor-here)
    (mouse-set-point event)))

(provide 'spacelite-multi-cursor)

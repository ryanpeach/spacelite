(defun spacelite/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (spacelite/set-leader-keys
      "ft" 'treemacs
      "pt" 'treemacs-projectile
      "fs" 'treemacs-switch-workspace))
  )

(provide 'spacelite-treemacs)

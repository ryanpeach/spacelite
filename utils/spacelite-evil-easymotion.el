(defun spacelite/init-evil-easymotion ()
  (use-package
    evil-easymotion
    :defer t
    :init
    (evilem-default-keybindings "SPC j")))

(provide 'spacelite-evil-easymotion)

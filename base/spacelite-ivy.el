;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacelite/init-ivy ()
  (use-package
    ivy
    :diminish 'ivy-mode)
  (ivy-mode 1)
  (setq ivy-use-virtual-bufffers t)
  (setq ivy-enable-recursive-minibuffers 1)
  (use-package ag)
  (use-package counsel-projectile
    :diminish 'ivy-mode)
  (spacelite/ivy-init-keybindings))

(defun spacelite/ivy-init-keybindings ()
  (spacelite/set-leader-keys
    "<SPC>" 'counsel-M-x
    "ff"    'counsel-find-file
    "bb"    'counsel-switch-buffer
    "//"    'counsel-ag
    )
  (global-set-key (kbd "M-x") 'counsel-M-x)
  )

(provide 'spacelite-ivy)

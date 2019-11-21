;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacelite/init-git ()
  (use-package
    magit
    :diminish 'auto-revert-mode
    :defer t)
  (spacelite/set-leader-keys
    "gb" 'magit-blame
    "gm" 'magit-dispatch-popup
    "gs" 'magit-status)
  (evil-define-key evil-magit-state magit-mode-map "p" 'magit-push)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  ;; TODO: Not sure why this isn't working
  ;; (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
  ;; (let ((mm-key config-major-mode-leader-key))
  ;;   (dolist (state '(normal motion))
  ;;     (evil-define-key state with-editor-mode-map
  ;;       (concat mm-key mm-key) 'with-editor-finish
  ;;       (concat mm-key "a")    'with-editor-cancel
  ;;       (concat mm-key "c")    'with-editor-finish
  ;;       (concat mm-key "k")    'with-editor-cancel)))
  (use-package
    evil-magit
    :defer t))

(provide 'spacelite-git)

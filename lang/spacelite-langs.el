;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'spacelite-jenkinsfile)
(require 'spacelite-bash)
(require 'spacelite-markdown)
(require 'spacelite-writing)
(require 'spacelite-rustlang)
(require 'spacelite-cloudformation)
(require 'spacelite-python)
(require 'spacelite-elisp)

(defun lazy-load (mode f)
  (add-hook mode f)

  ;; Remove the hook on first load
  (add-hook mode `(lambda ()
                    (when (member (quote ,f) ,mode)
                      (remove-hook (quote ,mode) (quote ,f)))
                    )
            )
  )


(defun spacelite/init-langs ()
  (lazy-load 'groovy-mode-hook 'spacelite/init-jenkinsfile)
  (lazy-load 'shell-mode-hook 'spacelite/init-bash)
  (lazy-load 'markdown-mode-hook 'spacelite/init-markdown)
  (lazy-load 'rust-mode-hook 'spacelite/init-rustlang)
  (lazy-load 'yaml-mode-hook 'spacelite/init-cloudformation)
  (lazy-load 'python-mode-hook 'spacelite/init-python)
  (lazy-load 'emacs-lisp-mode-hook 'spacelite/init-elisp)
  (lazy-load 'org-mode-hook 'spacelite/init-writing)
  (lazy-load 'text-mode-hook 'spacelite/init-writing)
  (lazy-load 'nix-mode-hook 'spacelite/init-nix-mode)
  )

(provide 'spacelite-langs)

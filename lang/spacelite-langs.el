;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'spacelite-haskell)
(require 'spacelite-jenkinsfile)
(require 'spacelite-bash)
(require 'spacelite-markdown)
(require 'writing)
(require 'rustlang)
(require 'cloudformation)
(require 'spacelite-python)
(require 'spacelite-elisp)

(defun spacelite/init-langs ()
  (add-hook 'haskell-mode-hook 'spacelite/init-haskell)
  (add-hook 'groovy-mode-hook 'spacelite/init-jenkinsfile)
  (add-hook 'shell-mode-hook 'spacelite/init-bash)
  (add-hook 'markdown-mode-hook 'spacelite/init-markdown)
  (add-hook 'rust-mode-hook 'rustlang/init)
  (add-hook 'yaml-mode-hook 'cloudformation/init)
  (add-hook 'python-mode-hook 'spacelite/init-python)
  (add-hook 'emacs-lisp-mode-hook 'spacelite/init-elisp)
  (add-hook 'org-mode-hook 'writing/init)
  (add-hook 'text-mode-hook 'writing/init)
  )
(provide 'spacelite-langs)

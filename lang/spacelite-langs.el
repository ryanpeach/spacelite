;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'spacelite-markdown)
(require 'writing)
(require 'rustlang)
(require 'cloudformation)
(require 'python)
(require 'spacelite-elisp)

(defun spacelite/init-langs ()
  (spacelite/init-markdown)
  (writing/init)
  (rustlang/init)
  (cloudformation/init)
  (python/init-python)
  (spacelite/init-elisp))

(provide 'spacelite-langs)

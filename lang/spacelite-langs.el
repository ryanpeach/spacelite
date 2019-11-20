;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'spacelite-bash)
(require 'spacelite-markdown)
(require 'writing)
(require 'rustlang)
(require 'cloudformation)
(require 'spacelite-python)
(require 'spacelite-elisp)

(defun spacelite/init-langs ()
  (spacelite/init-bash)
  (spacelite/init-markdown)
  (writing/init)
  (rustlang/init)
  (cloudformation/init)
  (spacelite/init-python)
  (spacelite/init-elisp))

(provide 'spacelite-langs)

(package-initialize)

(require 'server)
(or (server-running-p)
    (server-start))

(load-file (concat (file-name-directory user-emacs-directory)
                   "core/core-load-paths.el"))

(setq use-package-always-ensure t)
(setq gc-cons-threshold most-positive-fixnum)
(setq-default indent-tabs-mode nil)

(require 'core-spacelite)
(require 'spacelite-base)
(require 'spacelite-langs)
(require 'spacelite-utils)
(require 'spacelite-private)

(spacelite/init)
(spacelite/init-base)
(spacelite/init-langs)
(spacelite/init-utils)
(spacelite/init-private)

(use-package exec-path-from-shell :defer t :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
(load custom-file)

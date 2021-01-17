;; (package-initialize)
(toggle-debug-on-error)

(require 'server)
(or (server-running-p)
    (server-start))

;; Auto install use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(load-file (concat (file-name-directory user-emacs-directory)
                   "core/core-load-paths.el"))
(load-file (concat (file-name-directory user-emacs-directory)
                   "config.el"))

(setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(setq gc-cons-threshold most-positive-fixnum)
(setq-default indent-tabs-mode nil)

(require 'user-config)
(require 'core-spacelite)
(require 'spacelite-base)
(require 'spacelite-langs)
(require 'spacelite-utils)
(require 'spacelite-private)

(spacelite/init)
(spacelite/init-base)
(spacelite/init-utils)
(spacelite/init-langs)
(spacelite/init-private)

(use-package exec-path-from-shell :defer t :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; REF: https://emacs.stackexchange.com/questions/23773/disable-scrollbar-on-new-frame
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
(load custom-file)

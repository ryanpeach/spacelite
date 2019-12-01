;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacelite/init-makeup ()
  ;; hide column numbers
  (setq column-number-mode nil)

  ;; don't hightlight current line
  (global-hl-line-mode nil)

  ;; truncate lines please
  (set-default 'truncate-lines t)

  ;; stop blinking!!
  (blink-cursor-mode 0)

  ;; y or n is enough for me
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; draw underline lower
  (setq x-underline-at-descent-line t)

  ;; hide ui elements
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)

  ;; font
  (set-face-attribute 'default t :font "Inconsolata for Powerline")

  ;; set always fullscreen
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)

  (spacelite//init-theme)
  (spacelite//init-spaceline)

  ;; window numbers
  (use-package winum)
  (winum-mode)

  ;; evil-anzu for improving search result rendering
  (use-package
    evil-anzu
    :config (global-anzu-mode +1)
    :diminish 'anzu-mode)

  ;; relative line numbers + centered mode FTW
  (use-package
    linum-relative
    :diminish 'linum-relative-mode
    :init (setq linum-relative-current-symbol "")
    :config (linum-relative-global-mode))

  (use-package
    centered-cursor-mode
    :config (global-centered-cursor-mode)
    :diminish 'centered-cursor-mode)

  ;; parenthesis
  (show-paren-mode 1)
  (use-package
    highlight-parentheses
    :diminish 'highlight-parentheses-mode
    :config (add-hook 'prog-mode-hook #'highlight-parentheses-mode)))

(defun spacelite//init-theme ()
  (use-package
    monokai-theme
    :ensure t
    :demand t
    :config
    (load-theme 'monokai t)
    )

  ;; face attributes for helm
  (eval-after-load 'helm
    (lambda ()
      (set-face-attribute
       'helm-selection nil :background "#268bd2" :underline nil :foreground "black")))
  )

(defun spacelite//init-spaceline ()
  (setq-default
   powerline-height 24
   powerline-default-separator 'wave)

  ;; prevent winum to insert windows numbers to mode line
  (setq winum-auto-setup-mode-line nil)
  ;; prevent anzu to matched numbers to mode line
  (setq anzu-cons-mode-line-p nil)

  (use-package spaceline :ensure t)

  (use-package spaceline-config :ensure spaceline
    :config
    (spaceline-helm-mode 1)
    (spaceline-emacs-theme))
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-compile "spacelite" '(
                                   (window-number :face highlight-face)
                                   (buffer-modified)
                                   (line-column)
                                   (anzu :priority 4)
                                   (major-mode)
                                   (process :when active)
                                   (minor-modes :when active)
                                   (version-control :when active)
                                   ) '(buffer-position buffer-id :seperator "|"))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-spacelite)))))


;; Display all the monospace fonts available to Emacs in a dedicated buffer
;; REF: https://gist.github.com/haxney/3055728
(defun font-is-mono-p (font-family)
  ;; with-selected-window
  (let ((wind (selected-window))
        m-width l-width)
    (with-current-buffer "*Monospace Fonts*"
      (set-window-buffer (selected-window) (current-buffer))
      (text-scale-set 4)
      (insert (propertize "l l l l l" 'face `((:family ,font-family))))
      (goto-char (line-end-position))
      (setq l-width (car (posn-x-y (posn-at-point))))
      (newline)
      (forward-line)
      (insert (propertize "m m m m m" 'face `((:family ,font-family) italic)))
      (goto-char (line-end-position))
      (setq m-width (car (posn-x-y (posn-at-point))))
      (eq l-width m-width))))

(defun compare-monospace-fonts ()
  "Display a list of all monospace font faces."
  (interactive)
  (pop-to-buffer "*Monospace Fonts*")

  (erase-buffer)
  (dolist (font-family (font-family-list))
    (when (font-is-mono-p font-family)
      (let ((str font-family))
        (newline)
        (insert
         (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                             font-family ")\n") 'face `((:family ,font-family)))
         (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                             font-family ")\n") 'face `((:family ,font-family) italic)))))))

(provide 'core-makeup)

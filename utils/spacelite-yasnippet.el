(defun spacelite/init-yasnippet ()

  ;; (use-package yasnippet-snippets
  ;;   :ensure t)

  (use-package yasnippet
    :defer t
    :init
    ;; Keybindings
    (spacelite/declare-prefix "y" "yasnippet")
    (spacelite/set-leader-keys "yi" 'yas-insert-snippet)
    (spacelite/set-leader-keys "yc" 'yas-new-snippet)

    ;; REF: https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-yasnippet.el#L14-27
    ;; The directories listed in `yas-snippet-dirs' should contain snippet
    ;; folders only for the major modes where you are ever going to use
    ;; yasnippet.
    ;;   By default, `yas-snippet-dirs' also contains the snippets
    ;; directory that comes with the package, which contains major mode dirs
    ;; like `fundamental-mode' in which you are never going to use yasnippet!
    ;;   So the solution is to copy only the snippet folders that I am ever
    ;; going to use to `.emacs.d/snippets'.
    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets") ;; personal snippets
          )

    ;; In the file that controls the mode you want to enable this for, set the following
    ;; (add-hook major-mode-hook #'lazy-yas-minor-mode)
    )

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (use-package company
    :defer t
    :init
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    (add-hook 'after-init-hook 'global-company-mode)
    )
  )

(defun lazy-yas-minor-mode ()
  (require 'yasnippet)
  (yas-reload-all)
  (yas-minor-mode))

(provide 'spacelite-yasnippet)

;; TODO: You might want to change this to your preferences
(defvar spacelite/elisp-auto-format-on-save t
  "Automatically formats elisp code on save")

(defun load-this-file ()
  (interactive)
  (load-file (buffer-file-name)))

(defun elisp-mode-p ()
  (or (eq major-mode 'lisp-interaction-mode)
      (eq major-mode 'lisp-mode)
      (eq major-mode 'emacs-lisp-mode)))

(defun elisp-format-this-file ()
  (interactive)
  (when (elisp-mode-p)
    (save-excursion
      (indent-region (point-min) (point-max))
      (whitespace-cleanup))))

(defun spacelite/init-elisp ()
  ;; i for interactive toggle
  (spacelite/set-leader-keys-for-major-mode 'emacs-lisp-mode "i" 'lisp-interaction-mode)
  (spacelite/set-leader-keys-for-major-mode 'lisp-mode "i" 'lisp-interaction-mode)
  (spacelite/set-leader-keys-for-major-mode 'lisp-interaction-mode "i" 'emacs-lisp-mode)
  ;; r for run
  (spacelite/set-leader-keys-for-major-mode 'emacs-lisp-mode "r" 'load-this-file)
  (spacelite/set-leader-keys-for-major-mode 'lisp-mode "r" 'load-this-file)
  (spacelite/set-leader-keys-for-major-mode 'lisp-interaction-mode "r" 'load-this-file)

  ;; f for format
  (spacelite/set-leader-keys-for-major-mode 'emacs-lisp-mode "f" 'elisp-format-this-file)
  (spacelite/set-leader-keys-for-major-mode 'lisp-mode "f" 'elisp-format-this-file)
  (spacelite/set-leader-keys-for-major-mode 'lisp-interaction-mode "f" 'elisp-format-this-file)

  (defun elisp-auto-format-on-save ()
    (when spacelite/elisp-auto-format-on-save
      (elisp-format-this-file)))

  (add-hook 'before-save-hook 'elisp-auto-format-on-save)

  (use-package aggressive-indent
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
    )

  (use-package lispy
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
    )

  )

(provide 'spacelite-elisp)

;; TODO: You might want to change this to your preferences
(defvar spacelite/elisp-auto-format-on-save
  nil "Automatically formats elisp code on save")

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
    (elisp-format-file (buffer-file-name))))

(defun spacelite/init-elisp ()
  ;; i for interactive toggle
  (spacelite/set-leader-keys-for-major-mode
    'emacs-lisp-mode "i" 'lisp-interaction-mode)
  (spacelite/set-leader-keys-for-major-mode
    'lisp-mode "i" 'lisp-interaction-mode)
  (spacelite/set-leader-keys-for-major-mode
    'lisp-interaction-mode "i" 'emacs-lisp-mode)
  ;; r for run
  (spacelite/set-leader-keys-for-major-mode
    'emacs-lisp-mode "r" 'load-this-file)
  (spacelite/set-leader-keys-for-major-mode
    'lisp-mode "r" 'load-this-file)
  (spacelite/set-leader-keys-for-major-mode
    'lisp-interaction-mode "r" 'load-this-file)
  ;; f for format
  (use-package elisp-format
    :defer t
    :init (spacelite/set-leader-keys-for-major-mode
            'emacs-lisp-mode "f" 'elisp-format-this-file)(spacelite/set-leader-keys-for-major-mode
            'lisp-mode "f" 'elisp-format-this-file)(spacelite/set-leader-keys-for-major-mode
            'lisp-interaction-mode "f" 'elisp-format-this-file)(defun elisp-auto-format-on-save ()
            (when spacelite/elisp-auto-format-on-save
              (elisp-format-this-file)))(add-hook 'before-save-hook 'elisp-auto-format-on-save)))

(provide 'spacelite-elisp)

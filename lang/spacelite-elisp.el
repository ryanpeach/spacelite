(defun load-this-file () 
  (interactive) 
  (load-file (buffer-file-name)))

(defun elisp-format-this-file () 
  (interactive) 
  (elisp-format-file (buffer-file-name)))

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
  (use-package 
    elisp-format 
    :defer t 
    :init (spacelite/set-leader-keys-for-major-mode 'emacs-lisp-mode "f" 'elisp-format-this-file) 
    (spacelite/set-leader-keys-for-major-mode 'lisp-mode "f" 'elisp-format-this-file) 
    (spacelite/set-leader-keys-for-major-mode 'lisp-interaction-mode "f" 'elisp-format-this-file)))

(provide 'spacelite-elisp)

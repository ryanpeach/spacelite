(defun spacelite/init-elisp ()
  ;; TODO: Required for setting up your python environment
  (setq python-shell-interpreter "~/miniconda3/bin/ipython3") 
  (setq python-shell-interpreter-args "-i --simple-prompt")

  ;; Used for many python related tasks
  (use-package 
    elpy 
    :ensure t 
    :init (elpy-enable)

    ;; Running / Compiling
    (defalias python-run 'elpy-shell-send-region-or-buffer) 
    (spacelite/set-leader-keys-for-major-mode 'python-mode "c" 'python-run)

    ;; Unit Testing
    ;; TODO: Use M-x elpy-set-test-runner to set up your test running environment
    (defalias pytest 'elpy-test) 
    (spacelite/set-leader-keys-for-major-mode 'python-mode "t" 'pytest)

    ;; Debugging
    (spacelite/set-leader-keys-for-major-mode 'python-mode "b" 'elpy-pdb-toggle-breakpoint-at-point) 
    (spacelite/set-leader-keys-for-major-mode 'python-mode "d" 'elpy-pdb-debug-buffer)

    ;; Formatting
    (spacelite/set-leader-keys-for-major-mode 'python-mode "f" 'elpy-format-code)

    ;; Profiling
    (spacelite/set-leader-keys-for-major-mode 'python-mode "p" 'elpy-profile-buffer-or-region)

    ;; Navigation
    (defalias elpy-goto-return 'pop-tag-mark) 
    (spacelite/set-leader-keys-for-major-mode 'python-mode "." 'elpy-goto-definition) 
    (spacelite/set-leader-keys-for-major-mode 'python-mode ">" 'elpy-goto-definition-other-window) 
    (spacelite/set-leader-keys-for-major-mode 'python-mode "*" 'elpy-goto-return))

  ;; For refactoring
  (use-package 
    ropemacs 
    :defer t 
    :init (spacelite/declare-prefix-for-mode 'python-mode "r" "refactor"))

  ;; For syntax highlighting
  (use-package 
    flycheck 
    :defer t))

(provide 'spacelite-elisp)

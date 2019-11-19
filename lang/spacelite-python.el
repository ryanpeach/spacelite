(defun spacelite/init-python ()
  ;; TODO: Required for setting up your python environment
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "-i --simple-prompt")

  ;; Flycheck
  (use-package
    flycheck
    :defer t
    :config
    (flycheck-define-checker
     python-mypy ""
     :command ("mypy"
	       "--ignore-missing-imports" "--fast-parser"
	       "--python-version" "3.6"
	       source-original)
     :error-patterns
     ((error line-start (file-name) ":" line ": error:" (message) line-end))
     :modes python-mode)

    (add-to-list 'flycheck-checkers 'python-mypy t)
    (flycheck-add-next-checker 'python-pylint 'python-mypy t)
    )

  ;; Used for many python related tasks
  (use-package
    elpy
    :ensure t
    :init (elpy-enable)

    ;; TODO: Settings
    (setq elpy-rpc-python-command "python3")

    ;; Running / Compiling
    (defalias 'python-run 'elpy-shell-send-region-or-buffer)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "c" 'python-run)

    ;; Unit Testing
    ;; TODO: Use M-x elpy-set-test-runner to set up your test running environment
    (defalias 'pytest 'elpy-test)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "t" 'pytest)

    ;; Debugging
    (spacelite/set-leader-keys-for-major-mode 'python-mode "b" 'elpy-pdb-toggle-breakpoint-at-point)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "d" 'elpy-pdb-debug-buffer)

    ;; Formatting
    (spacelite/set-leader-keys-for-major-mode 'python-mode "f" 'elpy-format-code)

    ;; Profiling
    (spacelite/set-leader-keys-for-major-mode 'python-mode "p" 'elpy-profile-buffer-or-region)

    ;; Navigation
    (defalias 'elpy-goto-return 'pop-tag-mark)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "." 'elpy-goto-definition)
    (spacelite/set-leader-keys-for-major-mode 'python-mode ">" 'elpy-goto-definition-other-window)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "*" 'elpy-goto-return))

  ;; (use-package
  ;;   ropemacs
  ;;   :defer t
  ;;   :init
  ;;   (use-package pymacs
  ;;     :defer t
  ;;     :config
  ;;     (pymacs-load "ropemacs" "rope-")
  ;;     ;; Automatically save project python buffers before refactorings
  ;;     (setq ropemacs-confirm-saving 'nil)
  ;;     (spacelite/declare-prefix-for-mode 'python-mode "r" "refactor")))
  )

(provide 'spacelite-python)

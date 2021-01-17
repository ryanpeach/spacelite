(defun python-run-ensure-mypy ()
  "Run python if and only if mypy clears"
  (interactive)
  (if (python-mypy-p)
      (python-run)
    (python-mypy-run)))

(defun python-mypy-p ()
  ;; TODO: Make this print to message buffer somehow
  (call-process "mypy" nil nil nil (buffer-file-name)))

(defun python-mypy-run ()
  (interactive)
  (let* ((file (buffer-file-name))
         (new-buffer (get-buffer-create "*pythonscript*"))
         (script-proc-buffer
          (apply 'make-comint-in-buffer "script" new-buffer "mypy" nil '(file)))
         (script-proc (get-buffer-process script-proc-buffer)))
    (set-process-sentinel script-proc 'special-mode-sentinel)
    )
  )

(defun python-run ()
  (interactive)
  (let* ((file (buffer-file-name))
         (new-buffer (get-buffer-create "*pythonscript*"))
         (script-proc-buffer
          (apply 'make-comint-in-buffer "script" new-buffer "python" nil '(file)))
         (script-proc (get-buffer-process script-proc-buffer)))
    (set-process-sentinel script-proc 'special-mode-sentinel)
    )
  )

(defun spacelite/init-python ()
  ;; Flycheck
  (use-package
    flycheck
    :ensure t
    :init

    ;; TODO: Why doesn't the prefix work?
    (spacelite/declare-prefix-for-mode 'python-mode "s" "syntax" "flycheck-syntax")
    (spacelite/set-leader-keys-for-major-mode 'python-mode "ss" 'flycheck-mode)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "sn" 'flycheck-next-error)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "sp" 'flycheck-previous-error)

    ;; Only flycheck on save, but always keep enabled
    (global-flycheck-mode)

    :config

    (flycheck-define-checker
     python-mypy ""
     :command ("mypy"
               "--ignore-missing-imports"
               "--disallow-untyped-defs"
               source-original)
     :error-patterns
     ((error line-start (file-name) ":" line ": error:" (message) line-end))
     :modes python-mode)

    (add-to-list 'flycheck-checkers 'python-mypy t)
    (flycheck-add-next-checker 'python-pylint 'python-mypy t)
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    )


  (use-package python
    :mode ("\\.py" . python-mode)
    :ensure t
    :config
    ;; TODO: Required for setting up your python environment
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "-i --simple-prompt")
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

    ;; Comment with ;
    (add-hook 'python-mode-hook #'lazy-yas-minor-mode)
    (evil-define-key 'normal python-mode-map (kbd ";") 'comment-line)

    ;; Running / Compiling
    (spacelite/set-leader-keys-for-major-mode 'python-mode "c" 'python-mypy-run)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "r" 'python-run)
    (spacelite/set-leader-keys-for-major-mode 'python-mode "," 'python-run-ensure-mypy)

    ;; Used for many python related tasks
    (use-package
      elpy
      :ensure t
      :init
      ;; TODO: Settings
      (setq elpy-rpc-python-command "python3")

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
      (spacelite/set-leader-keys-for-major-mode 'python-mode "*" 'elpy-goto-return)

      :config
      (remove-hook 'elpy-modules 'elpy-module-flymake) ;; <- This removes flymake from elpy
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      )
    )

  (elpy-enable)
  ;; (use-package lispy
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

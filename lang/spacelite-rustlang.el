(defun rustlang/add-semicolon-at-the-end-of-this-line ()
  "If no semicolon is present at the end of the line, add it."
  (interactive)
  (save-excursion
    (end-of-line)
    (when (not (eq (char-before) ?\;))
      (insert ";")
      )
    )
  )

(defun rustlang/add-semicolon ()
  "If no semicolon is present here, add it."
  (interactive)
  (when (not (eq (char-after) ?\;))
    (insert ";")
    )
  )

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;; TODO: Ignore inline comments
(defun line-ends-in-p (ch)
  (save-excursion
    (end-of-line)
    (eq (char-before) ch))
  )

(defun rustlang/toggle-semicolon-at-the-end-of-this-line ()
  "Toggle semicolon at the end of this line."
  (interactive)
  (if (line-ends-in-p ?\;)
      (delete-char 1)
    (insert ";")
    )
  )

(defun rustlang/filter-semicolon-event-p ()
  (interactive)
  (let ((allowed-chars '(?\; ?, ?} ?{ ?\()))
    (and (not (current-line-empty-p))
         (not (-any-p 'line-ends-in-p allowed-chars))
         (not (-any-p '(lambda (y) (eq (char-after) y)) allowed-chars))
         (not (-any-p '(lambda (y) (eq (char-before) y)) allowed-chars))
         (y-or-n-p "Did you forget a semicolon? ")
         )
    )
  )

(defun spacelite/init-rustlang ()
  (use-package racer
    :defer t)

  (add-hook python-mode-hook #'lazy-yas-minor-mode)
  ;; Comment with ;
  (evil-define-key 'normal rust-mode-map (kbd ";") 'comment-line)
  (evil-define-key 'normal rust-mode-map (kbd "C-;") (lambda () (insert ";")))

  ;; Flycheck
  (use-package
    flycheck
    :defer t
    :init

    ;; TODO: Why doesn't the prefix work?
    (spacelite/declare-prefix-for-mode 'rust-mode "s" "syntax" "flycheck-syntax")
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "ss" 'flycheck-mode)
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "sn" 'flycheck-next-error)
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "sp" 'flycheck-previous-error)

    ;; Only flycheck on save, but always keep enabled
    (global-flycheck-mode)

    )


  (use-package rust-mode
    :defer t
    :init
    ;; Add hooks and keybindings
    ;; Newline and indent on semicolon
    (evil-define-key 'insert rust-mode-map (kbd "RET") (lambda () (interactive)
                                                         (when (rustlang/filter-semicolon-event-p)
                                                           (rustlang/add-semicolon))
                                                         (newline-and-indent)
                                                         )
      )
    (evil-define-key 'insert rust-mode-map (kbd "<S-return>") 'newline-and-indent)
    (evil-define-key 'insert rust-mode-map ";" (lambda () (interactive)
                                                 (rustlang/add-semicolon)
                                                 (newline-and-indent)
                                                 )
      )
    (evil-define-key 'insert rust-mode-map (kbd "C-;") (lambda () (interactive)
                                                         (rustlang/add-semicolon-at-the-end-of-this-line)
                                                         (newline-and-indent)
                                                         )
      )
    (evil-define-key 'insert rust-mode-map (kbd "M-;") 'rustlang/toggle-semicolon-at-the-end-of-this-line)

    ;; Common major mode keybindings
    ;; Debugging
    ;; (spacelite/set-leader-keys-for-major-mode 'rust-mode "b" 'rust-pdb-toggle-breakpoint-at-point)
    ;; (spacelite/set-leader-keys-for-major-mode 'rust-mode "d" 'rust-pdb-debug-buffer)

    ;; Formatting
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "f" 'rust-format-buffer)

    ;; Profiling
    ;; (spacelite/set-leader-keys-for-major-mode 'rust-mode "p" 'rust-profile-buffer-or-region)

    ;; Running / Compiling
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "b" 'rust-compile)
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "r" 'rust-run)

    ;; Unit Testing
    ;; TODO: Use M-x elpy-set-test-runner to set up your test running environment
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "t" 'rust-test)


    ;; Navigation
    ;; (spacelite/set-leader-keys-for-major-mode 'rust-mode "." 'rust-goto-definition)
    ;; (spacelite/set-leader-keys-for-major-mode 'rust-mode ">" 'rust-goto-definition-other-window)
    ;; (spacelite/set-leader-keys-for-major-mode 'rust-mode "*" 'rust-goto-return)
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    )

  (use-package rustic
    :defer t
    :init
    :config
    (setq rustic-format-trigger nil)

    (spacelite/declare-prefix-for-mode "c" "cargo" 'rust-mode)

    ;; Running / Compiling
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "cb" 'rustic-cargo-build)
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "cr" 'rustic-cargo-run)

    ;; Unit Testing
    ;; TODO: Use M-x elpy-set-test-runner to set up your test running environment
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "ct" 'rustic-cargo-test)

    ;; Formatting
    (spacelite/set-leader-keys-for-major-mode 'rust-mode "cf" 'rustic-cargo-format)

    )


  (use-package aggressive-indent
    :defer t
    :init
    (add-hook 'rust-mode-hook #'aggressive-indent-mode)
    )

  )

(provide 'spacelite-rustlang)

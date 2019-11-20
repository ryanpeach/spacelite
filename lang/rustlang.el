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

(defun rustlang/init ()
  (use-package racer
    :defer t)

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
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    )
  )

(provide 'rustlang)

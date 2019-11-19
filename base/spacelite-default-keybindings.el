(defun spacelite/init-default-keybindings ()
  (spacelite/declare-prefix "h" "help")
  (spacelite/set-leader-keys

    ;; Spacemacs like select windows
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9

    ;; Tmux like split windows
    "w%" 'split-window-vertically
    "w\"" 'split-window-horizontally

    ;; Help keys
    "ha" 'apropos
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key

    ;; Jump functions like idea-spacemacs
    ;; jj is easiest to type, but jF is the most practical
    ;; so switch jj to jf, jJ to jF, jf to jl, and jF to jL
    "jj" 'evilem-motion-find-char
    "jJ" 'evilem-motion-find-previous-char
    "jl" 'evilem-motion-next-line
    "jL" 'evilem-motion-previous-line))

(defun prompt-desc-check (inp-prompt check-f err-prompt)
  (interactive)
  (let ((val (read-string inp-prompt)))
    (if (check-f val)
	val
      (progn (read-string err-prompt)
	     (prompt-desc-check inp-prompt check-f err-prompt)))
    )
  )

(defun spacelite/create-keybinding ()
  (interactive)
  (let ((func (prompt-desc-check
	       "Please provide a function name: "
	       (lambda (x) (boundp (make-symbol x)))
	       "Function does not exist, try again."))
	(keys (prompt-desc-check
	       "Please provide a key sequence: "
	       (lambda (x) t)
	       "")))
    (save-excursion
      (with-current-buffer (find-file-noselect "~/.emacs.d/base/spacelite-default-keybindings.el")
	(search-forward "(defun spacelite/init-default-keybindings ()")
	(insert (concat "\n(spacelite/set-leader-keys \"" keys "\" '" func ")"))
	(spacelite/init-default-keybindings)
	)
      )
    )
  )

(provide 'spacelite-default-keybindings)

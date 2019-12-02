;; These renamings make things easier to read in which-key
(defalias 'win-0 'winum-select-window-0)
(defalias 'win-1 'winum-select-window-1)
(defalias 'win-2 'winum-select-window-2)
(defalias 'win-3 'winum-select-window-3)
(defalias 'win-4 'winum-select-window-4)
(defalias 'win-5 'winum-select-window-5)
(defalias 'win-6 'winum-select-window-6)
(defalias 'win-7 'winum-select-window-7)
(defalias 'win-8 'winum-select-window-8)
(defalias 'win-9 'winum-select-window-9)

(defun spacelite/init-default-keybindings ()
  ;; spacelite/set-leader-keys
  ;; spacelite/set-leader-keys-for-major-mode
  ;; spacelite/declare-prefix
  ;; spacelite/declare-prefix-for-mode
  ;; global-set-key

  ;; Some basic stuff
  (spacelite/set-leader-keys "!" 'shell-command)
  (spacelite/set-leader-keys "bs" 'save-buffer)
  (spacelite/set-leader-keys "fs" 'save-buffer)
  (spacelite/set-leader-keys "pg" 'helm-grep-do-git-grep)
  (spacelite/set-leader-keys "p/" 'helm-grep-do-git-grep)
  (spacelite/set-leader-keys "f/" 'helm-grep-do-git-grep)
  (spacelite/set-leader-keys "/" 'helm-grep-do-git-grep)
  (spacelite/set-leader-keys ";" 'comment-region)

  ;; Tabs
  (spacelite/declare-prefix "TAB" "indent")
  (spacelite/set-leader-keys "TAB TAB" 'indent-region)
  (spacelite/set-leader-keys "TAB DEL" 'indent-region)
  (spacelite/set-leader-keys "TAB l" 'indent-rigidly-right)
  (spacelite/set-leader-keys "TAB L" 'indent-rigidly-right-to-tab-stop)
  (spacelite/set-leader-keys "TAB h" 'indent-rigidly-left)
  (spacelite/set-leader-keys "TAB H" 'indent-rigidly-left-to-tab-stop)

  ;; Emacs
  (spacelite/declare-prefix "e" "emacs")
  (spacelite/set-leader-keys "er" 'spacelite/reload-init)

  (spacelite/declare-prefix "ek" "keybindings")
  (spacelite/set-leader-keys "eks" 'spacelite/create-spacelite-keybinding)
  (spacelite/set-leader-keys "ekk" 'spacelite/goto-keybindings-file)
  (spacelite/set-leader-keys "ekr" 'spacelite/reload-keybindings)

  (spacelite/declare-prefix "h" "help")
  (spacelite/set-leader-keys

    ;; Spacemacs like select windows
    "0" 'win-0
    "1" 'win-1
    "2" 'win-2
    "3" 'win-3
    "4" 'win-4
    "5" 'win-5
    "6" 'win-6
    "7" 'win-7
    "8" 'win-8
    "9" 'win-9

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

(defun spacelite/goto-keybindings-file ()
  (interactive)
  (find-file "~/.emacs.d/base/spacelite-default-keybindings.el")
  (search-forward "defun spacelite/init-default-keybindings"))

(defun spacelite/reload-file ()
  "Reloads the current file."
  (interactive)
  (save-buffer)
  (load-file (buffer-name))
  (load-file user-init-file))

(defun spacelite/reload-init ()
  "Reload the init file"
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(defun spacelite/reload-keybindings ()
  "Reload the keybindings file and init the keybindings"
  (interactive)
  (save-buffer)
  (load-file "~/.emacs.d/base/spacelite-default-keybindings.el")
  (spacelite/init-default-keybindings)
  )

(defun prompt-desc-check (inp-prompt check-f err-prompt)
  (interactive)
  (let ((val (read-string inp-prompt)))
    (if (funcall check-f val)
        val
      (progn (read-string err-prompt)
             (prompt-desc-check inp-prompt check-f err-prompt)))
    )
  )

(defun spacelite/create-spacelite-keybinding ()
  (interactive)
  (let ((func (prompt-desc-check
               "Please provide a function name: "
               (lambda (x) (fboundp (intern x)))
               "Function does not exist, try again."))
        (keys (prompt-desc-check
               "Please provide a key sequence: "
               (lambda (x) t)
               "")))
    (save-excursion
      (with-current-buffer (find-file-noselect "~/.emacs.d/base/spacelite-default-keybindings.el")
        (beginning-of-buffer)
        (search-forward "(defun spacelite/init-default-keybindings ()")
        (insert (concat "\n(spacelite/set-leader-keys \"" keys "\" '" func ")"))
        (spacelite/reload-keybindings)
        )
      )
    )
  )

(provide 'spacelite-default-keybindings)

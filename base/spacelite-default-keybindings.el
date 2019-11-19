(defun spacelite/init-default-keybindings ()
  (spacelite/declare-prefix "h" "help")
  (spacelite/set-leader-keys
    ;; Automatically Set

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

(defun spacelite/create-keybinding ()
  (interactive)
  )

(provide 'spacelite-default-keybindings)

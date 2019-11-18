;; Writing
(defun writing/init ()
  (spacelite/set-leader-keys
    "od" 'writing/wcdiff
    "of" 'writing/wcfile
    "op" 'writing/wcproj
    "ow" 'writing/writeroom-mode
    "ov" 'writing/visual-line-mode
    "og" 'writing/writegood-mode
    )

  ;; Enable flyspell in text mode
  ;; REF: https://www.emacswiki.org/emacs/FlySpell
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode-on))))

  ;; If you turn flyspell on, still only spell comments
  ;; REF: https://emacs.stackexchange.com/questions/31300/can-you-turn-on-flyspell-for-comments-but-not-strings
  (setq flyspell-prog-text-faces
	(delq 'font-lock-string-face
	      flyspell-prog-text-faces))

  )
  
;; Literary Functions
;; REF: https://www.emacswiki.org/emacs/WordCount
(defun writing/wcproj ()
  "Count the words in a project from shell."
  (interactive)
  (shell-command
   (concat "wc -w " (projectile-project-root) "**/*")
   )
  )

(defun writing/wcfile ()
  "Count the words in a project from shell."
  (interactive)
  (shell-command
   (concat "wc -w \"" buffer-file-name "\"")
   )
  )

(defun writing/wcdiff ()
  "Count word diff from git repo"
  (interactive)
  (setq gitwa
        (string-to-number
         (substring
          (shell-command-to-string "git diff --word-diff=porcelain origin/master | grep -e \"^+[^+]\" | wc -w | xargs")
          0 -1
          )))
  (setq gitwd
        (string-to-number
         (substring
          (shell-command-to-string "git diff --word-diff=porcelain origin/master | grep -e \"^-[^-]\" | wc -w | xargs")
          0 -1
          )))
  (print (- gitwa gitwd))
  )

;; Enter to open org tree in new buffer
;; TODO: Currently not working
(defun writing/org-tree-open-in-right-frame ()
  (interactive)
  (if (cl-search "-narrowclone" (buffer-name))
      (kill-buffer (buffer-name))
    (let ((ind-buf (concat (buffer-name) "-narrowclone")))
      (if (get-buffer ind-buf)
          (kill-buffer ind-buf))
      (clone-indirect-buffer-other-window ind-buf t)
      (org-narrow-to-subtree)
      (switch-to-buffer ind-buf)
      (windmove-right))))

(provide 'writing)


(defun keybindings/init-keybindings ()
  (require 'writing)
  (require 'autoorg)

  ;; Autoorg
  (global-set-key (kbd "C-l") 'autoorg/add-to-heading-interactive)

  ;; Easy open terminal
  (spacemacs/set-leader-keys "<RET>" 'term)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; Window Navigation
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-'") 'split-window-horizontally)
  (global-set-key (kbd "C-5") 'split-window-vertically)

  ;; TODO: Make it loop
  (defun other-window-backward ()
    "Select the previous window."
    (interactive)
    (other-window -1)
    )
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-S-<tab>") 'other-window-backward)

  ;; Pycharm keybindings
  (global-set-key (kbd "C-s") 'save-buffer)
  (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors)
  ;; TODO (evil-define-key 'insert evil-mc-key-map (kbd "M-<down>") 'evil-mc-make-cursor-line-below)
  ;; TODO (evil-define-key 'insert evil-mc-key-map (kbd "M-<up>") 'evil-mc-make-cursor-line-above)

  (defun toggle-comment-on-line ()
    "comment or uncomment current line"
    (interactive)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

  ;; Comment lines
  (define-key evil-normal-state-map (kbd ";") 'toggle-comment-on-line)
  (define-key evil-visual-state-map (kbd ";") 'comment-region)

  )

(provide 'keybindings)

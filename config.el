;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Personal default keybindings
(map!
 :leader
 "SPC" #'counsel-M-x
 "p l" #'projectile-switch-project
 "w w" #'ace-window
 "w n" #'evil-window-next
 "w p" #'evil-window-previous
 "g s" #'magit-stage-file
 "g S" #'magit-stage-modified
 "g p" #'magit-pull
 "g P" #'magit-push
 "p / /" #'+ivy/project-search
 "p / r" #'+ivy/project-find-replace
 "p / f" #'+ivy/projectile-find-file
 "p f" #'+ivy/projectile-find-file
 "TAB TAB" #'indent-rigidly
 )

;; Handle window splitting and size
;; REF: https://www.reddit.com/r/emacs/comments/a7wsp6/resize_frame_when_creating_new_windows/
; Set default frame size
(setq default-window-width 99) ; space included for fci-mode rule, ultimately comes to 79 characters, the PEP8 default
(when window-system (set-frame-size (selected-frame) default-window-width 48))

; An easy find replace function
(defun +ivy/project-find-replace ()
  (interactive)
  (progn (print "Press C-c C-e to make edits when search is complete")
         (+ivy/project-search))
  )

; Resize frame when opening new windows hor1izontally
(defun delete-other-windows-and-resize ()
  "delete and reset width"
  (interactive)
  (when window-system
    (progn (set-frame-width (selected-frame) default-window-width)
           (delete-other-windows))))

(defun delete-window-and-resize ()
  "delete this window and reset width"
  (interactive)
  (when window-system
    (progn (delete-window)
           (set-frame-width (selected-frame)
                            (- (frame-width) default-window-width 1))
           (balance-windows))))

(defun split-window-right-and-resize ()
  "split and increase width"
  (interactive)
  (when window-system
    (progn (set-frame-width (selected-frame)
			     (+ (frame-width) default-window-width 1))
	    (split-window-right)
	    (balance-windows))))

(map!
 :leader
 "w d" #'delete-window-and-resize
 "w f" #'delete-other-windows-and-resize
 "w \"" #'split-window-right-and-resize
 "w %" #'split-window-vertically)

;; M-x compile
;; REF: https://www.masteringemacs.org/article/compiling-running-scripts-emacs
;;; Shut up compile saves
(setq compilation-ask-about-save nil)
;;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))

(map!
 :leader
 (:prefix-map ("/" . "search"))
 "/ /" #'+ivy/project-search
 "/ r" #'+ivy/project-find-replace
 "/ f" #'+ivy/projectile-find-file
 )

(after! ivy
  (define-key ivy-minibuffer-map (kbd "C-c C-c") '+ivy/woccur)
)

;; mc mode
(map!
 :leader
 (:prefix-map ("C" . "multi-cursor"))
 "C m" #'evil-mc-make-all-cursors
 "C u" #'evil-mc-undo-all-cursors
 "C c" #'evil-mc-mode
 "C v" #'evil-mc-make-cursor-in-visual-selection-beg
 "C V" #'evil-mc-make-cursor-in-visual-selection-end
 "C n" #'evil-mc-make-and-goto-next-match
 )

(defun evil-mc-make-cursor-on-click (event)
  "Used by C-S-<mouse-1> to make a cursor wherever you click."
  (interactive "e")
  (progn
    (evil-mc-make-cursor-here)
    (mouse-set-point event)
    (evil-mc-make-cursor-here)))
(global-set-key (kbd "C-S-<mouse-1>") 'evil-mc-make-cursor-on-click)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Peach"
      user-mail-address "ryanpeach@me.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type "relative")


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

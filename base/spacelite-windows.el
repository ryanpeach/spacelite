;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacelite/init-windows ()
  ;; init keys
  (spacelite/set-leader-keys "w/" 'spacelite/split-window-right-and-focus "w-"
    'spacelite/split-window-below-and-focus "wh" 'evil-window-left "wj" 'evil-window-down "wk"
    'evil-window-up "wl" 'evil-window-right "wd" 'delete-window "w TAB" 'spacelite/alternate-window))

(defun spacelite/alternate-window () 
  (interactive) 
  (let ( ;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window 
      (user-error 
       "Last window not found.")) 
    (select-window prev-window)))

(defun spacelite/split-window-below-and-focus () 
  "Split the window vertically and focus the new window." 
  (interactive) 
  (split-window-below) 
  (windmove-down))

(defun spacelite/split-window-right-and-focus () 
  "Split the window horizontally and focus the new window." 
  (interactive) 
  (split-window-right) 
  (windmove-right))

; Resize frame when opening new windows hor1izontally
(defun spacelite/delete-other-windows-and-resize ()
  "delete and reset width"
  (interactive)
  (when window-system
    (progn (set-frame-width (selected-frame) default-window-width)
           (delete-other-windows))))

(defun spacelite/delete-window-and-resize ()
  "delete this window and reset width"
  (interactive)
  (when window-system
    (progn (delete-window)
           (set-frame-width (selected-frame)
                            (- (frame-width) default-window-width 1))
           (balance-windows))))

(defun spacelite/split-window-right-and-resize ()
  "split and increase width"
  (interactive)
  (when window-system
    (progn (set-frame-width (selected-frame)
			     (+ (frame-width) default-window-width 1))
	    (split-window-right)
	    (balance-windows))))

(provide 'spacelite-windows)

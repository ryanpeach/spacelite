(defun spacelite/init-google ()
  (use-package google-this
    :defer t
    :config (google-this-mode 1)
    :diminish 'google-this-mode
    :init (spacelite/set-leader-keys "sg" 'google-this-mode-submap)))

(provide 'spacelite-google)

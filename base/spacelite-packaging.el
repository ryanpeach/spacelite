(defun spacelite/init-packaging ()
  (spacelite/set-leader-keys "ea" 'spacelite/add-file-to-spacelite)
  (spacelite/set-leader-keys "er" 'spacelite/remove-file-from-spacelite))

(defun spacelite//get-this-package-name ()
  "Returns the name provided by this package"
  (save-excursion (end-of-buffer)
                  (search-backward "(provide '")
                  (search-forward "provide '")
                  (symbol-name (symbol-at-point))))

(defun spacelite//get-header-file-name ()
  "Returns the top level spacelite package containing the reference to this package."
  ;; This gets the folder name the current buffer is in. It should be one of the following
  (let ((folder-name (file-name-base (directory-file-name (file-name-directory
                                                           (buffer-file-name))))))
    (cond ((string= folder-name 'core)
           (concat (file-name-directory (buffer-file-name)) "spacelite-core.el"))
          ((string= folder-name 'base)
           (concat (file-name-directory (buffer-file-name)) "spacelite-base.el"))
          ((string= folder-name 'lang)
           (concat (file-name-directory (buffer-file-name)) "spacelite-langs.el"))
          ((string= folder-name 'utils)
           (concat (file-name-directory (buffer-file-name)) "spacelite-utils.el"))
          ((string= folder-name 'private)
           (concat (file-name-directory (buffer-file-name)) "spacelite-private.el"))
          (t
           (error
            (concat "folder-name is not known in spacelite: " folder-name))))))


(defun spacelite//get-init-function-name ()
  "Returns the init function contained in the file. It should be near the top with /init in the name and no args"
  (save-excursion (beginning-of-buffer)
                  (search-forward-regexp "defun spacelite/init.*()")
                  (search-backward "init")
                  (symbol-name (symbol-at-point))))

(defun spacelite/add-file-to-spacelite ()
  "Used for adding packages to the proper locations within spacelite."
  (interactive)
  (use-package s
    :demand t)
  (let ((file (spacelite//get-header-file-name))
        (package-name (spacelite//get-this-package-name))
        (init-function-name (spacelite//get-init-function-name)))
    (let ((mode-hook (if (s-suffix? package-name "-mode")
                         (concat package-name "-hook")
                       (concat package-name "-mode-hook"))))
      (save-excursion (with-current-buffer (find-file-noselect (print file))
                        (beginning-of-buffer)
                        (search-forward "require")
                        (search-backward "(")
                        (insert (concat "(require '" package-name ")\n"))
                        (search-forward-regexp "defun.*init.*()")
                        (insert (concat "\n(lazy-load '" mode-hook " '" init-function-name ")\n"))
                        (save-buffer)
                        (load-file user-init-file))))))

(defun spacelite/remove-file-from-spacelite ()
  "Used for removing packages to the proper locations within spacelite."
  (interactive)
  (let ((file (spacelite//get-header-file-name))
        (package-name (spacelite//get-this-package-name))
        (init-function-name (spacelite//get-init-function-name)))
    (save-excursion (with-current-buffer (find-file-noselect (print file))
                      (beginning-of-buffer)
                      (replace-regexp (concat "(require '" package-name ")") "")
                      (replace-regexp (concat "(lazy-load '.* '" init-function-name ")") "")
                      (save-buffer)
                      (load-file user-init-file)))))

(defun spacelite/reload-this-file ()
  "Reloads a file interactively and then reinits it"
  (interactive)
  (let ((file (buffer-file-name))
        (init-function-name (spacelite//get-init-function-name)))
    (save-buffer)
    (load-file file)
    (funcall (intern init-function-name))
    ))

(provide 'spacelite-packaging)

(defun spacelite//get-this-package-name ()
  "Returns the name provided by this package"
  (save-excursion (end-of-buffer)
		  (search-backward "(provide '")
		  (search-forward "provide '")
		  (symbol-at-point)))

(defun spacelite//get-header-file-name ()
  "Returns the top level spacelite package containing the reference to this package."
  ;; This gets the folder name the current buffer is in. It should be one of the following
  (let ((folder-name (file-name-nondirectory (directory-file-name (file-name-directory
								   (buffer-file-name))))))
    (cond ((eq folder-name "core")
	   (concat (file-name-directory (buffer-file-name)) "spacelite-core.el"))
	  ((eq folder-name "base")
	   (concat (file-name-directory (buffer-file-name)) "spacelite-base.el"))
	  ((eq folder-name "lang")
	   (concat (file-name-directory (buffer-file-name)) "spacelite-langs.el"))
	  ((eq folder-name "utils")
	   (concat (file-name-directory (buffer-file-name)) "spacelite-utils.el"))
	  ((eq folder-name "private")
	   (concat (file-name-directory (buffer-file-name)) "spacelite-private.el"))
	  (t
	   (error
	    "folder-name is not known in spacelite: "
	    folder-name)))))

(defun spacelite//get-init-function-name ()
  "Returns the init function contained in the file. It should be near the top with /init in the name and no args"
  (save-excursion (beginning-of-buffer)
		  (search-forward-regexp "defun.*init.*()")
		  (search-backward "init")
		  (symbol-at-point)))

(defun spacelite/add-file-from-emacs ()
  (let ((file (spacelite//get-header-file-name))
	(package-name (spacelite//get-this-package-name))
	(init-function-name (spacelite//get-init-function-name)))
    (save-excursion (with-current-buffer (find-file-noselect file)
		      (beginning-of-buffer)
		      (search-forward "require")
		      (search-backward ?\()
		      (insert "\n(require '" package-name ")")
		      (search-forward-regexp "defun.*init.*()")
		      (insert "\n(" init-function-name ")")))))

;; TODO: Write this
(defun spacelite/remove-file-from-emacs ())

(provide 'spacelite-packaging)

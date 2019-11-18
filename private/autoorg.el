
(defvar autoorg/todo-org-file "~/Org/Todos.org"
  "The location of the primary Todo org file.")

(defvar autoorg/todo-org-default-location "Home/Phone"
  "The default location for Todo's.")

(defvar autoorg/todo-org-default-tag "TODO"
  "The default tag to use for the TODO.")

(defun enumerate (lst)
  "Operates like python's enumerate function."
  (setq num 0)
  (mapcar
   #'(lambda (val)
      (setq num (1+ num))
      (let ((i (- num 1)))
        (list i val))
      )
   lst
   )
  )

(defmacro fstring (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (f-string \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.
  REF: https://kitchingroup.cheme.cmu.edu/blog/2018/05/14/f-strings-in-emacs-lisp/
"
  (let* ((matches (s-match-strings-all"${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" fmt))
         (agetter (cl-loop for (m0 m1 m2 m3) in matches
                           collect `(cons ,m3  (format (format "%%%s" (if (string= ,m2 "")
                                                                          (if s-lex-value-as-lisp "S" "s")
                                                                        ,m2))
                                                       (symbol-value (intern ,m1)))))))

    `(s-format ,fmt 'aget (list ,@agetter))))

(defun star (lst)
  "Creates a stared list of objects"
  (mapcar
   #'(lambda (cell)
      (concat
       (make-string (1+ (nth 0 cell)) ?*)
       " "
       (nth 1 cell))
      )
   (enumerate lst)
   )
  )

(defun autoorg/add-to-heading (file heading txt tag)
  "Adds TXT as a TAG under HEADING in FILE."
  (save-excursion
    (save-restriction
      (save-match-data
        (with-current-buffer (find-file-noselect file)
          (widen)
          (goto-char (point-min))

          (let ((this-star (star (split-string heading "/"))))
            (progn
              ;; Goto the point we are meant to goto
              (mapcar
               #'(lambda (cell)
                  (search-forward cell)
                  )
               this-star
               )

              ;; Insert the text
              (insert (concat "\n" (make-string (1+ (length this-star)) ?*) " " tag " " txt "\n"))
              )
            )
          )
        )
      )
    )
  )

(setq org-link-search-must-match-exact-headline nil)

(defun autoorg/add-to-heading-interactive ()
  (interactive)
  (save-window-excursion
    (let ((description (read-string-nonempty "Description: "))
          (tag (read-string-nonempty "Tag: " autoorg/todo-org-default-tag))
          (txt (substring (thing-at-point 'line t) 0 -1))
          (file (if (buffer-file-name)
                    (concat "~/" (file-relative-name (buffer-file-name) (expand-file-name "~")))
                    nil)))
      (let ((link-p (and (buffer-file-name)
                         (if (> (length txt) 0)
                           (yes-or-no-p (fstring "Do you want to include a link to '${file}::${txt}'?"))
                           (yes-or-no-p (fstring "Do you want to include a link to '${file}'?"))))))
        (with-current-buffer (find-file (read-string-exists "OrgFile: " autoorg/todo-org-file))
          (helm-org-in-buffer-headings)
          (end-of-line)
          (insert
           (concat
            "\n"
            ;; Make the stars at the beginning of the line based on the current heading's number of stars
            (make-string (1+ (count ?* (substring (thing-at-point 'line t) 0 -1))) ?*)
            " "
            tag
            " "
            ;; Create a link from the file text and description
            (if link-p
                (if (> (length txt) 0)
                    (fstring "[[file:${file}::${txt}][${description}]]\n")
                  (fstring "[[file:${file}][${description}]]\n")
                  )
              description
              )
            )
           )
          (save-buffer)
          (message "Todo Added")
          )
        )
      )
    )
  )


(defun read-string-nonempty (prompt &optional initial-input history default-value inherit-input-method)
  "Like read-string but wont allow empty input."
  (interactive)
  (let ((output (string-trim (read-string prompt initial-input history default-value inherit-input-method))))
    (if (= (length output) 0)
        (read-string-nonempty prompt initial-input history default-value inherit-input-method)
        output
        )
    )
  )


(defun read-string-exists (prompt &optional initial-input history default-value inherit-input-method)
  "Like read-string but wont allow empty or non existent files."
  (interactive)
  (let ((output (string-trim (read-string-nonempty prompt initial-input history default-value inherit-input-method))))
    (if (not (file-exists-p output))
        (read-string-exists prompt initial-input history default-value inherit-input-method)
      output
      )
    )
  )


(provide 'autoorg)

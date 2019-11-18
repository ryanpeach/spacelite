;; Copyright (c) 2017 Hüseyin Zengin
;;
;; Author: Hüseyin Zengin <hzenginx@gmail.com>
;; URL: https://github.com/hzenginx/spacelite
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun add-to-load-path (dir)
  (add-to-list 'load-path dir))

(defconst spacelite-core-directory (expand-file-name (concat user-emacs-directory "core/"))
  "Spacelite core directory.")

(defconst spacelite-base-directory (expand-file-name (concat user-emacs-directory "base/"))
  "Spacelite base directory.")

(defconst spacelite-lang-directory (expand-file-name (concat user-emacs-directory "lang/"))
  "Spacelite lang directory.")

(defconst spacelite-utils-directory (expand-file-name (concat user-emacs-directory "utils/"))
  "Spacelite utils directory.")

(defconst spacelite-private-directory (expand-file-name (concat user-emacs-directory "private/"))
  "Spacelite private directory.")

(mapc 'add-to-load-path `(,spacelite-core-directory
                          ,spacelite-base-directory
			  ,spacelite-lang-directory
			  ,spacelite-utils-directory
			  ,(concat spacelite-core-directory "libs/")
			  ,spacelite-private-directory
			  )
		)

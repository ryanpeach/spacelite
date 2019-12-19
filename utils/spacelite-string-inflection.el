(defun spacelite/init-string-inflection ()
  (use-package string-inflection
    :defer t
    :init
    (spacelite/declare-prefix "N" "names")
    (spacelite/set-leader-keys "Nn" 'string-inflection-all-cycle)
    (spacelite/set-leader-keys "Nc" 'string-inflection-camelcase-function)
    (spacelite/set-leader-keys "NC" 'string-inflection-capital-underscore-function)
    (spacelite/set-leader-keys "Nu" 'string-inflection-upcase-function)
    (spacelite/set-leader-keys "N_" 'string-inflection-underscore-function)
    (spacelite/set-leader-keys "N-" 'string-inflection-kebab-case-function)
    (spacelite/set-leader-keys "Np" 'string-inflection-pascal-case-function)
    )
  )

(provide 'spacelite-string-inflection)

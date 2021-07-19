;;; lsp-eglot.el -*- lexical-binding: t; -*-

(use-package! eglot
  :when (featurep! :tools lsp +eglot)
  :config
  (setq eglot-extend-to-xref t)
  (set-popup-rule! "^\\*eglot-help" :size 0.25 :quit t :select t)
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vls" "--stdio")))
  (map!
   (:leader :prefix "c"
    :desc "Find references" "D" #'(lambda () (interactive) (xref-find-references (symbol-at-point)))
    :desc "Organize imports" "o" #'eglot-code-action-organize-imports
    :desc "Find type definition" "t" #'eglot-find-typeDefinition
    :desc "Find implementation" "i" #'eglot-find-implementation)))

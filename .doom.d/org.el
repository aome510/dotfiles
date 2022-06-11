;;; org.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/")

(use-package! org
  :defer-incrementally t
  :config
  (add-hook! 'org-mode-hook
             #'org-fragtog-mode
             #'turn-off-smartparens-mode
             (setq-local tab-width 2))
  (setq org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "WATING(w)" "|" "DONE(d)" "DEFERRED(D)" "CANCELLED(c)"))
        org-download-image-html-width 600
        ;; setup `org-attach' to use relative directory
        org-attach-id-dir ".attach"
        org-attach-dir-relative t)
  (map!
   (:map org-mode-map
    :i "RET" nil
    :i [return] nil
    "C-c C-p" #'org-cliplink)))

(use-package! evil-org
  :defer-incrementally t
  :config
  (map! (:map evil-org-mode-map
         :i "RET" nil
         :i [return] nil)))

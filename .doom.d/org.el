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
        org-download-image-html-width 600))

;;; org.el -*- lexical-binding: t; -*-

;;; org.el -*- lexical-binding: t; -*-


(use-package! org
  :defer-incrementally t
  :init
  (add-hook 'org-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local tab-width 2)))
  (add-hook 'org-mode-hook #'org-fragtog-mode)
  :config
  (map! :map org-mode-map
        "C-c C-p" #'org-cliplink)
  (setq org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "WATING(w)" "|" "DONE(d)" "DEFERRED(D)" "CANCELLED(c)"))
        org-download-image-html-width 600))

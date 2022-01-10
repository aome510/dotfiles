;;; org.el -*- lexical-binding: t; -*-

(setq org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "WATING(w)" "|" "DONE(d)" "DEFERRED(D)" "CANCELLED(c)")))
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)

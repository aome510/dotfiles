;;; vue.el -*- lexical-binding: t; -*-

(define-derived-mode vue-mode web-mode "vue-mode"
  "A major derived from web-mode for editing .vue files")

(setq-hook! 'vue-mode-hook +format-with :none)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook! 'vue-mode-hook #'lsp!)

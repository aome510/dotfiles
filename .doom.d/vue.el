;;; vue.el -*- lexical-binding: t; -*-

(define-derived-mode vue-mode web-mode "vue-mode"
  "A major derived from web-mode for editing .vue files")

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook! 'vue-mode-hook #'lsp!)

;; only use eslint.fixAll to format
;; (setq-hook! 'vue-mode-hook +format-with :none)
;; (add-hook! 'vue-mode-hook #'add-eslint-fix-all-to-before-save-hook)
;; (setq-hook! 'vue-mode-hook flycheck-checker 'javascript-eslint)

;; (use-package! lsp-vetur
;;   :config
;;   (setq lsp-vetur-format-enable nil))

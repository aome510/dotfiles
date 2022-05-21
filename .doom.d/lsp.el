;;; lsp.el -*- lexical-binding: t; -*-

;;; --------------------------------------------------------------------
;;; lsp-mode configurations
;;; --------------------------------------------------------------------

(use-package! lsp-mode
  :when (featurep! :tools lsp)
  :defer-incrementally t
  :config
  (setq
   lsp--show-message nil
   lsp-ui-sideline-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil))

;;; lsp-rust
(use-package! lsp-rust
  :when (featurep! :lang rust +lsp)
  :defer-incrementally t
  :config
  (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
        lsp-rust-analyzer-cargo-watch-command "clippy"

        rustic-default-clippy-arguments ""

        ;; enable rust-analyzer inlay hints
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable t
        lsp-rust-analyzer-display-reborrow-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t))

;;; lsp-pyright
(use-package! lsp-pyright
  :when (featurep! :lang (python +lsp +pyright))
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;;; --------------------------------------------------------------------
;;; eglot (lsp) configurations
;;; --------------------------------------------------------------------

;; (use-package! eglot
;;   :when (featurep! :tools lsp +eglot)
;;   :config
;;   (set-popup-rule! "^\\*eglot-help" :size 0.25 :quit t :select t))

;;; --------------------------------------------------------------------
;;; tree-sitter configurations
;;; --------------------------------------------------------------------

(use-package! tree-sitter
  :init
  ;; enable tree-sitter globally
  (global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

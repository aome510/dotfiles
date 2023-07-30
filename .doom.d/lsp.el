;;; lsp.el -*- lexical-binding: t; -*-

;;; --------------------------------------------------------------------
;;; lsp-mode configurations
;;; --------------------------------------------------------------------

(use-package! lsp-mode
  :defer-incrementally t
  :when (modulep! :tools lsp)
  :config
  (setq
   lsp-inlay-hint-enable t
   lsp--show-message nil
   lsp-modeline-diagnostics-enable nil))

;;; lsp-rust
(use-package! lsp-rust
  :defer-incrementally t
  :when (modulep! :lang rust +lsp)
  :config
  (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
        lsp-rust-analyzer-cargo-watch-command "clippy"

        rustic-default-clippy-arguments ""

        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable t
        lsp-rust-analyzer-display-reborrow-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t))

;;; --------------------------------------------------------------------
;;; eglot (lsp) configurations
;;; --------------------------------------------------------------------

;; (use-package! eglot
;;   :when (modulep! :tools lsp +eglot)
;;   :config
;;   (set-popup-rule! "^\\*eglot-help" :size 0.25 :quit t :select t))

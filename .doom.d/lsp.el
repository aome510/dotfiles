;;; lsp.el -*- lexical-binding: t; -*-

;;; --------------------------------------------------------------------
;;; lsp-mode configurations
;;; --------------------------------------------------------------------

(use-package! lsp-mode
  :defer-incrementally t
  :when (featurep! :tools lsp)
  :config
  (setq
   lsp--show-message nil))

;;; lsp-rust
(use-package! lsp-rust
  :defer-incrementally t
  :when (featurep! :lang rust +lsp)
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
  :defer-incrementally t
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

;;; lsp-mode.el -*- lexical-binding: t; -*-

;;; lsp packages

(use-package! lsp-eslint
  :when (featurep! :tools lsp)
  :config
  (setq lsp-eslint-format nil
        lsp-eslint-auto-fix-on-save t))

(use-package! lsp-mode
  :when (featurep! :tools lsp)
  :defer t
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]screenshots\\'")
  (setq
   lsp-signature-doc-lines 8
   lsp-modeline-diagnostics-enable nil))

(use-package! lsp-ui
  :when (featurep! :tools lsp)
  :defer t
  :config
  (setq lsp-idle-delay 1
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-enable t
        lsp-ui-doc-max-height 16
        lsp-ui-doc-max-width 64))

(use-package! lsp-rust
  :when (featurep! :lang rust +lsp)
  :defer t
  :config
  (setq lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-run-build-scripts t))

(map!
 ;;; lsp
 (:when (featurep! :tools lsp)
  (:after lsp-mode
   :map lsp-mode-map
   (:leader :prefix "c"
    :desc "Restart workspace" "R" #'lsp-restart-workspace
    :desc "Find references" "r" #'lsp-find-references))))

;;; disable the lsp's provided formatter
(setq +format-with-lsp nil)

;;; config safe local variables
;; (add-to-list 'safe-local-variable-values '(+format-with-lsp . nil))

;;; eslint-related configurations

(setq lsp-eslint-validate (vconcat lsp-eslint-validate '("typescript" "vue")))

(defun add-eslint-fix-all-to-before-save-hook ()
  (add-hook! 'before-save-hook :local #'lsp-eslint-fix-all))

;; (add-hook! 'js-mode-hook #'add-eslint-fix-all-to-before-save-hook)
(add-hook! 'js2-mode-hook #'add-eslint-fix-all-to-before-save-hook)
(add-hook! 'typescript-mode-hook #'add-eslint-fix-all-to-before-save-hook)

;; (setq-hook! 'js-mode-hook flycheck-checker 'javascript-eslint)
;; (setq-hook! 'js2-mode-hook flycheck-checker 'javascript-eslint)

;;; lsp-mode.el -*- lexical-binding: t; -*-

;;; lsp packages

(use-package! lsp-mode
  :when (featurep! :tools lsp)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]screenshots\\'")
  (add-hook 'TeX-mode-hook #'lsp!)
  (setq
   +latex--company-backends '(:separate company-capf company-yasnippet company-dabbrev)
   ;; lsp-enable-file-watchers nil
   lsp-restart 'auto-restart
   lsp-enable-symbol-highlighting nil
   lsp-idle-delay 1.0
   ;; lsp-eldoc-render-all t
   lsp-signature-doc-lines 10
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil)
  (map!
   :when (featurep! :tools lsp)
   (:leader :prefix "c"
    :desc "LSP Treemacs Symbols" "T" #'lsp-treemacs-symbols)))

;; (use-package! lsp-ui
;;   :when (featurep! :tools lsp)
;;   :config
;;   (setq lsp-ui-doc-delay 0.2
;;         lsp-ui-doc-enable t
;;         lsp-ui-doc-max-height 16
;;         lsp-ui-doc-max-width 100))

;;; lsp-rust
(use-package! lsp-rust
  :when (featurep! :lang rust +lsp)
  :config
  (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
        ;; lsp-rust-analyzer-cargo-watch-enable nil
        lsp-rust-analyzer-cargo-watch-command "clippy"))

;;; lsp-eslint
(defun add-eslint-fix-all-to-before-save-hook ()
  (add-hook! 'before-save-hook :local #'lsp-eslint-fix-all))

(use-package! lsp-eslint
  :config
  (add-hook! 'vue-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (add-hook! 'js2-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (add-hook! 'typescript-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (setq lsp-eslint-validate (vconcat lsp-eslint-validate '("typescript" "vue"))))

;;; lsp-vetur
(use-package! lsp-vetur
  :config
  (setq lsp-vetur-format-enable nil))

;;; dap-mode
(use-package! dap-mode
  :config
  (require 'dap-go)
  (map!
   (:map dap-mode-map
    :localleader
    :desc "Dap hydra mode" "d" #'dap-hydra)))

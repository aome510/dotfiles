;;; lsp-mode.el -*- lexical-binding: t; -*-

;;; lsp packages

(defun add-eslint-fix-all-to-before-save-hook ()
  (add-hook! 'before-save-hook :local #'lsp-eslint-fix-all))

(use-package! lsp-mode
  :when (featurep! :tools lsp)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]screenshots\\'")
  ;; lsp on-change hack based on https://github.com/redguardtoo/emacs.d#set-up-lsp-mode
  ;; don't ping LSP language server too frequently
  ;; (defvar lsp-on-touch-time 0)
  ;; (defun my-lsp-on-change-hack (orig-fun &rest args)
    ;; do NOT run `lsp-on-change' too frequently
  ;;   (when (> (- (float-time (current-time))
  ;;               lsp-on-touch-time) 60) ;; 1 min
  ;;     (setq lsp-on-touch-time (float-time (current-time)))
  ;;     (apply orig-fun args)))
  ;; (advice-add 'lsp-on-change :around #'my-lsp-on-change-hack)
  ;; lsp variables
  (setq
   lsp-restart 'auto-restart
   lsp-enable-symbol-highlighting nil
   lsp-idle-delay 1.0
   ;; lsp-eldoc-render-all t
   lsp-signature-doc-lines 10
   lsp-modeline-diagnostics-enable nil))

;; (use-package! lsp-ui
;;   :when (featurep! :tools lsp)
;;   :config
;;   (setq lsp-ui-doc-delay 0.5
;;         lsp-ui-doc-enable t
;;         lsp-ui-doc-max-height 16
;;         lsp-ui-doc-max-width 64))

(add-to-list 'safe-local-variable-values '(+format-with-lsp . nil))

;;; lsp-rust
(use-package! lsp-rust
  :when (featurep! :lang rust +lsp)
  :config
  (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
        lsp-rust-analyzer-cargo-watch-command "clippy"))

;;; setup lsp server for latex-mode
(setq +latex--company-backends '(:separate company-capf company-yasnippet company-dabbrev))
(add-hook 'TeX-mode-hook #'lsp!)

;;; lsp-eslint
(use-package! lsp-eslint
  :config
  (add-hook! 'js2-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (add-hook! 'typescript-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (setq lsp-eslint-validate (vconcat lsp-eslint-validate '("typescript" "vue"))))

;;; setup lsp server for vue-mode
(add-hook! 'vue-mode-hook #'add-eslint-fix-all-to-before-save-hook)

(use-package! lsp-vetur
  :config
  (setq lsp-vetur-format-enable nil))

;;; dap-mode configurations
(use-package! dap-mode
  :config
  (require 'dap-go)
  (map!
   (:map dap-mode-map
    :localleader
    :desc "Dap hydra mode" "d" #'dap-hydra)))

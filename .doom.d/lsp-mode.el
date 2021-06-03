;;; lsp-mode.el -*- lexical-binding: t; -*-

;;; lsp packages

(defun add-eslint-fix-all-to-before-save-hook ()
  (add-hook! 'before-save-hook :local #'lsp-eslint-fix-all))

(use-package! lsp-eslint
  :config
  (add-hook! 'js2-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (add-hook! 'typescript-mode-hook #'add-eslint-fix-all-to-before-save-hook)
  (setq lsp-eslint-validate (vconcat lsp-eslint-validate '("typescript" "vue"))))

(use-package! lsp-mode
  :when (featurep! :tools lsp)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]screenshots\\'")
  (setq
   lsp-signature-doc-lines 8
   lsp-modeline-diagnostics-enable nil))

(use-package! lsp-ui
  :when (featurep! :tools lsp)
  :config
  (setq lsp-idle-delay 1
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-enable t
        lsp-ui-doc-max-height 16
        lsp-ui-doc-max-width 64))

(add-to-list 'safe-local-variable-values '(+format-with-lsp . nil))

;;; setup lsp server for latex-mode
(add-hook 'TeX-mode-hook #'lsp!)
(add-hook 'TeX-mode-hook (lambda () (setq-local +lsp-company-backends
                                                '(:separate company-capf company-yasnippet company-dabbrev))))

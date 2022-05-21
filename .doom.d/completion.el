;;; completion.el -*- lexical-binding: t; -*-

;;; ----------------------------------
;;; company
;;; ----------------------------------
;; (use-package! company
;;   :when (featurep! :completion company)
;;   :defer-incrementally t
;;   :config
;;   (setq
;;    company-idle-delay 0
;;    company-dabbrev-code-everywhere t
;;    company-selection-wrap-around t
;;    company-minimum-prefix-length 2
;;    +lsp-company-backends '(company-files company-capf company-yasnippet company-dabbrev))

;;   (set-company-backend! 'prog-mode '(company-capf company-dabbrev-code company-yasnippet)))

;;; ----------------------------------
;;; snippets
;;; ----------------------------------
(use-package! doom-snippets
  :load-path "~/.doom.d/snippets"
  :after yasnippet)

;;; ----------------------------------
;;; corfu related configurations
;;; ----------------------------------

;; necessary settings for `corfu' to work when `company' is installed
;; (use-package lsp-mode
;;   :custom
;;   (lsp-completion-provider :none))
;; (use-package! company
;;   :init
;;   (setq company-global-modes nil))

(use-package! corfu
  :init
  (global-corfu-mode)
  :config
  (add-hook! corfu-mode #'corfu-doc-mode)
  (setq
   corfu-cycle t
   corfu-auto t
   corfu-separator ?\s
   corfu-preview-current t
   corfu-echo-documentation t
   corfu-quit-no-match t
   corfu-quit-at-boundary t
   corfu-auto-delay 0
   corfu-auto-prefix 2))

(use-package! cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package! kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; ----------------------------------
;;; consult
;;; ----------------------------------
(use-package! consult
  :init
  (define-key!
      [remap recentf-open-files] #'consult-recent-file)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  ;; upon spliting window (evil-window-split), open `project-find-file'
  (setq evil-vsplit-window-right t
        evil-split-window-below t)
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (project-find-file)))

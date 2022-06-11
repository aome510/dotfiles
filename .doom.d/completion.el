;;; completion.el -*- lexical-binding: t; -*-

;;; ----------------------------------
;;; company
;;; ----------------------------------
;; (use-package! company
;;   :when (featurep! :completion company)
;;   :config
;;   (setq
;;    company-idle-delay 0
;;    company-dabbrev-code-everywhere t
;;    company-selection-wrap-around t
;;    company-minimum-prefix-length 2
;;    +lsp-company-backends '(company-files company-capf company-yasnippet company-dabbrev))

;;   (set-company-backend! 'prog-mode '(company-capf company-dabbrev-code company-yasnippet))

;;   (map!
;;    (:map company-active-map
;;     "RET" #'company-complete-selection
;;     [return] #'company-complete-selection
;;     "TAB" nil
;;     [tab] nil)))

;;; ----------------------------------
;;; snippets
;;; ----------------------------------
(use-package! doom-snippets
  :load-path "~/.doom.d/snippets"
  :after yasnippet)

;;; ----------------------------------
;;; corfu related configurations
;;; ----------------------------------


(use-package! corfu
  :init
  (global-corfu-mode)
  :config
  ;; use corfu for minibuffer completion
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

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
   corfu-auto-prefix 2)

  (map!
   (:map corfu-map
    "RET" #'corfu-insert
    [return] #'corfu-insert
    "TAB" nil
    [tab] nil
    :desc "Toggle corfu doc"      "M-d" #'corfu-doc-toogle
    :desc "Scroll corfu doc up"   "M-n" #'corfu-doc-scroll-up
    :desc "Scroll corfu doc down" "M-p" #'corfu-doc-scroll-down)))

(defun append-cape-capf-functions ()
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-tex t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))

(use-package! cape
  :init
  (append-cape-capf-functions)
  :config
  (setq
   cape-dabbrev-min-length 3))

;; `corfu' integration with `lsp-mode'
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :config
  ;; use `cape-dabbrev'
  (add-hook! 'lsp-completion-mode-hook #'append-cape-capf-functions))


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

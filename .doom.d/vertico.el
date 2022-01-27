;;; vertico.el -*- lexical-binding: t; -*-

(use-package! corfu
  :config
  (setq
   corfu-cycle t
   corfu-auto t
   corfu-quit-no-match t
   corfu-quit-at-boundary t
   corfu-auto-delay 0
   corfu-auto-prefix 2)
  (map!
   :i "C-n" #'corfu-next
   :i "C-p" #'corfu-previous)
  :init
  (corfu-global-mode))

(use-package! cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
)

(use-package! kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package! vertico
  :config
  (map! :i "C-;" #'completion-at-point))

;;; orderless integration
(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(use-package! orderless
  :config
  (advice-add 'company-capf--candidates :around #'just-one-face)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))


;;; marginalia integration
(use-package! marginalia
  :init
  (marginalia-mode)
  :config
  (add-hook! 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;;; consult integration
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

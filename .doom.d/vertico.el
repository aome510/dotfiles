;;; vertico.el -*- lexical-binding: t; -*-

;; (use-package! corfu
;;   :config
;;   (setq
;;    corfu-cycle t
;;    corfu-auto t
;;    corfu-auto-delay 0.2
;;    corfu-auto-prefix 2
;;    corfu-commit-predicate nil
;;    corfu-quit-at-boundary t)
;;   (map!
;;    :map corfu-map
;;    "TAB" nil
;;    [tab] nil
;;    "C-j" #'corfu-next
;;    "C-k" #'corfu-previous
;;    "C-f" #'corfu-insert)
;;   (add-hook! 'corfu-mode-hook (if (company-mode) (company-mode -1) (company-mode +1))))

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
    (project-find-file))
  (consult-customize
   consult-lsp-symbols
   :preview-key (kbd "C-SPC"))
  (map!
   :n "g SPC" #'consult-goto-line))

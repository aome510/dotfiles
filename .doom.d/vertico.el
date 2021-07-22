;;; vertico.el -*- lexical-binding: t; -*-

(use-package! corfu
  :config
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  :init
  (corfu-global-mode))

(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :config
  (add-hook 'vertico-mode-hook (lambda ()
                                 (setq completion-in-region-function
                                       (if vertico-mode
                                           #'consult-completion-in-region
                                         #'completion--in-region))))
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (setq vertico-cycle t)
  (map! :i "C-;" #'completion-at-point))

;;; orderless integration
(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(use-package orderless
  :config
(advice-add 'company-capf--candidates :around #'just-one-face)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;;; marginalia integration
(use-package marginalia
  :init
  (marginalia-mode))

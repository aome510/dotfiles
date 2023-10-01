;;; ui.el -*- lexical-binding: t; -*-

;;; --------------------------------------------------------------------
;;; General variable configurations
;;; --------------------------------------------------------------------

(setq doom-font (font-spec :family "IBM Plex Mono" :size 15)
      doom-big-font (font-spec :family "IBM Plex Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "IBM Plex Serif" :size 15))

(setq doom-theme 'nano-light)

(setq display-line-numbers-type 'relative)

;;; --------------------------------------------------------------------
;;; nano-theme related configurations
;;; --------------------------------------------------------------------

;; (defun get-buffer-file-path-relative-to-project-root ()
;;   (let ((project (projectile-project-root))
;;         (buffer (buffer-file-name)))
;;     (if (and project (string-prefix-p project buffer))
;;         (string-remove-prefix project buffer)
;;       buffer)))

(use-package! nano-modeline
  :config
  ;; modify some faces to make them compatible with nano-modeline
  (custom-theme-set-faces! '(nano-light nano-dark)
    '(mode-line :inhert nano-modeline-active)
    '(mode-line-inactive :inhertit nano-modeline-inactive))
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (nano-modeline-text-mode t))

(use-package! nano-theme
  :init
  ;; Doom default faces can be found in https://github.com/doomemacs/themes/blob/master/doom-themes-base.el for references.
  ;; Customize faces that don't work well with `nano-theme'
  (custom-theme-set-faces! '(nano-light nano-dark)
    ;; evil-ex search and replace faces don't play nicely with the nano-theme's subtle face
    ;; The subtle face is used for text selection background so it's impossible to distinguish
    ;; between the match face and the selection face during the search-and-replace operation.
    '(evil-ex-search                 :inherit lazy-highlight :foreground "base0" :weight bold)
    '(evil-ex-substitute-matches     :background "base0"     :foreground "red"   :weight bold :strike-through t)
    '(evil-ex-substitute-replacement :background "base0"     :foreground "green" :weight bold)
    '(lazy-highlight                 :inherit nano-popout-i)
    '(org-code                       :inherit nano-popout)))


;;; --------------------------------------------------------------------
;;; Other UI configurations
;;; --------------------------------------------------------------------

(use-package! doom-themes
  :config
  ;; customize treemacs theme using `doom-themes'
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

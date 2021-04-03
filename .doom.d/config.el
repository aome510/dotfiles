
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thang Pham"
      user-mail-address "phamducthang1234@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;


;; Package configurations

;;; company
(use-package! company
  :when (featurep! :completion company)
  :config
  (setq company-idle-delay 0))


;;; flx
(use-package! flx
  :when (featurep! :completion ivy +fuzzy)
  :config
  (setq ivy-flx-limit 1024))

;;; lsp packages
(use-package! lsp-ui
  :when (featurep! :tools lsp)
  :defer t
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-max-height 16
        lsp-ui-doc-max-width 64
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t))

;;; snippets
(use-package! doom-snippets
  :load-path "~/.doom.d/snippets"
  :after yasnippet)

;;; latex
(use-package! latex
  :when (featurep! :lang latex +lsp)
  :config
  (setq +latex-viewers '(pdf-tools evince)
        TeX-source-correlate-start-server  t)
  (add-hook! ('TeX-mode-hook) #'lsp!))

;;; format
(use-package! format
  :when (featurep! :editor format)
  :config
  (setq +format-on-save-enabled-modes '(not)))

;; User's defined key bindings
(map!
 ;; motions
 :mvn "g h" #'evil-beginning-of-line
 :mvn "g i" #'evil-first-non-blank
 :mvn "g l" #'evil-end-of-line

 :n "U" #'evil-redo
 )

(map!
 ;; user's defined keybindings with leader keys
 :leader
 (:prefix-map ("a" . "custom keybindings")
  :desc  "Align left"    "l" #'evil-lion-left
  :desc  "Align right"   "r" #'evil-lion-right)
 :prefix "c"
 :map tex-mode-map
 :desc  "LaTeX Run all" "r" #'TeX-command-run-all
 :desc  "LaTeX View"    "v" #'TeX-view
 :desc  "LaTeX Build"   "b" #'TeX-command-master
 )

(map!
 ;; key bindings for packages
 ;;; multi-cursors
 (:when (featurep! :editor multiple-cursors)
  (:after evil-mc
   :nv "C-n" #'evil-mc-make-and-goto-next-match
   :nv "C-p" #'evil-mc-make-and-goto-prev-match))
 ;;; ivy
 (:when (featurep! :completion ivy)
  (:after ivy
   :map ivy-minibuffer-map
   "C-h" #'ivy-backward-delete-char))
 ;;; company
 (:when (featurep! :completion company)
  (:after company
   (:map company-active-map
    [RET] nil
    [return] nil
    [tab] #'company-complete-selection
    "C-l" #'company-complete-selection))))
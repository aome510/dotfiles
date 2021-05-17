
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file! Some functionality uses this to identify you, e.g. GPG configuration, email clients, file templates and snippets. (setq user-full-name "Thang Pham" user-mail-address "phamducthang1234@gmail.com")

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

(load! "kak")
(load! "vue")

;; Custom commands
(defun term-other-window (&optional side size)
  (split-window (selected-window) size side)
  (term shell-file-name))

(evil-define-command evil-shell-command-on-region (beg end command)
  (interactive (let (string)
                 (unless (mark)
                   (user-error "The mark is not set now, so there is no region"))
                 (setq command (read-shell-command "Shell command on region: "))
                 (list (region-beginning) (region-end)
                       command)))
  (shell-command-on-region beg end command nil t shell-command-default-error-buffer t (region-noncontiguous-p)))

;; Package configurations

;;; doom-theme
(use-package doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config))

;;; company
(use-package! company
  :when (featurep! :completion company)
  :config
  (setq company-idle-delay 0.0
        company-selection-wrap-around t
        ;; company-dabbrev-code-everywhere t
        ;; company-dabbrev-char-regexp "[A-Za-z0-9]"
        company-minimum-prefix-length 1))

;;; gcmh
(use-package! gcmh
  :config
  (setq-default gcmh-high-cons-threshold (* 64 1024 1024)))

;;; lsp packages

(use-package! lsp-mode
  :when (featurep! :tools lsp)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (setq
   lsp-signature-doc-lines 8
   lsp-modeline-diagnostics-enable nil))

(use-package! lsp-ui
  :when (featurep! :tools lsp)
  :defer t
  :config
  (setq lsp-idle-delay 1
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 32))

(use-package! lsp-rust
  :when (featurep! :lang rust +lsp)
  :config
  (setq lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-run-build-scripts t))

;;; snippets
(use-package! doom-snippets
  :load-path "~/.doom.d/snippets"
  :after yasnippet)

;;; latex
(use-package! latex
  :when (featurep! :lang latex +lsp)
  :config
  ;; variable settings
  (setq TeX-electric-sub-and-superscript nil)
  ;; preview latex using pdf tools
  (setq +latex-viewers '(pdf-tools evince))
  (setq TeX-command-force "LatexMk")
  ;; disable smartparens in latex mode
  (add-hook 'TeX-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'TeX-mode-hook #'lsp!)
  (add-hook 'TeX-mode-hook (lambda () (setq-local +lsp-company-backends
                                                  '(:separate company-capf company-yasnippet company-dabbrev)))))

;;; pdf-tools
(use-package! pdf-tools
  :when (featurep! :tools pdf)
  :config
  (setq-default pdf-view-display-size 'fit-width))

;;; format
(use-package! format
  :when (featurep! :editor format)
  :config
  (setq +format-on-save-enabled-modes '(not sql-mode js-mode js2-mode vue-mode json-mode)))

;;; evil packages
(use-package! evil
  :when (featurep! :editor evil +everywhere)
  :config
  (setq evil-cross-lines t)
  (setq evil-snipe-scope 'visible))

;;; recentf

(defvar recentf-keep-dot-folders
  '("/home/aome510/.config" "/home/aome510/.doom.d" "/home/aome510/.cargo/registry/src"))

(defun recentf-dot-file-ignore-p (file)
  (if (string-match-p "^/home/aome510/\\.[-._[:alnum:]]+/" file)
      (not (seq-reduce
            (lambda (acc folder) (or acc (string-prefix-p folder file)))
            recentf-keep-dot-folders
            nil))
    nil))

(defun recentf-file-ignore-p (file)
  (if (string-match-p "^/home/aome510/" file)
      (or
       (recentf-dot-file-ignore-p file)
       (not (file-readable-p file)))
    t))

(use-package! recentf
  :config
  (setq recentf-exclude '(recentf-file-ignore-p)
        recentf-max-saved-items 1024))

;; user-defined key mappings

(map!
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease

 "M-<return>" #'toggle-frame-fullscreen
 "M-<escape>" #'normal-mode

 (:after flycheck
  :n "] e" #'flycheck-next-error
  :n "[ e" #'flycheck-previous-error)

 ;; custom leader key = "a"
 (:leader
  :prefix ("a" . "custom keybindings")
  :desc "Align Left"    "l" #'evil-lion-left
  :desc "Align Right"   "r" #'evil-lion-right)

 (:leader :prefix "p"
  :desc "Projectile Dired" "SPC" #'projectile-dired)

 (:leader :prefix "o"
  :desc "Open terminal in other window (right)" "t"
  #'(lambda (&optional size) (interactive "P") (term-other-window 'left size))
  :desc "Open small terminal in other window (below)" "T"
  #'(lambda (&optional size) (interactive "P") (term-other-window 'above (if size size 10))))


 (:when (featurep! :editor evil +everywhere)
  (:after evil
   ;; Kakoune-like key bindings
   :mvn "g h" #'evil-beginning-of-line
   :mvn "g i" #'evil-first-non-blank
   :mvn "g l" #'evil-end-of-line

   :mvn "M-n" #'evil-ex-search-previous

   :n "U" #'evil-redo

   :v "|" #'evil-shell-command-on-region

   (:map evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
   (:map evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block))))

;; Packages key bindings
(map!
 ;;; latex
 (:when (featurep! :lang latex)
  (:after latex
   (:map LaTeX-mode-map
    :localleader
    :desc  "LaTeX View"    "v" #'TeX-view
    :desc  "LaTeX Build"   "b" #'TeX-command-master
    :desc  "LaTeX Run all" "r" #'TeX-command-run-all)))

 ;; dired
 (:after dired
  :map dired-mode-map
  :n "h" #'dired-up-directory
  :n "l" #'dired-find-file)

 ;;; tex-evil
 (:after evil-tex
  (:map TeX-mode-map
   :nv "m" #'evil-set-marker))

 ;;; multi-cursors
 (:when (featurep! :editor multiple-cursors)
  (:after evil
   :nv "C-n" #'evil-mc-make-and-goto-next-match
   :nv "C-p" #'evil-mc-make-and-goto-prev-match))

 ;; evil
 (:when (featurep! :editor evil +everywhere)
  (:after evil
   :nv "TAB" #'evil-indent-line
   :nv [tab] #'evil-indent-line))

 ;; winum
 (:when (featurep! :ui window-select +numbers)
  (:after winum
   :leader
   :desc "winum-select-window-1" "1" #'winum-select-window-1
   :desc "winum-select-window-2" "2" #'winum-select-window-2
   :desc "winum-select-window-3" "3" #'winum-select-window-3
   :desc "winum-select-window-4" "4" #'winum-select-window-4
   :desc "winum-select-window-5" "5" #'winum-select-window-5
   :desc "winum-select-window-6" "6" #'winum-select-window-6
   :desc "winum-select-window-7" "7" #'winum-select-window-7
   :desc "winum-select-window-8" "8" #'winum-select-window-8
   :desc "winum-select-window-9" "9" #'winum-select-window-9))

 ;;; ivy
 (:when (featurep! :completion ivy)
  (:after ivy
   :map ivy-minibuffer-map
   "C-h" #'ivy-backward-delete-char))

 ;;; lsp
 (:when (featurep! :tools lsp)
  (:after lsp-mode
   :map lsp-mode-map
   (:leader :prefix "c"
    :desc "Restart workspace" "R" #'lsp-restart-workspace
    :desc "Find references" "r" #'lsp-find-references)))

 ;;; company
 (:when (featurep! :completion company)
  (:after company
   (:map company-active-map
    "RET" nil
    [return] nil
    "TAB" nil
    [tab] nil
    "C-l" #'company-complete-selection)))

 ;;; yasnippet
 (:when (featurep! :editor snippets)
  (:after yasnippet
   (:map yas-minor-mode-map
    "TAB" #'yas-next-field-or-maybe-expand
    [tab] #'yas-next-field-or-maybe-expand))))

;; others

;;; upon spliting window, open projectile-find-file
(after! evil (after! ivy
               (setq evil-vsplit-window-right t
                     evil-split-window-below t)
               (defadvice! prompt-for-buffer (&rest _)
                 :after '(evil-window-split evil-window-vsplit)
                 (counsel-buffer-or-recentf))))

;;; disable inserting tabs
(global-unset-key (kbd "TAB"))

;;; save file with C-s
(global-set-key (kbd "C-s") #'save-buffer)

;;; remove latex autofill
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;;; treat underscore as word character
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;;; enable evil-mc globally
(global-evil-mc-mode 1)

;;; auto scrolling
(setq scroll-conservatively 8
      scroll-step 8)

;;; eslint-related configurations

(defun add-eslint-fix-all-to-before-save-hook ()
  (add-hook! 'before-save-hook :local #'lsp-eslint-apply-all-fixes))

;; use eslint to format code not the default "prettier" defined in "format-all"
(add-hook! 'js-mode-hook #'add-eslint-fix-all-to-before-save-hook)
(add-hook! 'js2-mode-hook #'add-eslint-fix-all-to-before-save-hook)

;; use eslint for flycheck
(setq-hook! 'js-mode-hook flycheck-checker 'javascript-eslint)
(setq-hook! 'js2-mode-hook flycheck-checker 'javascript-eslint)

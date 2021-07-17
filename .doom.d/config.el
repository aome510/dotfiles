
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
(setq doom-font (font-spec :family "monospace" :size 16)
      doom-big-font (font-spec :family "monospace" :size 24)
      doom-variable-pitch-font (font-spec :family "monospace" :size 16))

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

;; ----------------------------------
;; additional loadings
;; ----------------------------------

(load! "vue")
;; (load! "lsp-eglot")
(load! "lsp-mode")

;; ----------------------------------
;; Custom commands/functions
;; ----------------------------------

(defun enable-big-font-mode ()
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 108)
  (set-face-attribute 'variable-pitch (selected-frame) :height 108))

(defun ripgrep-search-project (search-term &rest args)
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (if (require 'ripgrep nil 'noerror)
      (let ((rg-args (mapcar (lambda (val) (concat "--glob !" val))
                             (append projectile-globally-ignored-files
                                     projectile-globally-ignored-directories))))
        (ripgrep-regexp search-term
                        (projectile-acquire-root)
                        (append rg-args args)))))

(defun replace-from-clipboard (beg end)
  (interactive "r")
  (delete-region beg end)
  (goto-char beg)
  (insert-for-yank (current-kill 0)))

(defun save-buffer-without-auto-formatting ()
  (interactive)
  (setq temp before-save-hook)
  (setq before-save-hook nil)
  (save-buffer)
  (setq before-save-hook temp))

(defun term-other-window (&optional side size)
  (split-window (selected-window) size side)
  (vterm "*terminal*"))

;; ----------------------------------
;; Package configurations
;; ----------------------------------

;;; doom-theme
(use-package! doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

;;; company
(use-package! company
  :when (featurep! :completion company)
  :config
  (set-company-backend! 'prog-mode '(company-files company-capf company-yasnippet))
  (setq
   +lsp-company-backends '(company-files company-capf company-yasnippet)
   company-idle-delay 0.1
   company-async-redisplay-delay 0.001
   company-selection-wrap-around t
   ;; company-dabbrev-code-everywhere t
   ;; company-dabbrev-char-regexp "[A-Za-z0-9]"
   company-minimum-prefix-length 1))

;;; gcmh
(use-package! gcmh
  :config
  (setq-default gcmh-idle-delay 15)
  (setq-default gcmh-high-cons-threshold (* 50 1024 1024)))

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
  ;;; remove latex autofill
  (remove-hook 'text-mode-hook #'turn-on-auto-fill)
  ;; disable smartparens in latex mode
  (add-hook 'TeX-mode-hook #'turn-off-smartparens-mode))

;;; pdf-tools
(use-package! pdf-tools
  :when (featurep! :tools pdf)
  :config
  (setq-default pdf-view-display-size 'fit-width))

;;; format
(use-package! format
  :when (featurep! :editor format)
  :config
  (setq +format-on-save-enabled-modes '(not sql-mode tex-mode latex-mode vue-mode)))

;;; evil packages
(use-package! evil
  :when (featurep! :editor evil +everywhere)
  :config
  (setq evil-cross-lines t)
  (setq evil-snipe-scope 'visible))

;;; emacs-tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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

;;; kak
(use-package! kak)

;;; magit
(use-package! magit
  :config
  (setq git-commit-summary-max-length 100))

;;; ssh-agency
(use-package! ssh-agency)

;; auto save recentf list every 5 minutes
(run-at-time nil (* 5 60) 'recentf-save-list)

;; ----------------------------------
;; user-defined key mappings
;; ----------------------------------

(map!
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease

 :nv "TAB" #'indent-for-tab-command
 :nv [tab] #'indent-for-tab-command

 :v "R" #'replace-from-clipboard

 (:leader
  :desc "Expand Region" "=" #'er/expand-region)

 (:leader :prefix "b"
  :desc "Save buffer without formatting" "s" #'save-buffer-without-auto-formatting)

 (:leader :prefix "t"
  :desc "Treesitter mode" "t" #'tree-sitter-mode)

 "M-<return>" #'toggle-frame-fullscreen
 "M-<escape>" #'normal-mode

 (:when (featurep! :checkers syntax)
  (:after flycheck
   :map flycheck-mode-map
   :n "] e" #'flycheck-next-error
   :n "[ e" #'flycheck-previous-error))

 (:leader :prefix "s"
  :desc "Ripgrep Search Project" "g" #'ripgrep-search-project)

 (:leader :prefix "p"
  :desc "Projectile Dired" "SPC" #'projectile-dired)

 (:leader :prefix "o"
  :desc "Open terminal in other window (right)" "T"
  #'(lambda (&optional size) (interactive "P") (term-other-window 'left size))
  :desc "Open small terminal (below)" "t" #'vterm)

 ;; evil-related packages
 (:when (featurep! :editor evil +everywhere)
  (:leader
   :prefix ("a" . "custom keybindings")
   :desc "Align Left"    "l" #'evil-lion-left
   :desc "Align Right"   "r" #'evil-lion-right)

  ;; Kakoune-like key bindings
  :mvn "g h" #'evil-beginning-of-line
  :mvn "g i" #'evil-first-non-blank
  :mvn "g l" #'evil-end-of-line
  :mvn "M-n" #'evil-ex-search-previous
  :mvn "g %" #'mark-whole-buffer


  :n "U" #'undo-tree-redo
  :n "u" #'undo-tree-undo

  :v "|" #'evil-shell-command-on-region

  :n "g w" nil

  (:map evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
  (:map evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block))

 ;;; kak (evil-mc/multiple cursors)
 :v "|" #'kak-exec-shell-command
 :v "s" (lambda (beg end) (interactive "r") (kak-select beg end nil))
 :v "S" (lambda (beg end) (interactive "r") (kak-select beg end t))
 :v "M-s" #'kak-split-lines
 :v "M-k" (lambda () (interactive) (kak-filter t))
 :v "M-K" (lambda () (interactive) (kak-filter nil))
 :v ". #" #'kak-insert-index
 :v ". r" (lambda () (interactive) (kak-exec-shell-command "xsel -ob"))
 :mnv "h" #'backward-char ;; normal evil-backward-char doesn't allow cross-line when the fake cursor's at bol
 ;; :nv "g j" #'evil-mc-make-cursor-move-next-line
 ;; :nv "g k" #'evil-mc-make-cursor-move-prev-line

 ;; snipe/surround remap for kak commands in visual modes
 :v ". s" #'evil-snipe-s
 :v ". S" #'evil-surround-region
 (:after evil-surround
  (:map evil-surround-mode-map
   :v "S" nil
   :v ". S" #'evil-surround-region))

 ;;; latex
 (:when (featurep! :lang latex)
  (:after latex
   (:map LaTeX-mode-map
    :localleader
    :desc  "LaTeX View"    "v" #'TeX-view
    :desc  "LaTeX Build"   "b" #'TeX-command-master
    :desc  "LaTeX Run all" "r" #'TeX-command-run-all)))

 ;; dired
 (:when (featurep! :emacs dired)
  (:after dired
   :map dired-mode-map
   :n "h" #'dired-up-directory
   :n "l" #'dired-find-file))

 ;;; tex-evil
 ;; (:after evil-tex
 ;;  (:map TeX-mode-map
 ;;   :nv "m" #'evil-set-marker))

 ;;; multi-cursors
 (:when (featurep! :editor multiple-cursors)
  :nv "C-n" #'evil-mc-make-and-goto-next-match
  :nv "C-p" #'evil-mc-make-and-goto-prev-match)

 (:when (featurep! :ui treemacs)
  :leader
  :desc "Select treemacs window" "0" #'treemacs-select-window)

 ;; winum
 (:when (featurep! :ui window-select +numbers)
  :leader
  :desc "winum-select-window-1" "1" #'winum-select-window-1
  :desc "winum-select-window-2" "2" #'winum-select-window-2
  :desc "winum-select-window-3" "3" #'winum-select-window-3
  :desc "winum-select-window-4" "4" #'winum-select-window-4
  :desc "winum-select-window-5" "5" #'winum-select-window-5
  :desc "winum-select-window-6" "6" #'winum-select-window-6
  :desc "winum-select-window-7" "7" #'winum-select-window-7
  :desc "winum-select-window-8" "8" #'winum-select-window-8
  :desc "winum-select-window-9" "9" #'winum-select-window-9)

 ;;; ivy
 (:when (featurep! :completion ivy)
  (:after ivy
   :map ivy-minibuffer-map
   "C-h" #'ivy-backward-delete-char
   "C-f" #'ivy-alt-done
   :map ivy-reverse-i-search-map
   "C-k" #'previous-line
   "C-d" #'ivy-reverse-i-search-kill))

 ;;; company
 (:when (featurep! :completion company)
  (:after company
   (:map company-active-map
    "RET" nil
    [return] nil
    "TAB" nil
    [tab] nil
    "C-f" #'company-complete-selection)))

 ;;; yasnippet
 (:when (featurep! :editor snippets)
  (:after yasnippet
  (:map yas-minor-mode-map
    "TAB" #'yas-next-field-or-maybe-expand
    [tab] #'yas-next-field-or-maybe-expand))))

;; ----------------------------------
;; misc
;; ----------------------------------

;;; upon spliting window, open projectile-find-file
(after! evil (after! ivy
               (setq evil-vsplit-window-right t
                     evil-split-window-below t)
               (defadvice! prompt-for-buffer (&rest _)
                 :after '(evil-window-split evil-window-vsplit)
                 (counsel-buffer-or-recentf))))

;;; increase history length
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;;; save file with C-s or C-S (without formatting)
(global-set-key (kbd "C-s") #'save-buffer)

;;; tab always indent
(setq-default tab-always-indent nil)

;;; treat underscore as word character
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;;; global modes
(global-evil-mc-mode 1)
(add-hook 'after-init-hook #'solaire-global-mode)

;;; auto scrolling
(setq scroll-conservatively 8
      scroll-step 8)


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
(setq doom-font (font-spec :family "IBM Plex Mono" :size 15)
      doom-big-font (font-spec :family "IBM Plex Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "IBM Plex Serif" :size 15))

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

(load! "org")
(load! "vertico")

;; --------------------------------------------------------------------
;;                         Package configurations
;; --------------------------------------------------------------------

;;; ----------------------------------
;;; doom-theme
;;; ----------------------------------
(use-package! doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

;;; ----------------------------------
;;; dired
;;; ----------------------------------
(use-package! dired
  :when (featurep! :emacs dired)
  :config
  (map!
   :map dired-mode-map
   :n "h" #'dired-up-directory
   :n "l" #'dired-find-file))

;; ;;; ----------------------------------
;; ;;; lsp-mode
;; ;;; ----------------------------------
;; (use-package! lsp-mode
;;   :when (featurep! :tools lsp)
;;   :defer-incrementally t
;;   :config
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]target\\'")
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]screenshots\\'")
;;   (setq
;;    lsp-idle-delay 0.5
;;    lsp-signature-doc-lines 10
;;    lsp-modeline-code-actions-enable nil
;;    lsp-modeline-diagnostics-enable nil))

;; ;;; lsp-rust
;; (use-package! lsp-rust
;;   :when (featurep! :lang rust +lsp)
;;   :defer-incrementally t
;;   :config
;;   (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
;;         lsp-rust-analyzer-server-display-inlay-hints t
;;         lsp-rust-analyzer-display-chaining-hints t
;;         lsp-rust-analyzer-display-parameter-hints t
;;         lsp-rust-analyzer-cargo-watch-command "clippy"))

;; ;;; ----------------------------------
;; ;;; company
;; ;;; ----------------------------------
;; (use-package! company
;;   :when (featurep! :completion company)
;;   :defer-incrementally t
;;   :config
;;   (setq
;;    +lsp-company-backends '(company-files company-capf company-yasnippet company-dabbrev)
;;    company-idle-delay 0
;;    company-async-redisplay-delay 0.001
;;    company-selection-wrap-around t
;;    company-minimum-prefix-length 2)
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

(use-package! yasnippet
  :when (featurep! :editor snippets)
  :config
  (map!
   (:map yas-keymap
    "TAB" #'yas-next-field-or-maybe-expand
    [tab] #'yas-next-field-or-maybe-expand)))

;;; ----------------------------------
;;; latex
;;; ----------------------------------
(use-package! latex
  :when (featurep! :lang latex)
  :defer-incrementally t
  :config
  (setq
   +latex-viewers '(pdf-tools evince)
   TeX-electric-sub-and-superscript nil
   TeX-command-force "LatexMk")
  (remove-hook 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook! 'TeX-mode-hook #'turn-off-smartparens-mode)
  (map!
   :map LaTeX-mode-map
   :localleader
   :desc  "LaTeX View"    "v" #'TeX-view
   :desc  "LaTeX Build"   "b" #'TeX-command-master
   :desc  "LaTeX Run all" "r" #'TeX-command-run-all))

;;; ----------------------------------
;;; recentf
;;; ----------------------------------

(defvar home "/Users/aome510/")

(defvar recentf-keep-dot-folders
  `(,(concat home ".config") ,(concat home ".doom.d")))

;;;###autoload
(defun recentf-dot-file-ignore-p (file)
  (if (string-match-p (concat "^" home "\\.[-._[:alnum:]]+/") file)
      (not (seq-reduce
            (lambda (acc folder) (or acc (string-prefix-p folder file)))
            recentf-keep-dot-folders
            nil))
    nil))

;;;###autoload
(defun recentf-file-ignore-p (file)
  (if (string-match-p (concat "^" home) file)
      (or
       (recentf-dot-file-ignore-p file)
       (not (file-readable-p file)))
    t))

(use-package! recentf
  :config
  ;; auto save recentf list every 30 minutes
  (run-at-time nil (* 30 60) 'recentf-save-list)
  (setq recentf-exclude '(recentf-file-ignore-p)
        recentf-max-saved-items 1024))

;;; ----------------------------------
;;; kak
;;; ----------------------------------
(use-package! kak
  :config
  (map!
   ;; Kakoune-like key mappings
   :mvn "g h" #'evil-beginning-of-line
   :mvn "g i" #'evil-first-non-blank
   :mvn "g l" #'evil-end-of-line
   :mvn "M-n" #'evil-ex-search-previous
   :mvn "g %" #'mark-whole-buffer
   :n "M-o" #'+evil/insert-newline-below
   :n "M-O" #'+evil/insert-newline-above
   :v "|" #'kak-exec-shell-command
   :v "s" (lambda (beg end) (interactive "r") (kak-select beg end nil))
   :v "S" (lambda (beg end) (interactive "r") (kak-select beg end t))
   :v "M-s" #'kak-split-lines
   :v "M-k" (lambda () (interactive) (kak-filter t))
   :v "M-K" (lambda () (interactive) (kak-filter nil))
   :v ". #" #'kak-insert-index
   :v ". r" (lambda () (interactive) (kak-exec-shell-command "xsel -ob"))))

;; ;;; ----------------------------------
;; ;;; magit
;; ;;; ----------------------------------
(use-package! magit
  :when (featurep! :tools magit)
  :defer-incrementally t
  :config
  (setq git-commit-summary-max-length 100))

;;; ----------------------------------
;;; evil-mc
;;; ----------------------------------
(use-package! evil-mc
  :when (featurep! :editor multiple-cursors)
  :config
  (global-evil-mc-mode 1)
  (map!
   :nv "C-n" #'evil-mc-make-and-goto-next-match
   :nv "C-p" #'evil-mc-make-and-goto-prev-match))

;;; ----------------------------------
;;; ssh-agency
;;; ----------------------------------
(use-package! ssh-agency)

;;; custom functions

;;;###autoload
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

;;; variables

(setq evil-cross-lines t)
(setq evil-snipe-scope 'visible)

;;; key mappings

(map!
 (:leader :prefix "s"
  :desc "Ripgrep Search Project" "g" #'ripgrep-search-project)

 :n "U"       #'undo-tree-redo
 :n "u"       #'undo-tree-undo

 :n "] e" #'flycheck-next-error
 :n "[ e" #'flycheck-previous-error

 ;; `s' and `S' are binded to `kak.el' package's functions
 :v ". s"     #'evil-snipe-s
 (:map evil-surround-mode-map
  :v "S" nil
  :v ". S"     #'evil-surround-region)

 ;; markdown-mode remaps `DEL' shortcuts which doesn't play nicely with evil-mc
 (:map markdown-mode-map
  "DEL" nil))

;;; increase history length
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;;; modify `TAB' key behavior
(setq-default tab-always-indent nil)

;;; treat underscore as a word character
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;;; enable auto scrolling
(setq scroll-conservatively 8
      scroll-step 8)

;;; manually setup tree-sitter for emacs until M1 macos has a stable support
(add-to-list 'load-path (concat home "Projects/Build/elisp-tree-sitter/core"))
(add-to-list 'load-path (concat home "Projects/Build/elisp-tree-sitter/lisp"))
(add-to-list 'load-path (concat home "Projects/Build/elisp-tree-sitter/langs"))
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

;; enable tree-sitter globally
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

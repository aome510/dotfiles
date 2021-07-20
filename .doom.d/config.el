
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

(load! "vue")
;; (load! "lsp-eglot")
(load! "lsp-mode")
(load! "vertico")
;; (load! "corfu")
;; (load! "selectrum")

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

;;; ----------------------------------
;;; company
;;; ----------------------------------
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
   company-minimum-prefix-length 2)
  (map!
   (:map company-active-map
    "RET" nil
    [return] nil
    "TAB" nil
    [tab] nil
    "C-f" #'company-complete-selection)))

;;; ----------------------------------
;;; gcmh
;;; ----------------------------------
(use-package! gcmh
  :config
  (setq-default gcmh-idle-delay 15)
  (setq-default gcmh-high-cons-threshold (* 50 1024 1024)))

;;; ----------------------------------
;;; flycheck
;;; ----------------------------------
(use-package! flycheck
  :when (featurep! :checkers syntax)
  :config
  (map!
   :map flycheck-mode-map
   :n "] e" #'flycheck-next-error
   :n "[ e" #'flycheck-previous-error))

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
;;; ivy
;;; ----------------------------------
(use-package! ivy
  :when (featurep! :completion ivy)
  :config
  (map!
   (:map ivy-minibuffer-map
   "C-h" #'ivy-backward-delete-char
   "C-f" #'ivy-alt-done
   :map ivy-reverse-i-search-map
   "C-k" #'previous-line
   "C-d" #'ivy-reverse-i-search-kill)))

;;; ----------------------------------
;;; latex
;;; ----------------------------------
(use-package! latex
  :when (featurep! :lang latex)
  :config
  (setq
   TeX-electric-sub-and-superscript nil
   +latex-viewers '(pdf-tools evince)
   TeX-command-force "LatexMk")
  (remove-hook 'text-mode-hook #'turn-on-auto-fill)
  (add-hook 'TeX-mode-hook #'turn-off-smartparens-mode)
  (map!
   :map LaTeX-mode-map
   :localleader
   :desc  "LaTeX View"    "v" #'TeX-view
   :desc  "LaTeX Build"   "b" #'TeX-command-master
   :desc  "LaTeX Run all" "r" #'TeX-command-run-all))

;;; ----------------------------------
;;; pdf-tools
;;; ----------------------------------
(use-package! pdf-tools
  :when (featurep! :tools pdf)
  :config
  (setq-default pdf-view-display-size 'fit-width))

;;; ----------------------------------
;;; format
;;; ----------------------------------
(use-package! format
  :when (featurep! :editor format)
  :config
  (setq +format-on-save-enabled-modes '(not sql-mode tex-mode latex-mode vue-mode)))

;;; ----------------------------------
;;; emacs-tree-sitter
;;; ----------------------------------
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (map!
   (:leader :prefix "t"
    :desc "Treesitter mode" "t" #'tree-sitter-mode)))

;;; ----------------------------------
;;; recentf
;;; ----------------------------------
(defvar recentf-keep-dot-folders
  '("/home/aome510/.config" "/home/aome510/.doom.d"))

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

;;; ----------------------------------
;;; kak
;;; ----------------------------------
(use-package! kak
  :config
  (map!
   ;; Kakoune-like key bindings
   :mvn "g h" #'evil-beginning-of-line
   :mvn "g i" #'evil-first-non-blank
   :mvn "g l" #'evil-end-of-line
   :mvn "M-n" #'evil-ex-search-previous
   :mvn "g %" #'mark-whole-buffer
   :v "|" #'kak-exec-shell-command
   :v "s" (lambda (beg end) (interactive "r") (kak-select beg end nil))
   :v "S" (lambda (beg end) (interactive "r") (kak-select beg end t))
   :v "M-s" #'kak-split-lines
   :v "M-k" (lambda () (interactive) (kak-filter t))
   :v "M-K" (lambda () (interactive) (kak-filter nil))
   :v ". #" #'kak-insert-index
   :v ". r" (lambda () (interactive) (kak-exec-shell-command "xsel -ob"))))

;;; ----------------------------------
;;; magit
;;; ----------------------------------
(use-package! magit
  :when (featurep! :tools magit)
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
;;; vterm
;;; ----------------------------------
(defun term-other-window (&optional side size)
  (split-window (selected-window) size side)
  (vterm "*terminal*"))

(use-package! vterm
  :when (featurep! :term vterm)
  :config
  (map!
   :leader :prefix "o"
   :desc "Open terminal in other window (right)" "T"
   #'(lambda (&optional size) (interactive "P") (term-other-window 'left size))
   :desc "Open small terminal (below)" "t" #'vterm))

;;; ----------------------------------
;;; ssh-agency
;;; ----------------------------------
(use-package! ssh-agency)

;;; ----------------------------------
;;; evil-related and other packages
;;; ----------------------------------

;;;; custom functions

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

(defun save-buffer-without-hooks ()
  (interactive)
  (setq temp before-save-hook)
  (setq before-save-hook nil)
  (save-buffer)
  (setq before-save-hook temp))

;;;; variables

(setq evil-cross-lines t)
(setq evil-snipe-scope 'visible)

(map!
 (:leader :desc "Expand Region" "=" #'er/expand-region)

 (:leader :prefix "b"
  :desc "Save buffer without hooks" "s" #'save-buffer-without-hooks)

 (:leader :prefix "s"
  :desc "Ripgrep Search Project" "g" #'ripgrep-search-project)

 (:leader :prefix "p"
  :desc "Projectile Dired" "SPC" #'projectile-dired)

 (:leader
  :prefix ("a" . "custom keybindings")
  :desc "Align Left"    "l" #'evil-lion-left
  :desc "Align Right"   "r" #'evil-lion-right)

 "M-<escape>" #'normal-mode
 "M-="        #'text-scale-increase
 "M--"        #'text-scale-decrease
 :mnv "h"     #'backward-char
 :n "U"       #'undo-tree-redo
 :n "u"       #'undo-tree-undo
 :n "g w"     nil
 :v ". s"     #'evil-snipe-s
 :v ". S"     #'evil-surround-region
 :v "|"       #'evil-shell-command-on-region
 :v "R"       #'replace-from-clipboard
 (:map evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
 (:map evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block))


;; --------------------------------------------------------------------
;;                         Misc settings
;; --------------------------------------------------------------------

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

;;; tab always indent
(setq-default tab-always-indent nil)

;;; treat underscore as word character
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;;; auto scrolling
(setq scroll-conservatively 8
      scroll-step 8)

;;; auto save recentf list every 30 minutes
(run-at-time nil (* 30 60) 'recentf-save-list)

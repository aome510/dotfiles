;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(load! "ui")
(load! "completion")
(load! "lsp")
(load! "org")

;; ;;; --------------------------------------------------------------------
;; ;;; General package configurations
;; ;;; --------------------------------------------------------------------

(use-package! apheleia
  :init
  (apheleia-global-mode +1)
  :config
  (push '(markdown-mode . prettier) apheleia-mode-alist)
  (push '(yapf . ("yapf")) apheleia-formatters)
  (push '(latexindent . ("latexindent")) apheleia-formatters))


(use-package! latex
  :defer-incrementally t
  :when (modulep! :lang latex)
  :config
  (setq
   +latex-viewers '(pdf-tools evince)
   TeX-electric-sub-and-superscript nil
   TeX-command-force "LatexMk")

  (remove-hook! 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook! 'TeX-mode-hook #'turn-off-smartparens-mode)

  (map!
   (:map LaTeX-mode-map
    :localleader
    :desc  "LaTeX View"    "v" #'TeX-view
    :desc  "LaTeX Build"   "b" #'TeX-command-master
    :desc  "LaTeX Run all" "r" #'TeX-command-run-all)))


(use-package! recentf
  :config
  ;; auto save recentf list every 30 minutes
  (run-at-time nil (* 30 60) 'recentf-save-list)

  (defvar home "/Users/aome510/")

  (defvar recentf-keep-dot-folders
    `(,(concat home ".config") ,(concat home ".doom.d")))

  (defun recentf-dot-file-ignore-p (file)
    (if (string-match-p (concat "^" home "\\.[-._[:alnum:]]+/") file)
        (not (seq-reduce
              (lambda (acc folder) (or acc (string-prefix-p folder file)))
              recentf-keep-dot-folders
              nil))
      nil))

  (defun recentf-file-ignore-p (file)
    (if (string-match-p (concat "^" home) file)
        (or
         (recentf-dot-file-ignore-p file)
         (not (file-readable-p file)))
      t))

  (setq recentf-exclude '(recentf-file-ignore-p)
        recentf-max-saved-items 1024))


(use-package! kak
  ;; :load-path "/Users/aome510/Projects/kak.el"
  :config
  (map!
   :v "|" #'kak-exec-shell-command
   :v "s" (lambda (beg end) (interactive "r") (kak-select beg end nil))
   :v "S" (lambda (beg end) (interactive "r") (kak-select beg end t))
   :v "M-s" #'kak-split-lines
   :v "M-k" (lambda () (interactive) (kak-filter t))
   :v "M-K" (lambda () (interactive) (kak-filter nil))
   :v ". #" #'kak-insert-index
   :v ". r" (lambda () (interactive) (kak-exec-shell-command "pbpaste"))))


(use-package! magit
  :defer-incrementally t
  :when (modulep! :tools magit)
  :config
  (setq git-commit-summary-max-length 100))


(use-package! evil-mc
  :when (modulep! :editor multiple-cursors)
  :init
  (global-evil-mc-mode 1)
  :config
  (map!
   (:nv "C-n" #'evil-mc-make-and-goto-next-match
    :nv "C-p" #'evil-mc-make-and-goto-prev-match
    :map evil-mc-key-map
    :nv "C-n" #'evil-mc-make-and-goto-next-match
    :nv "C-p" #'evil-mc-make-and-goto-prev-match)))


(use-package! markdown-mode
  :config
  ;; disable font-lock-mode for markdown-mode
  (add-hook! markdown-mode #'font-lock-change-mode)
  (add-hook! markdown-mode (add-hook 'before-save-hook #'custom/markdown-preview-update nil t)))

;;; --------------------------------------------------------------------
;;; Custom functions
;;; --------------------------------------------------------------------

;;;###autoload
(defun custom/markdown-preview-update ()
  (setq output-buffer-name (markdown-standalone))
  (setq output-file-name (format! "%s.html" (buffer-file-name)))
  (message output-file-name)
  (with-current-buffer output-buffer-name
    (write-region (point-min) (point-max) output-file-name nil 'no-message))
  output-file-name)

;;;###autoload
(defun custom/markdown-preview ()
  (interactive)
  (setq output-file-name (custom/markdown-preview-update))
  (browse-url-of-file output-file-name))

;;;###autoload
(defun custom/ripgrep-search-project (search-term &rest args)
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

;;;###autoload
(defun custom/save-buffer-without-hooks ()
  (interactive)
  (let ((tmp-before-save-hooks before-save-hook)
        (tmp-after-save-hooks after-save-hook))
    (setq before-save-hook nil)
    (setq after-save-hook nil)
    (save-buffer)
    (setq before-save-hook tmp-before-save-hooks)
    (setq after-save-hook tmp-after-save-hooks)))

;; --------------------------------------------------------------------
;; Mappings
;; --------------------------------------------------------------------

(map!
 (:leader :prefix "s"
  :desc "Ripgrep Search Project" "g" #'custom/ripgrep-search-project)

 (:leader :prefix "b"
  ;; :desc "Format buffer" "f" #'+format/buffer
  :desc "Save buffer without hooks" "s" #'custom/save-buffer-without-hooks)

 :i "C-;" #'completion-at-point

 ;; remove some default bindings set by `evil' packages
 :i "C-n" nil
 :i "C-p" nil
 :i "C-g" nil

 :n "U"       #'undo-tree-redo
 :n "u"       #'undo-tree-undo

 ;; Kakoune-inspired movement mappings
 :mvn "g h" #'evil-beginning-of-line
 :mvn "g i" #'evil-first-non-blank
 :mvn "g l" #'evil-end-of-line

 :n "] e" #'flycheck-next-error
 :n "[ e" #'flycheck-previous-error

 (:map yas-keymap
       "TAB" #'yas-next-field-or-maybe-expand
       [tab] #'yas-next-field-or-maybe-expand)

 (:map dired-mode-map
  :n "h" #'dired-up-directory
  :n "l" #'dired-find-file)

 (:map wdired-mode-map
       [remap self-insert-command] nil)

 ;; `s' and `S' are binded to `kak.el' package's functions
 (:v ". s"     #'evil-snipe-s
  :map evil-surround-mode-map
  :v "S" nil
  :v ". S"     #'evil-surround-region)

 (:map markdown-mode-map
       ;; `markdown-mode' remaps `DEL' shortcuts which doesn't play nicely with `evil-mc'
       "DEL" nil
       (:localleader "p" #'custom/markdown-preview)))

;; --------------------------------------------------------------------
;; Misc settings
;; --------------------------------------------------------------------

(setq evil-cross-lines t)
(setq evil-snipe-scope 'visible)

;; treat underscore/dash as a word character
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;; increase history length
(setq-default history-length 1000)

;; modify `TAB' key behavior
(setq-default tab-always-indent nil)

;; setup custom env file
(doom-load-envvars-file "~/.doom.d/myenv")

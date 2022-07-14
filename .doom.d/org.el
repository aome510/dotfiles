;;; org.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/")

(use-package! org
  :defer-incrementally t
  :config
  (add-hook! 'org-mode-hook
             ;; #'org-fragtog-mode ;; auto-preview latex fragments upon entering/leaving the fragments
             #'turn-off-smartparens-mode
             (setq-local tab-width 2))
  (setq org-download-image-html-width 600
        ;; define custom org-capture templates
        +org-capture-notes-file "personal/notes.org"
        org-capture-templates '(
                                ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Notes") "* %u %?\n%i\n%a"))
        ;; setup `org-attach' to use relative directory
        org-attach-id-dir ".attach"
        org-attach-dir-relative t)
  (map!
   (:map org-mode-map
    ;; don't want org-mode keymaps to mess with "RET" key, which is used for completion
    :g "C-SPC" #'org-latex-preview
    :i "RET" nil
    :i [return] nil
    "C-c C-p" #'org-cliplink)))

(use-package! evil-org
  :defer-incrementally t
  :config
  (map! (:map evil-org-mode-map
         ;; don't want org-mode keymaps to mess with "RET" key, which is used for completion
         :i "RET" nil
         :i [return] nil)))

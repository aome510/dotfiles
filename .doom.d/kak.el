;;; kak.el -*- lexical-binding: t; -*-
;;;
;;;
;;;

;;; overidding `evil-mc-get-default-cursor' to use
;;; `evil-mc-from-real-cursor' that creates a fake cursor
;;; based on the real cursor.

(defun evil-mc-from-real-cursor ()
  "Return a cursor based on the real cursor"
  (let (cursor
        (state (evil-mc-read-cursor-state))
        (names (evil-mc-get-cursor-variables)))
    (dolist (name names)
      (setq cursor (evil-mc-put-cursor-property
                    cursor
                    name
                    (copy-tree (evil-mc-get-cursor-property state name)))))
    cursor))

(advice-add 'evil-mc-get-default-cursor :override
            #'evil-mc-from-real-cursor)

;;; variables
(defvar kak-current-buffer nil)
(defvar kak-region-beg nil)
(defvar kak-region-end nil)
(defvar kak-search-matches nil)
(defvar kak-invert-search nil)
(defvar kak-match-overlays nil)

;;; hooks
(defun kak-add-search-highlight (_beg _end _range)
  (progn
    (mapc #'delete-overlay kak-match-overlays)
    (unless (string= (minibuffer-contents) "")
      (condition-case err
          (let ((pattern (evil-ex-make-search-pattern (minibuffer-contents))))
            (with-current-buffer kak-current-buffer
              (progn
                (kak-get-matches-in-region pattern)
                (mapcar (lambda (overlay)
                          (overlay-put overlay 'priority 1000)
                          (overlay-put overlay 'face 'evil-ex-lazy-highlight))
                        kak-match-overlays))))
        (error
         (evil-ex-echo "%s" (cdr err)))))))

(defun kak-start-search-session()
  (add-hook 'after-change-functions #'kak-add-search-highlight nil t))

(defun kak-end-search-session ()
  (mapc #'delete-overlay kak-match-overlays)
  (remove-hook 'minibuffer-setup-hook #'kak-start-search-session)
  (remove-hook 'minibuffer-exit-hook #'kak-end-search-session)
  (remove-hook 'after-change-functions #'kak-add-search-highlight))

;; functions

(defun kak-get-invert-matches (matches beg end)
  (if (> beg end) nil
    (pcase matches
      ('nil `((,beg ,end)))
      (`(,match . ,matches)
       (if (eq beg (cl-first match))
           (kak-get-invert-matches matches (cl-second match) end)
         (cons `(,beg ,(cl-first match))
               (kak-get-invert-matches matches (cl-second match) end)))))))

(defun kak-get-matches-in-region (pattern)
  (let ((match)
        (case-fold-search (evil-ex-pattern-ignore-case pattern))
        (regex (evil-ex-pattern-regex pattern)))
    (setq! kak-search-matches nil
           kak-match-overlays nil)
    (goto-char kak-region-beg)
    (if (search-forward-regexp regex nil kak-region-end)
        (setq match (match-data 0)) (setq match nil))
    (while (and match (<= (cl-second match) kak-region-end))
      (push match kak-search-matches)
      (goto-char (cl-second match))
      ;; move to the next line if the current match's end is eol
      (when (= (point) (line-end-position))
        (forward-char 1))
      (if (search-forward-regexp regex nil kak-region-end)
          (setq match (match-data 0)) (setq match nil)))
    (setq kak-search-matches (reverse kak-search-matches))
    (when kak-invert-search
      (setq kak-search-matches (kak-get-invert-matches kak-search-matches kak-region-beg kak-region-end)))
    (setq kak-match-overlays (mapcar
                              (lambda (match) (make-overlay (cl-first match) (cl-second match)))
                              kak-search-matches))))

(defun kak-make-cursors-for-matches (matches)
  (pcase matches
    (`(,match . nil) (evil-visual-make-selection (cl-first match) (1- (cl-second match))))
    (`(,match . ,matches) (progn
                            (let ((cursor) (region)
                                  (pos (1- (cl-second match))))
                              (goto-char pos)
                              (setq region (evil-mc-create-region (cl-first match) pos 'char))
                              (setq cursor (evil-mc-put-cursor-property
                                            (evil-mc-read-cursor-state)
                                            'last-position pos
                                            'order (if (null evil-mc-cursor-list) 1
                                                     (1+ (apply #'max
                                                                (mapcar (lambda (cursor)
                                                                          (evil-mc-get-cursor-property cursor 'order))
                                                                        evil-mc-cursor-list))))
                                            'temporary-goal-column (evil-mc-column-number pos)
                                            'overlay (evil-mc-cursor-overlay-at-pos pos)
                                            'region region))
                              (evil-mc-run-cursors-before)
                              (evil-mc-insert-cursor cursor))
                            (kak-make-cursors-for-matches matches)))))

(defun kak-init-search (beg end)
  (progn
    (setq! kak-current-buffer (current-buffer)
           kak-region-beg beg
           kak-region-end end
           kak-invert-search nil
           kak-search-matches nil
           kak-match-overlays nil)
    (add-hook 'minibuffer-setup-hook #'kak-start-search-session)
    (add-hook 'minibuffer-exit-hook #'kak-end-search-session)))

(defun kak-select (beg end)
  (interactive "r")
  (progn
    (kak-init-search beg end)
    (read-string "pattern: ")
    (progn
      (goto-char beg)
      (if kak-search-matches
          (progn (evil-exit-visual-state) (kak-make-cursors-for-matches kak-search-matches))
        (user-error "no match")))))

(defun kak-split (beg end)
  (interactive "r")
  (progn
    (kak-init-search beg end)
    (setq kak-invert-search t)
    (read-string "pattern: ")
    (progn
      (goto-char beg)
      (if kak-search-matches
          (progn (evil-exit-visual-state) (kak-make-cursors-for-matches kak-search-matches))
        (user-error "no match")))))

(defun kak-restore-last-region ()
  (interactive)
  (if (and kak-region-beg kak-region-end)
      (evil-visual-make-region kak-region-beg (1- kak-region-end))))

;; 1, 2, 3

;; (defun get-all-cursor-regions ()
;;   (let ((matches
;;          (cons `(,(region-beginning) ,(region-end))
;;                (mapcar
;;                 (lambda (cursor)
;;                   (let ((region (evil-mc-get-cursor-region cursor)))
;;                     `(,(evil-mc-get-region-start region)
;;                       ,(evil-mc-get-region-end region))))
;;                 evil-mc-cursor-list))))
;;     (cl-sort matches #'< :key #'car)))

;; (defun filter-cursors-regexp (keep)
;;   "remove/keep all the cursors whose region matches the given regexp"
;;   (interactive)
;;   (let ((regex (read-regexp "regex: ")))
;;     (if (string= regex "")
;;         (message "search aborted")
;;       (if (evil-visual-state-p)
;;           (let ((matches
;;                  (get-all-cursor-regions)))
;;             (setq matches (seq-filter
;;                            (lambda (match)
;;                              (xor keep
;;                                   (not (string-match-p regex
;;                                                        (buffer-substring-no-properties
;;                                                         (cl-first match)
;;                                                         (cl-second match))))))
;;                            matches))
;;             (message "matches after soring, filter: %s" matches)
;;             (if matches
;;                 (progn
;;                   (evil-mc-undo-all-cursors)
;;                   (kakfor-matches-cursors-all-matches (reverse matches)))
;;               (user-error "no selections remaining")))
;;         (user-error "must be in visual state")))))

;; (defun evil-shell-command-on-region-all-cursors (command)
;;   "call a shell command on all cursors' region"
;;   (interactive (list (read-shell-command "Shell command on region: ")))
;;   (if (evil-visual-state-p)
;;       (evil-mc-execute-for-all-cursors
;;        (lambda (cursor)
;;          (let (region beg end (index (evil-mc-get-cursor-property cursor :index)))
;;            (if (eq index 0) ;; real cursor
;;                (progn
;;                  (setq beg (region-beginning))
;;                  (setq end (region-end))
;;                  (evil-shell-command-on-region beg end command)
;;                  (evil-exit-visual-state))
;;              (progn
;;                (setq region (evil-mc-get-cursor-region cursor))
;;                (setq beg (evil-mc-get-region-start region))
;;                (setq end (evil-mc-get-region-end region))
;;                (evil-shell-command-on-region beg end command))))))
;;     (user-error "must be in visual state")))

;; (defun insert-index-all-cursors (base)
;;   "insert a number based on a cursor's index after each cursor"
;;   (interactive "nBase index: ")
;;   (setq base (1- base))
;;   (evil-mc-execute-for-all-cursors
;;    (lambda (cursor)
;;      (let ((index (evil-mc-get-cursor-property cursor :index)))
;;        (if (eq index 0) ;; real cursor, assume that it's always the last cursor
;;            (progn
;;              (insert (number-to-string (+ index base (evil-mc-get-cursor-count))))
;;              (evil-exit-visual-state))
;;          (progn
;;            (goto-char (evil-mc-get-cursor-end cursor))
;;            (insert (number-to-string (+ index base)))
;;            (backward-char 1)))))))

(map!
 ;; :v ". |" #'evil-shell-command-on-region-all-cursors
 ;; :v ". #" #'insert-index-all-cursors
 :v ". s" #'kak-select
 :v ". S" #'kak-split
 ;; :v ". k" (lambda () (interactive) (filter-cursors-regexp t))
 ;; :v ". K" (lambda () (interactive) (filter-cursors-regexp nil))
 )

(provide 'kak)

;; test1
;; test2
;; Test3

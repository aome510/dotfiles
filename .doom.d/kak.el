;;; kak.el -*- lexical-binding: t; -*-
;;;
;;;
;;;

;;; overidding evil-mc-get-default-cursor to use
;;; evil-mc-from-real-cursor that creates a fake cursor
;;; based on real cursor's current state.

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

(defun search-matches-in-region-regexp (beg end pattern)
  "search a regex pattern in the current region then return the sequence of matches"
  (let ((match) (matches)
        (case-fold-search (evil-ex-pattern-ignore-case pattern))
        (regex (evil-ex-pattern-regex pattern)))
    (goto-char beg)
    (if (search-forward-regexp regex nil end)
        (setq match (match-data 0)) (setq match nil))
    (while (and match (<= (cl-second match) end))
      (push match matches)
      (goto-char (cl-second match))
      (if (search-forward-regexp regex nil end)
          (setq match (match-data 0)) (setq match nil)))
    (reverse matches)))

(defun make-cursors-all-matches (matches)
  "given all matches (\"beg end\" pairs), create multiple cursors
marking all the matches. The real cursor is placed at the last match"
  (pcase matches
    (`(,match . nil) (progn
                       (goto-char (1- (cl-second match)))
                       (push-mark (cl-first match))
                       (setq mark-active t)
                       (evil-visual-char)))
    (`(,match . ,matches) (progn
                            (let ((cursor) (region) (pos (1- (cl-second match))))
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
                            (make-cursors-all-matches matches)))))

;; Test
;; test

(defun update-search-highlighting (_beg _end _range)
  (save-match-data
    (let ((pattern (minibuffer-contents)))
      (with-current-buffer kak-current-buffer
        (progn
          (evil-ex-hl-set-region 'evil-ex-search kak-region-beg kak-region-end)
          (evil-ex-search-activate-highlight (evil-ex-make-search-pattern pattern)))))))

(defun start-search-session()
  (add-hook 'after-change-functions #'update-search-highlighting nil t))

(defun end-search-session ()
  (remove-hook 'minibuffer-setup-hook #'start-search-session)
  (remove-hook 'minibuffer-exit-hook #'end-search-session)
  (remove-hook 'after-change-functions #'update-search-highlighting))

;; Test
;; test

(defun select-in-region-regexp (beg end)
  "create multiple cursors marking all the matches matching
a given regex pattern in the current region"
  (interactive "r")
  (let ((kak-current-buffer (current-buffer))
        (kak-region-beg beg)
        (kak-region-end end))
    (add-hook 'minibuffer-setup-hook #'start-search-session)
    (add-hook 'minibuffer-exit-hook #'end-search-session)
    (let ((pattern (evil-ex-make-search-pattern (read-regexp "pattern: "))))
      (evil-ex-delete-hl 'evil-ex-search)
        (progn
          (goto-char beg)
          (let ((matches (search-matches-in-region-regexp beg end pattern)))
            (message "matches: %s" matches)
            (if matches
                (make-cursors-all-matches matches)
              (user-error "no match")))))))

(defun get-splits (matches beg end)
  (if (> beg end) nil
    (pcase matches
      ('nil `((,beg ,end)))
      (`(,match . ,matches)
       (if (eq beg (cl-first match))
           (get-splits matches (cl-second match) end)
         (cons `(,beg ,(cl-first match))
               (get-splits matches (cl-second match) end)))))))

(defun split-in-region-regexp (beg end)
  "create multiple cursors marking all intervals in the
current region that are splitted by the given regex pattern"
  (interactive "r")
  (let ((regex (read-regexp "regex: ")))
    (if (string= regex "")
        (message "search aborted")
      (progn
        (goto-char beg)
        (let* ((matches (search-matches-in-region-regexp beg end regex))
               (splits (get-splits matches beg end)))
          (message "matches: %s,\nsplits: %s" matches splits)
          (if splits
              (make-cursors-all-matches splits)
            (user-error "no split match")))))))

(defun get-all-cursor-regions ()
  (let ((matches
         (cons `(,(region-beginning) ,(region-end))
               (mapcar
                (lambda (cursor)
                  (let ((region (evil-mc-get-cursor-region cursor)))
                    `(,(evil-mc-get-region-start region)
                      ,(evil-mc-get-region-end region))))
                evil-mc-cursor-list))))
    (cl-sort matches #'< :key #'car)))

(defun filter-cursors-regexp (keep)
  "remove/keep all the cursors whose region matches the given regexp"
  (interactive)
  (let ((regex (read-regexp "regex: ")))
    (if (string= regex "")
        (message "search aborted")
      (if (evil-visual-state-p)
          (let ((matches
                 (get-all-cursor-regions)))
            (setq matches (seq-filter
                           (lambda (match)
                             (xor keep
                                  (not (string-match-p regex
                                                       (buffer-substring-no-properties
                                                        (cl-first match)
                                                        (cl-second match))))))
                           matches))
            (message "matches after soring, filter: %s" matches)
            (if matches
                (progn
                  (evil-mc-undo-all-cursors)
                  (make-cursors-all-matches (reverse matches)))
              (user-error "no selections remaining")))
        (user-error "must be in visual state")))))

(defun evil-shell-command-on-region-all-cursors (command)
  "call a shell command on all cursors' region"
  (interactive (list (read-shell-command "Shell command on region: ")))
  (if (evil-visual-state-p)
      (evil-mc-execute-for-all-cursors
       (lambda (cursor)
         (let (region beg end (index (evil-mc-get-cursor-property cursor :index)))
           (if (eq index 0) ;; real cursor
               (progn
                 (setq beg (region-beginning))
                 (setq end (region-end))
                 (evil-shell-command-on-region beg end command)
                 (evil-exit-visual-state))
             (progn
               (setq region (evil-mc-get-cursor-region cursor))
               (setq beg (evil-mc-get-region-start region))
               (setq end (evil-mc-get-region-end region))
               (evil-shell-command-on-region beg end command))))))
    (user-error "must be in visual state")))

(defun insert-index-all-cursors (base)
  "insert a number based on a cursor's index after each cursor"
  (interactive "nBase index: ")
  (setq base (1- base))
  (evil-mc-execute-for-all-cursors
   (lambda (cursor)
     (let ((index (evil-mc-get-cursor-property cursor :index)))
       (if (eq index 0) ;; real cursor, assume that it's always the last cursor
           (progn
             (insert (number-to-string (+ index base (evil-mc-get-cursor-count))))
             (evil-exit-visual-state))
         (progn
           (goto-char (evil-mc-get-cursor-end cursor))
           (insert (number-to-string (+ index base)))
           (backward-char 1)))))))

(map!
 :v ". |" #'evil-shell-command-on-region-all-cursors
 :v ". #" #'insert-index-all-cursors
 :v ". s" #'select-in-region-regexp
 :v ". S" #'split-in-region-regexp
 :v ". k" (lambda () (interactive) (filter-cursors-regexp t))
 :v ". K" (lambda () (interactive) (filter-cursors-regexp nil)))

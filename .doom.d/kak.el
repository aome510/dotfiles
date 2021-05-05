;;; kak.el -*- lexical-binding: t; -*-
;;;

(defun search-matches-in-region-regexp (beg end regex)
  (let ((match) (matches))
    (goto-char beg)
    (if (search-forward-regexp regex nil end)
        (setq match (match-data 0)) (setq match nil))
    (while (and match (<= (cl-second match) end))
      (push match matches)
      (goto-char (cl-second match))
      (if (search-forward-regexp regex nil end)
          (setq match (match-data 0)) (setq match nil)))
    matches))

(defun make-cursors-all-matches (matches)
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
                                            'order (if (null evil-mc-cursor-list) 1 ; ordered "chronologically"
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

(defun make-cursors-in-region-regexp (beg end)
  (interactive "r")
  (let ((regex (read-regexp "regex: "))
        (case-fold-search nil))
    (if (string= regex "")
        (message "search aborted")
      (progn
        (goto-char beg)
        (let ((matches (reverse (search-matches-in-region-regexp beg end regex))))
          (message "matches: %s" matches)
          (if matches
              (make-cursors-all-matches matches)
            (error "no match")))))))

(defun get-splits (matches beg end)
  (if (> beg end) nil
    (pcase matches
      ('nil `((,beg ,end)))
      (`(,match . ,matches)
       (if (eq beg (cl-first match))
           (get-splits matches (cl-second match) end)
         (cons `(,beg ,(cl-first match))
               (get-splits matches (cl-second match) end)))))))

(defun make-cursors-in-region-not-regexp (beg end)
  (interactive "r")
  (let ((regex (read-regexp "regex: "))
        (case-fold-search nil))
    (if (string= regex "")
        (message "search aborted")
      (progn
        (goto-char beg)
        (let* ((matches (reverse (search-matches-in-region-regexp beg end regex)))
               (splits (get-splits matches beg end)))
          (message "matches: %s,\nsplits: %s" matches splits)
          (if splits
              (make-cursors-all-matches splits)
            (error "no split match")))))))

(defun filter-selections-regexp (keep)
  (let ((regex (read-regexp "regex: "))
        (case-fold-search nil))
    (if (string= regex "")
        (message "search aborted")
      (if (evil-visual-state-p)
          (let ((matches
                 (cons `(,(region-beginning) ,(region-end))
                       (mapcar
                        (lambda (cursor)
                          (let ((region (evil-mc-get-cursor-region cursor)))
                            `(,(evil-mc-get-region-start region)
                              ,(evil-mc-get-region-end region))))
                        evil-mc-cursor-list))))
            (setq matches (cl-sort matches #'< :key #'car))
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
              (error "no selections remaining")))
        (error "must in visual state")))))

(map!
 (:leader
  :prefix ("k" . "kakoune")
  :desc "Kakoune Select" "s" #'make-cursors-in-region-regexp
  :desc "Kakoune Split" "S" #'make-cursors-in-region-not-regexp
  :desc "Kakoune Keep" "k" (lambda () (interactive) (filter-selections-regexp t))
  :desc "Kakoune Filter" "K" (lambda () (interactive) (filter-selections-regexp nil)))
 (:leader
  :desc "Expand Region" "v" #'er/expand-region))

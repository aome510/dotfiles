;;; kak.el -*- lexical-binding: t; -*-
;;;
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

(defun filter-selections-regexp (keep)
  (let ((regex (read-regexp "regex: "))
        (case-fold-search nil))
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
        (user-error "must in visual state")))))

;; (defun funcall-all-cursors-rec (fun matches id)
;;   (pcase matches
;;     (`(,match . nil) (funcall fun (cl-first match) (cl-second match) id))
;;     (`(,match . ,matches) (progn (funcall-all-cursors-rec fun matches (1+ id))
;;                                  (funcall fun (cl-first match) (cl-second match) id)))))

;; (defun funcall-all-cursors ()
;;   (interactive)
;;   (save-excursion (let ((matches (get-all-cursor-regions)))
;;                     ;; (funcall-all-cursors-rec fun matches 0)
;;                     (evil-mc-set-command-property :name #'evil-change)
;;                     (evil-mc-set-command-property :evil-state-end 'insert)
;;                     (evil-mc-execute-for-all)
;;                     (evil-exit-visual-state))))

(defun evil-shell-command-on-region-all-cursors (command)
  (interactive (list (read-shell-command "Shell command on region: ")))
  (funcall-all-cursors (lambda (beg end _)
                         (evil-shell-command-on-region beg end command))))

(defun test()
  (interactive)
  (evil-mc-execute-for-all-cursors (lambda (cursor) (message "%s" cursor))))

(map!
 (:leader
  :prefix ("k" . "kakoune")
  :desc "Kakoune Select" "s" #'make-cursors-in-region-regexp
  :desc "Kakoune Split" "S" #'make-cursors-in-region-not-regexp
  :desc "Test" "t" #'test
  :desc "Kakoune Keep" "k" (lambda () (interactive) (filter-selections-regexp t))
  :desc "Kakoune Filter" "K" (lambda () (interactive) (filter-selections-regexp nil)))
 (:leader
  :desc "Expand Region" "v" #'er/expand-region))

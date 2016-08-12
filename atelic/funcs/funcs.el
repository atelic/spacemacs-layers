;; My elisp functions. Beware

(defun /f (dividend divisor)
  "Floating point division"
  (/ (float dividend) divisor))

(defun atelic/insert-c-header ()
  "Creates a header skeleton for c-style languages"
  (interactive)
  (setq classname (file-name-nondirectory buffer-file-name))
  (insert "/*
* Eric Barbour
* emb4gu
* "(format-time-string "%d-%m-%Y")"
*"classname"
*/
"))

(defun atelic/date-at-point ()
  "Quick org-mode header for today in the org"
  (interactive)
  (insert
   "*** "(format-time-string "%Y-%m-%d %A")"
"))

(defun atelic/comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
  If no region is selected and current line is not blank and we are not at the end of the line,
  then comment current line.
  Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

(defun atelic/python-negate ()
  "False -> True or True -> false"
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text (buffer-substring-no-properties (car bounds)
                                               (cdr bounds))))
    (when (string= text "True")
      (delete-region (car bounds)
                     (cdr bounds))
      (insert "False"))
    (when (string= text "False")
      (delete-region (car bounds)
                     (cdr bounds))
      (insert "True"))))

(defun atelic/mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command)
                    (mark t))
               (region-active-p)))
      (set-mark (save-excursion
                  (when (< (mark) (point))
                    (setq arg (- arg)))
                  (goto-char (mark))
                  (forward-word arg)
                  (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))


(defun atelic/shell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'shell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'atelic/shell-clear-buffer)))

  (defun atelic/sort-lines-by-length (b e)
    (interactive "r")
    (save-excursion
      (save-restriction (narrow-to-region b e)
                        (let ((items (sort (split-string (buffer-substring (point-min)
                                                                           (point-max))
                                                         "[\n]")
                                           (lambda (x y)
                                             (< (length x) (length y))))))
                          (delete-region (point-min)
                                         (point-max))
                          (save-excursion
                            (point-min)
                            (insert (apply 'concat
                                           (map 'list
                                                (lambda (x)
                                                  (format "%s\n" x))
                                                items))))))))
  (defun atelic/dcaps-to-scaps ()
    "Convert word in DOuble CApitals to Single Capitals."
    (interactive)
    (and (= ?w (char-syntax (char-before)))
         (save-excursion
           (and (if (called-interactively-p)
                    (skip-syntax-backward "w")
                  (= -3 (skip-syntax-backward "w")))
                (let (case-fold-search)
                  (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
                (capitalize-word 1)))))
  (add-hook 'post-self-insert-hook #'atelic/dcaps-to-scaps nil 'local)

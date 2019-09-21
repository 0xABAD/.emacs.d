(require 'cl)

(defun self-format-comment ()
  "Formats a comment to a fill line."
  (interactive)
  (save-excursion
    (uncomment-region  (region-beginning) (region-end))
    (fill-region  (region-beginning) (region-end))
    (comment-region (region-beginning) (region-end))))

(defun self-highlight-c-function-calls ()
       (font-lock-add-keywords
        nil
        '(("\\(\\w+\\)\\s-*\\(\(\\|\{\\)" 1 font-lock-function-name-face))
        t))

(defmacro self-mode-align-with (regex-string)
  "Returns a command that will call `align-regexp' with
`regex-string' on the selected region."
  `(lambda ()
     (interactive)
     (align-regexp (region-beginning)
                   (region-end)
                   ,(concat "\\(\\s-*\\)" regex-string))))

(defun self-grep-symbol-at-point (&optional arg)
  "Performs a ripgrep search for the `symbol-at-point' as a whole
word search.

If no prefix `arg' is given then search will be performed within
the current directory.  If a prefix `arg' is given then the search
will start at `arg' parent directories (e.g. if `arg' is 2 then
'../..' will passed as the starting directory to search.
"
  (interactive "P")
  (let* ((count (if (equal nil arg)
                    0
                  (if (listp arg) (car arg) arg)))
         (dir (if (> count 0) ".." ".")))
    (dotimes (n count)
      (when (> (- count n) 1)
        (setq dir (concat "../" dir))))
    (grep (concat "rg --vimgrep -i -w "
                  (symbol-name (symbol-at-point))
                  " "
                  dir))))

(defun self-occur-symbol-at-point ()
  "Performs an occur search for the `symbol-at-point' surrounded by
reege word boundaries."
  (interactive)
  (occur (concat "\\b" (symbol-name (symbol-at-point)) "\\b")))

(defcustom self-build-file nil
  "Build file used for custom compiling.")

(defun self-compile ()
       "Make the current build."
       (interactive)
       (save-buffer)
       (compile self-build-file))

(defun self-guard-header ()
  "Inserts a guard header found a typical C/C++ header file."
  (interactive)
  (let* ((file (buffer-file-name))
         (base (file-name-base file))
         (ext  (file-name-extension file))
         (guard (concat "GUARD_" (upcase base) "_" (upcase ext))))
    (insert (concat "#ifndef " guard))
    (newline)
    (insert (concat "#define " guard))
    (newline)
    (newline)
    (insert (concat "#endif // " guard))))

(defun self-define-region (name)
  "Inserts '/**** BEGIN `name' ****/' and a corresponding 'END' comments
into the buffer at point.  If a region is selected then the 'BEGIN' and
'END' comments will be placed around that region.
"
  (interactive "sName: \n")
  (let* ((begin (concat "/**** BEGIN " name " ****/"))
         (end   (concat "/**** END " name " ****/")))
    (if (use-region-p)
        (let ((rb (region-beginning))
              (re (region-end)))
          (progn (goto-char rb)
                 (move-beginning-of-line nil)
                 (open-line 1)
                 (insert begin)
                 (goto-char re)
                 (move-end-of-line nil)
                 (newline)
                 (insert end)
                 (newline)))
      (progn (insert begin)
             (newline)
             (insert end)
             (newline)))))

(defun self-statement-annotate (&optional whole-statement)
  "Annotates the end of an if, while, or switch statement with a
comment.  For example,

    if (isTrue) {
        // long if block
        // ...
        // ...
        // ...    
    } // end if (isTrue)

By default this command will not put the expression (the '(isTrue)'
from the example) in the comment unless the universal-argument is
specified, which will bind `whole-statement' to non-nil.
"
  (interactive "P")
  (save-excursion
    (let ((end (end-of-line)))
      (beginning-of-line)
      (if (search-forward-regexp "\\(for\\|switch\\|if\\|while\\)\\s-*(\\(.+\\))" end t)
          (let ((match (match-string (if whole-statement 0 1))))
            (if (search-forward "{" nil t)
                (progn (backward-char)
                       (forward-sexp)
                       (insert (concat " // " match)))
              (message "No open switch curly bracket found.")))
        (message "No statement found.")))))

(defun self-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun self-send-quit-other-window ()
  "Switches to the other window, call `quit-restore-window', and
return to the previous window."
  (interactive)
  (other-window 1)
  (quit-window))

(provide 'self)

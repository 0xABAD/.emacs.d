;;; Some commands to interact with a windows environment.
;;; Makes use of ivy.

(require 'ivy)

(defun windows--get-services ()
  (with-temp-buffer
    (call-process "sc.exe" nil `(,(current-buffer) t) nil "query" "state=" "all")
    (goto-char (point-min))
    (let ((services '())
          (name)
          (display)
          (state))
      (while (re-search-forward "SERVICE_NAME:\\s-*\\b\\(.+\\)" nil t 1)
        (setq name (match-string 1))
        (re-search-forward "DISPLAY_NAME:\\s-*\\b\\(.+\\)$" nil t 1)
        (setq display (match-string 1))
        (re-search-forward "STATE\\s-*:\\s-*[[:digit:]]+\\s-*\\(\\w+\\)" nil t 1)
        (setq state (match-string 1))
        (push (propertize name 'state state 'display display) services))
      services)))

(defun windows-services ()
  "List all Windows' services.  If selected and it is running it will then
be stopped.  If it is already stopped then it will called to run."
  (interactive)
  (ivy-set-display-transformer
   'windows-services
   (lambda (candidate)
     (let ((name (substring-no-properties candidate))
           (state (get-text-property 0 'state candidate))
           (display (get-text-property 0 'display candidate)))
       (format "%-50s%-12s%s" name (capitalize state) display))))
  (ivy-read
   "Service: " (windows--get-services)
   :require-match t
   :sort t
   :caller 'windows-services
   :action (lambda (s)
             (let ((name (substring-no-properties s))
                   (state (get-text-property 0 'state s)))
               (cond ((string= state "RUNNING")
                      (call-process "sc.exe" nil nil nil "stop" name))
                     ((string= state "STOPPED")
                      (call-process "sc.exe" nil nil nil "start" name)))))))

(defun windows--get-processes ()
  (with-temp-buffer
    (call-process "tasklist.exe" nil `(,(current-buffer) t) nil)
    (goto-char (point-min))                         ;; Jump to the beginning of the buffer
    (re-search-forward "^\\bImage Name\\b" nil t 1) ;; Search for the header info
    (forward-line 1)                                ;; Jump to the line that has '=' to designate columns
    ;; Get the length of each column
    (let* ((columns   (split-string-and-unquote (thing-at-point 'line)))
           (imagen    (length (nth 0 columns)))
           (pidn      (length (nth 1 columns)))
           (snamen    (length (nth 2 columns)))
           (sessionn  (length (nth 3 columns)))
           (memn      (length (nth 4 columns)))
           (processes (list)))
      (forward-line 1)
      (while (not (= (point) (point-max)))
        (let* ((p       (point))
               (e       (+ p imagen))
               (image   (buffer-substring-no-properties p e))
               (p       (+ e 1))
               (e       (+ p pidn))
               (pid     (buffer-substring-no-properties p e))
               (p       (+ e 1))
               (e       (+ p snamen))
               (sname   (buffer-substring-no-properties p e))
               (p       (+ e 1))
               (e       (+ p sessionn))
               (session (buffer-substring-no-properties p e))
               (p       (+ e 1))
               (e       (+ p memn))
               (mem     (buffer-substring-no-properties p e)))
          (push (propertize image 'pid pid 'session-name sname 'session session 'mem-usage mem)
                processes)
          (forward-line 1)))
      processes)))

(defun windows-processes ()
  (interactive)
  (ivy-set-display-transformer
   'windows-processes
   (lambda (candidate)
     (let ((name (substring-no-properties candidate))
           (pid (get-text-property 0 'pid candidate))
           (mem (get-text-property 0 'mem-usage candidate)))
       (format "%s%s%s" name pid mem))))
  (ivy-read
   "Process: " (windows--get-processes)
   :require-match t
   :sort t
   :caller 'windows-processes
   :action (lambda (p)
             (call-process "taskkill.exe" nil nil nil
                           "/F"
                           "/PID" (get-text-property 0 'pid p)))))

(provide 'windows)

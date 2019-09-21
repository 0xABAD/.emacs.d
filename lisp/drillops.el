;;; drillops.el --- Lisp functions for doing stuff in DrillOps.

(require 'ivy)
(require 'windows)

(defun drillops-services ()
  "List Automation DrillOps services in the mini-buffer and selecting one
will start the service if it is stopped or it will stop that service if
it is running."
  (interactive)
  (ivy-set-display-transformer
   'drillops-services
   (lambda (candidate)
     (let ((name (substring-no-properties candidate))
           (state (get-text-property 0 'state candidate))
           (display (get-text-property 0 'display candidate)))
       (format "%-50s%-12s%s" name (capitalize state) display))))
  (ivy-read
   "Service: " (windows--get-services)
   :require-match t
   :sort t
   :caller 'drillops-services
   :predicate (lambda (s) (string-match "Symphony\\|Presto" s))
   :action (lambda (s)
             (let ((name (substring-no-properties s))
                   (state (get-text-property 0 'state s)))
               (cond ((string= state "RUNNING")
                      (call-process "sc.exe" nil nil nil "stop" name))
                     ((string= state "STOPPED")
                      (call-process "sc.exe" nil nil nil "start" name)))))))

(defun drillops-start-services ()
  "Start all the Automation DrillOps services include Presto Control Gateway."
  (interactive)
  (dolist (service (windows--get-services))
    (when (string-match "Symphony\\|Presto" service)
      (start-process (substring-no-properties service) nil "sc.exe" "start" service))))

(defun drillops-stop-services ()
  "Stop all the Automation DrillOps services include Presto Control Gateway."
  (interactive)
  (dolist (service (windows--get-services))
    (when (string-match "Symphony\\|Presto" service)
      (start-process (substring-no-properties service) nil "sc.exe" "stop" service))))

(defun drillops-start-toru ()
  "Start TORU in a shell."
  (interactive)
  (shell "*TORU*")
  (toggle-truncate-lines 1)
  (comint-clear-buffer)
  (comint-send-input)
  (insert "cd %TORU_DIR%")
  (comint-send-input)
  (insert "toru.exe")
  (comint-send-input))

(defun drillops-start-webpack ()
  "Start the webpack dev server."
  (interactive)
  (shell "*Webpack Dev*")
  (toggle-truncate-lines 1)
  (comint-clear-buffer)
  (comint-send-input)
  (insert "cd %PLANCK_SRC%/automation-hmi/src/HMI.Manager/Symphony.NodeServer/")
  (comint-send-input)
  (insert "npm run webpack-dev-server")
  (comint-send-input))

(defun drillops-start-node-server ()
  "Start the webpack dev server."
  (interactive)
  (shell "*Node Server*")
  (toggle-truncate-lines 1)
  (comint-clear-buffer)
  (comint-send-input)
  (insert "cd %PLANCK_SRC%/automation-hmi/src/HMI.Manager/Symphony.NodeServer/")
  (comint-send-input)
  (insert "npm run start-dev")
  (comint-send-input))

(defun drillops-start-dev ()
  "Bootstrap a working DrillOps for development."
  (interactive)
  (drillops-start-toru)
  (drillops-start-webpack)
  (drillops-start-node-server)
  (let ((dir (concat (getenv "PLANCK_SRC") "/" "automation/Source")))
    (omnisharp--do-server-start dir)))

(defun drillops-update-xpop-binary ()
  "Copy the compiled XPOP binary to its expected chocolately install location."
  (interactive)
  (copy-file "C:/Users/DAbad/code/planck-x/docker-xpop/xpop/xpop.exe"
             "C:/ProgramData/chocolatey/lib/xpop/bin/"
             t))  

(defun drillops-update-xpop-image ()
  "Compile and copy the XPOP PDDL image to its expected chocolately install location."
  (interactive)
  (let ((dir default-directory)
        (pddl "C:/Users/DAbad/code/planck-x/docker-pddl/")
        (image "drillops.xpi"))
    (setq default-directory pddl)
    (call-process "xpop.exe"
                  nil
                  `(,(get-buffer-create "*XPOP Copy Image*") t)
                  t
                  "compile" "-o" image (concat pddl "pddl"))
    (copy-file (concat pddl image)
               "C:/ProgramData/chocolatey/lib/xpop-image/bin/"
               t)
    (setq default-directory dir)))

(defun drillops-playback-xpop-image ()
  "Compile and copy the XPOP PDDL image to its expected chocolately install location."
  (interactive)
  (let ((image "C:/ProgramData/chocolatey/lib/xpop-image/bin/drillops.xpi"))
    (start-process "XPOP Playback"
                   "*XPOP Playback*"
                   "xpop.exe" "playback" image)))


(provide 'drillops)

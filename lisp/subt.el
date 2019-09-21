  
(defun subt-cmu-build (&optional package)
  (interactive "spackage: ")
  (let ((deps "--no-deps"))
    (unless package
      (setf package ""))
    (when (string= package "")
      (setf deps ""))
    (shell "*ROS_LAUNCH-CMU-Build*")
    (toggle-truncate-lines 1)
    (comint-clear-buffer) ;; in case of re-using previous shell
    (comint-send-input)   ;; reset prompt so next insert is placed in the correct spot
    (insert (concat "cd ~/workspace/subt_new; "
                    "source devel/setup.bash; "
                    "catkin build " package "; "
                    "catkin build " package " -i " deps " --catkin-make-args run_tests; "
                    "catkin_test_results build/" package))
    (comint-send-input)))

(provide 'subt)

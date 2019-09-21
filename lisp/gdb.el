
;;;; Run this command in GDB to open a new console window to see
;;;; stdout:
;;;;
;;;; (gdb) set new-console on
;;;;
;;;; Copied from stack-overflow.  Nifty but I haven't used it in
;;;; a while.

(add-hook
 'gdb-mode-hook
 '(lambda ()         
    (defun gdb-setup-windows ()
      "Layout the window pattern for option `gdb-many-windows'."
      (gdb-get-buffer-create 'gdb-locals-buffer)
      (gdb-get-buffer-create 'gdb-stack-buffer)
      (gdb-get-buffer-create 'gdb-breakpoints-buffer)
      (gdb-get-buffer-create 'gdb-disassembly-buffer)
      (set-window-dedicated-p (selected-window) nil)
      (switch-to-buffer gud-comint-buffer)
      (delete-other-windows)
      (let ((win0 (selected-window))
            (win1 (split-window nil ( / ( * (window-height) 3) 4)))
            (win2 (split-window nil ( / (window-height) 3)))
            (win3 (split-window-right)))
        (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
        (select-window win2)
        (set-window-buffer
         win2
         (if gud-last-last-frame
             (gud-find-file (car gud-last-last-frame))
           (if gdb-main-file
               (gud-find-file gdb-main-file)
             ;; Put buffer list in window if we
             ;; can't find a source file.
             (list-buffers-noselect))))
        (setq gdb-source-window (selected-window))
        (let ((win4 (split-window-right)))
          (gdb-set-window-buffer (gdb-disassembly-buffer-name) nil win4))
        (select-window win1)
        (gdb-set-window-buffer (gdb-stack-buffer-name))
        (let ((win5 (split-window-right)))
          (gdb-set-window-buffer (if gdb-show-threads-by-default
                                     (gdb-threads-buffer-name)
                                   (gdb-breakpoints-buffer-name))
                                 nil win5))
        (select-window win0)))))

(provide 'gdb)

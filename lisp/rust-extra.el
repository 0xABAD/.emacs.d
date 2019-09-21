
(defun cargo-test ()
  (interactive)
  ;; (compile "cargo test -q -- --nocapture"))
  (shell "*CARGO-TEST*")
  (comint-clear-buffer)
  (comint-send-input)
  (insert "cargo test -q -- --nocapture")
  (comint-send-input))

(defun cargo-format ()
  "`CARGO-FORMAT' runs rustfmt on the entire project directory by calling
'cargo +nightly fmt' with emacs' `COMPILE' command.  The command searches
for the root directory of the project by searching for the 'Cargo.toml' file
and is such file is not found then the formatting is not run.  If any errors
are encountered during the formatting operation the results will be displayed
in the `*compilation*'."
  (interactive)
  (let* ((curr-dir (cd "."))
         (main-dir nil))
    (do ((dir (cd ".")))
        ((or (string= dir "/")
             (not (eq main-dir nil))))
      (if (file-exists-p "Cargo.toml")
          (setf main-dir dir)
        (setf dir (cd ".."))))
    (if (eq main-dir nil)
        (message "[cargo-format] Can't find project root directory")
      (progn
        (setq compilation-finish-function
              (lambda (buf str)
                (if (null (string-match ".*exited abnormally.*" str))
                    (progn
                      (delete-window (get-buffer-window "*compilation*"))
                      (message "[cargo-format] Formatting complete")))))
        (compile "cargo +nightly fmt")
        (cd curr-dir)))))

(provide 'rust-extra)

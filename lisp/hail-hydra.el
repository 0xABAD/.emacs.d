(require 'hydra)

(defhydra hydra-scroll (:color amaranth)
  "Scroll"
  ("<" beginning-of-buffer "beg")
  ("K" beginning-of-buffer "beg")
  (">" end-of-buffer "end")
  ("J" end-of-buffer "end")
  ("j" scroll-up-command "down")
  ("n" (scroll-up-command 1) "down 1")
  ("k" scroll-down-command "up")
  ("p" (scroll-down-command 1) "up 1")
  ("q" nil "quit" :exit t)
  ("," nil "quit" :exit t)
  )

;; Ensures minibuffer is one line when this hydra is active.
(hydra-set-property 'hydra-scroll :verbosity 1)


(provide 'hail-hydra)

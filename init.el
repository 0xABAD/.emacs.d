;; -*- lexical-binding: t -*-

;; Make personal code available
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                              GUI Setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(add-to-list 'default-frame-alist '(width  . 185))
(add-to-list 'default-frame-alist '(height . 75))
(add-to-list 'default-frame-alist '(top . 25))
(add-to-list 'default-frame-alist '(left . 80))
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                              Editor Setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'grep)
(require 'paren)
(require 'cl)
(require 'self)

(prefer-coding-system 'utf-8)
(show-paren-mode 1)
(global-hl-line-mode 1)
(global-set-key (kbd "M-/") 'hippie-expand)
(setq apropos-do-all 1)
(setq-default line-spacing 0.2)
(setq-default indent-tabs-mode nil)
(setq show-paren-when-point-inside-paren t)
(setq-default tab-width 4)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(dolist (dir '("~/.local/bin" "~/.cargo/bin"))
  (add-to-list 'exec-path dir))

(let ((sep (if (eq system-type 'windows-nt) ";" ":")))
  (setenv "PATH"
          (concat "~/.local/bin" sep "~/.cargo/bin" sep (getenv "PATH"))))

(grep-apply-setting 'grep-template "rg --vimgrep -w <R> <F>")
(grep-apply-setting 'grep-command "rg --vimgrep -w ")
(grep-apply-setting 'grep-use-null-device nil)

(add-hook 'prog-mode-hook 'auto-revert-mode)
(add-hook 'c++-mode-hook 'self-highlight-c-function-calls)
(add-hook 'csharp-mode-hook 'self-highlight-c-function-calls)

(add-to-list 'auto-mode-alist '("\\.pddl\\'" . lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                           Comma Mode Setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comma-mode)

(dolist (hook '(emacs-lisp-mode-hook
                org-mode-hook
                prog-mode-hook
                text-mode-hook))
  (add-hook hook 'comma-mode))

;; Bind comma-mode to "C-," unless we are operating in a terminal enviroment
;; where "C-," is not recognized as control character.  Need a better solution
;; for this.
(if (display-graphic-p)
    (global-set-key (kbd "C-,") 'comma-mode)
  (progn
    (load-theme 'misterioso t)
    (global-set-key (kbd "C-c ,") 'comma-mode)))

(defmacro comma-mode-override (&rest body)
  "Allow a major mode to override comma-mode's keymap when used
with a `use-package' declaration.  For example, in Go code the
keybinding 'g f' will call 'go fmt' but in Rust code it is set
call 'cargo fmt'."
  `(lambda ()
     (let ((oldmap (cdr (assoc 'comma-mode minor-mode-map-alist)))
           (newmap (make-sparse-keymap)))
       (set-keymap-parent newmap oldmap)
       ,@body
       (make-local-variable 'minor-mode-overriding-map-alist)
       (push `(comma-mode . ,newmap) minor-mode-overriding-map-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                             Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Check for `use-package' manually as it will download and setup
;; all the other packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package ace-window
  :ensure t
  :init
  (setq aw-dispatch-always t)
  (global-set-key (kbd "M-o") 'ace-window))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package magit :ensure t)
(use-package hydra :ensure t)
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package ivy
  :ensure t
  :bind (:map comma-mode-map
              ("x b" . ivy-switch-buffer)))

(use-package ivy-rich
  :ensure t
  :hook (after-init . ivy-rich-mode))

(use-package counsel
  :ensure t
  :init
  (counsel-mode 1)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c G") 'counsel-git-grep)
  :bind (:map comma-mode-map
              ("c f" . counsel-rg)
              ("c m" . counsel-compile)
              ("x F" . counsel-buffer-or-recentf)
              ("x r l" . counsel-bookmark)))

(use-package base16-theme :ensure t)
(use-package doom-themes :ensure t :init (load-theme 'doom-one t))
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package js2-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-imenu-setup)
  (add-hook 'go-mode-hook
            (comma-mode-override
             (define-key newmap (kbd "c d") 'godef-jump)
             (define-key newmap (kbd "g f") 'gofmt)))
  :bind (:map go-mode-map
              ("M-q" . self-format-comment)))

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook
            (comma-mode-override
             (define-key newmap (kbd "g f") 'rust-format-buffer)
             (define-key newmap (kbd "g c") 'rust-compile)
             (define-key newmap (kbd "g t") 'rust-test)
             (define-key newmap (kbd "g r") 'rust-run)))
  :bind (:map rust-mode-map
              ("M-q" . self-format-comment)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'lsp-mode-hook
            (comma-mode-override
             (define-key newmap (kbd "c l h h") 'lsp-describe-thing-at-point)
             (define-key newmap (kbd "c l h s") 'lsp-signature-activate)
             (define-key newmap (kbd "c l g g") 'lsp-find-definition)
             (define-key newmap (kbd "c l g r") 'lsp-find-references)
             (define-key newmap (kbd "c l g t") 'lsp-find-type-definition)))
  :hook ((go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; This package makes many commands work on Mac OSX
;; emacs variables don't agree with the shell's variables.
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          Personal Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rust-extra)
(require 'hail-hydra)

(when (eq system-type 'windows-nt)
  (require 'windows))

;; MacOS specific setups
(when (eq system-type 'darwin)
  (setq python-shell-interpreter "python3")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))


;; ====================> BEG COMMA-MODE KEYMAP DEFINITIONS <====================

(define-key comma-mode-map (kbd "c g")   'self-grep-symbol-at-point)
(define-key comma-mode-map (kbd "c G")   'grep)
(define-key comma-mode-map (kbd "c o")   'self-occur-symbol-at-point)
(define-key comma-mode-map (kbd "c O")   'occur)
(define-key comma-mode-map (kbd "c t")   'xref-find-apropos)
(define-key comma-mode-map (kbd "c s")   'magit-status)
(define-key comma-mode-map (kbd "c S")   'magit-dispatch)
(define-key comma-mode-map (kbd "c b")   'magit-blame)
(define-key comma-mode-map (kbd "c a")   'align)
(define-key comma-mode-map (kbd "c r")   'align-regexp)
(define-key comma-mode-map (kbd "c i")   'imenu)
;; (define-key comma-mode-map (kbd "c :")   (comma-mode-align-with ":"))
;; (define-key comma-mode-map (kbd "c =")   (comma-mode-align-with "="))
(define-key comma-mode-map (kbd "c x")   'self-compile)
(define-key comma-mode-map (kbd "c w")   'toggle-truncate-lines)
(define-key comma-mode-map (kbd "c c")   'comment-region)
(define-key comma-mode-map (kbd "c u")   'uncomment-region)
(define-key comma-mode-map (kbd "c q")   'self-send-quit-other-window)
(define-key comma-mode-map (kbd "c M-p") 'beginning-of-buffer-other-window)
(define-key comma-mode-map (kbd "c M-n") 'end-of-buffer-other-window)
(define-key comma-mode-map (kbd "c v")   'hydra-scroll/body)

;; ====================> END COMMA-MODE KEYMAP DEFINITIONS <====================

;; END: Stays within init.el


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#f8f8f8" "#ab4642" "#538947" "#f79a0e" "#7cafc2" "#96609e" "#7cafc2" "#383838"])
 '(ansi-term-color-vector
   [unspecified "#f8f8f8" "#ab4642" "#538947" "#f79a0e" "#7cafc2" "#96609e" "#7cafc2" "#383838"] t)
 '(c-basic-offset 4)
 '(c-default-style
   '((c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "stroustrup")))
 '(c-doc-comment-style
   '((c-mode . javadoc)
     (c++-mode . javadoc)
     (java-mode . javadoc)
     (pike-mode . autodoc)))
 '(c-hanging-braces-alist 'set-from-style)
 '(c-offsets-alist
   '((extern-lang-open . 0)
     (namespace-open . 0)
     (extern-lang-close . 0)
     (namespace-close . 0)
     (inextern-lang . 0)
     (innamespace . 0)))
 '(column-number-mode t)
 '(compilation-error-regexp-alist
   '(absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line nim))
 '(compilation-message-face 'default)
 '(fci-rule-color "#373b41")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(lsp-ivy lsp-mode bison-mode magit-find-file exec-path-from-shell hydra ivy-rich all-the-icons-dired doom-modeline doom-themes go-imenu omnisharp yaml-mode toml-mode counsel ace-window which-key rust-mode cmake-mode js2-mode lua-mode magit magit-popup go-mode go-scratch quickrun kaolin-theme use-package cargo csharp-mode highlight-numbers highlight-quoted monokai-theme base16-theme))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

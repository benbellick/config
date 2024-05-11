;; Don't pollute with custom variables!
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package emacs
  :config
    (menu-bar-mode -1)
    (tool-bar-mode 0)
    (windmove-default-keybindings)
    (setq backup-by-copying t      ; don't clobber symlinks
	  backup-directory-alist
	  '(("." . "~/.emacs.d/.saves/"))    ; don't litter my fs tree
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t
	  create-lockfiles nil
	  auto-save-file-name-transforms `((".*" "~/.emacs.d/.saves/" t))
	  confirm-kill-emacs 'yes-or-no-p
	  revert-buffer-quick-short-answers t)
    (load-theme 'leuven-dark t)
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-keyboard-coding-system 'utf-8-unix)
    ;better modified mode-line
    (setq-default mode-line-modified (list
				      '(:propertize (:eval (cond (buffer-read-only "[x]") (t "[ ]")))
				      help-echo mode-line-read-only-help-echo local-map
						 (keymap
						  (mode-line keymap
							     (mouse-1 . mode-line-toggle-read-only)))
						 mouse-face mode-line-highlight)
                                      '(:propertize (:eval (cond ((buffer-modified-p) "[*]") (t "[·]")))
					 help-echo mode-line-modified-help-echo local-map
						    (keymap
						     (mode-line keymap
								(mouse-1 . mode-line-toggle-modified)))
						    mouse-face mode-line-highlight)))
    ;Get rid of annoying: mode-line-mule-info, mode-line-client, mode-line-remote
    (setq-default mode-line-format '("%e" mode-line-front-space
				    (:propertize ("" mode-line-modified) display (min-width (5.0)))
				    mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
				    (vc-mode vc-mode)
				    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))

;;No more accidental kill emacs while using merlin
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)


;;Add ability to switch to buffer when switching file
(use-package project
  :config
  (setq project-switch-commands '((project-find-file "Find file")
				  (project-switch-to-buffer "Find buffer")
				  (project-find-regexp "Find regexp")
				  (project-find-dir "Find directory")
				  (project-eshell "Eshell")
				  (project-kill-buffers "Kill all buffers")
				  (magit-project-status "Magit" ?m))))

(use-package prog-mode
  :hook (prog-mode . display-line-numbers-mode))
(use-package text-mode
  :hook (text-mode . turn-on-visual-line-mode))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)
	 ("M-s" . flymake-show-project-diagnostics)))

(use-package magit
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)))


(use-package markdown-mode
  :ensure t
  :config
    (setq markdown-fontify-code-blocks-natively t)) ; syntax highlight codeblocks

(use-package haskell-mode
  :ensure t
  :config
    (setq haskell-font-lock-symbols t
	  haskell-tags-on-save t
	  haskell-process-log t))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-black-fix-code nil t))))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package org
  :hook (org-mode . org-indent-mode)
  :ensure org-bullets
  :config (setq org-preview-latex-default-process 'dvisvgm
		org-format-latex-options (plist-put org-format-latex-options :scale 3.0)
		org-open-link-functions nil ;; don't use ctags
		org-hide-emphasis-markers t
		org-entities-user '(("mreal" "\\mathbb{R}" t "real" "real" "R" "ℝ"))))

(use-package org-bullets)

;; Setup compilation mode
(use-package compile
  :hook (compilation-filer-hook . ansi-color-compilation-filter))

(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

(use-package tex
  :ensure auctex
  :requires pdf-tools
  :config
    (setq TeX-view-program-selection
	  '((output-dvi "open")
	    (output-pdf "PDF Tools")
	    (output-html "open"))))

(use-package ocamlformat
  :ensure t
  :config
    (setq ocamlformat-show-errors 'echo))

(use-package tuareg
  :requires ocamlformat
  :ensure t
  :config
    (setq tuareg-prettify-symbols-full t)
    (defun opam-env ()
      (interactive nil)
      (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))
  :hook
    (tuareg-mode . prettify-symbols-mode)
    (tuareg-mode . (lambda() (add-hook 'before-save-hook 'ocamlformat-before-save)))
    (tuareg-mode . (lambda() (setq compile-command "dune build"))))

(use-package ocp-indent
  :ensure t)

(use-package utop
  :ensure t
  :config
    (setq utop-command "opam exec -- dune utop . -- -emacs")
  :hook
    (tuareg-mode . utop-minor-mode))

(setq opam-share (ignore-errors (car (process-lines "opam" "var" "share"))))
(setq opam-p (and opam-share (file-directory-p opam-share)))
(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

(use-package merlin
  :if opam-p
  :load-path (lambda () (expand-file-name "emacs/site-lisp" opam-share))
  :config (setq merlin-command 'opam)
  :hook (tuareg-mode . merlin-mode) (merlin-mode . company-mode))

(add-hook 'tuareg-mode-hook
	  (lambda()
	    (add-hook 'before-save-hook 'ocamlformat-before-save)))

(use-package imandra-mode
  :requires (tuareg)
  :load-path "~/local_emacs/imandra-mode/"
  :config
    (add-to-list 'auto-mode-alist '("\\.iml[i]?\\'" . imandra-mode)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

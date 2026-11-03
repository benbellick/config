;; Don't pollute with custom variables!
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)

;; My experimental packages
(add-to-list 'load-path "~/config/emacs/packages")

(use-package emacs
  :config
    (menu-bar-mode -1)
    (tool-bar-mode 0)
    (windmove-default-keybindings)
    (setq completion-cycle-threshold 3) ;; autocomplete when only three suggestions
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
	  visible-bell t
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
				    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
    ;So that lsp-mode performs better
    (setq read-process-output-max (* 1024 1024 ))
    (setq gc-cons-threshold 100000000)) ;;1mb

(use-package grep
  :config
  (setq grep-find-ignored-files
      (append grep-find-ignored-files
              '("*.tar.gz" "*.gz" "*.zip" "*.jar" "*.class" "*.pyc"))))

;;No more accidental kill emacs while using merlin
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)


(use-package ef-themes
  :straight t
  :config
  (modus-themes-load-theme 'ef-cyprus))


;;Add ability to switch to buffer when switching file
(use-package project
  :after (justl rg)
  :config
  (defun justl-in-project ()
    "Execute justl recipe in current project."
    (interactive)
    (let* ((project (project-current t))
           (default-directory (project-root project)))
      (call-interactively #'justl-exec-recipe-in-dir)))

  (defun justl-list-in-project ()
    "List justl recipes in current project."
    (interactive)
    (let* ((project (project-current t))
           (default-directory (project-root project)))
      (call-interactively #'justl)))

  (setq project-switch-commands '((project-find-file "Find file")
				  (project-switch-to-buffer "Find buffer")
				  (project-find-regexp "Find regexp")
				  (project-find-dir "Find directory")
				  (project-shell "Shell")
				  (project-kill-buffers "Kill all buffers")
				  (magit-project-status "Magit" ?m)
				  (rg-project "Ripgrep" ?R)
				  (justl-in-project "Just" ?j)
				  (justl-list-in-project "Just list" ?J))
	project-vc-extra-root-markers '(".project.el")
	project-compilation-buffer-name-function #'project-prefixed-buffer-name
	project-vc-ignores '("*.tar.gz"))
  :bind (("C-x p R" . rg-project)
	 ("C-x p j" . justl-in-project)
	 ("C-x p J" . justl-list-in-project)))

(use-package project-recipe
  :after project
  :bind ("C-x p t" . project-compile-recipe))

(use-package prog-mode
  :hook (prog-mode . display-line-numbers-mode)
        (prog-mode . flyspell-prog-mode))

(use-package text-mode
  :hook (text-mode . turn-on-visual-line-mode))

(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)
	 ("M-s" . flymake-show-project-diagnostics)))

(use-package magit
  :straight t
  :config
  (put 'magit-refs-sections-hook 'safe-local-variable 'listp)
  (put 'magit-status-headers-hook 'safe-local-variable 'listp)
  (put 'magit-status-sections-hook 'safe-local-variable 'listp)
  (setq magit-diff-refine-hunk t))

(use-package git-link
  :straight t
  :config
  (setq git-link-open-in-browser t))

;; (use-package forge
  ;; :after magit)

(use-package hl-todo
  :straight t
  :hook ((prog-mode . hl-todo-mode)))


(use-package markdown-mode
  :straight t
  :config
    (setq markdown-fontify-code-blocks-natively t)) ; syntax highlight codeblocks

(use-package antlr-mode
  :straight t
  :mode "\\.g4\\'")

(use-package haskell-mode
  :straight t
  :config
    (setq haskell-font-lock-symbols t
	  haskell-tags-on-save t
	  haskell-process-log t))

;; (use-package elpy
;;   :straight t
;;   :init
;;   (elpy-enable)
;;   :config
;;   (add-hook 'elpy-mode-hook (lambda ()
;;                             (add-hook 'before-save-hook
;;                                       'elpy-black-fix-code nil t))))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

	
(use-package org
  :hook (org-mode . org-indent-mode)
  :config (setq org-preview-latex-default-process 'dvisvgm
		org-format-latex-options (plist-put org-format-latex-options :scale 3.0)
		org-open-link-functions nil ;; don't use ctags
		org-hide-emphasis-markers t
		org-list-allow-alphabetical t
		org-id-link-to-org-use-id t
		org-entities-user '(("R" "\\mathbb{R}" t "real" "real" "R" "ℝ")
				    ("Z" "\\mathbb{Z}" t "ints" "ints" "Z" "ℤ")
				    ("N" "\\mathbb{N}" t "nat" "nat" "N" "ℕ")
				    ("Q" "\\mathbb{Q}" t "rationals" "rationals" "Q" "ℚ")
				    ("C" "\mathbb{C}" t "complex" "complex" "C" "ℂ")
				    ("tleq" "\trianglelefteqslant" t  ""  "" "" "⊴")
				    ("tle" "\trinalge" t "" "" "" "⊲")
				    ("qed" "\qedsymbol" t "" "" "" "□")
				    ("mapsto" "\mapsto" t "" "" "" "↦")
				    )
		org-babel-load-languages '((latex . t)
					   (emacs-lisp . t)))
  (add-to-list 'org-latex-packages-alist '("" "tikz-cd" t))
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  :bind ("C-c l" . 'org-store-link))

(use-package org-bullets
  :straight t)

;; Setup compilation mode
(use-package compile
  :hook (compilation-filer-hook . ansi-color-compilation-filter))

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (save-excursion 
                                  (goto-char compilation-filter-start) 
                                  (line-beginning-position))
                                (point))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package pdf-tools
  :straight t
  :config (pdf-tools-install))

(use-package tex
  :straight auctex
  :requires pdf-tools
  :config
    (setq TeX-view-program-selection
	  '((output-dvi "open")
	    (output-pdf "PDF Tools")
	    (output-html "open"))
	  exec-path (append exec-path '("/Library/TeX/texbin/"))))

(use-package ocamlformat
  :straight t
  :config
    (setq ocamlformat-show-errors 'echo))

(use-package tuareg
  :requires ocamlformat
  :straight t
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

;; (use-package ocp-indent
;;   :straight t)

(use-package utop
  :straight t
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

(use-package company
  :straight t
  :config (setq company-idle-delay 0.01))


(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l r" . eglot-rename)
	      ("C-c l h" . eldoc)
	      ("C-c l f" . eglot-format)
	      ("C-c l F" . eglot-format-buffer)
	      ("C-c l d" . xref-find-definitions-at-mouse)
	      ("C-c l R" . eglot-reconnect))
  :config
  ;; Format on save when eglot is active
  (add-hook 'eglot-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format nil t)))

  ;; Python LSP: try pyright → jedi → pylsp
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                          "jedi-language-server"
                                          "pylsp"))))

  ;; Protobuf LSP: buf
  (add-to-list 'eglot-server-programs
               '(protobuf-mode . ("buf" "lsp" "serve"))))

;; special java handling for eglot
(use-package eglot-java
  :straight t
  :after eglot)

(use-package jarchive
  :straight t
  :after eglot
  :config
  (jarchive-setup))

(use-package yasnippet                 
  :straight t
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
				 '("~/config/emacs/snippets")))
  (setq yas-wrap-around-region t)
  :hook ((prog-mode . yas-minor-mode)))

(use-package auth-source)

(use-package gptel
  :straight t
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key))

(use-package gptel-integrations
  :after gptel)

(use-package nix-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package terraform-mode
  :straight t)

(defun copy-buffer ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Copied buffer"))

(global-set-key (kbd "C-x M-w") 'copy-buffer)
(use-package kubernetes
    :straight t)


;; (require 'dap-lldb)
;; (require 'dap-cpptools)
;; (setq dap-lldb-debug-program '("/Library/Developer/CommandLineTools/usr/bin/lldb-dap"))

(use-package treesit
  :config
  (setq treesit-language-source-alist
	'((dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"))))


(use-package rg
  :straight t)

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))



(use-package dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-lldb)) 

(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

;; (use-package lsp-java
;;   :hook (java-mode . lsp)
;;   :config
;;   (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
;;         lsp-java-format-settings-profile "GoogleStyle"
;;         indent-tabs-mode nil
;;         tab-width 2
;;         c-basic-offset 2))

(use-package twen-twen-tw
  :straight (:type git :host github :repo "benbellick/twen-twen-tw.el"
             :protocol ssh)
  :config
  (twen-twen-tw-mode 1))

(use-package tree-sitter-langs
  :straight t)
(tree-sitter-require 'typescript)

(use-package shell-maker
  :straight t)

(use-package agent-shell
  :straight t
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))

;; for eat terminal backend:
(use-package eat :straight t)

(use-package ghostel
  :straight t)

(use-package just-mode
  :straight t)

(use-package justl
  :straight t)

;; This MUST be the last item in the list
(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package org-present
  :straight t
  :hook
  ((org-present-mode . (lambda ()
                         (org-present-big)
                         (org-display-inline-images)
                         (org-cycle-hide-drawers 'all)))
   (org-present-mode-quit . (lambda ()
                              (org-present-small)
                              (org-remove-inline-images)))))

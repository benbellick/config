(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '((output-dvi "open")
     (output-pdf "PDF Tools")
     (output-html "open")))
 '(confirm-kill-emacs 'yes-or-no-p)
 '(css-indent-offset 2)
 '(custom-enabled-themes '(leuven-dark))
 '(haskell-process-log t)
 '(org-hide-emphasis-markers t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(auctex org-bullets pdf-tools eglot rust-mode ement yaml-mode typescript-mode tuareg elfeed-org math-symbol-lists markdown-mode elpy haskell-mode magit))
 '(revert-buffer-quick-short-answers t)
 '(safe-local-variable-values '((eval turn-off-auto-fill))))

(menu-bar-mode -1)
(windmove-default-keybindings)

;; move file backups into different directory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq create-lockfiles nil)
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/.saves/" t)))

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; highlight whitespace after 80 characters
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(add-hook 'prog-mode-hook 'whitespace-mode)


;; flymake keybindings
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-s") 'flymake-show-project-diagnostics))

;; markdown-mode configuration
(setq markdown-fontify-code-blocks-natively t) ; syntax highlight codeblocks

;; haskell-mode configuration
(setq haskell-font-lock-symbols t) ; display as unicode
(setq haskell-tags-on-save t) ; Use hasktags on save

;; Add personal additions to TeX input mode
(with-temp-buffer
  (activate-input-method "TeX") ;; the input method has to be triggered for `quail-package-alist' to be non-nil
  (let ((quail-current-package (assoc "TeX" quail-package-alist)))
   (quail-define-rules ((append . t))
		       ("\\llBracket" ?⟦)
		       ("\\rrBracket" ?⟧)
		       ("\\varV" ?𝒱)
		       ("\\varE" ?ℰ)
		       ("\\varG" ?𝒢)
		       )))

;; Setup org mode
(add-hook 'org-mode-hook
   (lambda ()
     (set-input-method 'TeX))) ;; use TeX input mode in Org-mode
(setq org-adapt-indentation t) ;; indent by heading
(with-eval-after-load 'org-ctags (setq org-open-link-functions nil)) ;; don't use ctags

;; Setup compilation mode
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter) ;; use ansi colors

;; Setup pdf tools
(pdf-tools-install)

;; Setup Ocaml-mode
(add-to-list 'load-path "~/.opam/default/share/emacs/site-lisp") ;;ocp-indent
(require 'ocp-indent)

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(add-hook 'tuareg-mode-hook
          (lambda()
            (when (functionp 'prettify-symbols-mode)
              (prettify-symbols-mode))))

;; imandra
(add-to-list 'load-path "~/local_emacs/imandra-mode/")

(require 'imandra-mode)
(add-to-list 'auto-mode-alist '("\\.iml[i]?\\'" . imandra-mode))

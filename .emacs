(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(haskell-process-log t)
 '(org-hide-emphasis-markers t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(eglot rust-mode ement yaml-mode typescript-mode tuareg elfeed-org math-symbol-lists markdown-mode elpy haskell-mode))
 '(safe-local-variable-values '((eval turn-off-auto-fill))))

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

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; highlight whitespace after 80 characters
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

;; markdown-mode configuration
(setq markdown-fontify-code-blocks-natively t) ; syntax highlight codeblocks

;; haskell-mode configuration
(setq haskell-font-lock-symbols t) ; display as unicode
(setq haskell-tags-on-save t) ; Use hasktags on save

;; agda-mode configuration
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "brightblack" :foreground "magenta"))))
 '(italic ((t (:strike-through t :underline "cyan" :slant italic)))))

(add-hook 'agda2-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-g") 'agda2-goal-and-context)))

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

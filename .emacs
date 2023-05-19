(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide-emphasis-markers t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(project elfeed-org elfeed org-ref ivy-bibtex math-symbol-lists markdown-mode elpy haskell-mode))
 '(safe-local-variable-values '((eval turn-off-auto-fill))))

;; move file backups into different directory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; haskell-mode configuration
(setq haskell-font-lock-symbols t) ; display as unicode

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

;; Setup org-ref mode
(setq bibtex-completion-bibliography '("~/coding/notes/research/master.bib")
      bibtex-completion-library-path '("~/coding/notes/research/pdfs/")
      bibtex-completion-notes-path "~/coding/notes/research/notes.org")
(require 'org-ref)
(require 'org-ref-ivy)

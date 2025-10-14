(defvar project-recipes-file ".project.el"
  "Name of the file containing project recipes.")

(defun project-recipe--read-dot-file (project-root)
  "Read PROJECT-ROOT file."
  (let ((recipe-file (expand-file-name project-recipes-file project-root)))
    (when (file-exists-p recipe-file)
      (with-temp-buffer
        (insert-file-contents recipe-file)
        (read (current-buffer))))))

(defun project-recipe--get-recipes (project-root)
  "Read recipes from PROJECT-ROOT file."
  (let ((content (project-recipe--read-dot-file project-root)))
    (plist-get content :recipes)))

(defun project-recipe--get-recipe-names (project-root)
  "Get recipe names from PROJECT-ROOT file."
  (let ((recipes (project-recipe--get-recipes project-root)))
    ;; t))
    (mapcar #'car recipes)))

(defun project-compile-recipe (recipe-name)
  (interactive
   (let* ((project-root (project-root (project-current t)))
	  (recipe-names (project-recipe--get-recipe-names project-root)))
     (list (completing-read "Recipe: " recipe-names nil t))))
  (if-let* ((project-root (project-root (project-current t)))
	    (recipes (project-recipe--get-recipes project-root))
	    (command (alist-get (intern recipe-name) recipes)))
      (let ((default-directory project-root))
	(compile command))
  (user-error "Recipe '%s' not found" recipe-name)))

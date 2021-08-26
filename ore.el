;;; package -- Summary
;;;
;;; Commentary:
;;;
;;; Code:

;;;; Require other packages
(require 'org)

(defun ore/start-page ()
  "Generate opening html tags for the presentation."
  (print (concat "<!DOCTYPE html>\n"
                 "<html lang=\"en\">\n"
                 "<head>\n"
                 "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.1.2/reset.min.css\" integrity=\"sha512-Mjxkx+r7O/OLQeKeIBCQ2yspG1P5muhAtv/J+p2/aPnSenciZWm5Wlnt+NOUNA4SHbnBIE/R2ic0ZBiCXdQNUg==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\" />\n"
                 "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.1.2/reveal.min.css\" integrity=\"sha512-WFGU7IgfYR0dq5aORzbD+NApAXdExNZFb7LaoO8olYImBW/iZxAwjKEuT+oYcFR6gOd+DAFssq/icMn8YVbQxQ==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\" />\n"
                 "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.1.2/theme/black.css\" integrity=\"sha512-UM89RlvOqgNbcGojhsntvOI5NX/Bbv96ba1q9nVzwVEbQJYG5sRYewxQMfE8TR1vzGnqkXfZioj3xbnYGTcn2A==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\" />\n"
                 "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.1.2/reveal.min.js\" integrity=\"sha512-K7P1+dtPriNNHlE4aJr+JKx1X6R0wvy24QBqL2CxaHc4XdkQjrH2t2FCrgoxZGMh6s1TgigNLEdrWa6NJra6Zg==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\"></script>\n"
                 "</head>\n<body>"))
  )

(defun ore/end-page ()
  "Generate closing html tages for the presentation."
  (print "</body>\n</html>"))

(defun ore/add-section ()
  "Generate opening section html tag for a slide."
  (print "<section data-auto-animate>"))

(defun ore/end-section ()
  "Generate closing section html tag for a slide."
  (print "</section>"))

(defun ore/read-file (file)
  "Return the contents of the FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun ore/load-svg-element (file)
  "Using FILE add the contents of the svg."
  (let ((filename (concat (file-name-directory (buffer-file-name)) file)))
    (print (ore/read-file filename)))

  )

(defun ore/render-element (element)
  "Using ELEMENT load the svg at the required location on the page."
  (ore/load-svg-element (gethash "file" (json-parse-string element)))
  )

(defun ore/render-elements (headline ore-value)
  "Using HEADLINE and ORE-VALUE to render slide elements for this node."
  (progn
    (let ((location (org-element-property :contents-begin headline)))
      (goto-char location)
      (setq headline-properties (org-entry-properties))

        (dolist (element headline-properties)
          (setq element-key (car element))
          (if (and (stringp element-key)
                   (string-match "^ORE_\\([[:digit:]]+\\)$" element-key))
              (ore/render-element (cdr element))))))
  )

(defun ore/parse-document (tree)
  "Parse TREE and generate html presentation."
  (org-element-map tree 'headline
    (lambda (hl)
      (let ((value (org-element-property :ORE hl)))
        (when value
          (progn
            (print (concat "<!-- " (org-element-property :raw-value hl) " Key: ORE  Value: " value " -->"))
            (ore/add-section)
            (ore/render-elements hl value)
            (ore/end-section)
        )))))
  )

(defun ore/render-presentation()
    "Render the current buffer into a reveal.js presentation."
    (interactive)
    (progn (push-mark (point) t t)
           (let ((tree (org-element-parse-buffer)))
             (ore/start-page)
             (ore/parse-document tree))
           (print (ore/end-page))
           (pop-mark))
    )

(provide 'ore)
;;; ore.el ends here

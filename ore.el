;;; package -- Summary
;;;
;;; Commentary:
;;;
;;; Code:

;;;; Require other packages
(require 'org)

(defun ore/start-page ()
  "Generate opening html tags for the presentation."
  (print "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n</head>\n<body>"))

(defun ore/end-page ()
  "Generate closing html tages for the presentation."
  (print "</body>\n</html>"))

(defun ore/add-section ()
  "Generate opening section html tag for a slide."
  (print "<section data-auto-animate>"))

(defun ore/end-section ()
  "Generate closing section html tag for a slide."
  (print "</section>"))

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
              (print (cdr element))))))
  )

(defun ore/parse-document (tree)
  "Parse TREE and generate html presentation."
  (print "ore/parse-document")

  (org-element-map tree 'headline
    (lambda (hl)
      (let ((value (org-element-property :ORE hl)))
        (when value
          (progn
            (print (concat (org-element-property :raw-value hl) "\nKey: ORE  Value: " value ))
            (ore/render-elements hl value)
        )))))
  )

(defun ore/render-presentation()
    "Render the current buffer into a reveal.js presentation."
    (interactive)
    (progn (push-mark (point) t t)
           (let ((tree (org-element-parse-buffer)))
             (print "ore/render-presentation")
             (ore/start-page)
             (ore/parse-document tree))
           (print (ore/end-page))
           (pop-mark))
    )

(provide 'ore)
;;; ore.el ends here

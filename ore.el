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
  "Gernate closing html tages for the presentation."
  (print "</body>\n</html>"))

(defun ore/add-section ()
  "Generate opening section html tag for a slide."
  (print "<section data-auto-animate>"))

(defun ore/end-section ()
  "Generate closing section html tag for a slide."
  (print "</section>"))

(defun ore/render-elements (node elements)
  "Render ELEMENTS in this NODE."
  (print node)
  (print elements)

  (catch 'foo
  (dolist (element elements)
    (print element)

    (print (org-element-map tree 'headline
        (lambda (hl)
          (print node)
          (when (string= (org-element-property :raw-value hl) node)
                             (progn (print ">> Return Elements")
                                    (setq location (org-element-property :contents-begin hl))
                                    (print location)
                                    (goto-char location)
                                    (print (org-entry-properties))
                                    ;;; break out map as we have found the node in the headline properties
                                    (throw 'foo t))))))
           )))

(defun ore/process-orn (orn)
  "Process ORN tag."
  (print "ore/process-orn")
  (print orn))

(defun ore/parse-document ()
  "Parse document and generate html presentation."
  (print "ore/parse-document")

  (setq or-nodes (org-element-map tree 'headline
    (lambda (hl)
      (and (org-element-property :level hl)
           (org-element-property :ORN hl)
           ;; return
           (org-element-property :ORN hl)))))

  (print (length or-nodes))

  (dolist (element or-nodes)
    (setq node-elements (split-string element ","))
    (ore/render-elements (car node-elements) (cdr node-elements))))

(defun ore/render-presentation()
    "Render the current buffer into a reveal.js presentation."
    (interactive)
    (progn (setq ore-starting-point (point))
           (set-mark ore-starting-point)
           (setq tree (org-element-parse-buffer))

           (print "ore/render-presentation")
           (ore/start-page)
           (ore/parse-document)
           (print (ore/end-page))))

(provide 'ore)
;;; ore.el ends here

;;; lisp/roam-export.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre-Antoine Comby
;;
;; Author: Pierre-Antoine Comby
;;
;; This is a collection of function to perform recursive export of org-roam notes to a given format.
;;
(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'ox)

(defvar ox-roam-max-depth 4
  "Depth of recursive export for a note.")

(defvar ox-roam-appendix-name "Appendix"
  "Name of the appendix section for related notes.")


;; For a given note, find all the links to other notes and merge everything in a
;; single buffer.
;; The buffer is then exported to the given format.

;; 1. Get the root note file path
;; 2. Get the root note metadata (title, id, etc.)
;; 3. Get the root note content
;; 4. For every heading in the root note, if the heading is a link to another note:
;; 4.1. Get the child note content and add it under the heading.
;; 4.2 Repeat 4. for the child note. until the depth is reached.
;; NB: Keep track of the notes already exported to avoid infinite loop. (use note id)
;; If the note id is already in the list, don't import content, simply create a link.

;; 5. For links in the note content, that are not headings, get the note content and
;; add it under a Related Content heading in the exported note.

;; 6. Export the buffer to the given format using ox-<format> functions.
;; 7. Save the exported file in the given directory.
;; 8. Return the exported file path.

(defvar ox-roam-linkid-re "\\[\\[id:\\([[:alnum:][:punct:]]+\\)\\]\\[\\(.*\\)\\]\\]"
  "Regular expression matching a link to a note with a custom id.")

(defvar ox-roam--visited-nodes-list nil
  "List of visited nodes.")
(defvar ox-roam-excluded-tags '("noexport" "empty")
  "List of tags to exclude from export.")

(defun ox-roam-ignore-node-p (node)
  "Return t if the node should be ignored."
  (or (member node ox-roam--visited-nodes-list)
      (cl-intersection (org-roam-node-tags node) ox-roam-excluded-tags :test #'string=)))

(define-error 'ox-roam-empty-node-error "The node is empty.")

(defun ox-roam-demote (tree depth)
  "Demote all headers in the tree by depth level."
  (org-element-map tree '(headline)
    (lambda (el) (org-element-put-property el :level (+ depth (org-element-property :level el))))
    )
  tree)

(defun ox-roam-node-as-subtree (node depth)
  "Get the NODE as a subtree of org-element, indent to DEPTH,
  and return the org-element tree, expanded again."
  (with-temp-buffer
    (insert
     (org-roam-with-temp-buffer (org-roam-node-file node)
       (if (> (org-roam-node-level node) 0)
           (progn
             (goto-char (org-roam-node-point node))
             (org-narrow-to-element)
             (re-search-forward org-property-drawer-re nil t 1))
         (progn
           (goto-char (point-min))
           (re-search-forward "#\\+.*?\n[^#]" nil t 1)))
       (if (>= (match-end 0) (point-max))(signal 'ox-roam-empty-node-error '(node)))
       (buffer-substring-no-properties (match-end 0) (point-max))))
    (let ((tree (org-element-parse-buffer))
          (new-depth (1+ depth)))
      (ox-roam-demote tree new-depth)
      (ox-roam-expand-tree tree new-depth)
    tree)))

(defun ox-roam-expand-tree (tree depth)
  "Expand the TREE until DEPTH reaches ox-roam-max-depth."
  (if (>= depth ox-roam-max-depth)
    (org-element-map tree 'headline
      (lambda (headline)
        (when-let* ((raw-title (org-element-property :raw-value headline))
                    (headline-loc (org-element-property :begin headline))
                    (headline-level (org-element-property :level headline))
                    (headline-tag (or (org-element-property :tags headline) '('notags)))
                    (linked-headline (when (string-match ox-roam-linkid-re raw-title)
                                       (list :ID (match-string 1 raw-title)
                                             :title (match-string 2 raw-title))))
                    (child-node  (org-roam-node-from-id (plist-get linked-headline :ID))))
          (unless (or (ox-roam-ignore-node-p child-node)
                      (cl-intersection (org-roam-node-tags child-node) ox-roam-excluded-tags))
            (cl-pushnew child-node ox-roam--visited-nodes-list)
            (org-element-adopt-elements headline (ox-roam-node-as-subtree child-node 1))))))
        tree)
)

(defun ox-roam--expand-headlines ()
  "Expand all the headlines that are valid org roam links in the current buffer."
  (let ((tree (org-element-parse-buffer)))
    (org-element-map tree 'headline
      (lambda (headline)
        (when-let* ((raw-title (org-element-property :raw-value headline))
                    (headline-loc (org-element-property :begin headline))
                    (headline-level (org-element-property :level headline))
                    (headline-tag (or (org-element-property :tags headline) '('notags)))
                    (linked-headline (when (string-match ox-roam-linkid-re raw-title)
                                       (list :ID (match-string 1 raw-title)
                                             :title (match-string 2 raw-title))))
                    (child-node  (org-roam-node-from-id (plist-get linked-headline :ID))))
          ;; Reformat the headline with title and id properties.
          (unless (or (member child-node ox-roam--visited-nodes-list)
                      (cl-intersection (org-roam-node-tags child-node) ox-roam-excluded-tags))
            (cl-pushnew child-node ox-roam--visited-nodes-list)
            (org-element-put-property headline :title (plist-get linked-headline :title))
            (org-element-adopt-elements headline (format ":PROPERTIES:\n:ID: %s\n:END:\n"
                                                         (plist-get linked-headline :ID)))
            (org-element-adopt-elements headline (ox-roam-node-as-subtree child-node (1+ headline-level)))))))
    (erase-buffer)
    (insert (org-element-interpret-data tree))))

(defun ox-roam-include-links ()
  "Include all the links to org-roam notes at the end of buffer."
  (let ((unique-links '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when-let* ((link-id (org-element-property :path link))
                    (link-node (org-roam-node-from-id link-id))
                    (link-node-file (org-roam-node-file link-node))
                    (link-node-title (org-roam-node-title link-node)))
          (unless (or  (member link-id unique-links) (ox-roam-ignore-node-p link-node))
            (save-excursion
              (push link-id unique-links)
              (condition-case nil
                  (progn
                    (goto-char (point-max))
                    (insert (format "\n** %s\n" link-node-title))
                    (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n" link-id))
                    (insert (org-element-interpret-data (ox-roam-node-as-subtree link-node 2)))
                    )
                (ox-roam-empty-node-error
                 (set-mark (point))
                 (line-move -4)
                 (delete-region (mark) (point)))
                 (message "Node %s is empty" link-node-title)
                 ))
              ))))))

(defun ox-roam-export (&optional arg)
  (interactive "P")
  (let* ((node (org-roam-node-at-point))
         (dest (pcase arg
                (`nil (expand-file-name (concat (file-name-as-directory org-directory) "exports/" (org-roam-node-slug node) "_extended.org")))
                (`(4) (expand-file-name (read-file-name "Export to: ")))
                ((pred stringp) (expand-file-name arg)))
               ))
    (with-temp-file dest
      (make-local-variable 'ox-roam--visited-nodes-list)
      (erase-buffer)
      (insert "# this file is generated by ox-roam-export.el\n")
      ;; expands all the headlines that are valid org roam links.
      (insert-file-contents (org-roam-node-file node))
      (ox-roam--expand-headlines)
      ;; Add the Content of related nodes
      (goto-char (point-max))
      (insert (concat "\n*" ox-roam-appendix-name "\n"))

      ;; Cleanup: Remove the print_bibliography from all the imported notes,
      ;; and create a single one at the end.
      (goto-char (point-min))
      (while (re-search-forward  "#\\+print_bibliography:.*?\n" nil t) (replace-match "" nil t))
      (goto-char (point-max))
      (insert "\n#+print_bibliography:\n"))
    (message "Exported to %s" dest)))

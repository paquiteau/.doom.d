;;; lisp/roam-export.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre-Antoine Comby
;;
;; Author: Pierre-Antoine Comby <
;;
;; This is a collection of function to perform recursive export of org-roam notes to a given format.
;;
(require 'org-roam)
(require 'ox)

(defvar org-roam-multi-export-depth 1
  "Depth of recursive export for a note.")



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


(require 'org)
(require 'org-element)

(defvar oer-linkid-re "\\[\\[id:\\([[:alnum:][:punct:]]+\\)\\]\\[\\(.*\\)\\]\\]"
  "Regular expression matching a link to a note with a custom id.")

(defvar oer-visited-nodes-list nil
  "List of visited nodes.")

(defun oer-get-begin-of-content (tree)
  "Return the position of the first heading or paragraph in the org-tree."
  (org-element-map tree '(paragraph headline)
    (lambda (el) (org-element-property :begin el))
    :first-match t))

(defun oer-strip-tree-from-header (tree)
  "Remove everything from the tree before the first headline or paragraph."
  (let ((begin-of-content (oer-get-begin-of-content tree)))
        (org-element-map tree org-element-all-elements
          (lambda (el) (when ( <= (org-element-property :end el) begin-of-content)
                    (org-element-extract-element el))
            (>= (org-element-property :begin el) begin-of-content))
          :first-match t
          )
        )
  tree )

(defun oer-demote (tree depth)
  "Demote all headers in the tree by depth level."
  (org-element-map tree '(headline)
    (lambda (el) (org-element-put-property el :level (+ depth (org-element-property :level el))))
    )
  tree)

(defun oer-merge-subfiles (node-id &optional depth)
  (unless depth (setq depth 1))
  (with-temp-buffer
    (insert-file-contents (condition-case nil (org-roam-node-file node-id) (error node-id)))
    (let* ((tree-doc (org-element-parse-buffer)))
      (when (> depth 1)
        (setq tree-doc (oer-strip-tree-from-header tree-doc)))
      (org-element-map tree-doc 'headline
        (lambda (headline)
          (when-let* ((raw-title (org-element-property :raw-value headline))
                      (headline-loc (org-element-property :begin headline))
                      (headline-level (org-element-property :level headline))
                      (linked-headline (when (string-match oer-linkid-re raw-title)
                                         (list :ID (match-string 1 raw-title)
                                               :title (match-string 2 raw-title))))
                     (child-node  (org-roam-node-from-id (plist-get linked-headline :ID)))
                     ;; Recursion
               (child-node-tree (oer-merge-subfiles child-node (+ headline-level depth)))
               )
            ;; Format the headlines
            (org-element-put-property headline :title (plist-get linked-headline :title))
            (org-element-adopt-elements headline (format ":PROPERTIES:\n:ID: %s\n:END:\n"
                                                         (plist-get linked-headline :ID)))
            (org-element-adopt-elements headline child-node-tree)
            )))

      (if (eq depth 1)
          (progn
            (delete-region (point-min) (point-max))
            (insert (org-element-interpret-data tree-doc))
            (buffer-substring-no-properties (point-min) (point-max)))
        (progn
          ;;If we are in a subfile, return the tree,with indented headlines.
          (oer-demote tree-doc (1- depth))
           tree-doc)
      );; endif
    ) ;; let
  ) ;; with-temp-buffer
); defun

(defun oer-prepare-export (filename)
  "Prepare the export of the note with FILENAME."
  (with-temp-buffer
    (insert (oer-merge-subfiles filename))
    (goto-char (point-max))
    (insert "\n* Related Content")
    (let ((unique-links '()))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when-let* ((link-id (org-element-property :path link))
                      (link-node (org-roam-node-from-id link-id))
                      (link-node-file (org-roam-node-file link-node))
                      (link-node-title (org-roam-node-title link-node)))
            (unless (member link-id unique-links)
              (save-excursion
                (push link-id unique-links)
                (goto-char (point-max))
                (insert (format "\n** %s\n" link-node-title))
                (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n" link-id))
                (insert
                 (with-temp-buffer
                   (insert-file-contents link-node-file)
                   (let ((tree (oer-strip-tree-from-header (org-element-parse-buffer))))
                         (oer-demote tree 2) ; 1: Related Content, 2: link title
                         (org-element-interpret-data tree))))
                (insert "\n")
                )))))
      ) ; let unique-links
    (buffer-substring-no-properties (point-min) (point-max))
    )   ; with-temp-buffer
  )   ; defu n


(with-output-to-temp-buffer "*test*"
  (let ((results (oer-prepare-export "/home/pac/org/roam/test_node.org")))
    (princ "Results:\n\n")
    (princ results)
))

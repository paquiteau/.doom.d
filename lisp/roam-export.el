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



;; For a given note, find all the links to other notes and merge everything in a single buffer.
;; The buffer is then exported to the given format.

;; 1. Get the root note file path
;; 2. Get the root note metadata (title, id, etc.)
;; 3. Get the rott note content
;; 4. For every heading in the root note, if the heading is a link to another note:
;; 4.1. Get the child note content and add it under the heading.
;; 4.2 Repeat 4. for the child note. until the depth is reached.
;; NB: Keep track of the notes already exported to avoid infinite loop. (use note id)
;; If the note id is already in the list, don't import content, simply create a link.

;; 5. For links in the note content, that are not headings, get the note content and add it under a Related Content heading in the exported note.

;; 6. Export the buffer to the given format using ox-<format> functions.
;; 7. Save the exported file in the given directory.
;; 8. Return the exported file path.

(defun org-roam-multi-export--get-note-content (note-id)
  "Return the content of the note with id NOTE-ID."
  (let ((note-path (org-roam-id-find note-id)))
    (with-temp-buffer
      (insert-file-contents note-path)
      (buffer-string))))

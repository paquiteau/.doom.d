;;; ~/.doom.d/songbook.el -*- lexical-binding: t; -*-
;;; songbook.el -- Major mode for editing the song files of Patacrep songbook

;; Author: Romain Goffe <romain.goffe@gmail.com>
;; Created: Feb 28 2010
;; Keywords: patacrep songbook emacs-mode

;; Copyright (C) 2010-2012 Romain Goffe <romain.goffe@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;; The indentation method is based on the work of
;; Scott Andrew Borton <scott@pp.htv.fi>
;; through is tutorial on emacs modes:
;; http://two-wugs.net/emacs/mode-tutorial.html

(defvar songbook-commands
  '("capo" "gtab" "utab" "lilypond" "image" "nolyrics")
  "Patacrep songbook commands.")

(defvar songbook-macros
  '("cover" "Intro" "Outro" "Bridge" "Pattern")
  "Patacrep songbook macros.")

;; create the regex string for each class of keywords
(defvar songbook-commands-regexp
  (concat "\\\\\\(" (mapconcat 'identity songbook-commands "\\|") "\\)"))
(defvar songbook-macros-regexp
  (concat "\\\\\\(" (mapconcat 'identity songbook-macros "\\|") "\\)"))

;;clear memory
(setq songbook-commands nil)
(setq songbook-macros nil)

(defun songbook-indent-line ()
  "Indent current line as songbook code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)	   ; First line is always non-indented
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\\\end") ; If the line we are looking at is the end of a block, then decrease the indentation
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) 2)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    (if (looking-at "^[ \t]*\\\\end") ; This hint indicates that we need to indent at the level of the END_ token
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at "^[ \t]*\\\\begin") ; This hint indicates that we need to indent an extra level
		  (progn
		    (setq cur-indent (+ (current-indentation) 2)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

;;songbook-mode
(define-derived-mode songbook-mode latex-mode
  "songbook-mode"
  "A variant of LaTeX mode for Patacrep! songs."

  ;;add keywords for highlighting
  (font-lock-add-keywords nil `((,songbook-commands-regexp . 'font-lock-keyword-face)
				(,songbook-macros-regexp . 'font-lock-variable-name-face)
				("\\\\\\[[^\]]+\]" . 'font-lock-constant-face)))

  ;;register indenting function
  (set (make-local-variable 'indent-line-function) 'songbook-indent-line))

(provide 'songbook)
;;; songbook.el ends here

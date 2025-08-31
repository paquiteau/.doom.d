;;; lisp/reftex-capf.el -*- lexical-binding: t; -*-


(defvar cape-reftex-max-annotation-length 20)
(defvar cape-reftex-ref-regexp
  (rx "\\"
      ;; List taken from `reftex-ref-style-alist'
      (or "autoref"
          "autopageref"
          "Cpageref"
          "cpageref"
          "Cref"
          "cref"
          "eqref"
          "Fref"
          "fref"
          "pageref"
          "Ref"
          "ref"
          "vpageref"
          "Vref"
          "vref")
      "{"
      (group (* (not (any "}"))))
      (regexp "\\="))
  "Regular expression to use when looking for the ref prefix.
Group number 1 should be the prefix itself."
  )



(defun cape-reftex-get-preview (key content)
  "Find the content stringin the buffer and check if it has a display property. If it does, return it."
  (let ((reftype (string-limit key 3)))
    (message "key is %s key %s reftype" key reftype)
    (save-excursion (or (cond
                         ((string= "eq:" reftype) (cape-reftex--get-env key))
                         ((member reftype '("fig", "tab")) (cape-reftex--get-env key))
                         ((string= "sec" reftype) (cape-reftex--get-sec-name  key)))
                        content))))


(defun cape-reftex--get-env (key)
  "Find and return the latex environment that has CONTENT in it."

  ;; use regex to find the nearest match for the string content
  (when (re-search-backward (format "\\\\label{%s}" key) nil t)
    (let (beg-env end-env)
      (save-excursion
        ;;       (backward-sexp) ;; move to the label line
        (LaTeX-find-matching-begin) ;; move to the begining of the environment
        (setq beg-env (point)) ;; save the position
        (forward-sexp) ;; move back inside the env
        (LaTeX-find-matching-end) ;; move to the end of the environment
        (setq end-env (point))) ;; save the position
      (buffer-substring beg-env end-env)) ;; return the full environment.
    ))

(defun cape-reftex--get-sec-name (key)
  (when (re-search-backward (format "\\\\label{%s}" key) nil t)
    (re-search-backward "\\\\\\(?:chapter\\|par\\(?:agraph\\|t\\)\\|s\\(?:ection\\|ub\\(?:paragraph\\|s\\(?:\\(?:ubs\\)?ection\\)\\)\\)\\)" nil t)
    (buffer-substring (beginning-of-line) (end-of-line))
    )
  )

(defun cape-reftex-collect(key content)
  "Annotate KEY with CONTENT if the latter is not nil.
Obeys the setting of `company-reftex-max-annotation-length'."
  (cond
   ((not content) key)
   (t
    (progn
      (propertize key 'reftex-doc-display (cape-reftex-get-preview key content) 'reftex-annotation content)))))


(defun pac/cape-reftex-annotation (entry)
  (concat "  " (s-truncate cape-reftex-max-annotation-length (get-text-property 0 'reftex-annotation entry) "...")))




(defun pac/cape-reftex-doc-buffer (entry)
  (save-window-excursion)
  (with-current-buffer (get-buffer-create "*capf-reftex*")
    (setf (buffer-string) (get-text-property 0 'reftex-doc-display entry))
    (current-buffer)
    ))

(defvar pac/cape-reftex--do-parse t)
(setq pac/cape-reftex--do-parse t)
(defvar pac/cape-reftex--parse-data nil)

(defun pac/cape-reftex-get (_)
  (when pac/cape-reftex--do-parse
    (reftex-parse-all)
    (setq pac/cape-reftex--do-parse nil)
    (setq pac/cape-reftex--parse-data (cl-loop for entry in (symbol-value reftex-docstruct-symbol) if (stringp (car entry))
                                               collect
                                               (cape-reftex-collect (car entry) (cl-caddr entry)))))
  pac/cape-reftex--parse-data)

(defun pac/cape-reftex-label ()
  "If INTERACTIVE is nil the function acts like a Capf."

  (interactive "*")
  (when-let*
      ((trig (and reftex-mode (looking-back cape-reftex-ref-regexp nil)))
       (prefix (match-string-no-properties 1))
       (beg (match-beginning 1))
       (end (match-end 1))
       )
    (list beg end (completion-table-dynamic #'pac/cape-reftex-get)
          :annotation-function #'pac/cape-reftex-annotation
          :company-doc-buffer #'pac/cape-reftex-doc-buffer
          :exit-function (lambda (str _status) (insert) (setq pac/cape-reftex--do-parse t)))))

(add-hook 'LaTeX-mode-hook #'(lambda () (add-hook 'completion-at-point-functions #'pac/cape-reftex-label 0 t)))

;;(add-hook 'LaTeX-mode-hook #'(lambda () (add-to-list 'TeX-complete-list '("\\\\[Cc]ref{\\([^{}\n\\%,]*\\)" 1 LaTeX-label-list "}"))))

;;; lisp/conv-commit.el -*- lexical-binding: t; -*-


;; TODO add a completion prompt for the scope, and a function to detect scopes from git logs.
;; The availables scope for each projects should be save in file (plist ? table?) and loaded when the project is opened.
;; Once this is done, let's make a package out of it.
;;

(require 'consult)
(require 'subr-x)
(defvar conv-commit-type-desc nil
  "The type of conventional commit.")
(setq conv-commit-type-desc
'(("build"
      :desc "Changes that affect the build system or external dependencies."
      :icon ?
      :props (:foreground "#00008B" :height 1.2))
     ("chore"
      :desc "Updating grunt tasks."
      :icon ?󰑌
      :props (:foreground "gray" :height 1.2))
     ("ci"
      :desc "Changes to CI configuration files and scripts."
      :icon ?
      :props (:foreground "gray" :height 1.2))
     ("docs"
      :desc "Documentation only changes."
      :icon ?󰈙
      :props (:foreground "dark blue" :height 1.2))
     ("feat"
      :desc "A new feature."
      :icon ?☘
      :props (:foreground "green" :height 1.2))
     ("fix"
      :desc "A bug fix."
      :icon ?
      :props (:foreground "dark red" :height 1.2))
     ("perf"
      :desc "A code change that improves performance."
      :icon ?🗲
      :props (:foreground "" :height 1.2))
     ("refactor"
      :desc "A code changes that neither fixes a bug nor adds a feature."
      :icon ?✀
      :props (:foreground "dark green" :height 1.1))
     ("revert"
      :desc "For commits that reverts previous commit(s)."
      :icon ?⭯
      :props (:foreground "dark red" :height 1.2))
     ("style"
      :desc "Changes that do not affect the meaning of the code."
      :icon ?
      :props (:foreground "dark green" :height 1.2))
))

(defun conv-commit-type-completion-decorate (type)
  "Decorate the completions candidates with icon prefix and description suffix.

TYPE is the type of conventional commit.
Return a list (candidate, icon, description)."

  (let ((type-data (cdr (assoc type conv-commit-type-desc))))
    (list
     type
     (concat
     (propertize (string (plist-get type-data :icon))
                'face (plist-get type-data :props))
     "   ")
     (concat
      (string-pad " " (- 10 (length type)))
      (propertize (plist-get type-data :desc) 'face '(:foreground "gray" ))))))


(defun conv-commit-type-prompt ()
  (interactive)
  (consult--read conv-commit-type-desc
                 :prompt "Commit type: "
                 :annotate #'conv-commit-type-completion-decorate
                 )
)

(defun conv-commit-scope-prompt ()
  (interactive)
  (when-let ((scope (read-string "Scope: ")))
    (if (string-empty-p scope) "" (concat "(" scope ")"))))

(defun conv-commit-format ()
  (interactive)
  (let ((type (conv-commit-type-prompt))
        (scope (conv-commit-scope-prompt)))
      (if (or type scope)
      (format "%s%s: " type scope)
    )))

(provide 'conv-commit)

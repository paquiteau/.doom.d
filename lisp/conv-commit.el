;;; lisp/conv-commit.el -*- lexical-binding: t; -*-

(require 'consult)
(defvar conv-type-desc nil
  "The type of conventional commit.")
(setq conv-type-desc
'(("build"
      :desc "Changes that affect the build system or external dependencies."
      :icon ?🧰
      :props (:foreground "#00008B" :height 1.2))
     ("chore"
      :desc "Updating grunt tasks."
      :icon ?🔧
      :props (:foreground "gray" :height 1.2))
     ("ci"
      :desc "Changes to CI configuration files and scripts."
      :icon ?🛠
      :props (:foreground "gray" :height 1.2))
     ("docs"
      :desc "Documentation only changes."
      :icon ?🖹
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
      :icon ?🚀
      :props (:foreground "dark green" :height 1.2))
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

(defun conv-type-completion-decorate (type)
  "Decorate the completions candidates with icon prefix and description suffix.

TYPE is the type of conventional commit.
Return a list (candidate, icon, description)."

  (let ((type-data (cdr (assoc type conv-type-desc))))
    (list
     type
     (concat
     (propertize (string (plist-get type-data :icon))
                'face (plist-get type-data :props))
     "   ")
     (concat
      (string-pad " " (- 10 (length type)))
      (propertize (plist-get type-data :desc) 'face '(:foreground "gray" :slant "italic"))))))


(defun conv-type-completion-read ()
  (interactive)
  (consult--read conv-type-desc
                 :prompt "Commit type: "
                 :annotate #'conv-type-completion-decorate
                 )
)

(defvar use-magit-commit-prompt-p nil)
(defun use-magit-commit-prompt (&rest args)
  (setq use-magit-commit-prompt-p t))

(defun conv-type-prompt ()
  (interactive)
  (when use-magit-commit-prompt-p
    (when-let ((type (conv-type-completion-read))
               (scope (read-string "Scope: ")))
      (unless (string-empty-p scope) (setq scope (format "(%s)" scope))) ;; only format scope when non empty.
      (setq use-magit-commit-prompt-p nil)
      (insert (format "%s%s: " type scope)))))

(remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
(add-hook 'git-commit-setup-hook 'conv-type-prompt)
(advice-add 'magit-commit :after 'use-magit-commit-prompt)

(provide 'conv-commit)

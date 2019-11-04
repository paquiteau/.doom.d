;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;; ---------------------
;; Private
;; ---------------------

(setq user-mail-adress "pac@crans.org"
      user-full-name "Pierre-Antoine Comby")
;; ---------------------
;; Themes
;; ---------------------
;;

;; All theme are safe to load
(setq custom-safe-themes t)

;; Custom doom-one configuration
  (when (custom-theme-enabled-p 'doom-one)
    (after! org
      ;; Purple boxes for Org BEGIN_SRC and END_SRC
      (set-face-attribute 'org-block-begin-line nil
                          :background "#5c3d5c"
                          :foreground "#a16ba1"
                          :weight 'bold
                          :height 0.9
                          :box '(:line-width 2 :color "#5c3d5c"))))
(after! imenu-list
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select t :ttl 0))


(setq treemacs-width 20)



;; ---------------------
;; LaTeX
;; ---------------------


(after! latex
  ;; Save without asking when invoking TeX commands
  (setq TeX-save-query nil)
  ;; While inserting commands in comment sections, do not be intelligent and comment the command
  (setq LaTeX-insert-into-comments nil)
  ;; if the babel language is german, set the quotes as if english
  (add-hook 'TeX-language-fr-hook
            (lambda ()
              (setq TeX-quote-language `("francais" "``" "''" ,TeX-quote-after-quote))))
(turn-off-smartparens-mode)
;;  (setq LaTeX-electric-left-right-brace t)
  ;; Do not spellcheck latex documents when opened, this takes a lot of time.
  ;;(remove-hook 'flyspell-mode-hook #'+spellcheck|immediately)
  (after! tex
    (setq-hook! 'TeX-mode-hook +flyspell-immediately nil))
  (customize-set-variable 'LaTeX-math-abbrev-prefix (kbd "!"))
  (setq LaTeX-command (concat LaTeX-command " -shell-escape"))
  )

;; Enable whitespace mode with latex
;;(add-hook 'LaTeX-mode-hook #'whitespace-mode)

;; Enable LaTeX-math-mode by default: add math symbols with the key `LaTeX-math-abbrev-prefix'
(add-hook 'LaTeX-mode-hook (lambda () (LaTeX-math-mode)
                             (prettify-symbols-mode)
                            ; (latex-extra-mode)
                             (visual-line-mode)
                             (set-fill-column 2000)
                             ))
;; ---------------------
;; OrgMode
;; ---------------------

(after! org
  (setq org-startup-folded nil ; do not start folded
        org-tags-column -100 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        org-fontify-done-headline nil ; do not change the font of DONE items
        org-ellipsis " ↴ "
        org-babel-min-lines-for-block-output 5 ; when to wrap results in #begin_example
        org-return-follows-link t  ; RET follows links
        org-hide-emphasis-markers t ; do not show format markers
        org-startup-with-inline-images t ; open buffers show inline images
        ;;ob-async-no-async-languages-alist '("ipython" "jupyter") ; do not use async for these languages
        ;; visual-fill-column-width 120 ; size for usage with visual fill column mode
        ;;org-babel-default-header-args:sh

        )
  )
;; ------------
;; Org & LaTeX
;; ------------
(after! org
  ;; Include the latex-exporter
  ;; Add minted to the defaults packages to include when exporting
  (add-to-list 'org-latex-packages-alist '("margin=2cm" "geometry"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "caption"))
  ;; Tell the latex export to use the minted package for source
  ;; code coloration.
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines") ("linenos=true")))

  ;; Let the exporter use the -shell-escape option to let latex
  ;; execute external programs.
  ;; This obviously and can be dangerous to activate!

  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; fix color handling in org-preview-latex-fragment
(let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
  (plist-put dvipng--plist :use-xcolor t)
  (plist-put dvipng--plist :image-converter '("dvipng -D %D -T tight -o %O %f")))

)

(def-package! org-ref
  :after org
  :config
  )


;; ------------
;; Org Files
;; ------------
(after! org
  ;; Purple boxes for Org BEGIN_SRC and END_SRC
  (set-face-attribute 'org-block-begin-line nil
                      :background "#5c3d5c"
                      :foreground "#a16ba1"
                      :weight 'bold
                      :height 0.9
                      :box '(:line-width 2 :color "#5c3d5c"))
  (add-hook 'org-mode-hook (lambda () (hl-fill-column-mode -1)))


  ;; Do not enable auto-fill-mode by default
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Do not move my buffer after cycling visibility
  (remove-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change)

  ;; Enable flyspell
  (add-hook 'org-mode-hook #'flyspell-mode)

  ;; After evaluating a SRC_BLOCK, redisplay inline images
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  )
;; ------
;; Org & Getting things done
;; ------
(after! org
  (setq org-agenda-files '("~/org/todo.org"
                           "~/org/gtd.org"))

  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 2)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)))
)



;; -----------------------
;; Tools
;; -----------------------

(add-hook! magit-mode (visual-line-mode +1))


;; -----------------------
;; UI
;; -----------------------
(setq doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-minor-modes nil
      doom-modeline-enable-word-count t
      doom-modeline-checker-simple-format t
      doom-modeline-persp-name t
      doom-modeline-lsp t)

;; Show trailing white spaces
(setq show-trailing-whitespace t)

;; Disable trailing whitespaces in the minibuffer
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)

;; song book mode

(load! "songbook")
(setq auto-mode-alist
(append '(("\\.sg$" . songbook-mode)
) auto-mode-alist))
(autoload 'songbook-mode "songbook" "Major-mode for Patacrep songbook's songs" t)

;;
;; Wiki Moin Moin
;;
(load! "moinmoin-mode")

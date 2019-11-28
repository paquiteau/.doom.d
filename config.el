;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;; Code:
;; -----------------
;; USER CONFIG
;; -----------------


(setq user-mail-adress "pac@crans.org"
      user-full-name "Pierre-Antoine Comby")

;; -----------------
;; THEME
;; -----------------

;; Custom doom-one configuration
(after! imenu-list
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select t :ttl 0)
  )
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

;; --------------------
;;        IDE
;; --------------------

(add-hook! magit-mode (visual-line-mode +1))
(setq magit-repository-directories '(("~/Repositories" . 3)))
(setq projectile-project-search-path  '("~/Repositories"))

(after! treemacs
  (setq treemacs-width 20)
  )

(after! company
  (setq company-idle-delay 0))

;; Create new workspace when switching project
(setq +workspaces-on-switch-project-behavior t)

(after! dired
  (map!
   :map dired-mode-map
   :nv "DEL" #'dired-up-directory)
  )

(setq  ispell-dictionary "english" )

(let ((langs '("english" "francais" "deutsch" )))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key [f6] 'cycle-ispell-languages)


;; -----------------
;;  LaTeX
;; -----------------

(after! latex
  ;; Save without asking when invoking TeX commands
  (setq TeX-save-query nil)
  ;; use --shell-escape required for minted package
  (setq LaTeX-command (concat LaTeX-command " -shell-escape"))
  ;; While inserting commands in comment sections, do not be intelligent and comment the command
  (setq LaTeX-insert-into-comments nil)
  ;; if the babel language is french set the quotes as if english
  (add-hook 'TeX-language-fr-hook
            (lambda ()
              (setq TeX-quote-language `("francais" "``" "''" ,TeX-quote-after-quote))))
  ;; use ! instead of ` for symbol shortcut
  (customize-set-variable 'LaTeX-math-abbrev-prefix (kbd "!"))
  (add-hook 'LaTeX-mode-hook (lambda () (LaTeX-math-mode)
                               (prettify-symbols-mode)
                               ;;(latex-extra-mode)
                               (visual-line-mode)
                               (set-fill-column 2001)
                               )
            )
  )
;; need to be done separetly
(setq LaTeX-math-list (quote (
                             (?I "int" "" 8734)
                             (?8 "infty" "" 8747))
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
        org-image-actual-width 200       ; same size for all images
        ;;ob-async-no-async-languages-alist '("ipython" "jupyter") ; do not use async for these languages
        ;; visual-fill-column-width 120 ; size for usage with visual fill column mode
        ;;org-babel-default-header-args:sh

        )
  )
;; Style for org boxes
(when (custom-theme-enabled-p 'doom-one)
    (after! org
      ;; Purple boxes for Org BEGIN_SRC and END_SRC
      (set-face-attribute 'org-block-begin-line nil
                          :background "#5c3d5c"
                          :foreground "#a16ba1"
                          :weight 'bold
                          :height 0.9
                          :box '(:line-width 2 :color "#5c3d5c")
                          )
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


;; --------------
;; MATLAB
;; --------------
(add-hook! matlab-mode
           (linum-mode)
           (matlab-cedet-setup)
           )

;; --------------
;; song book mode
;; --------------
(load! "songbook")
(setq auto-mode-alist
(append '(("\\.sg$" . songbook-mode)
) auto-mode-alist))
(autoload 'songbook-mode "songbook" "Major-mode for Patacrep songbook's songs" t)

;; --------------
;; Wiki Moin Moin
;; --------------
(load! "moinmoin-mode")

;; ;
;;  Disable arrow keys !
;; (define-key evil-insert-state-map [left] 'undefined)
;; (define-key evil-insert-state-map [right] 'undefined)
;; (define-key evil-insert-state-map [up] 'undefined)
;; (define-key evil-insert-state-map [down] 'undefined)

;; (define-key evil-motion-state-map [left] 'undefined)
;; (define-key evil-motion-state-map [right] 'undefined)
;; (define-key evil-motion-state-map [up] 'undefined)
;; (define-key evil-motion-state-map [down] 'undefined)
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))
;; (global-unset-key (kbd "<C-left>"))
;; (global-unset-key (kbd "<C-right>"))
;; (global-unset-key (kbd "<C-up>"))
;; (global-unset-key (kbd "<C-down>"))
;; (global-unset-key (kbd "<M-left>"))
;; (global-unset-key (kbd "<M-right>"))
;; (global-unset-key (kbd "<M-up>"))
;; (global-unset-key (kbd "<M-down>"))

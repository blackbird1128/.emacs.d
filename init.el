(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))
(straight-use-package 'org)

(use-package no-littering
  :straight t
  :init
  (require 'no-littering))

(use-package emacs
  :demand
  :bind (( "C-x C-b" . switch-to-buffer))
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  :config
  (set-face-attribute 'default nil :font "IosevkaNerdFontMono" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "IosevkaNerdFontMono" :height 120)
  (set-face-attribute 'variable-pitch nil :font "IosevkaNerdFontPropo" :height 120 :weight 'regular)
  (set-face-attribute  'fixed-pitch-serif nil  :font "IosevkaNerdFontMono" :height 110 :weight 'light :weight 'bold)
  
  (defalias 'yes-or-no-p 'y-or-n-p)
  
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (setq delete-auto-save-files t)
  
  (set-fringe-mode 10)
  (setq visual-fill-column-width 80)
  (setq visible-bell t)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq sentence-end-double-space nil)
  
  (dolist (mode '(org-mode-hook
	          term-mode-hook
		  shell-mode-hook
		  eshell-mode-hook
		pdf-view-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))


(defun remove-elc-when-visit ()
  "When visit, remove <filename>.elc"
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-when-visit)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (scheme-mode . rainbow-delimiters-mode)
	 ))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled)
  (load-theme 'doom-gruvbox t ))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  (marginalia-mode))

(use-package vertico
  :straight t
  ;  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ;("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)


;(add-to-list 'auto-mode-alist '("\\.rktl\\'" . scheme-mode))

(use-package embark
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

;(use-package geiser-racket)

(defun initialize-recentf ()
  (interactive)
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))
(initialize-recentf)


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package corfu
   :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo))
  :config
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
	corfu-cycle t
	corfu-preselect 'directory
	corfu-quit-no-match t
        corfu-auto-delay  0
        corfu-auto-prefix 1)

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :ensure nil
  :config
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay 0.5))

(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("M-j" . avy-goto-char-timer)))

;; languagetool:

(use-package languagetool
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
	languagetool-console-command "~/languageTool/languagetool-commandline.jar"
	languagetool-server-command "~/languageTool/languagetool-server.jar")
  :commands (languagetoolkcheck
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :bind
  (("C-c d" . languagetool-correct-at-point)))

(use-package jinx
  :defer 2
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config

  (setq pdf-annot-color-history '("red" "yellow" "blue" "green"))
  (pdf-loader-install)
  (pdf-tools-install))

(use-package visual-fill-column
  :hook (LaTeX-mode . visual-fill-column-mode))

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  (setq +latex-viewers '(pdf-tools))
  (setq TeX-command-extra-options "--shell-escape")
  (add-hook 'pdf-view-mode-hook 'windmove-display-right)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq lsp-tex-server 'digestiff)
              (put 'LaTeX-mode 'eglot-language-id "latex")
              (eglot-ensure)))
  
  (add-hook 'LaTeX-mode-hook #'eglot-ensure)
  (setq TeX-source-correlate-method "synctext")
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode 1)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package citar
  :straight t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

;; Yasnippet settings
(use-package yasnippet
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))
  (setq yas-triggers-in-field t)
  
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field)
	 (LaTeX-mode . turn-on-cdlatex))
  :bind (:map cdlatex-mode-map  ("<tab>" . cdlatex-tab))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
		("<tab>" . yas-next-field-or-cdlatex)
		("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))
    
    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; Org settings


(use-package org-roam
  :straight t
  :defer 2
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/life_db"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup))

(defun efs/org-mode-setup ()
  (setq visual-fill-column-width 140)
  (setq visual-fill-column-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 0)
  (display-line-numbers-mode -1)
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (setq header-line-format " ")
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t))

(use-package org
  :hook
  (org-mode . efs/org-mode-setup)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c t" . org-set-tags-command)
	 ("C-c b" . org-switchb))
  :config
  (setq org-ellipsis "▾")
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-timeblock-files org-agenda-files)
  ; (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-log-done nil)
  (setq org-log-into-drawer nil)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-time-grid
	'((daily today require-timed)
	  (800 1000 1200 1400 1600 1800 2000)
	  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"◀── now ─────────────────────────────────────────────────")


  (setq org-todo-keywords
	'((sequence "NEXT(n)" "PROJ(p)"  "WAITING(w)" "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "WISH(i)" "|" "REALIZED(w)")
	  (sequence "TO_READ(r)" "STARTED(s)"   "|" "FINISHED(f)")))
  
  (setq  org-agenda-prefix-format
	 '((agenda . " %i %-12:c %?-12t% s")
	   (todo . " %i %-12:c")
	   (tags . " %i %-12:c")
	   (search . " %i %-12:c")))
  
  (setq org-tag-alist
      '((:startgroup)
	("@home" . ?H)
	("@college" . ?c)
	("@work" . ?w)
					; Put mutually exclusive tags here
	(:endgroup)
	("agenda" . ?a)
	("note" . ?n)
	("idea" . ?i)))
  
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("n" "Next Tasks"
	   ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))
	  ("p" "Painpoints" todo "TODO" ((org-agenda-files '( "~/org/painpoint.org"))))
	  ("c" "College Tasks" tags-todo "+@college")
          ("w" "Wishes" todo "WISH" ((org-agenda-prefix-format "")))))
  
  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+headline "~/org/tasks.org" "Inbox")
	   "* TODO %?\n  %u" )
	  
	  ("tp" "Painpoint" entry (file+headline "~/org/painpoint.org"  "Todo")
	   "* TODO %?\n")

	  ("i" "Ideas" entry (file+headline, "~/org/ideas.org" "Ideas")
	   "* %?\n")
	  
	  ("b" "Inbox" entry (file+headline, "~/org/inbox.org" "Inbox")
	   "* %?\n")
	  
	  ("a" "Ask question" plain (file+headline, "~/org/questions.org" "Questions")
	   "**** %?\n")      
	  
	  ("l" "List of items")
	  ("lb" "To buy" entry (file+olp, "~/org/buy_list.org" "Buy list")
	   "*** %?\n")
	  
	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n*** %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :empty-lines 1)
	  
	  ("m" "Metrics Capture")
	  ("mm" "Mood" table-line (file+headline "~/org/metrics.org" "Mood")
	   "| %U | %^{Mood rating (1-10)|1|2|3|4|5|6|7|8|9|10} | %^{Notes} |" :kill-buffer t)))
  
  
  (add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2)))
  ;;
  ;; -------- ORG BABEL ------------
  
  (setq org-babel-python-command "python3")
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (shell . t)))
  
  (setq org-confirm-babel-evaluate nil)

  (defun my/capture-inbox()
    (interactive)
    (org-capture nil "b"))
  
  (global-set-key "\C-ci" 'my/capture-inbox)
  (global-set-key "\C-cw" 'dictionary-lookup-definition)
  ;; -------------------
  
  )
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;(use-package calfw
;  :commands (cfw:open-org-calendar)
;  :config
;  (require 'calfw)
;  (use-package calfw-org))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.50)
                  (org-level-2 . 1.30)
                  (org-level-3 . 1.10)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
  (set-face-attribute (car face) nil :font "IosevkaNerdFontPropo" :weight 'regular :height (cdr face))))

(use-package org-modern
  :mode ("\\.org\\'" . org-mode)
  :config (setq org-hide-emphasis-markers t)
  :hook (org-mode . global-org-modern-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; -------------------- Org config end --------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
 '(org-timeblock-scale-options '(6 . 22))
 '(warning-suppress-log-types
   '((server)
     (pdf-view)
     (pdf-view)
     (yasnippet backquote-change)))
 '(warning-suppress-types '((pdf-view) (pdf-view) (yasnippet backquote-change))))

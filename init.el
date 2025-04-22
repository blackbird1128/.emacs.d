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

(setq auth-sources
    '((:source "~/.authinfo")))

(setq native-comp-async-report-warnings-errors nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defun normalize-to-filename (input)
  "Normalize a string INPUT into a filename-friendly format."
  (let* ((normalized (downcase input)) ; Step 1: Convert to lowercase
         (normalized (replace-regexp-in-string "\n" " " normalized)) ; Step 2: Replace newlines with spaces
         (normalized (replace-regexp-in-string "[^[:alnum:][:space:]_]" "" normalized)) ; Step 3: Remove special characters
         (normalized (replace-regexp-in-string "[[:space:]]+" "_" normalized)) ; Step 4: Replace spaces with underscores
         (normalized (replace-regexp-in-string "^_+" "" normalized)) ; Step 5: Remove leading underscores
         (normalized (replace-regexp-in-string "_+$" "" normalized))) ; Step 6: Remove trailing underscores
    normalized))

(defun create-file-with-normalized-filename ()
  "Normalize a string or region into a filename and create a new file."
  (interactive)
  (let* ((input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Enter string to normalize: ")))
         (filename (normalize-to-filename input)))
    (let ((filepath (read-file-name "Create file at directory: " nil nil nil filename)))
      (unless (file-exists-p filepath)
        (write-region "" nil filepath)
        (message "File created: %s" filepath))
      (find-file filepath))))

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
  (set-face-attribute 'default nil :font "IosevkaNerdFontMono" :height 155)
  (set-face-attribute 'fixed-pitch nil :font "IosevkaNerdFontMono" :height 155)
  (set-face-attribute 'variable-pitch nil :font "IosevkaNerdFontPropo" :height 155 :weight 'regular)
  (set-face-attribute  'fixed-pitch-serif nil  :font "IosevkaNerdFontMono" :height 150 :weight 'light :weight 'bold)

  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq dired-dwim-target t)

  (setq isearch-allow-scroll t       ; scrolling shouldn't cancel search
      isearch-lazy-count t         ; display count of search results in minibuffer
      )
  
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (setq delete-auto-save-files t)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (set-fringe-mode 10)
  (setq visual-fill-column-width 80)
  (setq visible-bell t)
  (column-number-mode t)
  (setq sentence-end-double-space nil)

  (setq grep-command
	"rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)")
  (setq compilation-scroll-output t)
  (setq compilation-max-output-line-length nil)
  (setopt dictionary-server "dict.org")
  (savehist-mode 1)
  
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))

(defun remove-elc-when-visit ()
  "When visit, remove <filename>.elc"
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-when-visit)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
			

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(use-package try)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (scheme-mode . rainbow-delimiters-mode) ))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t ))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(basic orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
				   (command (styles flex )))))

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-man)
	 ("C-c s" . consult-ripgrep)
         ;("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

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

(use-package wgrep)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

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
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
	
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package cape
  :straight t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-file))


(defun my-makefile-completion-setup ()
  (setq-local completion-at-point-functions
              (append (remove 'makefile-completions-at-point completion-at-point-functions)
                      '(cape-file tags-completion-at-point-function makefile-completions-at-point))))

(defun org-completion-setup ()
  (setq-local completion-at-point-functions
              (append completion-at-point-functions '( 'cape-emoji))))

(add-hook 'org-mode-hook #'org-completion-setup)
(add-hook 'makefile-mode-hook #'my-makefile-completion-setup)


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

(use-package jinx
  :defer 2
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package magit
  :bind
  (("C-c g" . magit))
  :straight t)

;;;;;;;;;;;;;;;;;;;;; coq-setup ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package proof-general
  :straight t
  :config
  (setq
	proof-three-window-enable t
	proof-splash-enable nil
	proof-next-command-insert-space nil
	proof-electric-terminator-enable nil
	PA-one-command-per-line nil))

(custom-set-faces
 '(proof-locked-face ((t (:background "#3c3836")))))


(use-package company-coq
  :straight t
  :hook (coq-mode . company-coq-mode))

;;;;;;;;;;;;;;;;;; ocaml setup ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

(opam-env)
(add-to-list 'exec-path (concat (getenv "OPAM_SWITCH_PREFIX") "/bin"))


(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode))
  :hook (tuareg-mode . display-line-numbers-mode)
	  )

(defun remove-highlight () (setq mouse-highlight nil))

(use-package eglot
  :init
  (add-hook 'eglot-managed-mode-hook 'remove-highlight)
  (add-hook 'eglot-managed-mode-hook 'display-line-numbers-mode)
  :bind
  (:map eglot-keymap
   ("r" . eglot-rename)                ;; Rename
   ("a" . eglot-code-actions)         ;; Code actions
   ("f" . eglot-format)               ;; Format buffer
   ("h" . eldoc-doc-buffer)           ;; Show documentation
   ("d" . eglot-find-declaration)     ;; Go to declaration
   ("i" . eglot-find-implementation)  ;; Go to implementation
   ("t" . eglot-find-type-definition)) ;; Go to type definition
  :config
  (define-prefix-command 'eglot-keymap)    ;; Define the prefix keymap
  (global-set-key (kbd "C-c l") 'eglot-keymap) ;; Bind C-c l to the prefix keymap
  :hook ((tuareg-mode .  eglot-ensure ) (python-mode . eglot-ensure))
  :commands (eglot))

(use-package opam-switch-mode
  :ensure t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))

(use-package ocamlformat
  :straight t
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))

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
  (org-mode . citar-capf-setup)
  :config
  (setq citar-bibliography '("~/org/phd/doctorat_aj.bib")))

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
;; Productivity stuff

(use-package hammy
  :straight t
  :config
  ;; We name the timer with the Unicode TOMATO character, and propertize
;; it with a tomato-colored face.
(hammy-define (propertize "🍅" 'face '(:foreground "tomato"))
  :documentation "The classic pomodoro timer."
  :intervals
  (list
   (interval :name "Work"
             :duration "25 minutes"
             :before (do (announce "Starting work time.")
                         (notify "Starting work time."))
             :advance (do (announce "Break time!")
                          (notify "Break time!")))
   (interval :name "Break"
             :duration (do (if (and (not (zerop cycles))
                                    (zerop (mod cycles 3)))
                               ;; If a multiple of three cycles have
                               ;; elapsed, the fourth work period was
                               ;; just completed, so take a longer break.
                               "30 minutes"
                             "5 minutes"))
             :before (do (announce "Starting break time.")
                         (notify "Starting break time."))
             :advance (do (announce "Break time is over!")
                          (notify "Break time is over!")))))
  :defer 3)

(use-package gptel
  :defer 2
  :straight t)

;; Org settings

(use-package org
   :bind (
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c t" . org-set-tags-command)
	 ("C-c b" . org-switchb))
  :config
  (setq visual-fill-column-width 140)
  (setq visual-fill-column-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 0)
  (display-line-numbers-mode -1)
  (setq left-margin-width 2
      right-margin-width 2
      header-line-format " "
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "▾"
      org-agenda-files (directory-files-recursively "~/org/" "\\.org$")
      org-timeblock-files org-agenda-files
      org-agenda-skip-unavailable-files t
      org-agenda-mouse-1-follows-link t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-log-done nil
      org-log-into-drawer nil
      line-spacing 0.2)
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
  (global-set-key "\C-cw" 'dictionary-lookup-definition))

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
  (setq org-modern-star 'replace)
  
  :hook (org-mode . global-org-modern-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; -------------------- Org config end --------------------

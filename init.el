;;; init.el -*- lexical-binding: t -*-

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(unless package-archive-contents
  (package-refresh-contents))

(use-package compile-angel
  :ensure t
  :demand t
  :config
  (setq compile-angel-verbose nil)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/custom.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(setq auth-sources
      '((:source "~/.authinfo")))

(defun my/get-api-key (host user)
  "Get API key from .authinfo for HOST and USER."
  (let ((entry (car (auth-source-search :host host :user user :require '(:secret)))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(setq native-comp-async-report-warnings-errors nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'transient)

(use-package no-littering
  :vc (:url "https://github.com/emacscollective/no-littering"
	    :rev :newest)
  :init
  (require 'no-littering))

(use-package exec-path-from-shell
  :vc (:url "https://github.com/purcell/exec-path-from-shell"
	    :rev :newest)
  :config
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "INFOPATH")))

(use-package emacs
  :demand
  :bind (("C-x C-b" . switch-to-buffer))
  :init
  (setq auto-mode-alist (remove (rassoc 'verilog-mode auto-mode-alist) auto-mode-alist))
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq tab-always-indent 'complete)
  :config
  (set-face-attribute 'default nil :font "IosevkaNerdFontMono" :height 155)
  (set-face-attribute 'fixed-pitch nil :font "IosevkaNerdFontMono" :height 155)
  (set-face-attribute 'variable-pitch nil :font "IosevkaNerdFontPropo" :height 155 :weight 'regular)
  (set-face-attribute  'fixed-pitch-serif nil  :font "IosevkaNerdFontMono" :height 150 :weight 'light :weight 'bold)

  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (blink-cursor-mode 0)
  (column-number-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq dired-dwim-target t)
  
  (setq isearch-allow-scroll t ; scrolling shouldn't cancel search
        isearch-lazy-count t   ; display count of search results in minibuffer
	)
  
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  
  (set-fringe-mode 10)
  (setq delete-auto-save-files t
        visual-fill-column-width 80
        visible-bell t
        sentence-end-double-space nil
        grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
        compilation-scroll-output t
        compilation-max-output-line-length nil)

  (setopt dictionary-server "dict.org"
          text-mode-ispell-word-completion nil)
  (savehist-mode 1)
  
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files)
  )

(defun my/chicken-icon (_1 _2 _3)
  (propertize "🐔"
	      'face '(:family "IosevkaNerdFontMono" :height 1.0)))

(use-package nerd-icons
  :config
  (setq nerd-icons-extension-icon-alist
	(assoc-delete-all "v" nerd-icons-extension-icon-alist))
  (setq nerd-icons-extension-icon-alist
	(append
	 `(("v"
            ,#'my/chicken-icon
            "🐔" :face nerd-icons-yellow )) ;; This face is ignored
	 nerd-icons-extension-icon-alist)))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq mode-line-right-align-edge 'right-fringe))

(use-package eat
  :hook ((eshell-mode . eat-eshell-mode)))

(use-package try)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (scheme-mode . rainbow-delimiters-mode) ))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t ))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package vertico
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
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c s" . consult-ripgrep)
	 ("C-c f" . consult-flymake)
	 ("C-c m" . consult-imenu)
					;("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer))      ;; orig. project-switch-to-buffer

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-;" . embark-act)) ;; pick some comfortable binding
    
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :defer 5)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

;; (use-package breadcrumb
;;   :hook
;;   (prog-mode . breadcrumb-local-mode)
;;   )

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
  :config
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
	corfu-cycle t
	corfu-preselect 'directory
	corfu-quit-no-match t
	corfu-on-exact-match nil
        corfu-auto-delay  0
        corfu-auto-prefix 2)

  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package cape
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

(add-hook 'makefile-mode-hook #'my-makefile-completion-setup)

(use-package corfu-popupinfo
  :ensure nil
  :config
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay 0.5))

(use-package yasnippet-snippets)

(use-package yasnippet
  :hook (tuareg-mode . yas-minor-mode)
  :config
  ( push (expand-file-name "~/.config/emacs/snippets/") yas-snippet-dirs )
  (yas-reload-all))

(use-package yasnippet-capf)

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
  (("C-c g" . magit)))

(use-package crux
  :bind
  (("C-c d" . crux-delete-file-and-buffer)))

(use-package flymake
  :ensure t
  :pin gnu
  :config
  (setq flymake-diagnostic-format-alist
        '((t . (origin code message))))) ; Fix messages not being completely displayed in Ocaml for example

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
	      (list (cape-capf-super
                     #'eglot-completion-at-point
		     #'cape-file
                     #'yasnippet-capf))))

(use-package eglot
  :init
  (add-hook 'eglot-managed-mode-hook 'remove-highlight)
  (add-hook 'eglot-managed-mode-hook 'display-line-numbers-mode)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  :bind
  (:map eglot-keymap
	("r" . eglot-rename)               ;; Rename
	("a" . eglot-code-actions)         ;; Code actions
	("f" . eglot-format)               ;; Format buffer
	("h" . eldoc-doc-buffer)           ;; Show documentation
	("d" . eglot-find-declaration)     ;; Go to declaration
	("i" . eglot-find-implementation)  ;; Go to implementation
	("t" . eglot-find-type-definition)) ;; Go to type definition
  :config
  (define-prefix-command 'eglot-keymap)    ;; Define the prefix keymap
  (global-set-key (kbd "C-c l") 'eglot-keymap) ;; Bind C-c l to the prefix keymap
  (add-to-list 'eglot-server-programs
	       '(why3-mode . ("why3find" "lsp" "--port" :autoport)))
  (setq eglot-headerline-breadcrumb-segments
	'(project file symbols))

  
  :hook ((tuareg-mode .  eglot-ensure ) (python-mode . eglot-ensure) (why3-mode . eglot-ensure))
  :commands (eglot))

;;;;;;;;;;;;;;;;;;;;; coq-setup ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package proof-general
  :defer 4
  :config
  (setq
   proof-three-window-enable t
   proof-splash-enable nil
   proof-next-command-insert-space nil
   proof-electric-terminator-enable nil
   PA-one-command-per-line nil))

 ;; (use-package rocq-mode
 ;;    :vc (:url "https://codeberg.org/jpoiret/rocq-mode.el.git"
 ;;         :rev :newest)
 ;;    :mode "\\.v\\'"
 ;;    :hook
 ;;    (rocq-mode . rocq-follow-viewport-mode)
 ;;    (rocq-mode . rocq-auto-goals-at-point-mode))

(custom-set-faces
 '(proof-locked-face ((t (:background "#3c3836"))))
 '(rocq-mode-last-goal-request ((t (:background "#3c3836"))))
 )

;; (use-package company-coq
;;   :hook (rocq-mode . company-coq-mode))

;;;;;;;;;;;;;;;;; why3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package why3
  :defer 3
  :load-path "lisp"
  :mode ("\\.mlw\\'" . why3-mode)
  :bind (:map why3-mode-map
	      ("C-c C-c" . compile)))

;;;;;;;;;;;;;;;;;; ocaml setup ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package direnv
  :config
  (direnv-mode))

(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode))
  :hook (tuareg-mode . display-line-numbers-mode))

(defun remove-highlight () (setq mouse-highlight nil))

(use-package opam-switch-mode
  :ensure t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))

(use-package ocamlformat
  :custom
  (ocamlformat-enable 'enable-outside-detected-project)
  :hook
  (before-save .  ocamlformat-before-save))

(use-package dune
  :mode ("\\`dune\\(?:-project\\)?\\'" . dune-mode))

;;;;;;;;;;;;;;;; python config ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package poetry
  :ensure t)

;;;;;;;;;;;;;; markdown config ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode)

;;;;;;;;;;;; yaml config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode)

;;;;; nix config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nix-mode
  :mode "\\.nix\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)

  :config
  (defun my/pdf-annot-highlight (color)
    "Highlight the active region in COLOR"
    (let ((edges (pdf-view-active-region t)))
      (unless edges
	(user-error "No active region to highlight"))
      (pdf-annot-add-highlight-markup-annotation
       edges color )))

  (transient-define-prefix my/pdf-highlight-transient ()
    "PDF Highlight Menu"
    [["Highlight Types"
      ("s" "Syntaxic errors (blue)"   (lambda () (interactive)
					(my/pdf-annot-highlight "CadetBlue1")))
      ("m" "Semantic errors (green)"  (lambda () (interactive)
					(my/pdf-annot-highlight "yellow green" )))
      ("l" "Loved part (pink)"        (lambda () (interactive)
					(my/pdf-annot-highlight "plum1" )))]])  
  (pdf-loader-install)
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map ("h" . my/pdf-highlight-transient)))

;;;;;;;;;;;;;;;;;;; Latex config ;;;;;;;;;;;;;;;;;;;;;

(use-package visual-fill-column
  :hook (LaTeX-mode . visual-fill-column-mode))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab))
  :config
  (with-eval-after-load 'yasnippet
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
            (goto-char minp)
            t))))

    (defun yas-next-field-or-cdlatex ()
      "Jump to the next Yas field correctly with cdlatex active."
      (interactive)
      (if (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))

    (define-key yas-keymap (kbd "<tab>") #'yas-next-field-or-cdlatex)
    (define-key yas-keymap (kbd "TAB") #'yas-next-field-or-cdlatex)
    (add-hook 'cdlatex-tab-hook #'yas-expand)
    (add-hook 'cdlatex-tab-hook #'cdlatex-in-yas-field)))

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . eglot-ensure )
	 (LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . TeX-fold-mode)
	 (LaTeX-mode . outline-minor-mode))
  
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
	      (setq lsp-tex-server 'digestif)
	      (put 'LaTeX-mode 'eglot-language-id "latex")
	      ))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  
  (setq +latex-viewers '(pdf-tools)
	TeX-command-extra-options "--shell-escape"
	TeX-auto-save t
	TeX-parse-self t
	reftex-plug-into-AUCTeX t
	default-TeX-master nil
	TeX-source-correlate-method "synctext"
	TeX-source-correlate-start-server t
	TeX-source-correlate-mode 1)

  (defun my/latexindent-region (beg end)
    "Run latexindent -l on the active region."
    (interactive "r")
    (unless (use-region-p)
      (user-error "No active region"))
    (save-excursion
      (shell-command-on-region
       beg end "latexindent -l" (current-buffer) t)))
  :bind ( "<backtab>" . my/latexindent-region)
  )

(use-package citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (setq citar-bibliography '("~/org/phd/doctorat_aj.bib")))

;;;;;;;;;;;;;;;; writing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productivity stuff

(use-package hammy
  :defer 3)

(use-package gptel
  :defer 5
  :config
  (gptel-make-gemini "Gemini" :key (my/get-api-key "generativelanguage.googleapis.com" "apikey") :stream t))

;;;;;;;;;;;; howm ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package howm-org
  :after org
  :ensure nil)   ;; ensures it's loaded early enough

(use-package howm
  :after howm-org
  :defer 3
  :hook ((org-mode . howm-mode))
  :init
  (setq howm-directory "~/org/"
        howm-create-id t
        howm-follow-theme t
        howm-view-use-grep t
        howm-view-grep-command "rg"
        howm-view-grep-option "-nH --no-heading --color never"
        howm-view-grep-fixed-option "-F")
  
  :config
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  (advice-add 'howm-list-all :after (lambda () (howm-view-sort-by-date t))))

;; Org settings

(defun my/org--font-exists-p (font)
  (and font (find-font (font-spec :name font))))

(defun my/org-apply-faces ()
  "Apply Org faces in a theme-safe way."
  (when (featurep 'doom-themes) ;; if you use doom-themes colors
    (let* ((variable-font (cond
                           ((my/org--font-exists-p "IosevkaNerdFontMono") "IosevkaNerdFontMono")
                           ((my/org--font-exists-p "Sans Serif") "Sans Serif")
                           (t nil)))
           (mono-font (cond
                       ((my/org--font-exists-p "IosevkaNerdFontMono") "IosevkaNerdFontMono")
                       ((my/org--font-exists-p "Iosevka") "Iosevka")
                       (t nil)))
           (headline '(:weight normal))
           (fg (doom-color 'fg))
           (yellow (doom-color 'yellow))
           (green (doom-color 'green))
           (violet (doom-color 'violet)))
      (custom-theme-set-faces
       'user
       ;; Base faces
       `(org-default ((t (:inherit default ,@(when variable-font `(:font ,variable-font))))))
       `(variable-pitch ((t (,@(when variable-font `(:font ,variable-font)) :height 1.01))))
       `(fixed-pitch ((t (,@(when mono-font `(:font ,mono-font)) :height 1.10))))

       ;; Headings
       `(org-level-8 ((t (,@headline ,@(when variable-font `(:font ,variable-font))))))
       `(org-level-7 ((t (,@headline ,@(when variable-font `(:font ,variable-font))))))
       `(org-level-6 ((t (,@headline ,@(when variable-font `(:font ,variable-font))))))
       `(org-level-5 ((t (,@headline ,@(when variable-font `(:font ,variable-font))))))
       `(org-level-4 ((t (,@headline ,@(when variable-font `(:font ,variable-font)) :height 1.05))))
       `(org-level-3 ((t (,@headline ,@(when variable-font `(:font ,variable-font)) :height 1.07 :foreground ,violet))))
       `(org-level-2 ((t (,@headline ,@(when variable-font `(:font ,variable-font)) :height 1.10 :foreground ,green))))
       `(org-level-1 ((t (,@headline ,@(when variable-font `(:font ,variable-font)) :height 1.20 :foreground ,yellow))))
       `(org-document-title ((t (,@headline ,@(when variable-font `(:font ,variable-font)) :height 1.20 :underline nil))))

       ;; Fixed-pitch areas
       `(org-table ((t (:inherit fixed-pitch :foreground ,fg))))
       `(org-checkbox ((t (:foreground ,fg))))
       `(org-block ((t (:inherit fixed-pitch :foreground unspecified))))
       `(org-code ((t (:inherit (shadow fixed-pitch)))))
       `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
       `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
       `(org-link ((t (:inherit fixed-pitch :weight bold :underline t))))))))

(defun my/org-agenda-files ()
  (append
   (directory-files-recursively (expand-file-name "~/org/agenda/") "\\.org\\'")
   (directory-files-recursively (expand-file-name "~/org/people/") "\\.org\\'")))

(defun my/org-buffer-setup ()
  (visual-line-mode 1)
  (display-line-numbers-mode -1)
  (setq-local line-spacing 0.25)
  (setq-local header-line-format " ")
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c t" . org-set-tags-command)
         ("C-c b" . org-switchb)
         ("C-c i" . my/capture-inbox))
  :hook (
	 (org-mode . my/org-buffer-setup)
         ;; Re-apply faces when org loads (and whenever theme changes below)
         (org-mode . my/org-apply-faces)
	 )
  :init
  ;; Theme-safe: re-apply after any theme load
  (add-hook 'after-load-theme-hook #'my/org-apply-faces)
  (setq org-hide-emphasis-markers t)

  :custom
  ;; display niceties
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-use-sub-superscripts "{}")

  ;; agenda behavior
  (org-agenda-skip-unavailable-files t)
  (org-agenda-mouse-1-follows-link t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)

  ;; logging
  (org-log-done nil)
  (org-log-into-drawer nil)

  :config
  (setq org-agenda-files (my/org-agenda-files))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (advice-add 'org-refile :after #'org-save-all-org-buffers)

  (setq org-todo-keywords
        '((sequence "NEXT(n)" "PROJ(p)" "WAITING(w)" "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
          (sequence "WISH(i)" "|" "REALIZED(w)")
          (sequence "TO_READ(r)" "STARTED(s)" "|" "FINISHED(f)")))

  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c %?-12t% s")
          (todo   . " %i %-12:c")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  (setq org-tag-alist
        '((:startgroup)
          ("@home" . ?H)
          ("@work" . ?w)
          (:endgroup)))

  (setq org-agenda-custom-commands
        '(("n" "Next Tasks"
           ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))
          ("p" "Painpoints"
           ((todo "TODO"
                  ((org-agenda-files '("~/org/painpoint.org"))
                   (org-agenda-overriding-header "Painpoints")))))
          ("w" "Wishes"
           ((todo "WISH"
                  ((org-agenda-prefix-format "")
                   (org-agenda-overriding-header "Wishes")))))))

  (setq org-capture-templates
        '(("t" "Tasks / Projects")
          ("tp" "Painpoint" entry (file+headline "~/org/painpoint.org" "Todo")
           "* TODO %?\n")

          ("i" "Ideas" entry (file+headline "~/org/ideas.org" "Ideas")
           "* %?\n")

          ("b" "Inbox" entry (file+headline "~/org/inbox.org" "Inbox")
           "* %?\n")

          ("l" "List of items")
          ("lb" "To buy" entry (file+olp "~/org/buy_list.org" "Buy list")
           "*** %?\n")))

  ;; -------- ORG BABEL ------------
  (setq org-babel-python-command "python3"
        org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (shell . t))))

(defun my/capture-inbox ()
  (interactive)
  (org-capture nil "b"))

;;;; ---------------- EXTRAS ----------------

(use-package org-fragtog
  :after org
  :init
  (setq org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :config
  (setq org-format-latex-options
        (let ((opts (copy-sequence org-format-latex-options)))
          (plist-put (plist-put (plist-put opts :scale 2)
                                :foreground 'auto)
                     :background 'auto))))


(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "◉○"))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t))

;; -------------------- Org config end --------------------

(use-package elfeed
  :defer 4
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  :bind (("C-c e" . elfeed))
  :config
  (setq elfeed-feeds
	'("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml")))

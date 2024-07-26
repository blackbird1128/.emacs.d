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

(set-face-attribute 'default nil :font "IosevkaNerdFontMono" :height 120)
(set-face-attribute 'fixed-pitch nil :font "IosevkaNerdFontMono" :height 120)
(set-face-attribute 'variable-pitch nil :font "IosevkaNerdFontPropo" :height 120 :weight 'regular)
(set-face-attribute  'fixed-pitch-serif nil  :font "IosevkaNerdFontMono" :height 110 :weight 'light :weight 'bold)

(defalias 'yes-or-no-p 'y-or-n-p)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun remove-elc-when-visit ()
  "When visit, remove <filename>.elc"
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-when-visit)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq delete-auto-save-files t)

(set-fringe-mode 10)
(setq visual-fill-column-width 80)
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
	        term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled)
  (load-theme 'doom-gruvbox t ))

(use-package ivy
  :bind (("C-c s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)))

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
  ;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
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

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

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
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :hook (emacs-lisp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

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

(use-package casual-calc
  :bind (:map calc-mode-map ("C-o" . #'casual-calc-tmenu)))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))


(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq +latex-viewers '(pdf-tools))
  (add-hook 'pdf-view-mode-hook 'windmove-display-right)
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
  (setq laTeX-default-options "--synctex=1 shell-escape")
  (setq TeX-command-extra-options "--shell-escape")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (server-start)
  (add-hook 'LaTeX-mode-hook #'eglot-ensure)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (setq TeX-source-correlate-method "synctext")
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode 1)

  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0) 
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

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
	 ("C-c a" . org-agena)
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
  (add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2))))


(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

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

(defun my/capture-inbox()
  (interactive)
  (org-capture nil "b"))

(global-set-key "\C-ci" 'my/capture-inbox)

(use-package calfw
  :commands (cfw:open-org-calendar)
  :config
  (require 'calfw)
  (use-package calfw-org))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.15)
                  (org-level-2 . 1.05)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
  (set-face-attribute (car face) nil :font "IosevkaNerdFontPropo" :weight 'regular :height (cdr face))))

(use-package org-modern
  :mode ("\\.org\\'" . org-mode)
  :config (setq org-hide-emphasis-markers t)
  :init (with-eval-after-load 'org (global-org-modern-mode))
  )

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; -------------------- Org config end --------------------

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

;; --------------------

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
 '(package-selected-packages
   '(jinx avy marginalia discover-my-major org-noter calfw calfw-org org-fragtog org-timeblock languagetool company-mode auctex cdlatex command-log-mode counsel doom-themes eglot helpful ivy-rich org-appear org-modern pdf-tools rainbow-delimiters visual-fill-column which-key yasnippet))
 '(warning-suppress-log-types
   '((server)
     (pdf-view)
     (pdf-view)
     (yasnippet backquote-change)))
 '(warning-suppress-types '((pdf-view) (pdf-view) (yasnippet backquote-change))))

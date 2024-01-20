
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(set-face-attribute 'default nil :font "IosevkaNerdFontMono" :height 115) ; Change to a font we actually have

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "IosevkaNerdFontMono" :height 115)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "IosevkaNerdFontPropo" :height 115 :weight 'regular)


(defalias 'yes-or-no-p 'y-or-n-p)


(defun remove-elc-when-visit ()
  "When visit, remove <filename>.elc"
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-when-visit)



(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

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

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t ))

(use-package ivy
  :diminish
  :bind (("C-c s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1)
)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))


(use-package ivy-rich
  :diminish
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-keys))

;; Latex Settings
;; (server-start)

(setq ispell-dictionnary "french-lrg")
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--sug-mode=slow"))
(setq-default ispell-program-name "aspell")


(setq TeX-source-correlate-method "synctext")
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-mode 1)
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq +latex-viewers '(pdf-tools))
)
(add-hook 'pdf-view-mode-hook 'windmove-display-right)


(use-package visual-fill-column
  :hook (LaTeX-mode . visual-fill-column-mode))

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
         ("C-S-e" . latex-math-from-calc))
  :config
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

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab)))

;; Yasnippet settings
(use-package yasnippet
  :ensure t
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
         (cdlatex-tab . cdlatex-in-yas-field))
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

(pdf-loader-install)
(setq laTeX-default-options "--synctex=1 shell-escape")
(setq TeX-command-extra-options "--shell-escape")



;; Org settings 


(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 140
  (setq visual-fill-column-mode 1)))


(defun  efs/org-mode-setup ()
  (variable-pitch-mode 1)
  (visual-line-mode 0)
  (display-line-numbers-mode -1)
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (setq header-line-format " ")
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
 )

(use-package org
  :hook
    (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis "▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-todo-keywords
      '((sequence "NEXT(n)" "PROJ(p)"  "WAITING(w)" "TODO(t)" "|" "DONE(d!)" "CANCELED(c!)")
	(sequence " WISH(i)" "|" "REALIZED(r)")
	))

(setq org-tag-alist
      '((:startgroup)
	   ; Put mutually exclusive tags here
	(:endgroup)
	("@home" . ?H)
	("@college" . ?c)
	("agenda" . ?a)
	("note" . ?n)
	("idea" . ?i))
)


;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(	
	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))

	("p" "Painpoints" todo "TODO" ((org-agenda-files '( "~/org/painpoint.org"))))
	("c" "College Tasks" tags-todo "+@college")
        ("w" "Wishes" todo "WISH" ((org-agenda-prefix-format "")
				   ))
	;; Low-effort next actions
	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
       "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("tp" "Painpoint" entry (file+olp "~/org/painpoint.org" "Painpoints")
       "*** TODO %?\n")


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





(use-package org-modern
    :mode ("\\.org\\'" . org-mode)
)

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config (setq org-hide-emphasis-markers t))



;; -------------------- Org config end --------------------

;; -------------------- Org style -------------------- 


(with-eval-after-load 'org-faces
(dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
  
(set-face-attribute (car face) nil :font "IosevkaNerdFontPropo" :weight 'regular :height (cdr face))))

(setq org-refile-targets
      '(("~/org/archive.org" :maxlevel . 1)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)


(setq  org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "<── now ────────────────────────────")


(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2)))


(with-eval-after-load 'org (global-org-modern-mode))

;;
;; -------- ORG BABEL ------------

(setq org-babel-python-command "python3")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (dot . t)
   (shell . t))
)

(setq org-confirm-babel-evaluate nil)

;; --------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "PDF Tools") (output-html "xdg-open")))
 '(org-agenda-files
   '("~/org/painpoint.org" "/home/blackbird1128/org/bucket_list.org"
     "/home/blackbird1128/org/tasks.org"
     "/home/blackbird1128/.emacs.d/hello.org"))
 '(package-selected-packages
   '(auctex cdlatex command-log-mode counsel doom-themes eglot helpful
	    ivy-rich org-appear org-modern pdf-tools
	    rainbow-delimiters visual-fill-column which-key yasnippet))
 '(warning-suppress-log-types
   '((server) (pdf-view) (pdf-view) (yasnippet backquote-change)))
 '(warning-suppress-types '((pdf-view) (pdf-view) (yasnippet backquote-change))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq inhibit-startup-message t)
(setq byte-compile-warnings nil)
(setq byte-compile-verbose nil)


(setq idle-update-delay 1.0)


(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)



(setq normal-gc-cons-threshold (* 64 1024 1024))
(setq init-gc-cons-threshold (* 512  1024 1024))
  
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
   (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

(setq load-prefer-newer noninteractive)


(put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
(setq-default mode-line-format nil)
(dolist (buf (buffer-list))
  (with-current-buffer buf (setq mode-line-format nil)))

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)


(unless noninteractive
  ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
  ;;   larger than the system font) appears to impact startup time
  ;;   dramatically. The larger the delta in font size, the greater the delay.
  ;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
  ;;   depending on font size.
  (setq frame-inhibit-implied-resize t)
  
  ;; PERF,UX: Site files tend to use `load-file', which emits "Loading X..."
  ;;   messages in the echo area. Writing to the echo-area triggers a
  ;;   redisplay, which can be expensive during startup. This may also cause
  ;;   an flash of white when creating the first frame.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))
  ;; COMPAT: But undo our `load-file' advice later, as to limit the scope of
  ;;   any edge cases it could induce.
  (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
    (advice-remove #'load-file #'load-file@silence))

  (advice-add #'display-startup-echo-area-message :override #'ignore)
  ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
  ;;   with `inhibit-startup-screen', but it would still initialize anyway.
  ;;   This involves some file IO and/or bitmap work (depending on the frame
  ;;   type) that we can no-op for a free 50-100ms boost in startup time.
  (advice-add #'display-startup-screen :override #'ignore)
  

 )


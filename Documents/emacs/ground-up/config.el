;; Functions
(defun kill-all-buffers ()
  "Kill all buffers."
  (delete-other-windows)
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun mns/color-hex-to-float (color)
  "Converts a 3 component 8-bit RGB color to 32-bit normalized floats"
  (interactive "X8-bit color: ")
  (let ((rr (/ (logand (lsh color -16) #xff) (float #xff)))
	(gg (/ (logand (lsh color -8) #xff) (float #xff)))
	(bb (/ (logand (lsh color -0) #xff) (float #xff))))
    (message "%f, %f, %f" rr gg bb)))

(defun mns/set-default-variable-pitch-font ()
  (set-face-attribute 'variable-pitch nil :font "Input Sans Narrow" :height 165 :weight 'normal))

;; Easily toggle between a readable font and a basic font
(setq mns/is-readable-font nil)
(defun mns/toggle-readable-font ()
  "Toggles between the specified default variable pitch font and a more readable font"
  (interactive)
  (if mns/is-readable-font
      (progn
        (mns/set-default-variable-pitch-font)
	(message "Variable pitch font toggled to the default one")
	(setq mns/is-readable-font nil))
    (set-face-attribute 'variable-pitch nil :font "OpenDyslexicAlta Nerd Font" :height 170)
    (message "Variable pitch font toggled to the readable one")
    (setq mns/is-readable-font t))
  )

;; Fix copy-paste in Wayland
;; I only use Wayland based WMs/DEs so not a problem
;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process)) nil
;;     (shell-command-to-string "wl-paste -t text -n 2>/dev/null")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)
;; (setq confirm-kill-processes nil)

;; When running in daemon mode, the font is not set since there is no frame
(defun mns/frame-init ()
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 160)
  (mns/set-default-variable-pitch-font)
  ;; To display unicodes in the range
  (set-fontset-font t '(#x11000 . #x1107f) (font-spec :family "Noto Sans Brahmi"))
  (setq doom-modeline-icon t)
  (toggle-scroll-bar -1))
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(mns/frame-init)))
  (mns/frame-init))

;; Set tab or spaces
(defun mns/setup-indent (n)
  ;; java/c/c++
  (setq-default c-basic-offset n)
  ;; rust
  (setq-default rust-indent-offset n)
  ;; web development
  (setq-default coffee-tab-width n) ; coffeescript
  (setq-default javascript-indent-level n) ; javascript-mode
  (setq-default js-indent-level n) ; js-mode
  (setq-default js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-default css-indent-offset n) ; css-mode
  )
(defun mns/personal-code-style ()
  (interactive)
  ;; use space instead of tab
  (setq-default indent-tabs-mode nil)
  (mns/setup-indent 4))
(mns/personal-code-style)

;; The menubar is more or less useless in terminal mode
(defun mns/contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines 
                       (if (display-graphic-p frame)
                           1 0)))
;; (add-hook 'after-make-frame-functions 'mns/contextual-menubar)

;; Variables
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-screen t)
;; (setq initial-buffer-choice t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Modes
(global-tree-sitter-mode 1)

;; Hooks
(add-hook 'c++-mode-hook (lambda () (hs-minor-mode) (hide-ifdef-mode)))

;; Modeline
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                " "
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                mode-line-position
                " "
                ;; mode-line-modes
                mode-name
                " "
                mode-line-misc-info
                mode-line-end-spaces))

;; (defun simple-mode-line-render (left right)
;;   "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively."
;;   (let ((available-width
;;          (- (window-total-width)
;;             (+ (length (format-mode-line left))
;;                (length (format-mode-line right))))))
;;     (append left
;;             (list (format (format "%%%ds" available-width) ""))
;;             right)))

;; (setq-default
;;  mode-line-format
;;  '((:eval
;;     (simple-mode-line-render
;;      ;; Left.
;;      '("%e "
;;        mode-line-buffer-identification
;;        " "
;;        mode-line-mule-info
;;        " %l : %c "
;;        evil-mode-line-tag
;;        "[%*] ")
;;      ;; Right.
;;      '("%p"
;;        mode-line-frame-identification
;;        mode-name
;;        " "
;;        mode-line-misc-info)))))

;; Theme
;; (load-theme 'gruvbox t) ;; this function doesn't disable other themes
(require 'counsel)

(setq modus-themes-bold-constructs t
      modus-themes-intense-hl-line nil
      modus-themes-syntax nil
      modus-themes-mode-line nil
      modus-themes-completions nil
      modus-themes-variable-pitch-ui t)

;; (counsel-load-theme-action "doom-laserwave")
;; (counsel-load-theme-action "modus-operandi")
;; (counsel-load-theme-action "modus-vivendi")
;; (counsel-load-theme-action "gruvbox-dark-soft")
(counsel-load-theme-action "doom-monokai-classic")

;; Ability to set image dimensions from within the org document
(setq org-image-actual-width nil)

;; Optimizations, most are stolen from DOOM Emacs
(global-so-long-mode 1)
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

;; Don't render cursors or regions in in non-focused windows
(setq-default cursor-in-non-selected-windows nil
	      highlight-nonselected-windows nil)

;; Rapid scrolling over unfontified regions
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be terribly expensive
(setq frame-inhibit-implied-resize t)

;; Wait until idle time to gc
(gcmh-mode 1)
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 256 1024 1024) ;; 256 MB
      gcmh-verbose nil)

;; Update the UI less often
(setq idle-update-delay 1.0)

;; Font compacting is expensive. Increases memory usage
(setq inhibit-compacting-font-caches t)

;; Inhibit fontification while receiving input, helps in scrolling performace
(setq redisplay-skip-fontification-on-input t)

;; Options
;; Relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute nil)

;; Highlight the current line
(global-hl-line-mode 1)

;; Smooth scroll
;; (setq scroll-step 1
;;       scroll-conservatively 10000)

(setq-default truncate-lines nil) ;; Line folding
(tab-bar-mode -1) ;; Tabs are the one of the most useful features
(show-paren-mode 1) ;; Highlight matched bracket
(electric-pair-mode 1) ;; Automatically pair parens

;; Annoyances
(setq-default cursor-type '(bar . 2))
(setq server-client-instructions nil)
(blink-cursor-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)
;; (global-flycheck-mode -1)

;; Sane scrolling so it doesn't feel like emacs is having manic disorders
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta) . nil) ((control) . text-scale)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-tilt-scroll nil)

;; Make mouse work in the terminal
(xterm-mouse-mode 1)

;; All backup files are stored in one direcctory
(let ((backup-dir "~/.cache/my_emacs/backup/")
      (auto-saves-dir "~/.cache/my_emacs/save/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq delete-old-versions t  ; Clean up the backups
      version-control nil    ; Use version numbers on backups
      kept-new-versions 2    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

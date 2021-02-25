;; straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Escape immediately quits
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;; Handy function
(defun kill-all-buffers ()
  (delete-other-windows)
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun color-hex-to-float (color)
  "Converts a 3 component 8-bit RGB color to 32-bit normalized floats"
  (interactive "X8-bit color: ")
  (let ((rr (/ (logand (lsh color -16) #xff) (float #xff)))
	(gg (/ (logand (lsh color -8) #xff) (float #xff)))
	(bb (/ (logand (lsh color -0) #xff) (float #xff))))
    (message "%f, %f, %f" rr gg bb)))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package gcmh)
(use-package all-the-icons)
(use-package magit)

(use-package lsp-mode
  :hook
  (c++-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (objc-mode . lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands (lsp lsp-deferred))
;; (use-package lsp-ui
;;   :config
;;   (lsp-ui-doc-enable nil))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil))))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("<f2> j" . counsel-set-variable))
(use-package swiper
  :bind
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward))
(use-package ivy
  :bind
  ("C-x b" . ivy-switch-buffer)
  ("C-c v" . ivy-push-view)
  ("C-c V" . ivy-pop-view)
  :config (ivy-mode 1))
(use-package lsp-ivy)

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  company-idle-delay 0.300)
(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck
  :init (global-flycheck-mode))

;; (use-package evil
;;   :init
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))
;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))
;; (use-package evil-nerd-commenter)
;; (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-lines)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(use-package which-key
  :config
  (which-key-mode))

;; (use-package nov
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; (use-package treemacs)
;; (use-package treemacs-all-the-icons)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

;; recently used commands
(use-package amx)

(use-package doom-themes
  :config
  (doom-themes-org-config))
(use-package kaolin-themes)
(use-package gruvbox-theme)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(load-theme 'doom-gruvbox t)

(setq is-readable-font nil)
(defun toggle-readable-font ()
  "Toggles between the specified default variable pitch font and a more readable font"
  (interactive)
  (if is-readable-font
      (progn
	(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 150)
	(message "Variable pitch font toggled to the default one")
	(setq is-readable-font nil))
    (set-face-attribute 'variable-pitch nil :font "OpenDyslexicAlta Nerd Font" :height 150)
    (message "Variable pitch font toggled to the readable one")
    (setq is-readable-font t))
  )

;; I only use Wayland based wm/de
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process)) nil
    (shell-command-to-string "wl-paste -t text -n 2>/dev/null")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)
(setq confirm-kill-processes nil)

;; When running in daemon mode, the font is not set since there is no frame
(defun my-frame-init ()
  (set-face-attribute 'default nil :font "Input Mono Narrow" :height 165 :weight 'normal)
  (set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 150)
  (setq doom-modeline-icon t)
  (toggle-scroll-bar -1))
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(my-frame-init)))
  (my-frame-init))

;; Set tab or spaces
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; rust
  (setq-local rust-indent-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )
(defun my-personal-code-style ()
  (interactive)
  ;; use space instead of tab
  (setq-default indent-tabs-mode nil)
  ;; indent 2 spaces width
  (my-setup-indent 2))
(my-personal-code-style)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice t)
(setq initial-scratch-message nil)

;; Org mode
(setq org-image-actual-width nil)

;; -- Optimizations, much of it is stolen from DOOM Emacs --
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

;; Remove irrelevent command line options
(setq command-line-ns-option-alist nil)

;; Relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Smooth scroll
;; (setq scroll-step 1
;;       scroll-conservatively 10000)

;; Disable line folding
(set-default 'truncate-lines nil)

;; Match the other bracket pair
(show-paren-mode 1)

;; Automatically pair parens
(electric-pair-mode 1)

;; Disable some annoyances
(blink-cursor-mode -1)
(tool-bar-mode -1)
(global-flycheck-mode -1)

;; The menubar is more or less useless in terminal mode
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines 
                       (if (display-graphic-p frame)
                           1 0)))
(add-hook 'after-make-frame-functions 'contextual-menubar)

;; So that scrolling doesn't feel like emacs is having manic disorders
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
(setq mouse-wheel-progressive-speed nil)

;; Mouse mode in the terminal
(xterm-mouse-mode 1)

;; All backup files are stored in one direcctory
;;(setq backup-directory-alist '(("." . "~/.cache/.saves"))
;;      delete-old-versions t)
;; Put backup files neatly away
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "c9415c9f5a5ed67914d1d64a0ea7d743ef93516f1f2c8501bc5ffb87af2066d3" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "e58e0bd0ca1f1a8c1662aeb17c92b7fb49ed564aced96435c64df608ee6ced6d" "98db748f133d9bb82adf38f8ae7834eefa9eefd6f7ea30909213164e1aa36df6" "6f895d86fb25fac5dd4fcce3aec0fe1d88cf3b3677db18a9607cf7a3ef474f02" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "7236acec527d58086ad2f1be6a904facc9ca8bf81ed1c19098012d596201b3f1" "4e9e56ec06ede9857c876fea2c44b75dd360cd29a7fe927b706c45f804f7beff" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "b9e406b52f60a61c969f203958f406fed50b5db5ac16c127b86bbddd9d8444f7" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "73320ccc14ab4987fe2e97cfd810b33a1f4a115f5f056c482c3d38a4429e1705" "78c01e1b7f3dc9e47bdd48f74dc98dc1a345c291f83b68ac8a1b40191f24d658" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "620b9018d9504f79344c8ef3983ea4e83d209b46920f425240889d582be35881" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" default))
 '(default-input-method "devanagari-itrans"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

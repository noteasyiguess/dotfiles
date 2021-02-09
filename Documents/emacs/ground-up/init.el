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
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Handy function
(defun kill-all-buffers ()
  (delete-other-windows)
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package all-the-icons)
(use-package magit)

(use-package lsp-mode
  :hook
  (c++-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (objc-mode . lsp-deferred)
  :commands (lsp lsp-deferred))
;; (use-package lsp-ui
;;   :config
;;   (lsp-ui-doc-enable nil))

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
	company-idle-delay 0.100)
(add-hook 'after-init-hook 'global-company-mode)

;; (use-package irony
;;   :config
;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)
;;     (add-hook 'objc-mode-hook 'irony-mode)
;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;; (use-package company-irony)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

(use-package flycheck
  :init (global-flycheck-mode))
;; (use-package flycheck-irony)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-nerd-commenter)
(global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-lines)

(use-package tree-sitter)
;;  :config
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

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

;; When running in daemon mode, the font is not set since there is no frame
(defun my-frame-init ()
  (set-face-attribute 'default nil :font "GoMono Nerd Font Mono" :height 150)
  (set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 150)
  (setq doom-modeline-icon t))
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(my-frame-init)))
  (my-frame-init))

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)

;; Long line performance improvements
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Disable line folding
(set-default 'truncate-lines t)

;; Match the other bracket pair
(show-paren-mode 1)

;; Automatically pair parens
(electric-pair-mode 1)

;; Disable that annoying blinking cursor
(blink-cursor-mode -1)

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
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control nil    ; Use version numbers on backups
      kept-new-versions 2    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7236acec527d58086ad2f1be6a904facc9ca8bf81ed1c19098012d596201b3f1" "4e9e56ec06ede9857c876fea2c44b75dd360cd29a7fe927b706c45f804f7beff" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "b9e406b52f60a61c969f203958f406fed50b5db5ac16c127b86bbddd9d8444f7" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "73320ccc14ab4987fe2e97cfd810b33a1f4a115f5f056c482c3d38a4429e1705" "78c01e1b7f3dc9e47bdd48f74dc98dc1a345c291f83b68ac8a1b40191f24d658" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "620b9018d9504f79344c8ef3983ea4e83d209b46920f425240889d582be35881" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" default))
 '(default-input-method "devanagari-itrans"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

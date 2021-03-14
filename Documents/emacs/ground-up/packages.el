(use-package gcmh)
(use-package magit)
(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))
;; (use-package vterm)

(use-package w3m
  :config
  (setq w3m-default-display-inline-images t
        w3m-use-favicon nil))

;; (use-package emms
;;   :config
;;   (emms-all)
;;   (setq emms-source-file-default-directory "~/Music/"
;;         emms-player-list '(emms-player-vlc)
;;         emms-player-vlc-command-name "cvlc"))

;; (use-package bongo
;;   :config
;;   (setq bongo-default-directory "~/Music"
;;         bongo-insert-whole-directory-trees t
;;         bongo-mark-played-tracks t
;;         bongo-display-track-icons nil
;;         bongo-display-header-icons nil
;;         bongo-display-playback-mode-indicator t
;;         bongo-display-inline-playback-progress t
;;         bongo-enabled-backends '(vlc mpv)
;;         bongo-vlc-program-name "cvlc"
;;         bongo-custom-backend-matchers '((vlc local-file "m4a")
;;                                         (vlc local-file "opus"))))

(use-package company
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.300)
  (add-hook 'after-init-hook 'global-company-mode))

;; (use-package dap-mode)

;; (use-package treemacs
;;   :config (setq
;;            treemacs-width 25
;;            treemacs-show-hidden-files nil
;;            treemacs-indentation 1
;;            treemacs-indentation-string " ")
;;   :bind
;;   (:map global-map
;;         ("M-[" . treemacs-select-window)
;;         ("M-]" . treemacs)
;;         ("C-x t i" . treemacs-find-file)))

;; (use-package projectile
;;   :config
;;   (projectile-mode 1)
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; (use-package lsp-mode
;;   :hook
;;   (c++-mode . lsp-deferred)
;;   (c-mode . lsp-deferred)
;;   (objc-mode . lsp-deferred)
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   :commands (lsp lsp-deferred))
;; (use-package lsp-ui
;;   :config
;;   (lsp-ui-doc-enable nil))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil))))

(use-package counsel
  :bind
  ("C-x C-r" . counsel-recentf)
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
  :config
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))
;; (use-package lsp-ivy)

;; (use-package flycheck
;;   ;; :init (global-flycheck-mode)
;;   )

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

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

(use-package amx) ;; recently used commands

;; Modeline
;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode))

;; (use-package powerline
;;   :config
;;   (powerline-center-theme))

;; (use-package diminish
;;   :config
;;   (diminish 'eldoc-mode)
;;   (diminish 'gcmh-mode)
;;   (diminish 'ivy-mode)
;;   (diminish 'which-key-mode)
;;   (diminish 'company-mode)
;;   (diminish 'auto-revert-mode)
;;   (diminish 'projectile-mode)
;;   (diminish 'hide-ifdef-mode)
;;   (diminish 'hs-minor-mode)
;;   (diminish 'flycheck-mode)
;;   (diminish 'tree-sitter-mode)
;;   (diminish 'abbrev-mode)
;;   (diminish 'lsp-mode)
;;   (diminish 'page-break-lines-mode)
;;   )

;; Themes
(use-package doom-themes
  :config (doom-themes-org-config))
(use-package kaolin-themes)
(use-package gruvbox-theme)
(use-package poet-theme)
(use-package ample-theme)
(use-package srcery-theme)
(use-package zerodark-theme)
;; (use-package circadian)

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner '1
;;         dashboard-center-content nil
;;         dashboard-items '((recents . 5)
;;                           (projects . 5)
;;                           (agenda . 5))
;;         dashboard-set-heading-icons t
;;         dashboard-set-file-icons nil
;;         dashboard-set-init-info t
;;         initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
;; (add-hook 'dashboard-after-initialize-hook (lambda () (setq dashboard-init-info (greet-other-lang))
;;                                              (dashboard-refresh-buffer)))

;; Run by emacs -Q -nw alias

(load-theme 'wombat)

(electric-pair-mode 1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)

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

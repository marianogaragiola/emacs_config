;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Mode-line
(use-package doom-modeline
  :custom
  ;; (doom-modeline-icon centaur-icon)
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-mu4e nil)
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil))
  ;; :bind (:map doom-modeline-mode-map
  ;;        ("C-<f6>" . doom-modeline-hydra/body))
  )

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

;; A minor-mode menu for mode-line
(when emacs/>=25.2p
  (use-package minions
    :hook (doom-modeline-mode . minions-mode)))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or sys/win32p (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t))
  :config
  (with-no-warnings
    ;; FIXME: Align the directory icons
    ;; @see https://github.com/domtronn/all-the-icons.el/pull/173
    (defun my-all-the-icons-icon-for-dir (dir &optional chevron padding)
      "Format an icon for DIR with CHEVRON similar to tree based directories."
      (let* ((matcher (all-the-icons-match-to-alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist))
             (path (expand-file-name dir))
             (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
             (padding (or padding "\t"))
             (icon (cond
                    ((file-symlink-p path)
                     (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.0))
                    ((all-the-icons-dir-is-submodule path)
                     (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.0))
                    ((file-exists-p (format "%s/.git" path))
                     (format "%s" (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.0)))
                    (t (apply (car matcher) (list (cadr matcher) :v-adjust 0.0))))))
        (format "%s%s%s%s%s" padding chevron padding icon padding)))
    (advice-add #'all-the-icons-icon-for-dir :override #'my-all-the-icons-icon-for-dir)

    (defun all-the-icons-reset ()
      "Reset (unmemoize/memoize) the icons."
      (interactive)
      (ignore-errors
        (dolist (f '(all-the-icons-icon-for-file
                     all-the-icons-icon-for-mode
                     all-the-icons-icon-for-url
                     all-the-icons-icon-family-for-file
                     all-the-icons-icon-family-for-mode
                     all-the-icons-icon-family))
          (memoize-restore f)
          (memoize f)))
      (message "Reset all-the-icons")))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here

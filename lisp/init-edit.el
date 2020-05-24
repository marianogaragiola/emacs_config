;;; Code:

;; (require 'init-const)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
;; (use-package goto-addr
;;   :ensure nil
;;   :hook ((text-mode . goto-address-mode)
;;          (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
;; (use-package avy
;;   :bind (("C-:" . avy-goto-char)
;;          ("C-'" . avy-goto-char-2)
;;          ("M-g f" . avy-goto-line)
;;          ("M-g w" . avy-goto-word-1)
;;          ("M-g e" . avy-goto-word-0))
;;   :hook (after-init . avy-setup-default)
;;   :config (setq avy-all-windows nil
;;                 avy-all-windows-alt t
;;                 avy-background t
;;                 avy-style 'pre))

;; Kill text between the point and the character CHAR
;; (use-package avy-zap
;;   :bind (("M-z" . avy-zap-to-char-dwim)
;;          ("M-Z" . avy-zap-up-to-char-dwim)))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                         'java-mode 'go-mode 'swift-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Redefine M-< and M-> for some modes
;; (when emacs/>=25.3p
;;   (use-package beginend
;;     :diminish (beginend-mode beginend-global-mode)
;;     :hook (after-init . beginend-global-mode)
;;     :config
;;     (mapc (lambda (pair)
;;             (add-hook (car pair) (lambda () (diminish (cdr pair)))))
;;           beginend-modes)))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ;

;; Drag stuff (lines, words, region, etc...) around
;; (use-package drag-stuff
;;   :diminish
;;   :commands drag-stuff-define-keys
;;   :hook (after-init . drag-stuff-global-mode)
;;   :config
;;   (add-to-list 'drag-stuff-except-modes 'org-mode)
;;   (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Edit multiple regions in the same way simultaneously
;; (use-package iedit
;;   :defines desktop-minor-mode-table
;;   :bind (("C-;" . iedit-mode)
;;          ("C-x r RET" . iedit-rectangle-mode)
;;          :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
;;          :map esc-map ("C-;" . iedit-execute-last-modification)
;;          :map help-map ("C-;" . iedit-mode-toggle-on-function))
;;   :config
;;   ;; Avoid restoring `iedit-mode'
;;   (with-eval-after-load 'desktop
;;     (add-to-list 'desktop-minor-mode-table
;;                  '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; Smartly select region, rectangle, multi cursors
;; (use-package smart-region
;;   :hook (after-init . smart-region-on))

;; On-the-fly spell checker
;; (use-package flyspell
;;   :ensure nil
;;   :diminish
;;   :if (executable-find "aspell")
;;   :hook (((text-mode outline-mode) . flyspell-mode)
;;          (prog-mode . flyspell-prog-mode)
;;          (flyspell-mode . (lambda ()
;;                             (dolist (key '("C-;" "C-," "C-."))
;;                               (unbind-key key flyspell-mode-map)))))
;;   :init (setq flyspell-issue-message-flag nil
;;               ispell-program-name "aspell"
;;               ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
;;   :config
;;   Correcting words with flyspell via Ivy
;;   (use-package flyspell-correct-ivy
;;     :after ivy
;;     :bind (:map flyspell-mode-map
;;                 ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
;;     :init (setq flyspell-correct-interface #'flyspell-correct-ivy))
;;   )

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Windows-scroll commands
(use-package pager
  :bind (([remap scroll-up-command] . pager-page-down)
         ([remap scroll-down-command] . pager-page-up)
         ([next]   . pager-page-down)
         ([prior]  . pager-page-up)
         ([M-up]   . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line

;; Treat undo history as a tree
;; (use-package undo-tree
;;   :diminish
;;   :hook (after-init . global-undo-tree-mode)
;;   :init
;;   (setq undo-tree-visualizer-timestamps t
;;         undo-tree-enable-undo-in-region nil
;;         undo-tree-auto-save-history nil)

;;   ;; HACK: keep the diff window
;;   (with-no-warnings
;;     (make-variable-buffer-local 'undo-tree-visualizer-diff)
;;     (setq-default undo-tree-visualizer-diff t)))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Record and jump to the last point in the buffer
(use-package goto-last-point
  :diminish
  :bind ("C-M-," . goto-last-point)
  :hook (after-init . goto-last-point-mode))

;; Preview when `goto-char'
(use-package goto-char-preview
  :bind ([remap goto-char] . goto-char-preview))

;; Preview when `goto-line'
(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Hideshow
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding)))

;; Open files as another user
;; (unless sys/win32p
;;   (use-package sudo-edit))

;; Narrow/Widen
;; (use-package fancy-narrow
;;   :diminish
;;   :hook (after-init . fancy-narrow-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here

;; init-ivy.el --- Ivy

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(use-package ivy
  :ensure t
  :config (ivy-mode 1))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package posframe
  :ensure t)
(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-min-width 90
        ivy-posframe-width 110)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-enable))
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))


(use-package iedit
  :ensure t)

(provide 'init-ivy)

;;; init-ivy ends here

;;; package --- Sumary
;;; Commentary:
;;; Code:

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-modified-marker t)
  ;; (setq centaur-tabs-set-modified-marker "*")
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-close-buttom "X")
  (setq centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  :bind
  ("M-S-<left>" . centaur-tabs-backward)
  ("M-S-<right>" . centaur-tabs-forward)
  ("M-S-<up>" . centaur-tabs-move-current-tab-to-left)
  ("M-S-<down>" . centaur-tabs-move-current-tab-to-right))

(provide 'init-tabs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tabs.el ends here

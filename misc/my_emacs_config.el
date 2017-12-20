;; proxy
;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "<write your proxy>:8080")
;;     ("https" . "<write your proxy>:8080")))

;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; tango-dark theme
(custom-set-variables
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces )

;; see: https://fonts.google.com/specimen/Inconsolata?selection.family=Inconsolata
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "PfEd" :slant normal :weight normal :height 143 :width normal)))))

;; turn off splash screen
(setq inhibit-splash-screen t)

;; turn on transient mark mode
(transient-mark-mode 1)

;; no toolbar
(tool-bar-mode -1)

;; show matching parenthesis
(show-paren-mode t)

;; stop creating backup~ files
(setq make-backup-files nil) 

;; stop creating #autosave# files
(setq auto-save-default nil) 

;; sane window resize shortcuts
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally  )
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally )
(global-set-key (kbd "S-C-<down>")  'shrink-window               )
(global-set-key (kbd "S-C-<up>")    'enlarge-window              )

;; zoom in/out on C-mousewheel
(global-set-key [C-wheel-up]   'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)

;; turn off c++-mode's automatic identing 
(setq-default c-basic-offset 2
              tab-width 2 )
(require 'cc-mode)
(add-to-list 'c-mode-common-hook
             (lambda () (setq c-syntactic-indentation nil)))


;; org-mode settings
(setq org-src-fontify-natively t)
;; proxy
;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "<write your proxy>:8080")
;;     ("https" . "<write your proxy>:8080")))

;; packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
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

;; recompile on f7
;; this assumes Makefile is in parent directory to where source files are located (see my 'generic' Makefile for details)
(setq compile-command "make -C ..")
(define-key global-map [f7] 'recompile)

;; my c++-mode options (taken from stlab.cc/legacy/emacs-questions.html)
(defun my-c-mode-common-hook ()
  (setq tab-width 2)
  ;; don't treat _ as word delimiter
  (modify-syntax-entry ?_ "w")
  ;; this will make sure spaces are used instead of tabs
  (setq indent-tabs-mode t)
  ;; we don't like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-state -1)
  (setq c-basic-offset 2)
	(setq c-syntactic-indentation nil)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


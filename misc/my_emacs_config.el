;;
;; proxy, if you need one
;;

;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "<write your proxy>:8080")
;;     ("https" . "<write your proxy>:8080")))

;;
;;  packages
;;

;; taken from https://melpa.org/#/getting-started
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

(setq package-list 
  '(sr-speedbar cmake-mode rust-mode haskell-mode json-mode json-reformat))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;
;; general editor options
;;

;; tango-dark theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (json-mode haskell-mode rust-mode cmake-mode sr-speedbar)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;; turn off splash screen
(setq inhibit-splash-screen t)

;; truncate long lines by default
(setq-default truncate-lines t)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

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

;;
;; org-mode options
;;

;; org-mode settings
(setq org-src-fontify-natively t)

;; org-mode TODO states
(setq org-todo-keywords
    '((sequence "TODO" "BLOCKED" "|" "DOING" "DONE")))

;;
;; cc-mode options
;;

;; turn off c++-mode's automatic identing 
(setq-default c-basic-offset 2
              tab-width 2 )
(require 'cc-mode)
(add-to-list 'c-mode-common-hook
             (lambda () (setq c-syntactic-indentation nil)))

;; taken from stlab.cc/legacy/emacs-questions.html
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


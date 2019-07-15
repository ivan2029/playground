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

;;
(require 'package)

; list the repositories containing them
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(setq package-list '(sr-speedbar 
		     treemacs
		     ;;
		     cmake-mode
		     ;;
		     go-mode
		     rust-mode 
		     haskell-mode
		     ;;
		     json-mode 
		     json-reformat
		     ;;
		     web-mode ))

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

;; theme
(custom-set-variables '(custom-enabled-themes (quote (wombat))))

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
;; org-mode 
;;

;; enable syntax highlighting
(setq org-src-fontify-natively t)

;;
;; cc-mode options
;;

(defun my-c-mode-common-hook ()
  ;; don't treat _ as word delimiter
  (modify-syntax-entry ?_ "w")
  ;; this will make sure spaces are used instead of tabs
  (setq indent-tabs-mode t)
  ;; we don't like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-state -1)
  ;; spaces instead of tabs and tab width is 4
  (setq tab-width 4)
  (setq c-basic-offset 4)
  ;; please no automatic indentation
  (setq c-syntactic-indentation nil)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


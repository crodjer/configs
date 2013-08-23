;; ------------------
;; Start emacs server
;; ------------------
(server-start)

;; ----------
;; Load paths
;; ----------
(add-to-list 'load-path "~/.elisp/")

;; ---------
;; Autoloads
;; ---------
(require 'fill-column-indicator)

;; ----------------------
;; General customizations
;; ----------------------
;; Variables
(setq-default

 ;; Tabs and indentation
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 python-indent 4
 js-indent-level 2

 ;; Misc
 require-final-newline t
 column-number-mode t
 next-line-add-newlines nil
 blink-matching-paren t
 winner-mode t
 visible-bell t

 ;; Startup
 inhibit-startup-echo-area-message t
 inhibit-startup-message t
)

;; Ido mode
(ido-mode t)

;; UI

(load-theme 'tango)
;; Options: 
;; adwaita 	deeper-blue 	dichromacy 	light-blue 	manoj-dark 	misterioso
;; tango 	tango-dark 	tsdh-dark 	tsdh-light 	wheatgrass 	whiteboard
;; wombat


(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


(set-face-attribute 'default nil :height 90)

(setq linum-format "%4d")
(global-linum-mode 1)

(setq fci-rule-width 2)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Custom keybindings
(global-set-key "\M-z" 'eval-last-sexp)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((virtualenv-workon . "zlemma")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Bundled in distro
(require 'uniquify)
(require 'saveplace)
(require 'markdown-mode)

;; Installed in ~/.elisp
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
 uniquify-buffer-name-style 'forward
 save-place t
 x-select-enable-clickboard t
 x-select-enable-primary t
 vc-follow-symlinks t

 ;; Startup
 inhibit-startup-echo-area-message t
 inhibit-startup-message t
)

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$\\|.md$\\|.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.rake$\\|Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.html$\\|hbs$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . javascript-mode))

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

(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;

;; Shift region (http://www.emacswiki.org/emacs/IndentingText)
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;;;;;;;;;;;;;;;;;;;;;
;; Custom keybindings
;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-z" 'eval-last-sexp)
(global-set-key "\C-j" 'newline)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-<") 'shift-left)
(global-set-key (kbd "C->") 'shift-right)

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

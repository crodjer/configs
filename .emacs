;;; .emacs --- Emacs Configuration

;;; Commentary:
;; Personal version controlled configuration for Emacs.

;;; Code:

;; Start emacs server
;; ------------------
(server-start)

;; ----------
;; Load paths
;; ----------
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; ---------
;; Autoloads
;; ---------

;; Bundled in distro
(require 'package)
(require 'uniquify)
(require 'saveplace)
(require 'gnus)

;; Installed in ~/.elisp
(require 'fill-column-indicator)
(require 'markdown-mode)

;; El get
;; Tracked in ~/.emacs.d/el-get/.status.el

(add-to-list
  'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

(require 'haskell-mode-autoloads)
(require 'yasnippet)
(require 'auto-complete-config)
(require 'js2-mode)
(require 'whitespace)
(require 'git-commit)
(require 'scala-mode2)
(require 'virtualenv)
(require 'flymake)
(require 'flymake-cursor)
(require 'python-django)
(require 'flymake-jshint)
(require 'paredit)
(require 'geiser-install)

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
 js2-basic-offset 2
 js2-bounce-indent-p nil

 ;; Backups
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 16
 kept-old-versions 2
 version-control t
 backup-by-copying-when-linked t

 ;; Startup
 inhibit-startup-echo-area-message t
 inhibit-startup-message t

 ;; Email
 user-mail-address "crodjer@gmail.com"
 user-full-name "Rohan Jain"

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
 whitespace-style '(face tabs trailing)
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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))

;; ------------------
;; General mode hooks
;; ------------------

;; ------------
;; Autocomplete
;; ------------
(ac-config-default)

;; ---------
;; Ido mode
;; ---------
(ido-mode t)

;; -------------
;; Flyspell mode
;; -------------
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'html-mode-hook
          (lambda () (flyspell-mode 0)))

;; ---------------
;; Git commit mode
;; ---------------
(add-hook 'git-commit-mode-hook
          (lambda () (fci-mode 0)))
(add-hook 'git-commit-mode-hook 'flyspell-mode)

;; --------
;; FCI mode
;; --------
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'text-mode-hook 'fci-mode)

;; ------------
;; Haskell mode
;; ------------
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; --------
;; JS2 mode
;; --------
(setq-default js2-global-externs '("$" "_")
              js2-include-browser-externs t
              js2-include-node-externs t
              js2-include-jslint-globals t
              js2-strict-inconsistent-return-warning nil)

;; -----------
;; Python Mode
;; -----------
(setq virtualenv-workon-starts-python nil)
(add-hook 'python-mode-hook
          (lambda() (require 'virtualenv)))
;; ----------
;; Javascript
;; ----------
(add-hook 'js2-mode-hook
     (lambda () (flymake-mode t)))

(defun jshint-mode-restart ()
  (jshint-mode-stop)
  (jshint-mode-init))
;; (jshint-mode-restart)


;; -------
;; Flymake
;; -------
(global-set-key "\C-c\M-s" 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-c\M-n" 'flymake-goto-next-error)
(global-set-key "\C-c\M-p" 'flymake-goto-previous-error)
(setq flymake-cursor-error-display-delay 0.1).

;; ------------
;; Paradit Mode
;; ------------
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; ----
;; Gnus
;; ----
(setq gnus-asynchronous t
      gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "crodjer@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; UI
(load-theme 'tango)
;; Options:
;; adwaita 	deeper-blue 	dichromacy 	light-blue 	manoj-dark 	misterioso
;; tango 	tango-dark 	tsdh-dark 	tsdh-light 	wheatgrass 	whiteboard
;; wombat

(global-whitespace-mode t)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-face-attribute 'default nil :height 90)

(setq linum-format "%4d")
(global-linum-mode 1)

(setq fci-rule-width 2)

(show-paren-mode 1)


;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;

;; Shift region (http://www.emacswiki.org/emacs/IndentingText)
(defun shift-region (distance)
  "Shift the selected region horizontally, based on provided DISTANCE."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  "Shift the region to the right by 1."
  (interactive)
  (shift-region 1))

(defun shift-left ()
  "Shift the region to the left by 1."
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

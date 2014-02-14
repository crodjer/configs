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
(require 'auto-complete-config)
(require 'js2-mode)
(require 'whitespace)
(require 'scala-mode2)
(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-jshint)
(require 'paredit)
(require 'geiser-install)
(require 'erlang)

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
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions t
 kept-new-versions 16
 kept-old-versions 2
 version-control t
 backup-by-copying-when-linked t

 ;; Autosave
 auto-save-interval 60
 auto-save-file-name-transforms '((".*" "~/.emacs.d/saves/\\1" t))

 ;; Startup
 inhibit-startup-echo-area-message t
 inhibit-startup-message t

 ;; Email
 user-mail-address "crodjer@gmail.com"
 user-full-name "Rohan Jain"

 ;; Misc
 fill-column 80
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
 create-lockfiles nil
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
(add-to-list 'auto-mode-alist '("\\.pyx$" . python-mode))

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
              js2-additional-externs nil
              js2-include-browser-externs t
              js2-include-node-externs t
              js2-include-jslint-globals t
              js2-strict-inconsistent-return-warning nil
              js2-skip-preprocessor-directives t)

;; -----------
;; Python Mode
;; -----------
(defun join-list (list delimiter)
  "Join a list of strings by a delimiter string."
  (mapconcat 'identity list delimiter))
(defun python-calculate-env ()
  "Calculate env variables for current python virtualenv."
  (join-list
   (cons
    (format "PATH=%s" (join-list (python-shell-calculate-exec-path) ":"))
    (python-shell-calculate-process-environment)
    )
   " "))

(defun python-virtualenv-exec (command args)
  "Generate a flymake friendly list executable in virtualenv, for provided
commands."
  (list "env" (append (list (python-calculate-env) command) args)))

(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (python-virtualenv-exec
       "pylint"
       (list
        ;; Pylint args. Will depend on the checker being used.
        "-r" "n"
        "--msg-template='{path}:{line}:{category} [{msg_id} {obj}] {msg}'"
        local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init)))

(defun flymake-mode-hook-function ()
  (when (derived-mode-p 'python-mode)
    (flymake-mode t)))
;; Run flymake mode hook function after the local variables are set (eg: through
;; .dir-locals.el
(add-hook 'hack-local-variables-hook #'flymake-mode-hook-function)
(setq virtualenv-workon-starts-python nil)

;; ----------
;; Javascript
;; ----------
(setq jshint-mode-jshint-path "jshint")
(add-hook 'js2-mode-hook
     (lambda () (flymake-mode t)))

(defun jshint-mode-restart ()
  (jshint-mode-stop)
  (jshint-mode-init))
;; (jshint-mode-restart)
;; -------
;; Flymake
;; -------
(global-set-key "\C-cs" 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)
(setq
 flymake-cursor-error-display-delay 0.1
 flymake-log-level -1
)
(custom-set-faces
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((t))))

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

;;;;;;;
;; Misc
;;;;;;;
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
	  (delete-region (point) (progn (skip-chars-forward " \t") (point)))))


;;;;;;;;;;;;;;;;;;;;;
;; Custom keybindings
;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-z" 'eval-last-sexp)
(global-set-key "\C-j" 'newline)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-<") 'shift-left)
(global-set-key (kbd "C->") 'shift-right)


;;;;;;;;;;;;;;;;;
;; Autogenerated
;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((python-shell-extra-pythonpaths . "/home/rohan/workspace/zlemma/zlemma") (erlang-indent-level . 4) (js2-additional-externs (quote ("Ember" "Cockpit")))))))

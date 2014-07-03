;;; .emacs --- Emacs Configuration

;;; Commentary:
;; Personal version controlled configuration for Emacs.

;;; Code:

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
(require 'tramp)

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
 tramp-default-method "ssh"
)

;; Server
;; ------------------
(server-start)

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
(add-to-list 'auto-mode-alist '("\\.html$\\$" . html-mode))
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
(setq ido-ignore-extensions t)

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
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(defadvice ghc-display
  (after ghc-display-auto-pop-advice ())
  (pop-to-buffer ghc-error-buffer-name))
; (ad-activate 'ghc-display)

(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))
;; -----------
;; Python Mode
;; -----------
(defun python-calculate-env ()
  "Calculate env variables for current python virtualenv."
  ;; ;; This also overrides PYTHONHOME to "", which breaks the process
  ;; ;; environment. Otherwise, a neat idea.
  ;; (mapcar
  ;;  (lambda (string)
  ;;    (let ((splitted-string (split-string string "=")))
  ;;      (format
  ;;       "%s=\"%s\""
  ;;       (car splitted-string)
  ;;       (or (cadr splitted-string)
  ;;           ""))))
  ;; (python-shell-calculate-process-environment)))
  (remove-if
   (lambda (x)
     (or
      ;; If environment
      (string-match " " x)
      (not (string-match "=" x))))
   (python-shell-calculate-process-environment)))

(defun python-virtualenv-exec (command args)
  "Generate a flymake friendly list executable in virtualenv, for provided
commands."
  (list "env" (append (python-calculate-env) (list command) args)))

(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intmp))
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
(setq jshint-mode-jshint-path "jshint"
      jshint-mode-create-temp-helper 'flymake-create-temp-intmp)
(add-hook 'js2-mode-hook
     (lambda () (flymake-mode t)))

(defun jshint-mode-restart ()
  (jshint-mode-stop)
  (jshint-mode-init))
;; (jshint-mode-restart)

(setq-default js2-global-externs '("$" "_")
              js2-additional-externs nil
              js2-include-browser-externs t
              js2-include-node-externs t
              js2-include-jslint-globals t
              js2-strict-inconsistent-return-warning nil
              js2-skip-preprocessor-directives t
              js2-strict-trailing-comma-warning nil
              js2-strict-missing-semi-warning nil)

;; Override the function which highlights undeclared vars. I'll let jshint take
;; care of this.
(defun js2-highlight-undeclared-vars ())

;; -------
;; Flymake
;; -------
(global-set-key "\C-cs" 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)
(setq
 flymake-cursor-error-display-delay 0.1
 flymake-log-level 0
 ;; Match Convention/Refactor apart from warnings (from pylint)
 ;; flymake-warning-re "^\\([wW]arning\\|[cC]onvention\\|[rR]efactor\\)"
 flymake-warning-re (rx (or "warning" "Warning" "convention" "Convention"
                            "refactor" "Refactor" "info" "Info"))
 )

(defun flymake-create-temp-intmp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intmp: file=%s temp=%s" file-name temp-name)
    temp-name))

;; ------------
;; Paradit Mode
;; ------------
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; ---------
;; Mail mode
;; ---------
(add-hook 'mail-mode-hook
     (lambda () (setq-local fill-column 70)))


;; --
;; UI
;; --


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

(set-face-attribute 'default nil :height 96)

(setq linum-format "%4d")
(global-linum-mode 1)

(setq fci-rule-width 2)

(show-paren-mode 1)


;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))
(global-set-key (kbd "C-x v") 'halve-other-window-height)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t ("red"))))
 '(flymake-warnline ((t nil)))
 '(whitespace-trailing ((t (:background "gainsboro")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(safe-local-variable-values (quote (python-shell-extra-pythonpaths . "/home/rohan/workspace/zlemma/zlemma") (erlang-indent-level . 4))))

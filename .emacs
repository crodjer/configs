(require 'cl)
(require 'package)

;; ----------------------------------------
;; Packages
;; ----------------------------------------
(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/")
             t)
(package-initialize)

(defvar my-packages
  '(ag
    auto-complete
    better-defaults
    cider
    company
    fill-column-indicator
    git-commit
    magit
    paredit
    xclip))

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; Elisp files
(let ((elisp-directory "~/.emacs.d/elisp"))
  (when (not (file-exists-p "~/.emacs.d/elisp"))
    (make-directory elisp-directory))
  (mapc (lambda (name)
          (load-file (concat (file-name-as-directory elisp-directory)
                             name)))
        (directory-files elisp-directory nil "\\.el$")))

;; Local files
(let ((local-elisp-directory "~/.emacs.d/local"))
  (when (not (file-exists-p "~/.emacs.d/local"))
    (make-directory local-elisp-directory))
  (mapc (lambda (name)
          (load-file (concat (file-name-as-directory local-elisp-directory)
                             name)))
        (directory-files local-elisp-directory nil "\\.el$")))

;; ----------------------------------------
;; Defaults
;; ----------------------------------------
(setq-default vc-follow-symlinks t)

;; ----------------------------------------
;; Ag
;; ----------------------------------------
(global-set-key (kbd "C-c C-s") 'ag-project)

;; ----------------------------------------
;; Auto complete
(ac-config-default)
;; ----------------------------------------

;; ----------------------------------------
;; Buffers and files
;; ----------------------------------------
(require 'saveplace)
(setq-default save-place-mode t)
(setq save-place-file "~/.emacs.d/saved-places"
      save-place-forget-unreadable-files nil)

(global-auto-revert-mode 1)
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))
(global-set-key (kbd "C-c r")
                'revert-buffer-no-confirm)
(recentf-mode t)

;; ----------------------------------------
;; Clojure
;; ----------------------------------------
(defun enable-cider-mode ()
  (cider-mode 1))
(defun enable-clj-refactor-mode ()
  (clj-refactor-mode 1))

(with-eval-after-load 'clojure-mode
  (require 'subr-x)
  ;; (require 'clj-refactor)


  (setq cljr-favor-prefix-notation nil)
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-use-pretty-printing t)

  ;; (define-key cider-mode-map (kbd "C-x c d n") 'cider-browse-ns)

  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'enable-cider-mode)
  ;; (add-hook 'clojure-mode-hook 'enable-clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; (add-hook 'cider-repl-mode-hook 'enable-clj-refactor-mode)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))


;; ----------------------------------------
;; Company
;; ----------------------------------------
(global-company-mode)
(global-set-key (kbd "M-TAB") #'company-complete)


;; ----------------------------------------
;; Dired
;; ----------------------------------------
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file ".."))) ))

;; ----------------------------------------
;; Elisp
;; ----------------------------------------
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; ----------------------------------------
;; Ido
;; ----------------------------------------
(ido-mode 1)
(ido-everywhere 1)

;; ----------------------------------------
;; Fill column indicator
;; ----------------------------------------
(require 'fill-column-indicator)
(setq-default fill-column 80)
(defun fci-mode-in-graphics-display ()
  (if (display-graphic-p)
      (progn
        (fci-mode 1))))
(with-eval-after-load 'fill-column-indicator
  (add-hook 'prog-mode-hook 'fci-mode-in-graphics-display)
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'text-mode-hook 'fci-mode-in-graphics-display))

;; ----------------------------------------
;; Flyspell mode
;; ----------------------------------------
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'html-mode-hook
          (lambda () (flyspell-mode 0)))
(add-hook 'html-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; ----------------------------------------
;; Git commit mode
;; ----------------------------------------
(global-git-commit-mode)

;; ----------------------------------------
;; Haskell
;; ----------------------------------------
(with-eval-after-load 'haskell-mode
  (setq haskell-ask-also-kill-buffers nil)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

;; ----------------------------------------
;; Look and Feel
;; ----------------------------------------
(load-theme 'tango)
(setq-default inhibit-startup-message t)

;; Whitespace
(setq-default whitespace-style '(face tabs trailing))
(global-whitespace-mode)

;; ----------------------------------------
;; MacOS
;; ----------------------------------------
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (set-face-attribute 'default nil :height (* 125))
  (setq
   ring-bell-function 'ignore
   visible-bell nil
   frame-title-format "%b"
   icon-title-format  "%b")
  (global-unset-key (kbd "s-p")) ;; stupid binding
  (global-unset-key (kbd "M-TAB"))
  (menu-bar-mode 1))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 140)

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)

  ;; Disable native fullscreen in OSX.
  (setq ns-use-native-fullscreen nil))


;; ----------------------------------------
;; Markdown mode
;; ----------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ----------------------------------------
;; Magit
;; ----------------------------------------
(global-set-key (kbd "C-x g")
                'magit-status)

;; ----------------------------------------
;; Org mode
;; ----------------------------------------
(setq org-startup-indented t)
(add-hook 'org-mode-hook (lambda ()
                           (setq-local fill-column 72)
                           (fci-mode 0)))

;; ----------------------------------------
;; Split windows
;; ----------------------------------------
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

;; -------------
;; Autogenerated
;; -------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :background "gainsboro"))))
 '(shm-current-face ((t (:background "\"WhiteSmoke\""))))
 '(whitespace-trailing ((t (:background "gainsboro")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (company magit projectile xclip auto-complete ag exec-path-from-shell paredit fill-column-indicator cider better-defaults))))

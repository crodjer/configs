((auto-complete status "installed" recipe
                (:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (ctable status "installed" recipe
         (:name ctable :description "Table Component for elisp" :type github :pkgname "kiwanami/emacs-ctable"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (deferred status "installed" recipe
   (:name deferred :description "Simple asynchronous functions for emacs lisp" :website "https://github.com/kiwanami/emacs-deferred" :type github :pkgname "kiwanami/emacs-deferred" :features "deferred"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (epc status "installed" recipe
      (:name epc :description "An RPC stack for Emacs Lisp" :type github :pkgname "kiwanami/emacs-epc" :depends
             (deferred ctable)))
 (epl status "installed" recipe
      (:name epl :description "EPL provides a convenient high-level API for various package.el versions, and aims to overcome its most striking idiocies." :type github :pkgname "cask/epl"))
 (erlang-mode status "installed" recipe
              (:name erlang-mode :description "Major mode for editing and running Erlang" :type http :url "http://www.erlang.org/download/contrib/erlang.el" :post-init
                     (progn
                       (add-to-list 'auto-mode-alist
                                    '("\\.erl$" . erlang-mode)))))
 (f status "installed" recipe
    (:name f :website "https://github.com/rejeep/f.el" :description "Modern API for working with files and directories in Emacs" :type github :pkgname "rejeep/f.el"))
 (find-file-in-project status "installed" recipe
                       (:name find-file-in-project :type github :pkgname "technomancy/find-file-in-project" :description "Quick access to project files in Emacs"))
 (flymake-cursor status "installed" recipe
                 (:name flymake-cursor :type github :pkgname "illusori/emacs-flymake-cursor" :description "displays flymake error msg in minibuffer after delay (illusori/github)" :website "http://github.com/illusori/emacs-flymake-cursor"))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (geiser status "installed" recipe
         (:name geiser :website "http://www.nongnu.org/geiser/" :description "Geiser is a collection of Emacs major and minor modes that conspire with one or more Scheme interpreters to keep the Lisp Machine Spirit alive. It draws inspiration (and a bit more) from environments such as Common Lisp's Slime, Factor's FUEL, Squeak or Emacs itself, and does its best to make Scheme hacking inside Emacs (even more) fun." :type git :url "git://git.sv.gnu.org/geiser.git" :load-path
                ("./elisp")
                :build
                `("./autogen.sh" "./configure" ,(concat "make EMACS=" el-get-emacs)
                  ,(concat "make EMACS=" el-get-emacs "info-recursive"))
                :build/windows-nt
                `("sh ./autogen.sh" "sh ./configure" "make" ,(concat "cd doc & " el-get-install-info " --dir-file=./dir *.info"))
                :info "doc" :features geiser-load))
 (git-modes status "installed" recipe
            (:name git-modes :description "GNU Emacs modes for various Git-related files" :type github :pkgname "magit/git-modes"))
 (haskell-mode status "installed" recipe
               (:name haskell-mode :description "A Haskell editing mode" :type github :pkgname "haskell/haskell-mode" :info "." :build
                      `(("make" ,(format "EMACS=%s" el-get-emacs)
                         "all"))
                      :post-init
                      (progn
                        (require 'haskell-mode-autoloads)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
 (highlight-indentation status "installed" recipe
                        (:name highlight-indentation :description "Function for highlighting indentation" :type git :url "https://github.com/antonj/Highlight-Indentation-for-Emacs"))
 (idomenu status "installed" recipe
          (:name idomenu :type emacswiki :description "imenu tag selection a la ido" :load-path "."))
 (iedit status "installed" recipe
        (:name iedit :description "Edit multiple regions with the same content simultaneously." :type emacswiki :features iedit))
 (jedi status "installed" recipe
       (:name jedi :description "An awesome Python auto-completion for Emacs" :type github :pkgname "tkf/emacs-jedi" :build
              (("make" "requirements"))
              :build/windows-nt
              (("make" "requirements" "PYTHON=python.exe" "BINDIR=Scripts"))
              :build/berkeley-unix
              (("gmake" "requirements"))
              :submodule nil :depends
              (epc auto-complete)))
 (js2-mode status "installed" recipe
           (:name js2-mode :website "https://github.com/mooz/js2-mode#readme" :description "An improved JavaScript editing mode" :type github :pkgname "mooz/js2-mode" :prepare
                  (autoload 'js2-mode "js2-mode" nil t)))
 (jshint-mode status "installed" recipe
              (:name jshint-mode :website "https://github.com/daleharvey/jshint-mode" :description "Integrate JSHint into Emacs via a node.js server. JSHint (http://www.jshint.com/) is a static code analysis tool for JavaScript." :type github :pkgname "daleharvey/jshint-mode"))
 (nose status "installed" recipe
       (:type github :pkgname "emacsmirror/nose" :name nose :website "https://bitbucket.org/durin42/nosemacs" :description "Emacs extension to provide easy nosetest integration." :type emacsmirror :pkgname nose))
 (paredit status "installed" recipe
          (:name paredit :description "Minor mode for editing parentheses" :type http :prepare
                 (progn
                   (autoload 'enable-paredit-mode "paredit")
                   (autoload 'disable-paredit-mode "paredit"))
                 :url "http://mumble.net/~campbell/emacs/paredit.el"))
 (pkg-info status "installed" recipe
           (:name pkg-info :description "Provide information about Emacs packages." :type github :pkgname "lunaryorn/pkg-info.el" :depends s))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (python-django status "installed" recipe
                (:name python-django :description "An Emacs package for managing Django projects" :type github :pkgname "fgallina/python-django.el"))
 (rust-mode status "installed" recipe
            (:name rust-mode :type http :url "https://raw.github.com/mozilla/rust/master/src/etc/emacs/rust-mode.el" :description "Emacs mode for Rust" :features rust-mode))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el" :features s))
 (scala-mode2 status "installed" recipe
              (:name scala-mode2 :website "https://github.com/hvesalai/scala-mode2" :description "A new scala-mode for Emacs 24" :type github :pkgname "hvesalai/scala-mode2" :load-path "." :features scala-mode2))
 (virtualenv status "installed" recipe
             (:name virtualenv :description "Virtualenv for Python" :type github :pkgname "aculich/virtualenv.el"))
 (yasnippet status "installed" recipe
            (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
                   (unless
                       (or
                        (boundp 'yas/snippet-dirs)
                        (get 'yas/snippet-dirs 'customized-value))
                     (setq yas/snippet-dirs
                           (list
                            (concat el-get-dir
                                    (file-name-as-directory "yasnippet")
                                    "snippets"))))
                   :post-init
                   (put 'yas/snippet-dirs 'standard-value
                        (list
                         (list 'quote
                               (list
                                (concat el-get-dir
                                        (file-name-as-directory "yasnippet")
                                        "snippets")))))
                   :compile nil :submodule nil)))

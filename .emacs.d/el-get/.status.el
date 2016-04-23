((4clojure status "installed" recipe
           (:name 4clojure :description "Open and evaluate 4clojure.com questions in emacs" :website "https://github.com/losingkeys/4clojure" :type github :depends
                  (json request)
                  :pkgname "losingkeys/4clojure.el"))
 (ag status "installed" recipe
     (:name ag :description "A simple ag frontend, loosely based on ack-and-half.el." :type github :pkgname "Wilfred/ag.el" :depends
            (dash s)))
 (ample-regexps status "installed" recipe
                (:name ample-regexps :description "Compose and reuse Emacs regular expressions with ease" :type github :pkgname "immerrr/ample-regexps.el"))
 (auto-complete status "installed" recipe
                (:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)
                       :features auto-complete-config :post-init
                       (progn
                         (add-to-list 'ac-dictionary-directories
                                      (expand-file-name "dict" default-directory))
                         (ac-config-default))))
 (cider status "installed" recipe
        (:name cider :description "CIDER is a Clojure IDE and REPL." :type github :pkgname "clojure-emacs/cider" :depends
               (seq queue clojure-mode pkg-info spinner)))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (clj-refactor status "installed" recipe
               (:name clj-refactor :description "A collection of simple clojure refactoring functions" :type github :depends
                      (dash s clojure-mode yasnippet paredit multiple-cursors cider edn inflections hydra)
                      :pkgname "magnars/clj-refactor.el"))
 (clojure-mode status "installed" recipe
               (:name clojure-mode :website "https://github.com/clojure-emacs/clojure-mode" :description "Emacs support for the Clojure language." :type github :pkgname "clojure-emacs/clojure-mode"))
 (ctable status "installed" recipe
         (:name ctable :description "Table Component for elisp" :type github :pkgname "kiwanami/emacs-ctable"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (deferred status "installed" recipe
   (:name deferred :description "Simple asynchronous functions for emacs lisp." :type github :pkgname "kiwanami/emacs-deferred"))
 (delight status "installed" recipe
          (:name delight :auto-generated t :type elpa :description "A dimmer switch for your lighter text." :repo nil))
 (edn status "installed" recipe
      (:name edn :description "Edn.el is an emacs lisp library for reading and writing the data format edn." :type github :depends
             (dash cl-lib s peg)
             :pkgname "expez/edn.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
                ("el-get.*\\.el$" "methods/")
                :features el-get :post-init
                (when
                    (memq 'el-get
                          (bound-and-true-p package-activated-list))
                  (message "Deleting melpa bootstrap el-get")
                  (unless package--initialized
                    (package-initialize t))
                  (when
                      (package-installed-p 'el-get)
                    (let
                        ((feats
                          (delete-dups
                           (el-get-package-features
                            (el-get-elpa-package-directory 'el-get)))))
                      (el-get-elpa-delete-package 'el-get)
                      (dolist
                          (feat feats)
                        (unload-feature feat t))))
                  (require 'el-get))))
 (elm-mode status "installed" recipe
           (:name el-get :type git :url "git://github.com/jcollard/elm-mode.git" :features elm-mode))
 (emacs-async status "installed" recipe
              (:name emacs-async :description "Simple library for asynchronous processing in Emacs" :type github :pkgname "jwiegley/emacs-async"))
 (epl status "installed" recipe
      (:name epl :description "EPL provides a convenient high-level API for various package.el versions, and aims to overcome its most striking idiocies." :type github :pkgname "cask/epl"))
 (erlang-mode status "installed" recipe
              (:name erlang-mode :description "Major mode for editing and running Erlang" :type http :url "http://www.erlang.org/download/contrib/erlang.el" :prepare
                     (progn
                       (add-to-list 'auto-mode-alist
                                    '("\\.erl$" . erlang-mode)))))
 (exec-path-from-shell status "installed" recipe
                       (:name exec-path-from-shell :website "https://github.com/purcell/exec-path-from-shell" :description "Emacs plugin for dynamic PATH loading" :type github :pkgname "purcell/exec-path-from-shell"))
 (f status "installed" recipe
    (:name f :website "https://github.com/rejeep/f.el" :description "Modern API for working with files and directories in Emacs" :depends
           (s dash)
           :type github :pkgname "rejeep/f.el"))
 (fill-column-indicator status "installed" recipe
                        (:name fill-column-indicator :type github :website "https://github.com/alpaker/Fill-Column-Indicator#readme" :description "An Emacs minor mode that graphically indicates the fill column." :pkgname "alpaker/Fill-Column-Indicator"))
 (flymake-cursor status "installed" recipe
                 (:name flymake-cursor :type github :pkgname "illusori/emacs-flymake-cursor" :description "displays flymake error msg in minibuffer after delay (illusori/github)" :website "http://github.com/illusori/emacs-flymake-cursor"))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (geiser status "installed" recipe
         (:name geiser :website "http://www.nongnu.org/geiser/" :description "Geiser is a collection of Emacs major and minor modes that conspire with one or more Scheme interpreters to keep the Lisp Machine Spirit alive. It draws inspiration (and a bit more) from environments such as Common Lisp's Slime, Factor's FUEL, Squeak or Emacs itself, and does its best to make Scheme hacking inside Emacs (even more) fun." :type git :url "git://git.sv.gnu.org/geiser.git" :load-path
                ("./elisp")
                :build
                `(("./autogen.sh")
                  ("./configure")
                  ("make" ,(format "EMACS=%s" el-get-emacs))
                  ("make" ,(format "EMACS=%s" el-get-emacs)
                   "info-recursive"))
                :build/windows-nt
                `(("sh" "./autogen.sh")
                  ("sh" "./configure")
                  ("make")
                  ("sh" "-c" ,(concat "cd doc && " el-get-install-info " --dir-file=./dir *.info")))
                :info "doc" :autoloads nil :features geiser-load))
 (ghc-mod status "installed" recipe
          (:name ghc-mod :description "Happy Haskell programming" :type github :pkgname "kazu-yamamoto/ghc-mod" :load-path "elisp"))
 (groovy-mode status "installed" recipe
              (:name groovy-mode :auto-generated t :type elpa :description "Groovy mode derived mode" :repo nil))
 (haskell-mode status "installed" recipe
               (:name haskell-mode :description "A Haskell editing mode" :type github :pkgname "haskell/haskell-mode" :info "." :build
                      `(("make" ,(format "EMACS=%s" el-get-emacs)
                         "all"))
                      :post-init
                      (progn
                        (require 'haskell-mode-autoloads)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
 (hydra status "installed" recipe
        (:name hydra :description "make Emacs bindings that stick around" :type github :depends
               (cl-lib)
               :pkgname "abo-abo/hydra"))
 (ido-ubiquitous status "installed" recipe
                 (:name ido-ubiquitous :description "Use ido (nearly) everywhere" :website "https://github.com/DarwinAwardWinner/ido-ubiquitous" :type github :depends
                        (cl-lib s)
                        :pkgname "DarwinAwardWinner/ido-ubiquitous"))
 (inflections status "installed" recipe
              (:name inflections :description "Convert english words between singular and plural" :type elpa))
 (js2-mode status "installed" recipe
           (:name js2-mode :website "https://github.com/mooz/js2-mode#readme" :description "An improved JavaScript editing mode" :type github :pkgname "mooz/js2-mode" :prepare
                  (autoload 'js2-mode "js2-mode" nil t)))
 (json status "installed" recipe
       (:name json :description "JavaScript Object Notation parser / generator" :type http :builtin "23" :url "http://edward.oconnor.cx/elisp/json.el"))
 (ledger-mode status "installed" recipe
              (:name ledger-mode :description "A major mode for editing ledger .dat files" :type github :pkgname "ledger/ledger" :checkout "v3.1" :load-path "lisp" :prepare
                     (progn
                       (add-to-list 'auto-mode-alist
                                    '("\\.dat$" . ledger-mode)))))
 (let-alist status "installed" recipe
            (:name let-alist :description "Easily let-bind values of an assoc-list by their names." :builtin "25.0.50" :type elpa :url "https://elpa.gnu.org/packages/let-alist.html"))
 (linum-relative status "installed" recipe
                 (:name linum-relative :type emacswiki :description "Display relative line number in the left margin" :features linum-relative))
 (lua-mode status "installed" recipe
           (:name lua-mode :description "A major-mode for editing Lua scripts" :depends
                  (ample-regexps)
                  :type github :pkgname "immerrr/lua-mode"))
 (magit status "installed" recipe
        (:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :branch "master" :minimum-emacs-version "24.4" :depends
               (dash with-editor emacs-async)
               :info "Documentation" :load-path "lisp/" :compile "lisp/" :build
               `(("make" ,(format "EMACSBIN=%s" el-get-emacs)
                  "docs")
                 ("touch" "lisp/magit-autoloads.el"))
               :build/berkeley-unix
               `(("gmake" ,(format "EMACSBIN=%s" el-get-emacs)
                  "docs")
                 ("touch" "lisp/magit-autoloads.el"))
               :build/windows-nt
               (with-temp-file "lisp/magit-autoloads.el" nil)))
 (makey status "installed" recipe
        (:name makey :description "Interactive commandline mode" :type github :pkgname "mickeynp/makey"))
 (markdown-mode status "installed" recipe
                (:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :website "http://jblevins.org/projects/markdown-mode/" :type github :pkgname "jrblevin/markdown-mode" :prepare
                       (add-to-list 'auto-mode-alist
                                    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (multiple-cursors status "installed" recipe
                   (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el"))
 (neotree status "installed" recipe
          (:name neotree :website "https://github.com/jaypei/emacs-neotree" :description "An Emacs tree plugin like NerdTree for Vim." :type github :branch "master" :pkgname "jaypei/emacs-neotree"))
 (org-mode status "installed" recipe
           (:name org-mode :website "http://orgmode.org/" :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system." :type git :url "git://orgmode.org/org-mode.git" :info "doc" :build/berkeley-unix `,(mapcar
                                                                                                                                                                                                                                                                                                       (lambda
                                                                                                                                                                                                                                                                                                         (target)
                                                                                                                                                                                                                                                                                                         (list "gmake" target
                                                                                                                                                                                                                                                                                                               (concat "EMACS="
                                                                                                                                                                                                                                                                                                                       (shell-quote-argument el-get-emacs))))
                                                                                                                                                                                                                                                                                                       '("oldorg"))
                  :build `,(mapcar
                            (lambda
                              (target)
                              (list "make" target
                                    (concat "EMACS="
                                            (shell-quote-argument el-get-emacs))))
                            '("oldorg"))
                  :load-path
                  ("." "contrib/lisp" "lisp")
                  :load
                  ("lisp/org-loaddefs.el")))
 (package status "installed" recipe
          (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin "24" :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
                 (progn
                   (let
                       ((old-package-user-dir
                         (expand-file-name
                          (convert-standard-filename
                           (concat
                            (file-name-as-directory default-directory)
                            "elpa")))))
                     (when
                         (file-directory-p old-package-user-dir)
                       (add-to-list 'package-directory-list old-package-user-dir)))
                   (setq package-archives
                         (bound-and-true-p package-archives))
                   (mapc
                    (lambda
                      (pa)
                      (add-to-list 'package-archives pa 'append))
                    '(("ELPA" . "http://tromey.com/elpa/")
                      ("melpa" . "http://melpa.org/packages/")
                      ("gnu" . "http://elpa.gnu.org/packages/")
                      ("marmalade" . "http://marmalade-repo.org/packages/")
                      ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (paredit status "installed" recipe
          (:name paredit :description "Minor mode for editing parentheses" :type github :prepare
                 (progn
                   (autoload 'enable-paredit-mode "paredit")
                   (autoload 'disable-paredit-mode "paredit"))
                 :pkgname "emacsmirror/paredit"))
 (peg status "installed" recipe
      (:name peg :type emacswiki :description "Parsing Expression Grammars in Emacs Lisp" :website "http://www.emacswiki.org/emacs/download/peg.el"))
 (pkg-info status "installed" recipe
           (:name pkg-info :description "Provide information about Emacs packages." :type github :pkgname "lunaryorn/pkg-info.el" :depends
                  (dash epl)))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :submodule nil :depends cl-lib :pkgname "auto-complete/popup-el"))
 (queue status "installed" recipe
        (:name queue :description "Queue data structure" :type elpa))
 (request status "installed" recipe
          (:name request :description "Easy HTTP request for Emacs Lisp" :type github :submodule nil :pkgname "tkf/emacs-request" :depends
                 (deferred)))
 (rust-mode status "installed" recipe
            (:name rust-mode :type github :pkgname "rust-lang/rust-mode" :description "Emacs mode for Rust"))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el"))
 (seq status "installed" recipe
      (:name seq :description "Sequence manipulation library for Emacs" :builtin "25" :type github :pkgname "NicolasPetton/seq.el"))
 (spinner status "installed" recipe
          (:name spinner :description "Emacs mode-line spinner for operations in progress." :type github :pkgname "Bruce-Connor/spinner.el"))
 (twittering-mode status "installed" recipe
                  (:name twittering-mode :description "Major mode for Twitter" :type github :pkgname "hayamiz/twittering-mode" :features twittering-mode :compile "twittering-mode.el"))
 (with-editor status "installed" recipe
   (:name with-editor :description "Use the Emacsclient as $EDITOR" :type github :pkgname "magit/with-editor"))
 (xml-rpc status "installed" recipe
          (:name xml-rpc :auto-generated t :type elpa :description "An elisp implementation of clientside XML-RPC" :repo nil))
 (yaml-mode status "installed" recipe
            (:name yaml-mode :description "Simple major mode to edit YAML file for emacs" :type github :pkgname "yoshiki/yaml-mode"))
 (yasnippet status "installed" recipe
            (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :compile "yasnippet.el" :submodule nil :build
                   (("git" "submodule" "update" "--init" "--" "snippets")))))

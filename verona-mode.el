;;; verona-mode.el --- A major mode for the Verona programming language
;;
;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; URL: https://github.com/damon-kwok/verona-mode
;; Keywords: languages programming
;; Package-Requires: ((emacs "25.1") (dash "2.17.0") (hydra "0.15.0") (hl-todo "3.1.2") (yafolding "0.4.1") (yasnippet "0.14.0") (rainbow-delimiters "2.1.4") (fill-column-indicator "1.90"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2020 Damon Kwok
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a major mode for the Verona programming language
;;
;; For more details, see the project page at
;; https://github.com/damon-kwok/verona-mode
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install verona-mode
;;
;; Or, copy verona-mode.el to some location in your Emacs load
;; path.  Then add "(require 'verona-mode)" to your Emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'verona-mode)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cl-lib)
(require 'js)
(require 'dash)
(require 'xref)
(require 'hydra)
(require 'imenu)
(require 'hl-todo)
(require 'easymenu)
(require 'yafolding)
(require 'yasnippet)
(require 'whitespace)
(require 'rainbow-delimiters)
(require 'fill-column-indicator)

(defvar verona-mode-hook nil)

(defcustom verona-indent-trigger-commands ;
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `verona-indent-line' call."
  :type '(repeat symbol)
  :group 'verona)

(defconst verona-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; fontify " using verona-keywords

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments, which can be nested
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; string
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; Don't treat underscores as whitespace
    (modify-syntax-entry ?_ "w" table) table))

(defvar verona-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;; (define-key map (kbd "<C-return>") 'yafolding-toggle-element) ;
    map)
  "Keymap for Verona major mode.")

(defconst verona-keywords
  '("module" "type" "class"                        ;
     "if" "else" "while" "for" "in" "match" "when" ;
     "where" "static" "private"                    ;
     "break" "continue" "return" "yield"           ;
     "new"                                         ;
     "var" "let")
  "Verona language keywords.")

(defconst verona-declaration-keywords   ;
  '("module" "type" "class")
  "Verona declaration keywords.")

(defconst verona-preprocessor-keywords '("imports")
  "Verona preprocessor keywords.")

(defconst verona-careful-keywords
  '("builtin" "create" "new" "where" "static" "private" ;
     "break" "continue" "return" "yield"                ;
     "cown")
  "Verona language careful keywords.")

(defconst verona-builtin-keywords '("imm" "mut" "iso")
  "Verona language keywords.")

(defconst verona-constants              ;
  '("false" "true")
  "Common constants.")

(defconst verona-operator-functions
  '("add" "sub" "mul" "div" "mod" "shl" "shr" ;
     "lt" "gt" "le" "ge" "eq" "ne" "and" "or")
  "Verona language operators functions.")

;; create the regex string for each class of keywords

(defconst verona-keywords-regexp (regexp-opt verona-keywords 'words)
  "Regular expression for matching keywords.")

(defconst verona-declaration-keywords-regexp ;
  (regexp-opt verona-declaration-keywords 'words)
  "Regular expression for matching declaration keywords.")

(defconst verona-preprocessor-keywords-regexp ;
  (regexp-opt verona-preprocessor-keywords 'words)
  "Regular expression for matching preprocessor keywords.")

(defconst verona-careful-keywords-regexp ;
  (regexp-opt verona-careful-keywords 'words)
  "Regular expression for matching careful keywords.")

(defconst verona-builtin-keywords-regexp (regexp-opt verona-builtin-keywords 'words)
  "Regular expression for matching builtin type.")

(defconst verona-constant-regexp        ;
  (regexp-opt verona-constants 'words)
  "Regular expression for matching constants.")

(defconst verona-operator-functions-regexp ;
  (regexp-opt verona-operator-functions 'words)
  "Regular expression for matching operator functions.")

(defconst verona-font-lock-keywords
  `(
     ;; builtin
     (,verona-builtin-keywords-regexp . font-lock-builtin-face)

     ;; careful
     (,verona-careful-keywords-regexp . font-lock-warning-face)

     ;; declaration
     (,verona-declaration-keywords-regexp . font-lock-keyword-face)

     ;; preprocessor
     (,verona-preprocessor-keywords-regexp . font-lock-preprocessor-face)

     ;; delimiter: modifier
     ("\\(->\\|=>\\|\\.>\\|:>\\|:=\\|\\.\\.\\||\\)" 1 'font-lock-keyword-face)

     ;; delimiter: . , ; separate
     ("\\($?[.,;]+\\)" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: operator symbols
     ("\\($?[+-/*//%~=<>]+\\)$?,?" 1 'font-lock-negation-char-face)
     ("\\($?[?^!&]+\\)" 1 'font-lock-warning-face)

     ;; delimiter: = : separate
     ("[^+-/*//%~^!=<>]\\([=:]\\)[^+-/*//%~^!=<>]" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: brackets
     ("\\(\\[\\|\\]\\|[(){}]\\)" 1 'font-lock-comment-delimiter-face)

     ;; operator methods
     (,verona-operator-functions-regexp . font-lock-builtin-face)

     ;; macro
     ("#\\(?:include\\|flag\\)" . 'font-lock-builtin-face)

     ;; type
     ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)

     ;; method
     ("\\(?:builtin\s+\\)*\\([a-z_]$?[a-z0-9_]?+\\)[ \t]*\\(.*\\)(" 1 'font-lock-function-name-face)

     ;; constants references
     (,verona-constant-regexp . font-lock-constant-face)

     ;; @
     ("@[A-Za-z_]*[A-Z-a-z0-9_]*" . 'font-lock-builtin-face)

     ;; parameter
     ("\\(?:(\\|,\\)\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1 'font-lock-variable-name-face)
     ("\\(?:(\\|,\\)[ \t]+\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1
       'font-lock-variable-name-face)

     ;; tuple references
     ("[.]$?[ \t]?\\($?_[1-9]$?[0-9]?*\\)" 1 'font-lock-variable-name-face)

     ;; keywords
     (,verona-keywords-regexp . font-lock-keyword-face) ;;font-lock-keyword-face

     ;; character literals
     ("\\('[\\].'\\)" 1 'font-lock-constant-face)

     ;; numeric literals
     ("[ \t/+-/*//=><([,;]\\([0-9]+[0-9a-zA-Z_]*\\)+" 1 'font-lock-constant-face)

     ;; variable references
     ("\\([a-z_]+[a-z0-9_']*\\)+" 1 'font-lock-variable-name-face))
  "An alist mapping regexes to font-lock faces.")

(defun verona-project-root-p (PATH)
  "Return t if directory `PATH' is the root of the Verona project."
  (let* ((files '("CMakeLists.txt" "make.bat" "Makefile" ;
                   "Dockerfile" ".editorconfig" ".gitignore"))
          (foundp nil))
    (while (and (> (length files) 0)
             (not foundp))
      (let* ((filename (car files))
              (filepath (concat (file-name-as-directory PATH) filename)))
        (setq files (cdr files))
        (setq foundp (file-exists-p filepath)))) ;
    foundp))

(defun verona-project-root
  (&optional
    PATH)
  "Return the root of the Verona project.
Optional argument PATH: project path."
  (let* ((bufdir (if buffer-file-name   ;
                   (file-name-directory buffer-file-name) default-directory))
          (curdir (if PATH (file-name-as-directory PATH) bufdir))
          (parent (file-name-directory (directory-file-name curdir))))
    (if (or (not parent)
          (string= parent curdir)
          (string= parent "/")
          (verona-project-root-p curdir)) ;
      curdir                              ;
      (verona-project-root parent))))

(defun verona-project-name ()
  "Return Verona project name."
  (file-name-base (directory-file-name (verona-project-root))))

(defun verona-project-file-exists-p (FILENAME)
  "Return t if file `FILENAME' exists."
  (file-exists-p (concat (verona-project-root) FILENAME)))

(defun verona-run-command (COMMAND &optional PATH)
  "Return `COMMAND' in the root of the Verona project.
Optional argument PATH: project path."
  (setq default-directory (if PATH PATH (verona-project-root PATH)))
  (compile COMMAND))

(defun verona-project-build ()
  "Build project with veronac."
  (interactive)
  (if (verona-project-file-exists-p "Makefile")
    (verona-run-command "make")
    (verona-run-command "veronac .")))

(defun verona-project-open ()
  "Open `Makefile' file."
  (interactive)
  (if (verona-project-file-exists-p "Makefile")
    (find-file (concat (verona-project-root) "Makefile"))))

(defun verona-buffer-dirname ()
  "Return current buffer directory file name."
  (directory-file-name (if buffer-file-name (file-name-directory buffer-file-name)
                         default-directory)))

(defun verona-project-run ()
  "Run project."
  (interactive)
  (let* ((bin1 (concat (verona-project-root) "bin/" (verona-project-name)))
          (bin2 (concat (verona-project-root) "/" (verona-project-name)))
          (bin3 (concat (verona-buffer-dirname) "/" (verona-project-name))))
    (if (file-exists-p bin1)
      (verona-run-command bin1)
      (if (file-exists-p bin2)
        (verona-run-command bin2)
        (if (file-exists-p bin3)
          (verona-run-command bin3))))))

(easy-menu-define verona-mode-menu verona-mode-map ;
  "Menu for Verona mode."                          ;
  '("Verona"                                       ;
     ["Build" verona-project-build t]              ;
     ["Run" verona-project-run t]                  ;
     "---"                                         ;
     ("Community"                                  ;
       ["Home" (verona-run-command "xdg-open https://github.com/microsoft/verona") t]
       ["Contribute" ;;
         (verona-run-command
           "xdg-open https://github.com/microsoft/verona/blob/master/CONTRIBUTING.md") t])))

(defun verona-banner-default ()
  "Verona banner."
  "
  __   _____ _ __ ___  _ __   __ _
  \\ \\ / / _ \\ '__/ _ \\| '_ \\ / _` |
   \\ V /  __/ | | (_) | | | | (_| |
    \\_/ \\___|_|  \\___/|_| |_|\\__,_|
")

(defhydra verona-hydra-menu
  (:color blue
    :hint none)
  "
%s(verona-banner-default)
  Project     |  _b_: Build     _r_: Run
  _q_: Quit"                            ;
  ("b" verona-project-build "Build")
  ("r" verona-project-run "Run")
  ("q" nil "Quit"))

(defun verona-menu ()
  "Open verona hydra menu."
  (interactive)
  (verona-hydra-menu/body))

(defun verona-folding-hide-element
  (&optional
    RETRY)
  "Hide current element.
Optional argument RETRY."
  (interactive)
  (let* ((region (yafolding-get-element-region))
          (beg (car region))
          (end (cadr region)))
    (if (and (eq RETRY nil)
          (= beg end))
      (progn (yafolding-go-parent-element)
        (verona-folding-hide-element t))
      (yafolding-hide-region beg end))))

(defun verona-build-tags ()
  "Build tags for current project."
  (interactive)
  (let ((tags-buffer (get-buffer "TAGS"))
         (tags-buffer2 (get-buffer (format "TAGS<%s>" (verona-project-name)))))
    (if tags-buffer (kill-buffer tags-buffer))
    (if tags-buffer2 (kill-buffer tags-buffer2)))
  (let* ((verona-path (string-trim (shell-command-to-string "which veronac")))
          (verona-executable (string-trim (shell-command-to-string (concat "readlink -f "
                                                                     verona-path))))
          (packages-path (concat (file-name-directory verona-executable) "../stdlib"))
          (ctags-params                 ;
            (concat  "ctags --languages=-v --langdef=v --langmap=v:.v "
              "--regex-v=/[ \\t]*builtin[ \\t]+([a-zA-Z0-9_]+)/\\1/m,method/ "
              "--regex-v=/[ \\t]*create[ \\t]+([a-zA-Z0-9_]+)/\\1/n,constructor/ "
              "--regex-v=/^[ \\t]*class[ \\t]+([a-zA-Z0-9_]+)/\\1/c,class/ " ;
              "-e -R . " packages-path)))
    (if (file-exists-p packages-path)
      (progn
        (setq default-directory (verona-project-root))
        (let (result (shell-command-to-string ctags-params))
          (if (not (eq "" result))
            (message "ctags:%s" result)))
        (verona-load-tags)))))

(defun verona-load-tags
  (&optional
    BUILD)
  "Visit tags table.
Optional argument BUILD If the tags file does not exist, execute the build."
  (interactive)
  (let* ((tags-file (concat (verona-project-root) "TAGS")))
    (if (file-exists-p tags-file)
      (progn (visit-tags-table (concat (verona-project-root) "TAGS")))
      (if BUILD (verona-build-tags)))))

(defun verona-after-save-hook ()
  "After save hook."
  (shell-command (concat  "veronac fmt " (buffer-file-name)))
  (revert-buffer
    :ignore-auto
    :noconfirm)
  (if (not (executable-find "ctags"))
    (message "Could not locate executable '%s'" "ctags")
    (verona-build-tags)))

(defalias 'verona-parent-mode
                                        ;
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode verona-mode verona-parent-mode
  "Verona"
  "Major mode for editing Verona files."
  :syntax-table verona-mode-syntax-table
  (setq bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-start "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local indent-line-function 'js-indent-line)
  (setq-local js-indent-level 2)

  ;; (setq-local font-lock-defaults        ;
  ;; '(verona-font-lock-keywords ;
  ;; nil nil nil nil         ;
  ;; (font-lock-syntactic-face-function . verona-mode-syntactic-face-function)))
  (setq-local font-lock-defaults '(verona-font-lock-keywords))
  (font-lock-ensure)

  ;; (setq-local syntax-propertize-function verona-syntax-propertize-function)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local buffer-file-coding-system 'utf-8-unix)
  ;;
  (hl-todo-mode)
  (setq-local hl-todo-keyword-faces ;;
    '(("TODO" . "green")
       ("FIXME" . "yellow")
       ("DEBUG" . "DarkCyan")
       ("GOTCHA" . "red")
       ("STUB" . "DarkGreen")))
  (whitespace-mode)
  (setq-local whitespace-style ;;
    '(face spaces tabs newline space-mark tab-mark newline-mark trailing))
  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq-local whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183]
        [46])         ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (newline-mark 10 [182 10])       ; LINE FEED,
       (tab-mark 9 [9655 9]
         [92 9])))

  ;; (setq-local whitespace-style '(face trailing))
  (setq-local fci-rule-column 80)
  (setq-local fci-handle-truncate-lines nil)
  (setq-local fci-rule-width 1)
  (setq-local fci-rule-color "grey30")
  ;;
  (rainbow-delimiters-mode t)
  ;;
  ;; (defalias 'yafolding-hide-element 'verona-folding-hide-element)
  (yafolding-mode t)
  ;;
  (setq-local imenu-generic-expression ;;
    '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
       ("method" "[ \t]*builtin[ \t]+\\([a-z0-9_]+\\)[ \t]*" 1)
       ("constructor" "[ \t]+\\(create\\)[ \t]*" 1)
       ("class" "^[ \t]*class[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("module" "[ \t]*module[ \t]+\\([a-zA-Z0-9_]+\\)" 1)))
  (imenu-add-to-menubar "Index")
  ;;
  (add-hook 'after-save-hook 'verona-after-save-hook nil t)
  (verona-load-tags))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verona\\'" . verona-mode))

;;
(provide 'verona-mode)

;;; verona-mode.el ends here

;;; tera.el --- Major mode for Tera templating engine

;; Copyright (c) 2011-2014 paradoxxxzero
;; Copyright (c) 2020 vednoc

;; Author: vednoc
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Entirety of the code is taken from `jinja2-mode' since Tera templating
;; engine uses very similar syntax with a few differences. Kudos to
;; `paradoxxxzero' and `jinja2-mode' contributors for their work.
;; Forked from: <https://github.com/paradoxxxzero/jinja2-mode>

;;; Code:

(require 'sgml-mode)

(defgroup tera nil
  "Major mode for editing Tera files."
  :prefix "tera-"
  :group 'languages)

(defcustom tera-user-keywords nil
  "Custom keyword names."
  :type '(repeat string)
  :group 'tera)

(defcustom tera-user-functions nil
  "Custom function names."
  :type '(repeat string)
  :group 'tera)

(defun tera-closing-keywords ()
  (append
   tera-user-keywords
   '("if" "for" "block" "filter" "raw" "macro")))

(defun tera-indenting-keywords ()
  (append
   (tera-closing-keywords)
   '("else" "elif")))

(defun tera-builtin-keywords ()
  '(
    "get_url" "loop" "last" "super" "none"
    "and" "or" "not" "in" "as" "include"
    "set" "set_global" "break" "continue"
    "true" "false" "import" "extends" "from"))

(defun tera-function-keywords ()
  (append
   tera-user-keywords
   '("lower" "upper" "capitalize" "slugify"
     "replace" "range" "addslashes" "title"
     "length" "odd" "even" "trim" "truncate"
     "first" "nth" "last" "join" "reverse"
     "sort" "striptags" "unique" "slice"
     "group_by" "map" "concat" "urlencode"
     "pluralize" "round" "filesizeformat"
     "date" "escape" "escape_xml" "safe"
     "get" "split" "int" "float" "json_encode"
     "as_str" "default" "defined" "undefined"
     "string" "number" "divisibleby" "iterable"
     "object" "starting_with" "ending_with"
     "containing" "matching" "now" "throw"
     "get_random" "get_env" "wordcount"
     )))

(defun tera-find-open-tag ()
  (if (search-backward-regexp
       (rx-to-string
        `(and "{%"
              (? "-")
              (* whitespace)
              (? (group "end"))
              (group
               ,(append '(or)
                        (tera-closing-keywords)))
              (group
               (*? anything))
              (* whitespace)
              (? "-")
              "%}")) nil t)
      (if (match-string 1) ;; End tag, going on
          (let ((matches (tera-find-open-tag)))
            (if (string= (car matches) (match-string 2))
                (tera-find-open-tag)
              (list (match-string 2) (match-string 3))))
        (list (match-string 2) (match-string 3)))
    nil))

(defun tera-close-tag ()
  "Close the previously opened template tag."
  (interactive)
  (let ((open-tag (save-excursion (tera-find-open-tag))))
    (if open-tag
        (insert
         (if (string= (car open-tag) "block")
             (format "{%% end%s%s %%}"
                     (car open-tag) (nth 1 open-tag))
           (format "{%% end%s %%}"
                   (match-string 2))))
      (error "Nothing to close")))
  (save-excursion (tera-indent-line)))

(defun tera-insert-tag ()
  "Insert an empty tag."
  (interactive)
  (insert "{% ")
  (save-excursion
    (insert " %}")
    (tera-indent-line)))

(defun tera-insert-var ()
  "Insert an empty variable."
  (interactive)
  (insert "{{ ")
  (save-excursion
    (insert " }}")
    (tera-indent-line)))

(defun tera-insert-comment ()
  "Insert an empty comment."
  (interactive)
  (insert "{# ")
  (save-excursion
    (insert " #}")
    (tera-indent-line)))

(defconst tera-font-lock-comments
  `(
    (,(rx "{#"
          (* whitespace)
          (group
           (*? anything))
          (* whitespace)
          "#}")
     . (1 font-lock-comment-face t))))

(defconst tera-font-lock-keywords-1
  (append
   tera-font-lock-comments
   sgml-font-lock-keywords-1))

(defconst tera-font-lock-keywords-2
  (append
   tera-font-lock-keywords-1
   sgml-font-lock-keywords-2))

(defconst tera-font-lock-keywords-3
  (append
   tera-font-lock-keywords-1
   tera-font-lock-keywords-2
   `(
     (,(rx "{{"
           (* whitespace)
           (group
            (*? anything))
           (*
            "|" (* whitespace) (*? anything))
           (* whitespace)
           "}}") (1 font-lock-variable-name-face t))
     (,(rx (group "|" (* whitespace))
           (group (+ word)))
      (1 font-lock-keyword-face t)
      (2 font-lock-warning-face t))
     (,(rx-to-string `(and (group "|" (* whitespace))
                           (group
                            ,(append '(or)
                                     (tera-function-keywords)))))
      (1 font-lock-keyword-face t)
      (2 font-lock-function-name-face t))
     (,(rx-to-string `(and word-start
                           (? "end")
                           ,(append '(or)
                                    (tera-indenting-keywords))
                           word-end)) (0 font-lock-keyword-face))
     (,(rx-to-string `(and word-start
                           ,(append '(or)
                                    (tera-builtin-keywords))
                           word-end)) (0 font-lock-builtin-face))
     (,(rx (or "{%" "%}" "{%-" "-%}")) (0 font-lock-function-name-face t))
     (,(rx (or "{{" "}}")) (0 font-lock-type-face t))
     (,(rx "{#"
           (* whitespace)
           (group
            (*? anything))
           (* whitespace)
           "#}")
      (1 font-lock-comment-face t))
     (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
     )))

(defvar tera-font-lock-keywords
  tera-font-lock-keywords-1)

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (sgml-calculate-indent))))
    (if (null indent-col)
        0
      (if savep
          (save-excursion indent-col)
        indent-col))))

(defun tera-calculate-indent-backward (default)
  "Return indent column based on previous lines"
  (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
    (forward-line -1)
    (if (looking-at "^[ \t]*{%-? *end") ; Don't indent after end
        (current-indentation)
      (if (looking-at (concat "^[ \t]*{%-? *.*?{%-? *end" (regexp-opt (tera-indenting-keywords))))
          (current-indentation)
        (if (looking-at (concat "^[ \t]*{%-? *" (regexp-opt (tera-indenting-keywords)))) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (tera-calculate-indent-backward default))))))))


(defun tera-calculate-indent ()
  "Return indent column"
  (if (bobp)  ; Check beginning of buffer
      0
    (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
      (if (looking-at "^[ \t]*{%-? *e\\(nd\\|lse\\|lif\\)") ; Check close tag
          (save-excursion
            (forward-line -1)
            (if
                (and
                 (looking-at (concat "^[ \t]*{%-? *" (regexp-opt (tera-indenting-keywords))))
                 (not (looking-at (concat "^[ \t]*{%-? *.*?{% *end" (regexp-opt (tera-indenting-keywords))))))
                (current-indentation)
              (- (current-indentation) indent-width)))
        (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
            default
          (save-excursion
            (tera-calculate-indent-backward default)))))))

(defun tera-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((old_indent (current-indentation)) (old_point (point)))
    (move-beginning-of-line nil)
    (let ((indent (max 0 (tera-calculate-indent))))
      (indent-line-to indent)
      (if (< old_indent (- old_point (line-beginning-position)))
          (goto-char (+ (- indent old_indent) old_point)))
      indent)))

(defun tera-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (when (eq major-mode 'tera-mode)
    (save-excursion
      (indent-region (point-min) (point-max)))))

;;;###autoload
(define-derived-mode tera-mode html-mode "Tera"
  "Major mode for editing Tera files."
  :group 'tera
  ;; Disabling this because of this emacs bug:
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2002-09/msg00041.html
  ;; (modify-syntax-entry ?\'  "\"" sgml-mode-syntax-table)
  (set (make-local-variable 'comment-start) "{#")
  (set (make-local-variable 'comment-start-skip) "{#")
  (set (make-local-variable 'comment-end) "#}")
  (set (make-local-variable 'comment-end-skip) "#}")
  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '((tera-font-lock-keywords
          tera-font-lock-keywords-1
          tera-font-lock-keywords-2
          tera-font-lock-keywords-3)
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords)))
  (set (make-local-variable 'indent-line-function) 'tera-indent-line))

(define-key tera-mode-map (kbd "C-c c") 'tera-close-tag)
(define-key tera-mode-map (kbd "C-c t") 'tera-insert-tag)
(define-key tera-mode-map (kbd "C-c v") 'tera-insert-var)
(define-key tera-mode-map (kbd "C-c #") 'tera-insert-comment)

(add-to-list 'auto-mode-alist '("templates/.*\\.html\\'" . tera-mode))
(add-to-list 'auto-mode-alist '("\\.tera\\'" . tera-mode))
(add-hook 'before-save-hook #'tera-indent-buffer)

(provide 'tera)
;;; tera.el ends here

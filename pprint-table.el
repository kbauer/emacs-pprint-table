;; -*- mode:emacs-lisp;coding:utf-8-unix;lexical-binding:t;byte-compile-dynamic:t; -*-

(require 'cl-macs)


(defun pprint-table--nil-terminated-list-p (obj)
  (while (consp obj) (ignore (pop obj)))
  (null obj))


(defun pprint-table--sequence-p (obj)
  "`vectorp' or `listp'"
  (or (pprint-table--nil-terminated-list-p obj) 
      (vectorp obj)))


(defun pprint-table--cell-to-string (cell-data)
  (let ((print-escape-newlines t))
    (format "%S" cell-data)))


(defun pprint-table--row-to-strings (row)
  (cond ((vectorp row)
         (apply #'vector (mapcar #'pprint-table--cell-to-string row)))
        ((pprint-table--nil-terminated-list-p row)
         (mapcar #'pprint-table--cell-to-string row))
        (t (pprint-table--cell-to-string row))))


(defun pprint-table--table-to-strings (table)
  (cond ((pprint-table--sequence-p table) 
         (mapcar #'pprint-table--row-to-strings table))
        (t (signal 'wrong-type-argument (list #'pprint-table--sequence-p table)))))


(defun pprint-table--row-get-col-widths (row-as-strings)
  (cond ((stringp row-as-strings) nil)
        (t (cl-coerce (mapcar #'length row-as-strings) 'list))))


(defun pprint-table--table-get-col-widths (table-as-strings)
  (let* ((row-wise-widths 
          (mapcar #'pprint-table--row-get-col-widths 
                  table-as-strings))
         (numcols (cl-loop for widths in (cons nil row-wise-widths)
                           maximize (length widths))))
    (cl-loop for w-list-1 in row-wise-widths
             for w-list 
             = (append w-list-1
                       (make-list (- numcols (length w-list-1)) 0))
             with colwidths = (make-list numcols 0) do
             (cl-loop for w in w-list 
                      for index upfrom 0
                      if (> w (nth index colwidths))
                      do (setf (nth index colwidths) w))
             finally return colwidths)))


(defun pprint-table--row-to-string (row-as-strings col-widths)
  (let ((--error-- 
         (lambda ()
           (signal 'wrong-type-argument
                   (list #'pprint-table--sequence-p row-as-strings)))))
    (apply #'concat
           `(,(cond ((vectorp row-as-strings) "[")
                    ((listp row-as-strings) "(")
                    ((stringp row-as-strings) "")
                    (t (funcall --error--)))
             ,@(cond ((stringp row-as-strings)
                      (list row-as-strings))
                     (t (cl-loop 
                         with row-as-vec = (cl-coerce row-as-strings 'vector)
                         for i upfrom 0
                         for rest on col-widths 
                         for w = (car rest)
                         for s = (if (< i (length row-as-vec))
                                     (aref row-as-vec i)
                                   "")
                         collect s
                         if (cdr rest)
                         collect (make-string (- (1+ w) (length s)) ? ))))
             ,(cond ((vectorp row-as-strings) "]")
                    ((listp row-as-strings) ")")
                    ((stringp row-as-strings) "")
                    (t (funcall --error--)))))))


(defun pprint-table-to-string (table)
  "Like `pprint-table', but returns the table sexp as a string
instead of printing."
  (if (pprint-table--sequence-p table)
      (let* ((table-as-strings (pprint-table--table-to-strings table))
             (col-widths (pprint-table--table-get-col-widths table-as-strings))
             (--error-- 
              (lambda ()
                (signal 'wrong-type-argument
                        (list #'pprint-table--sequence-p table)))))
        (concat 
         (cond ((vectorp table) "[")
               ((listp table) "(")
               (t (funcall --error--)))
         (mapconcat 
          (lambda (row-as-strings)
            (pprint-table--row-to-string row-as-strings col-widths))
          table-as-strings "\n ")
         (cond ((vectorp table) "]")
               ((listp table) ")")
               (t (funcall --error--)))))
    (pprint-table--cell-to-string table)))


(defun pprint-table-display (table &optional buffer)
  "Replaces contents of BUFFER by tabularized TABLE 
and displays BUFFER.

The format of TABLE and displayed output is as described in
`pprint-table'.

BUFFER
   A buffer or buffer name. The buffer will be cleared before
   inserting the table. Defaults to \"*pprint-table*\".

This fuction is primarily meant to by used together with
\\[eval-expression].

Interactively, evaluates the sexp near point as value of TABLE.
With prefix argument, the sexp is not evaluated.
"
   (interactive 
    (let*((sexp (or (sexp-at-point)
                    (save-excursion
                      (backward-sexp)
                      (sexp-at-point))))
          (value (if (not current-prefix-arg) (eval sexp) sexp)))
      (list value nil)))
  (unless (pprint-table--sequence-p table)
    (setq table 
      (list "pprint-table-display: Input was not a table" table)))
  (with-selected-window (display-buffer 
                          (get-buffer-create 
                            (or buffer "*pprint-table*")))
    (erase-buffer)
    (toggle-truncate-lines +1)
    (pprint-table table (current-buffer))
    (unless (eq major-mode 'emacs-lisp-mode)
      (emacs-lisp-mode))
    (goto-char (point-min))))


(defun pprint-table (table &optional printcharfun)
  "Print TABLE in a tabular with PRINTCHARFUN as used by `print'.

TABLE must be a vector or list, whose elements we shall refer to
as the ROWSs of the table.

If ROW is a vector or list, its items will be printed in a
tabulated manner, such that elements with equal index have the
same indentation. 

If ROW is anything else, it will be printed without alignment.

For example applying `pprint-table' to

\(this-is-a-long-symbol
 \"and this a long string\"
 (1 2 3 4 5 6)
 [11 12 13 14 15]
 (foo bar baz)
 (m o r e items)
 (less items))

will be printed as 

\(this-is-a-long-symbol
 \"and this a long string\"
 (1    2     3   4  5     6 )
 [11   12    13  14 15      ]
 (foo  bar   baz            )
 (m    o     r   e  items   )
 (less items                ))

Interactively, the sexp at or before point will be used as TABLE,
the result will be displayed in a buffer `*pprint-table*' and if
a prefix argument is given the sexp will be evaluated before
producing the table. 

See also 
   `pprint-table-to-string'
   `pprint-table-display'"
  (princ (concat (pprint-table-to-string table) "\n") printcharfun))

;; '(1 (1 2 3) ("1" "2" "3") [foo bar baz more stuff] "a string is just printed")

(provide 'pprint-table)

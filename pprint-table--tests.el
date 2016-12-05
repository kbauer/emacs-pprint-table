;; -*- mode:emacs-lisp;coding:utf-8-unix;lexical-binding:t;byte-compile-dynamic:t; -*-

(require 'pprint-table)
(require 'ert)


(ert-deftest pprint-table-to-string-rereadable ()
  "Test if printed table can be re-read to give the original value."
  (let ((reread-equal
         (lambda (sexp)
           (let ((re-read (read (pprint-table-to-string sexp))))
             (if (equal re-read sexp) t
               (signal 'error 
                       (list "Reread sexp didn't match sexp"
                             (list 'original sexp)
                             (list 'reread re-read))))))))
    (should (funcall reread-equal
                     '((simple table case)
                       (all regular shaped)
                       (all lists only))))
    (should (funcall reread-equal
                     '((simple table case)
                       (irregular table shape with empty line)
                       ())))
    (should (funcall reread-equal
                     '((table with numbers)
                       (1 2 3 4)
                       ("and" "strings"))))
    (should (funcall reread-equal 
                     '(table
                       with
                       atoms
                       (and lists mixed)
                       "in place")))
    (should (funcall reread-equal
                     '((table with)
                       [vector entries])))
    (should (funcall reread-equal
                     '[(a vector table)
                       (with list entries only)]))
    (should (funcall reread-equal
                     '[[vector list]
                       (with vector entries)]))))
                       
                              
                       

           

;;; kv.el --- key/value data structure functions

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.5
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 7th September 2012


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some routines for working with key/value data structures like
;; hash-tables and alists and plists.

;; This also takes over the dotassoc stuff and provides it separately.

;;; Code:

(eval-when-compile (require 'cl))

(defun kvalist->hash (alist &rest hash-table-args)
  "Convert ALIST to a HASH.

HASH-TABLE-ARGS are passed to the hash-table creation."
  (let ((table (apply 'make-hash-table hash-table-args)))
    (mapc
     (lambda (pair)
       (puthash (car pair) (cdr pair) table))
     alist)
    table))

(defun kvhash->alist (hash)
  "Convert HASH to an ALIST."
  (when hash
    (let (store)
    (maphash
     (lambda (key value)
       (setq
        store
        (append (list (cons key value)) store)))
     hash)
    store)))

(defun kvalist->plist (alist)
  "Convert an alist to a plist."
  ;; Why doesn't elisp provide this?
  (loop for pair in alist
     append (list
             (intern
              (concat
               ":"
               (cond
                 ((symbolp (car pair))
                  (symbol-name (car pair)))
                 ((stringp (car pair))
                  (car pair)))))
             (cdr pair))))

(defun kvplist->alist (plist)
  "Convert PLIST to an alist.

The keys are expected to be :prefixed and the colons are removed.
The keys in the resulting alist are symbols."
  ;; RECURSION KLAXON
  (labels
      ((plist->alist-cons (a b lst)
         (let ((key (intern (substring (symbol-name a) 1))))
           (if (car-safe lst)
               (cons
                (cons key b)
                (plist->alist-cons
                 (car lst)
                 (cadr lst)
                 (cddr lst)))
               ;; Else
               (cons (cons key b) nil)))))
    (plist->alist-cons
     (car plist)
     (cadr plist)
     (cddr plist))))

(defun kvalist2->plist (alist2)
  "Convert a list of alists too a list of plists."
  (loop for alist in alist2
       append
       (list (kvalist->plist alist))))

(defun kvalist->keys (alist)
  "Get just the keys from the alist."
  (mapcar (lambda (pair) (car pair)) alist))

(defun kvalist->values (alist)
  "Get just the values from the alist."
  (mapcar (lambda (pair) (cdr pair)) alist))

(defun kvalist-sort (alist pred)
  "Sort ALIST (by key) with PRED."
  (sort alist (lambda (a b) (funcall pred (car a) (car b)))))

(defun kvalist-sort-by-value (alist pred)
  "Sort ALIST by value with PRED."
  (sort alist (lambda (a b) (funcall pred (cdr a) (cdr b)))))

(defun kvalist->filter-keys (alist &rest keys)
  "Return the ALIST filtered to the KEYS list.

Only pairs where the car is a `member' of KEYS will be returned."
  (loop for a in alist
     if (member (car a) keys)
     collect a))

(defun kvplist->filter-keys (plist &rest keys)
  "Filter the plist to just those matching KEYS.

KEYS must actually be :-less symbols.

`kvalist->filter-keys' is actually used to do this work."
  (kvalist->plist
   (apply
    'kvalist->filter-keys
    (cons (kvplist->alist plist) keys))))

(defun kvplist2->filter-keys (plist2 &rest keys)
  "Return the PLIST2 (a list of plists) filtered to the KEYS."
  (loop for plist in plist2
     collect (apply 'kvplist->filter-keys (cons plist keys))))

(defun kvalist2->filter-keys (alist2 &rest keys)
  "Return the ALIST2 (a list of alists) filtered to the KEYS."
  (loop for alist in alist2
     collect (apply 'kvalist->filter-keys (cons alist keys))))

(defun kvalist2->alist (alist2 car-key cdr-key &optional proper)
  "Reduce the ALIST2 (a list of alists) to a single alist.

CAR-KEY is the key of each alist to use as the resulting key and
CDR-KEY is the key of each alist to user as the resulting cdr.

If PROPER is `t' then the alist is a list of proper lists, not
cons cells."
  (loop for alist in alist2
       collect (apply (if proper 'list 'cons)
                      (list
                       (assoc-default car-key alist)
                       (assoc-default cdr-key alist)))))

(defun kvcmp (a b)
  "Do a comparison of the two values using printable syntax.

Use this as the function to pass to `sort'."
  (string-lessp (if a (format "%S" a) "")
                (if b (format "%S" b) "")))

(defun kvdotassoc-fn (expr table func)
  "Use the dotted EXPR to access deeply nested data in TABLE.

EXPR is a dot separated expression, either a symbol or a string.
For example:

 \"a.b.c\"

or:

 'a.b.c

If the EXPR is a symbol then the keys of the alist are also
expected to be symbols.

TABLE is expected to be an alist currently.

FUNC is some sort of `assoc' like function."
  (let ((state table)
        (parts
         (if (symbolp expr)
             (mapcar
              'intern
              (split-string (symbol-name expr) "\\."))
             ;; Else it's a string
             (split-string expr "\\."))))
    (catch 'break
      (while (listp parts)
        (let ((traverse (funcall func (car parts) state)))
          (setq parts (cdr parts))
          (if parts
              (setq state (cdr traverse))
              (throw 'break (cdr traverse))))))))

(defun kvdotassoc (expr table)
  "Dotted expression handling with `assoc'."
  (kvdotassoc-fn expr table 'assoc))

(defun kvdotassq (expr table)
  "Dotted expression handling with `assq'."
  (kvdotassoc-fn expr table 'assq))

(defalias 'dotassoc 'kvdotassoc)
(defalias 'dotassq 'kvdotassq)

(provide 'kv)
(provide 'dotassoc)

;;; kv.el ends here

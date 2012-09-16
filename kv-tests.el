(require 'kv)
(require 'ert)

(ert-deftest kvhash->alist ()
  "Test making alists from hashes."
  (should
   (equal
    (sort
     (kvhash->alist
      (kvalist->hash '((name1 . value1)
                       (name2 . value2))))
     (lambda (a b)
       (string-lessp (symbol-name (car a))
                     (symbol-name (car b)))))
    '((name1 . value1)
      (name2 . value2)))))

(ert-deftest kvalist-sort ()
  (should
   (equal
    (kvalist-sort
     (list '("z" . 20)
           '("a" . 20)
           '("b" . 17))
     'string-lessp)
    '(("a" . 20)
      ("b" . 17)
      ("z" . 20)))))

(ert-deftest kvalist-sort-by-value ()
  (should
   (equal
    (kvalist-sort-by-value
     (list '("z" . 20)
           '("a" . 20)
           '("b" . 17))
     '<)
    '(("b" . 17)
      ("z" . 20)
      ("a" . 20)))))

(ert-deftest kvcmp ()
  "Test the general cmp function."
  (should
   (equal
    '((a . 10)(b . 20)(c . 5))
   (sort '((a . 10)(b . 20)(c . 5)) 'kvcmp)))
  (should
   (equal
    '((a . 10)(b . 20)(c . 5))
   (sort '((b . 20)(c . 5)(a . 10)) 'kvcmp))))

(ert-deftest kvalist-keys->symbols ()
  "Test the key transformation."
  (should
   (equal
    '((a . 10)(\10 . 20)(\(a\ b\ c\) . 30))
    (kvalist-keys->symbols
     '(("a" . 10)(10 . 20)((a b c) . 30))))))

(ert-deftest kvdotassoc ()
  (should
   (equal
    (dotassoc "a.b.c" '(("a" . (("b" . (("c" . 10)))))))
    10)))

(ert-deftest kvdotassq ()
  (should
   (equal
    (dotassq 'a.b.c '((a . ((b . ((c . 10)))))))
    10)))

(ert-deftest kvalist->plist ()
  "Make alists into plists."
  (should
   (equal
    '(:a1 value1 :a2 value2)
    (kvalist->plist '((a1 . value1)(a2 . value2))))))

;;; kv-tests.el ends here

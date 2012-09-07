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

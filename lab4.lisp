(defun bubble-pass-pairs (lst test)
  "Один прохід bubble sort по списку пар (element . key)"
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((x (car lst))
            (y (cadr lst)))
        (if (funcall test (cdr x) (cdr y))
            ;; порядок правильний — x залишаємо першим
            (cons x (bubble-pass-pairs (cdr lst) test))
            ;; обмін — y першим, x після y
            (cons y (bubble-pass-pairs (cons x (cddr lst)) test))))))


(defun bubble-sort-functional (lst &key (key #'identity) (test #'<))
  (let* ((pairs (mapcar (lambda (x) (cons x (funcall key x))) lst))
         (n (length pairs)))
    (labels ((bubble-n (lst count)
               (if (<= count 0)
                   lst
                   (bubble-n (bubble-pass-pairs lst test)
                             (1- count)))))
      (mapcar #'car (bubble-n pairs (1- n))))))



(defun test-bubble-sort ()
  ;; Тест 1: сортування чисел за зростанням
  (format t "Test 1: ~a~%" (bubble-sort-functional '(5 4 3 2 1)))
  
  ;; Тест 2: сортування чисел за спаданням
  (format t "Test 2: ~a~%" (bubble-sort-functional '(5 4 3 2 1) :test #'>))
  
  ;; Тест 3: сортування за ключем
  (format t "Test 3: ~a~%" (bubble-sort-functional '((3 . c) (1 . a) (2 . b)) :key #'car))
  
  ;; Тест 4: пустий список
  (format t "Test 4: ~a~%" (bubble-sort-functional '()))
  
  ;; Тест 5: список з одним елементом
  (format t "Test 5: ~a~%" (bubble-sort-functional '(42)))
  
  ;; Тест 6: повторювані елементи
  (format t "Test 6: ~a~%" (bubble-sort-functional '(3 1 2 3 1 2)))
  
  ;; Тест 7: сортування рядків за довжиною
  (format t "Test 7: ~a~%" (bubble-sort-functional '("aaaa" "a" "aa" "aaa") :key #'length))

  (format t "All tests completed.~%"))

(test-bubble-sort)

(defun remove-each-nth-fn (n &key (key #'identity))
  "Повертає функцію для remove-if, яка видаляє кожен n-й елемент списку,
   для якого (funcall key element) не nil. Якщо key не передано, видаляє просто кожен n-й."
  (lambda (lst)
    (let ((counter 0))
      (remove-if (lambda (x)
                   (incf counter)
                   (and (= (mod counter n) 0)
                        (funcall key x)))  ; якщо key(x) не nil, видаляємо
                 lst))))


(defun test-remove-each-nth-fn ()
  ;; Видалити кожен 2-й елемент
  (let ((f (remove-each-nth-fn 2)))
    (format t "Test 1 (every 2nd element): ~a~%" (funcall f '(a b c d e f g))))

  ;; Видалити кожен 3-й елемент
  (let ((f (remove-each-nth-fn 3)))
    (format t "Test 2 (every 3rd element): ~a~%" (funcall f '(1 2 3 4 5 6 7 8 9))))

  ;; Видалити кожен 2-й елемент, який є парним
  (let ((f (remove-each-nth-fn 2 :key #'evenp)))
    (format t "Test 3 (every 2nd element if even): ~a~%" (funcall f '(1 2 2 2 3 4 4 4 5))))

  ;; Видалити кожен 1-й елемент, який є символом 'a
  (let ((f (remove-each-nth-fn 1 :key (lambda (x) (eq x 'a)))))
    (format t "Test 4 (every 1st element if 'a): ~a~%" (funcall f '(a b a c a))))

  ;; Порожній список
  (let ((f (remove-each-nth-fn 3)))
    (format t "Test 5 (empty list): ~a~%" (funcall f '())))

  ;; Одноелементний список
  (let ((f (remove-each-nth-fn 2)))
    (format t "Test 6 (single element): ~a~%" (funcall f '(42))))

  (format t "All remove-each-nth-fn tests completed.~%"))


(test-remove-each-nth-fn)
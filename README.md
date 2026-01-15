<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Кузнецов Дмитро Сергійович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:

- використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. No3);

 - додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.
12). При цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.
## Варіант першої частини 2
Алгоритм сортування обміном No1 (без оптимізацій) за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
(defun bubble-pass-pairs (lst test)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((x (car lst))
            (y (cadr lst)))
        (if (funcall test (cdr x) (cdr y))
            (cons x (bubble-pass-pairs (cdr lst) test))
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
```
### Тестові набори та утиліти першої частини
```lisp
(defun test-bubble-sort ()
  (format t "Test 1: ~a~%" (bubble-sort-functional '(5 4 3 2 1)))
  (format t "Test 2: ~a~%" (bubble-sort-functional '(5 4 3 2 1) :test #'>))
  (format t "Test 3: ~a~%" (bubble-sort-functional '((3 . c) (1 . a) (2 . b)) :key #'car))
  (format t "Test 4: ~a~%" (bubble-sort-functional '()))
  (format t "Test 5: ~a~%" (bubble-sort-functional '(42)))
  (format t "Test 6: ~a~%" (bubble-sort-functional '(3 1 2 3 1 2)))
  (format t "Test 7: ~a~%" (bubble-sort-functional '("aaaa" "a" "aa" "aaa") :key #'length))
  (format t "All tests completed.~%"))
```
### Тестування першої частини
```lisp
(test-bubble-sort)

Test 2: (5 4 3 2 1)
Test 3: ((1 . A) (2 . B) (3 . C))
Test 4: NIL
Test 5: (42)
Test 6: (1 1 2 2 3 3)
Test 7: (a aa aaa aaaa)
All tests completed.
```
## Варіант другої частини 7
Написати функцію remove-each-nth-fn , яка має один основний параметр n та один ключовий параметр — функцію key . remove-each-nth-fn має повернути функцію, яка при застосуванні в якості першого аргументу remove-if робить наступне: кожен n -ний елемент списку-аргумента remove-if , для якого функція key повертає значення t (або не nil ), видаляється зі списку. Якщо користувач не передав функцію key у remove-each-nth-fn , тоді зі списку видаляється просто кожен n -ний елемент.

```lisp
CL-USER> (remove-if (remove-each-nth-fn 2) '(1 2 3 4 5))
(1 3 5)

CL-USER> (remove-if (remove-each-nth-fn 2 :key #'evenp) '(1 2 2 2 3 4 4 4 5))
(1 2 2 3 4 5)
```
## Лістинг реалізації другої частини завдання
```lisp
(defun remove-each-nth-fn (n &key (key #'identity))
  (lambda (lst)
    (let ((counter 0))
      (remove-if (lambda (x)
                   (incf counter)
                   (and (= (mod counter n) 0)
                        (funcall key x)))
                 lst))))
```
### Тестові набори та утиліти другої частини
```lisp
(defun test-remove-each-nth-fn ()
  (let ((f (remove-each-nth-fn 2)))
    (format t "Test 1 (every 2nd element): ~a~%" (funcall f '(a b c d e f g))))
  (let ((f (remove-each-nth-fn 3)))
    (format t "Test 2 (every 3rd element): ~a~%" (funcall f '(1 2 3 4 5 6 7 8 9))))
  (let ((f (remove-each-nth-fn 2 :key #'evenp)))
    (format t "Test 3 (every 2nd element if even): ~a~%" (funcall f '(1 2 2 2 3 4 4 4 5))))
  (let ((f (remove-each-nth-fn 1 :key (lambda (x) (eq x 'a)))))
    (format t "Test 4 (every 1st element if 'a): ~a~%" (funcall f '(a b a c a))))
  (let ((f (remove-each-nth-fn 3)))
    (format t "Test 5 (empty list): ~a~%" (funcall f '())))
  (let ((f (remove-each-nth-fn 2)))
    (format t "Test 6 (single element): ~a~%" (funcall f '(42))))
  (format t "All remove-each-nth-fn tests completed.~%"))
```
### Тестування другої частини
```lisp
(test-remove-each-nth-fn)

Test 1 (every 2nd element): (A C E G)
Test 2 (every 3rd element): (1 2 4 5 7 8)
Test 3 (every 2nd element if even): (1 2 3 4 5)
Test 4 (every 1st element if 'a): (B C)
Test 5 (empty list): NIL
Test 6 (single element): (42)
All remove-each-nth-fn tests completed.
```

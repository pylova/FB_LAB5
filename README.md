<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студентка</b>: 
 Пильова Д.М КВ-11</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 8
База даних: Наукові статті
Тип записів: Геш-таблиця
## Лістинг реалізації завдання
```lisp

;; ======= Базові утиліти =======

;; Функція для розділення рядка за роздільником
(defun split-string (string &optional (delimiter #\,))
  "Розділяє рядок на підрядки за вказаним роздільником"
  (loop for start = 0 then (1+ position)
        for position = (position delimiter string :start start)
        collect (string-trim " " (subseq string start position))
        while position))

;; Функція для створення запису у вигляді хеш-таблиці
(defun create-record (fields headers)
  "Створює запис у вигляді хеш-таблиці з полів CSV"
  (let ((record (make-hash-table :test 'equal)))
    (loop for field in fields
          for header in headers
          do (setf (gethash header record) field))
    record))

;; ======= Основні функції роботи з даними =======

;; Функція для читання CSV файлу
(defun read-csv (filename)
  "Читає CSV файл і повертає список хеш-таблиць"
  (with-open-file (stream filename :direction :input)
    (let* ((headers (split-string (read-line stream)))
           (records nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (push (create-record (split-string line) headers) records))
      (nreverse records))))

;; Функція для фільтрації записів за заданими критеріями
(defun filter-records (records filters)
  "Фільтрує записи за заданими критеріями"
  (if (null filters)
      records
      (let ((key (first filters))
            (value (second filters))
            (rest-filters (cddr filters)))
        (filter-records
         (remove-if-not
          (lambda (record)
            (equal (gethash key record) value))
          records)
         rest-filters))))

;; Функція для вибірки записів
(defun select (filename)
  "Повертає лямбда-вираз для вибірки записів"
  (lambda (&rest filters)
    (let ((records (read-csv filename)))
      (filter-records records filters))))

;; Функція для запису в CSV файл
(defun write-csv (filename records)
  "Записує записи в CSV файл"
  (when records
    (with-open-file (stream filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (let ((headers (loop for key being the hash-keys of (first records)
                          collect key)))
        ;; Записуємо заголовки
        (format stream "~{~A~^,~}~%" headers)
        ;; Записуємо дані
        (dolist (record records)
          (format stream "~{~A~^,~}~%"
                  (loop for header in headers
                        collect (gethash header record))))))))

;; Функція для конвертації хеш-таблиці в асоціативний список
(defun hash-to-alist (hash-table)
  "Конвертує хеш-таблицю в асоціативний список"
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    (nreverse result)))

;; Функція для красивого виведення
(defun print-records (records)
  "Красиво виводить записи таблиці"
  (when records
    (let ((headers (loop for key being the hash-keys of (first records)
                        collect key)))
      ;; Виводимо заголовки
      (format t "~%~{~15A~}" headers)
      (format t "~%~{---------------~}" headers)
      ;; Виводимо дані
      (dolist (record records)
        (format t "~%~{~15A~}"
                (loop for header in headers
                      collect (gethash header record)))))))

```
### Тестові набори та утиліти
```lisp
;; ======= Тестові функції =======

;; Тест для читання CSV файлу
(defun test-read-csv ()
  "Тест читання CSV файлу"
  (format t "~%=== Тест читання CSV ===~%")
  (let ((records (read-csv "articles.csv")))
    (format t "Прочитано записів: ~A~%" (length records))
    (format t "Перша запис:~%")
    (maphash (lambda (k v)
               (format t "~A: ~A~%" k v))
             (first records))))

;; Тест функції select
(defun test-select ()
  "Тест функції select"
  (format t "~%=== Тест вибірки даних ===~%")
  ;; Всі записи
  (format t "~%Всі статті:~%")
  (print-records (funcall (select "articles.csv")))
  ;; Фільтрація за спеціальністю
  (format t "~%Статті з математики:~%")
  (print-records (funcall (select "articles.csv") "Specialty" "Mathematics")))

;; Тест функції запису в файл
(defun test-write ()
  "Тест запису в файл"
  (format t "~%=== Тест запису в файл ===~%")
  (let ((physics-records 
         (funcall (select "articles.csv") "Specialty" "Physics")))
    (write-csv "physics_articles.csv" physics-records)
    (format t "Записи збережено в physics_articles.csv~%")
    (print-records physics-records)))

;; Тест конвертації форматів
(defun test-conversion ()
  "Тест конвертації форматів"
  (format t "~%=== Тест конвертації форматів ===~%")
  (let ((record (first (funcall (select "articles.csv")))))
    (format t "Оригінальна хеш-таблиця:~%")
    (maphash (lambda (k v) (format t "~A: ~A~%" k v)) record)
    (format t "~%У вигляді асоціативного списку:~%")
    (format t "~A~%" (hash-to-alist record))))

;; Головна тестова функція
(defun run-all-tests ()
  "Запускає всі тести"
  (format t "~%Початок тестування...~%")
  (test-read-csv)
  (test-select)
  (test-write)
  (test-conversion)
  (format t "~%Тестування завершено.~%"))
```

### Тестування
```lisp
* (load "lab6.lisp")
T
* ;; Отримати всі статті з файлу "articles.csv"
(funcall (select "articles.csv"))
(#<HASH-TABLE :TEST EQUAL :COUNT 3 {23965AA9}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23965D79}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23966021}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {239662D9}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23966561}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23966889}>)
*
;; Отримати статті по фізиці з файлу "articles.csv"
(funcall (select "articles.csv") "Specialty" "Physics")
(#<HASH-TABLE :TEST EQUAL :COUNT 3 {23969521}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23969849}>)
*
;; Зберегти відфільтровані статті (по фізиці) у файл "physics.csv"
(write-csv "physics.csv"
          (funcall (select "articles.csv") "Specialty" "Physics"))
NIL
*
;; Конвертувати перший запис в асоціативний список
(hash-to-alist (first (funcall (select "articles.csv"))))
(("ID" . "1") ("Specialty" . "Computer Science")
")) "Machine Learning Basics
```



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

;; ======= Базовые утилиты =======

;; Функция для разделения строки по разделителю
(defun split-string (string &optional (delimiter #\,))
  "Разделяет строку на подстроки по разделителю"
  (loop for start = 0 then (1+ position)
        for position = (position delimiter string :start start)
        collect (string-trim " " (subseq string start position))
        while position))

;; Функция для создания записи в виде хеш-таблицы
(defun create-record (fields headers)
  "Создает запись в виде хеш-таблицы из полей CSV"
  (let ((record (make-hash-table :test 'equal)))
    (loop for field in fields
          for header in headers
          do (setf (gethash header record) field))
    record))

;; ======= Основные функции работы с данными =======

;; Функция для чтения CSV файла
(defun read-csv (filename)
  "Читает CSV файл и возвращает список хеш-таблиц"
  (with-open-file (stream filename :direction :input)
    (let* ((headers (split-string (read-line stream)))
           (records nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (push (create-record (split-string line) headers) records))
      (nreverse records))))

;; Функция для фильтрации записей
(defun filter-records (records filters)
  "Фильтрует записи по заданным критериям"
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

;; Функция select
(defun select (filename)
  "Возвращает лямбда-выражение для выборки записей"
  (lambda (&rest filters)
    (let ((records (read-csv filename)))
      (filter-records records filters))))

;; Функция для записи в CSV
(defun write-csv (filename records)
  "Записывает записи в CSV файл"
  (when records
    (with-open-file (stream filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (let ((headers (loop for key being the hash-keys of (first records)
                          collect key)))
        ;; Записываем заголовки
        (format stream "~{~A~^,~}~%" headers)
        ;; Записываем данные
        (dolist (record records)
          (format stream "~{~A~^,~}~%"
                  (loop for header in headers
                        collect (gethash header record))))))))

;; Функция для конвертации хеш-таблицы в ассоциативный список
(defun hash-to-alist (hash-table)
  "Конвертирует хеш-таблицу в ассоциативный список"
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    (nreverse result)))

;; Функция красивого вывода
(defun print-records (records)
  "Красиво выводит записи таблицы"
  (when records
    (let ((headers (loop for key being the hash-keys of (first records)
                        collect key)))
      ;; Выводим заголовки
      (format t "~%~{~15A~}" headers)
      (format t "~%~{---------------~}" headers)
      ;; Выводим данные
      (dolist (record records)
        (format t "~%~{~15A~}"
                (loop for header in headers
                      collect (gethash header record)))))))



```
### Тестові набори та утиліти
```lisp
(defun test-read-csv ()
  "Тест чтения CSV файла"
  (format t "~%=== Тест чтения CSV ===~%")
  (let ((records (read-csv "articles.csv")))
    (format t "Прочитано записей: ~A~%" (length records))
    (format t "Первая запись:~%")
    (maphash (lambda (k v)
               (format t "~A: ~A~%" k v))
             (first records))))

(defun test-select ()
  "Тест функции select"
  (format t "~%=== Тест выборки данных ===~%")
  ;; Все записи
  (format t "~%Все статьи:~%")
  (print-records (funcall (select "articles.csv")))
  ;; Фильтрация по специальности
  (format t "~%Статьи по математике:~%")
  (print-records (funcall (select "articles.csv") "Specialty" "Mathematics")))

(defun test-write ()
  "Тест записи в файл"
  (format t "~%=== Тест записи в файл ===~%")
  (let ((physics-records 
         (funcall (select "articles.csv") "Specialty" "Physics")))
    (write-csv "physics_articles.csv" physics-records)
    (format t "Записи сохранены в physics_articles.csv~%")
    (print-records physics-records)))

(defun test-conversion ()
  "Тест конвертации форматов"
  (format t "~%=== Тест конвертации форматов ===~%")
  (let ((record (first (funcall (select "articles.csv")))))
    (format t "Исходная хеш-таблица:~%")
    (maphash (lambda (k v) (format t "~A: ~A~%" k v)) record)
    (format t "~%В виде ассоциативного списка:~%")
    (format t "~A~%" (hash-to-alist record))))

;; Главная тестовая функция
(defun run-all-tests ()
  "Запускает все тесты"
  (format t "~%Начало тестирования...~%")
  (test-read-csv)
  (test-select)
  (test-write)
  (test-conversion)
  (format t "~%Тестирование завершено.~%"))

```

### Тестування
```lisp
* (load "lab6.lisp")
T
* ;; Получить все статьи
(funcall (select "articles.csv"))
(#<HASH-TABLE :TEST EQUAL :COUNT 3 {23966ED1}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {239671A1}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23967449}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23967701}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23967989}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {23967CB1}>)
*
;; Получить статьи по физике
(funcall (select "articles.csv") "Specialty" "Physics")
(#<HASH-TABLE :TEST EQUAL :COUNT 3 {2396A939}>
 #<HASH-TABLE :TEST EQUAL :COUNT 3 {2396AC61}>)
*
;; Сохранить отфильтрованные статьи в файл
(write-csv "physics.csv"
          (funcall (select "articles.csv") "Specialty" "Physics"))
NIL
*
;; Конвертировать запись в ассоциативный список
(hash-to-alist (first (funcall (select "articles.csv"))))
(("ID" . "1") ("Specialty" . "Computer Science")
")) "Machine Learning Basics
*
```



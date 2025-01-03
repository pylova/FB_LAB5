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

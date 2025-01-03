(defstruct publication
  id
  title
  description
  author
  publication-date
  specialty-id)

(defstruct specialty
  id
  name
  type
  description)

(defun publication-slots ()
  '(id title description author publication-date specialty-id))

(defun specialty-slots ()
  '(id name type description))

(defun my-split-sequence (delimiter string)
  "Splits STRING by the DELIMITER into a list of substrings."
  (let ((start 0)
        (result '()))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (1+ pos)))
    (push (subseq string start) result)
    (nreverse result)))

(defun create-publication-record (row)
  "Converts a CSV row into a PUBLICATION structure."
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 6)  ;; Fixed to expect 6 fields
        (make-publication
         :id (parse-integer (nth 0 fields))
         :title (nth 1 fields)
         :description (nth 2 fields)
         :author (nth 3 fields)
         :publication-date (nth 4 fields)
         :specialty-id (parse-integer (nth 5 fields)))
        (progn
          (format t "Error: Invalid row format ~a~%" row)
          nil))))

(defun create-specialty-record (row)
  "Converts a CSV row into a SPECIALTY structure."
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 4)
        (make-specialty
         :id (parse-integer (nth 0 fields))
         :name (nth 1 fields)
         :type (nth 2 fields)
         :description (nth 3 fields))
        (progn
          (format t "Error: Invalid row ~a~%" row)
          nil))))

(defun read-csv-file (file-path create-record-fn)
  "Reads a CSV file and returns a list of records created using create-record-fn."
  (with-open-file (stream file-path :if-does-not-exist nil)
    (if stream
        (let ((header (read-line stream nil)))
          (if (some (lambda (field) (string= field "ID")) (my-split-sequence #\, header))
              (loop for line = (read-line stream nil)
                    while line
                    do (format t "Processing line: ~a~%" line)
                    collect (funcall create-record-fn line))
              (cons (funcall create-record-fn header)
                    (loop for line = (read-line stream nil)
                          while line
                          do (format t "Processing line: ~a~%" line)
                          collect (funcall create-record-fn line))))))))

(defun select (file-path create-record-fn)
  "Returns a lambda that selects records from a CSV file, optionally filtering by field values."
  (let ((records (read-csv-file file-path create-record-fn)))
    (lambda (&rest filters)
      (let ((filtered-records records))
        (dolist (filter filters)
          (let ((field (car filter))
                (value (cdr filter)))
            (format t "Filtering by ~a = ~a~%" field value)
            (setf filtered-records
                  (remove-if-not
                   (lambda (record)
                     (let ((slot-val (slot-value record field)))
                       (format t "Checking record ~a | Field ~a = ~a~%" record field slot-val)
                       (equal value slot-val)))
                   filtered-records))))
        filtered-records))))

(defun write-records-to-csv (file-path records)
  "Writes a list of records to a CSV file."
  (with-open-file (stream file-path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (if stream
        (progn
          (dolist (record records)
            (let ((slots (cond
                          ((typep record 'publication) (publication-slots))
                          ((typep record 'specialty) (specialty-slots)))))
              (format stream "~{~a~^,~}~%" 
                      (mapcar (lambda (slot) (slot-value record slot)) slots))))
          (format t "Data successfully written to ~a~%" file-path))
        (format t "Error: Could not open file ~a~%" file-path))))

(defstruct joined-data
  publication
  specialty)

(defun join-publications-with-specialties (publications specialties)
  "З'єднує публікації з спеціальностями за specialty-id."
  (mapcar
   (lambda (publication)
     (let ((specialty (find (publication-specialty-id publication) specialties :key #'specialty-id)))
       (make-instance 'joined-data
                      :publication publication
                      :specialty specialty))) 
   publications))

;; Testing the functions
(defvar *publications* (read-csv-file "C:/Users/Diana/Desktop/lisp/publications.csv" #'create-publication-record))
(defvar *specialties* (read-csv-file "C:/Users/Diana/Desktop/lisp/specialties.csv" #'create-specialty-record))

(format t "Records to write to output.csv:~%")
(write-records-to-csv "C:/Users/Diana/Desktop/lisp/output.csv" *publications*)

(let ((select-publications (select "C:/Users/Diana/Desktop/lisp/publications.csv" #'create-publication-record)))
  (let ((filtered-publications (funcall select-publications '(specialty-id . 5))))
    (if filtered-publications
        (progn
          (format t "Filtered Publications:~%")
          (write-records-to-csv "C:/Users/Diana/Desktop/lisp/filtered_publications.csv" filtered-publications))
        (format t "No publications found for specialty-id = 5~%"))))

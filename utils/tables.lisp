;;;; Copyright (c) 2007 Albert Krewinkel
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :clcb-utils)

;;;; -------------------------------------------------------------------------
;;;; Some New Types 
(defun float-or-nil-p (x) 
  "Indicates of the argument is a floating point number or nil. Admittedly, this is somewhat counterintuitive here.

Example:
* (FLOAT-OR-NIL-P 0)

NIL
* (FLOAT-OR-NIL-P 0.0)

T
* (FLOAT-OR-NIL-P 1)

NIL
* (FLOAT-OR-NIL-P 1.0)

T
* (null 0)

NIL
* (null nil)

T"
  (or (null x) (floatp x)))
(deftype float-or-nil () `(satisfies float-or-nil-p))

(defun number-or-nil-p (x) 
  "True if the argumet is numeric.

Examples:
* (number-or-nil-p 0)

T
* (number-or-nil-p 0.0)

T
* (number-or-nil-p "0")

NIL"
  (typep x '(or null number)))
(deftype number-or-nil () `(satisfies number-or-nil-p))



;;;; -------------------------------------------------------------------------
;;;; Tables and Columns
(defparameter *separator* (list #\tab)
  "The default column separator in tables.")

(defparameter *default-table-size* 50
  "Specifies the default size of the adjustable array underlying a
  table.")

(defun make-table-rows (&optional (size *default-table-size*))
  "A new column is prepared as an array of the length as specified in the first argument. If that is omitted, the *default-table-size* is chosen."
  (make-array size :adjustable t :fill-pointer 0))

(defclass table ()
  ((rows :accessor table-rows :initarg :rows :initform (make-table-rows))
   (schema :accessor table-schema :initarg :schema))
  (:documentation "This class presents a two-dimensional array that is interpreted as a table or spreadsheet. This implementation is comparable with the dump of a relational database in which a particular column is of an invariant type. Rows represent observations/individuals."))

(defclass column ()
  ((name :reader column-name
         :initarg :name
	 :documentation "The name of a column as it would be presented in a header line when printed.")
   (equality-predicate :reader column-equality-predicate
                       :initarg :equality-predicate
		       :documentation "A function indicating of two entries in a column are equal or not.")
   (comparator :reader column-comparator
               :initarg :comparator
	       :documentation "A function(a,b) indicating if a<b -> -1, a>b -> 1 or a==b -> 0.")
   (default-value :reader column-default-value
                  :initarg :default-value
                  :initform nil
		  :documentation "The default is to insert NIL when no other value is specified. This can be adapted columnwise with this attribute.")
   (value-normalizer :reader column-value-normalizer
                     :initarg :value-normalizer
                     :initform #'(lambda (v column)
                                    (declare (ignore column))
                                    v))
   (parse-function :reader column-parse-function
                   :initarg :parse-function
		   :documentation "A function to read a column from a string."))
  (:documentation "A column is a vertical slot in a table."))

(defun not-nullable (value column)
  (or value (error "Column ~A can't be null." (column-name column))))

(defgeneric make-column (name type &optional default-value)
  (:documentation "Create a new column type."))

(defmethod print-object ((col column) (stream stream))
  (print-unreadable-object (col stream :type t)
    (format stream ":name ~A" (slot-value col 'name))))

(defmethod make-column (name (type (eql 'string)) 
                              &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'string<
   :equality-predicate #'string=
   :default-value default-value
   :value-normalizer #'not-nullable
   :parse-function #'identity))

(defmethod make-column (name (type (eql 'number))
                        &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'<
   :equality-predicate #'=
   :default-value default-value
   :parse-function #'read-from-string))

(defmethod make-column (name (type (eql 'character))
                        &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'char<
   :equality-predicate #'char=
   :default-value default-value
   :value-normalizer #'not-nullable
   :parse-function #'(lambda (x)
                       (if (= 1 (length x))
                           (char x 0)
                           (read-from-string x)))))

(defmethod make-column (name (type (eql 'float)) &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'<
   :equality-predicate #'=
   :default-value default-value
   :parse-function #'(lambda (x) (float (read-from-string x)))))

(defmethod make-column (name (type (eql 'number-or-nil))
                        &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'<
   :equality-predicate #'=
   :default-value default-value
   :parse-function #'(lambda (x) 
                       (if (zerop (length x)) nil (read-from-string x)))))

;;;; -------------------------------------------------------------------------
;;;; Convert Table rows to objects
;;;; -------------------------------------------------------------------------
(defun class->columns (class-specifier)
  "Create named columns with types according to the slots of the given
class."
  (flet ((slot-type (slot)
           (getf (moptilities:slot-properties class-specifier slot)
                 :type
                 'string))
         (slot-name (slot)
           (string (getf (moptilities:slot-properties class-specifier slot)
                         :name))))
    (let ((slots (moptilities:slot-names class-specifier)))
      (mapcar #'(lambda (slot)
                  (make-column (slot-name slot) (slot-type slot)))
              slots))))

(defun nappend-line-to-table-data (table-data line)
  (vector-push-extend line table-data))

(defun read-table-line (stream separator)
  "A row is read from a stream and an array of attributes returned as splitted according to the specified separator."
  (split-sequence separator (read-line stream nil nil)))

(defun ordered-columns (columns col-order)
  "Retrieves columns in a particular order."
  (mapcar #'(lambda (x)
              (find x columns :key #'column-name
                    :test #'string-equal))
          col-order))

(defun text-line->table-row (line column-order)
  "A text string is converted into a table row. The second argument allows to specify an assignment from the attributes read to the column in which they should be stored."
  (map 'vector #'funcall
       (mapcar #'column-parse-function column-order)
       line))


(defun add-table-line (line table)
  "A row is added to the table."
  (vector-push-extend line (table-rows table)))

(defun class->table (class)
  "Make a new table, modeled after the given class."
  (make-instance 'table
                 :schema (class->columns class)))

;; FIXME: Honor comment-char and header arguments
(defun read-table-from-class (stream class &key
                   (separator #\tab) (header nil) (comment-char #\#))
  "Read in a table from a text file. This function should behave
   similar to the read.table function in R."
  (declare (ignore header comment-char))
  (let* ((header (read-table-line stream separator))
         (file-col-order (ordered-columns (class->columns class)
                                          header))
         (table (make-instance 'table :schema file-col-order)))
    (loop
       for line = (read-table-line stream separator)
       while (not (null (car line)))
       for row = (text-line->table-row line file-col-order)
       do (add-table-line row table)
       finally (return table))))

;; FIXME: This relies on the fact, that the initargs have the same
;; name as the columns. Use MOP instead.
(defun table->objects (table class)
  (let ((col-names (mapcar #'column-name (table-schema table))))
    (labels ((initargs-plist (row)
             (loop for col in col-names
                            for el across row
                            nconc (list (make-keyword col) el)))
           (row->object (row)
             (apply #'make-instance class (initargs-plist row))))
      (map 'list #'row->object (table-rows table)))))

(defun read-objects-from-table (stream class &key
                                (separator #\tab) (header nil)
                                (comment-char #\#))
  "Creates objects from a table, one object per line."
  (table->objects
   (read-table-from-class stream class
                          :separator separator
                          :header header
                          :comment-char comment-char)
   class))


;;; ----------------------------------------------
;;; Table methods
(defgeneric table-row (table row-id)
  (:documentation "Get row from table, i.e, table[row-id,] in R syntax."))

(defgeneric table-column (table col-id)
  (:documentation "Get column from table, i.e., table[,col-id] in R syntax."))

(defmethod table-row ((table table) row)
  (aref (table-rows table) row))

(defmethod table-column ((row vector) col)
  (aref row col))

(defmethod table-column ((table table) (col integer))
  (let* ((num-rows (length (table-rows table)))
         (res (make-array num-rows)))
    (loop for i below num-rows
          do (setf (aref res i)
                   (aref (aref (table-rows table) i) col)))
    res))


(defgeneric taref (table row column)
  (:documentation "Get an element from the table."))
(defmethod taref (table row column)
  (table-column (table-row table row)
                column))

(defun table-num-rows (table)
  "Number of rows in a table, equivalent to nrow(table) in R."
  (length (table-rows table)))

(defun table-num-cols (table)
  "Number of columns in a table, equivalent to ncol(table) in R. The value is undefined if no row has yet been inserted."
  (length (car (table-rows table))))

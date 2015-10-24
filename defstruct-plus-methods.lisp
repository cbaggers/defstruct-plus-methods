(in-package #:defstruct-plus-methods)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun listify (x) (if (listp x) x (list x)))

(defun remove-keys (lambda-list keys)
  (loop :for key :in keys :do
     (let ((pos (position key lambda-list)))
       (when pos
         (setf lambda-list (remove-subseq lambda-list pos (+ 2 pos))))))
  lambda-list)

(defun remove-subseq (seq &optional (start 0) (end (length seq)))
  (concatenate (type-of seq) (subseq seq 0 start) (subseq seq end)))

(defmacro defstruct+methods (name-and-options &body slot-descriptions)
  (let ((method-keys '(:reader :writer :accessor)))
    (destructuring-bind (with without)
        (loop :for sd :in slot-descriptions
           :for pos = (and (listp sd)
                           (first (remove nil (mapcar (lambda (x) (position x sd))
                                                      method-keys))))
           :if pos :collect (remove-keys sd method-keys) :into without-methods
           :and :collect
           (destructuring-bind
                 (name _ &key reader writer accessor &allow-other-keys)
               sd
             (declare (ignore _))
             (list name reader writer accessor))
           :into with-methods
           :else :collect sd :into without-methods
           :finally (return (list with-methods without-methods)))
      (let* ((struct-name (if (listp name-and-options)
                              (first name-and-options)
                              name-and-options))
             (conc (or (and (listp name-and-options)
                            (second
                             (find :conc-name name-and-options :key
                                   (lambda (x) (and (listp x) (first x))))))
                       (symb struct-name '-))))
        `(progn
           (defstruct ,name-and-options ,@without)
           ,@(loop :for (name reader writer accessor) :in with :append
                (if (and accessor (or reader writer))
                    (error "Not valid to specify :accessor aswell as :reader or :writer")
                    (let ((reader (or reader accessor))
                          (writer (or writer accessor)))
                      (list
                       (when reader
                         `(defmethod ,reader ((object ,struct-name))
                            (,(symb conc name) object)))
                       (when writer
                         `(defmethod (setf ,writer) (value (object ,struct-name))
                            (setf (,(symb conc name) object) value))))))))))))


;; (defstruct+methods (test (:conc-name bleep-))
;;   (a 1 :type fixnum)
;;   (b nil :type t :read-only t)
;;   (c nil :type t :reader jam :writer jom)
;;   d
;;   (e 0 :accessor boom))

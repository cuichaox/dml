;;;;CLOS model for activity diagram
(uiop:define-package :act.core
    (:mix #:cl
          #:iterate)
  (:export #:act-node))

(in-package :act.core)

#|
a tracer demo
(defun tracer-demo (nstep  ; steps from beginning, 0
                    offset ; offset from main-line - most top-left side line. 0
                    node)
  ())
|#


(defgeneric walk (node tracer &optional from-step from-offset)
  (:documentation "Walk node, return max-steps, max-offsets"))

(defclass act-node ((node))
  ((id :reader id
       :initform (gensym)))
  (:documentation "Base class for activity node. "))

(defclass primative (act-node)
  ((name :reader name
         :initarg name
         :type string
         :initform (error "Must supply a name for primative."))
   (role :reader role
         :initarg role
         :type string
         :initform nil))
  (:documentation "A single process. "))

(defmethod walk ((node primative) tracer &optional (from-step 0) (from-offset 0))
  (progn (funcall tracer node from-step from-offset)
         (list 1 1)))

(defclass sequence-sructure (act-node)
  ((subnodes :reader subnodes
             :type list
             :initarg subnodes
             :initform (error "Must supply subnodes for sequnce-structure."))
   (label :reader label
          :initarg label
          :type string
          :initform nil))
  (:documentation "A Sequence Structure. "))

(defmethod walk ((node sequence-sructure) tracer &optional (from-step 0) (from-offset 0))
  (iter (for h )))


(defclass condition/parallel-structure (act-node)
  ((cases :reader cases
          :type list
          :initarg cases
          :initform (error "Must supply cases for conditon/paraller structure."))
   (sub-type :reader sub-type
             :type symbol
             :initarg sub-type
             :initform :condition))
  (:documentation "A Condition/paraller Structure. "))

(defmethod initialize-instance :after ((c condition/parallel-structure) &key)
  (if (not (member (sub-type c) (list :condition :parall)))
      (error "Wrong sub-type for conditon/parallel structure. ")
      (when (and (eql :condition (sub-type c))
                 (iter (for i in-vector (cases c))
                   (thereis (null (label i)))))
            (error "sequence-structure as  cases in condition must have a label."))))

(defclass loop-structure (act-node)
  ((forward-sequence :reader forward-sequence
                     :initarg forward-sequence
                     :type sequence-structure)
   (backward-sequence :reader backward-sequence
                      :initarg backward-sequence
                      :type sequence-structure))
  (:documentation "A Loop Structure. "))

(defmethod initialize-instance :after ((l loop-structure) &key)
  (when (and (null (forward-sequence l))
             (null (backward-sequence l)))
    (error "Must supply a forward or backward sequence for loop-structure.")))


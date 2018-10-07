;;;;CLOS model for activity diagram
(uiop:define-package :act.core
    (:mix #:cl
          #:iterate)
  (:export #:act-node))

(in-package :act.core)

(defclass act-node ()
  ()
 (:documentation "Base class for activity node. "))

(defclass primative (act-node)
  ((name :reader name
         :initarg name
         :type string
         :initform (error "Must supply a name for primative."))
   (role :reader rols
         :initarg role
         :type string
         :initform nil))
  (:documentation "A single process. "))

(defclass sequence-sructure (act-node)
  ((subnodes :reader subnodes
             :type (vector act-node *)
             :initarg subnodes
             :initform (error "Must supply subnodes for sequnce-structure."))
   (label :reader label
          :initarg label
          :type string
          :initform nil))
  (:documentation "A Sequence Structure. "))

(defclass condition/parallel-structure (act-node)
  ((cases :reader cases
          :type (vector sequence *)
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


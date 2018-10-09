;;;;CLOS model for activity diagram
(uiop:define-package :act.core
    (:mix #:cl
          #:iterate)
  (:export #:act-node))

(in-package :act.core)

#|
a tracer demo
(defun tracer-demo (nstep))  ; steps from beginning, 0
offset ; offset from main-line - most top-left side line. 0
node
()
|#


(defgeneric walk (node tracer &optional from-step from-offset)
  (:documentation "Walk node, return max-steps, max-offsets"))

(defclass act-node ()
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
  (if (null (subnodes node))
      (list 0 0)
      (iter
        (for subnode in (subnodes node))
        (for (s o) = (funcall tracer
                              subnode
                              (+ from-step (if-first-time 0 total-s))
                              from-offset))
        (sum s into total-s)
        (maximize o into max-o)
        (finally (return (list total-s max-o))))))

(defclass condition/parallel-structure (act-node)
  ((subcases :reader subcases
             :type list
             :initarg subcases
             :initform (error "Must supply cases for conditon/paraller structure."))
   (subtype :reader subtype
             :type symbol
             :initarg subtype
             :initform :condition))
  (:documentation "A Condition/paraller Structure. "))

(defmethod initialize-instance :after ((c condition/parallel-structure) &key)
  (if (not (member (sub-type c) (list :condition :parall)))
      (error "Wrong sub-type for conditon/parallel structure. ")
      (when (and (eql :condition (sub-type c))
                 (iter (for i in-vector (cases c))
                   (thereis (null (label i)))))
            (error "sequence-structure as  cases in condition must have a label."))))

(defmethod walk ((node condition/parallel-structure) tracer &optional (from-step 0) (from-offset 0))
  (if (null (subcases node))
      (list 0 0)
      (iter (for subcase in (subcases node))
        (for (s o) = (funcall tracer
                              subcase
                              from-step
                              from-offset))
        (maximize s into max-s)
        (sum o to total-s)
        (finally (return (list max-s total-s))))))

(defclass loop-structure (act-node)
  ((forward-sequence :reader forward-sequence
                     :initarg forward-sequence
                     :type sequence-structure)
   (backward-sequence :reader backward-sequence
                      :initarg backward-sequence
                      :type sequence-structure))
  (:documentation "A Loop Structure. "))

(defmethod walk )

(defmethod initialize-instance :after ((l loop-structure) &key)
  (if (and (null (forward-sequence l))
           (null (backward-sequence l)))
      (error "Must supply a forward or backward sequence for loop-structure.")))



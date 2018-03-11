(in-package :cl-user)

(defpackage :dml-seq
  (:use :cl))

(in-package :dml-seq)

(defgeneric draw (any) (:documentation "Draw the given objectect in current contex."))

(defclass object.bar ()
  ((call-message :accessor call-message :initarg call-message :initform nil)
   (ret-message :accessor ret-message :initarg ret-message :initform nil))
  (:documentation "Active strip on  life cycle Line."))

(defclass object()
  ((name :accessor name :initarg name :initform "")
   (is-active :accessor is-active :initarg is-active :initform nil)
   (bars :accessor bars :initarg bars :initform nil)
   (new-message :accessor new-message :initarg new-message :initform nil)
   (end-message :accessor end-message :initarg end-message :initform nil))
  (:documentation "Objectect on cylinders."))

(defclass message ()
  ((label :accessor label :initarg label :initform ""))
  (:documentation "message or message group"))

(defclass call-message (message)
  ((from-object :accessor from-object :initarg from-object :initform nil)
   (to-object :accessor to-object :initarg to-object :initform nil))
  (:documentation "Call to other."))
  
(defclass syn-call (call-message) ())
(defclass asy-call (call-message) ())
(defclass ret-call (call-message) ())
(defclass new-call (call-message) ())
(defclass del-call (call-message) ())

(defclass self-call(call-message)
  ((to-object :initarg from-xpos)))

(defclass mutil-message (message) ())

(defclass group-message (multi-message)
  ((messages :accessor messages :initarg messages :initform nil)))

(defclass guard-group (group-message)
  ((guard :accessor guard :initarg guard :initform "")))

(defclass opt-groups (guard-message)
  ((name :initform "opt")))

(defclass opt-groups (guard-message)
  ((name :initform "loop")))

(defclass alt-groups (multi-message)
  ((name :initform "alt")
   (ifgroup :accessor ifgroup :initarg ifgroup :initform nil)
   (elsegroup :accessor elsegroup :initarg elsegroup :initform nil)))

(defgeneric all-messages(msg))

(defmethod all-messages ((msg alt-groups))
  (append (ifgroup msg) (elsegroup msg)))

(defparameter *context-objects* nil)
(defun find-object-by-name (n)
  (find-if #'(lambda (object)
               (string= n (name object)))
           *context-objects*))

(defun intern-object (name &optional ( new-by nil))
  (or (find-object-by-name name)
      (let* ((is-active (char=  #\! (char name 0)))
             (obj (make-instance 'object
                                 :name (if is-active (subseq name 1) name)
                                 :is-active is-active 
                                 :new-message new-by)))                           
        (alexandria:appendf *context-objects* (list obj))
        obj)))

(defun message-symbol (ch)
  (cdr (assoc ch '((#\= 'syn-call)
                   (#\- 'asy-call)
                   (#\n 'new-call)
                   (#\x 'del-call)
                   (#\/ 'ret-call)))))

(defun parse-message-director (sym)
  (:documentation "return (type-ch object-name call-name return-name)")
  (cl-ppcre:register-groups-bind
      (type obj-name msg-name ret-name)
      ("(.)=>([^\./]+)\.?([^/]*)/?(.*)"
       (if (stringp sym) sym (symbol-name sym)))      
      (list (message-symbol type-ch)
            obj-name
            msg-name
            ret-name)))

(defun make-by-director (director)
  (:documentation "return values "))



(defun mprogn (&rest any))


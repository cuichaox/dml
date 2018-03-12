(in-package :cl-user)

(defpackage :dml-seq
  (:use :cl))

(in-package :dml-seq)

(defgeneric draw (any) (:documentation "Draw the given objectect in current contex."))

(defclass object.bar ()
  ((call-message :accessor call-message :initarg :call-message :initform nil)
   (ret-message :accessor ret-message :initarg :ret-message :initform nil))
  (:documentation "Active strip on  life cycle Line."))

(defclass object()
  ((name :accessor name :initarg :name :initform "")
   (is-active :accessor is-active :initarg :is-active :initform nil)
   (bars :accessor bars :initarg :bars :initform nil)
   (new-message :accessor new-message :initarg :new-message :initform nil)
   (end-message :accessor end-message :initarg :end-message :initform nil))
  (:documentation "Objectect on cylinders."))

(defclass message ()
  ((label :accessor label :initarg :label :initform ""))
  (:documentation "message or message group"))

(defclass call-message (message)
  ((from-object :accessor from-object :initarg :from-object :initform nil)
   (to-object :accessor to-object :initarg :to-object :initform nil))
  (:documentation "Call to other."))
  
(defclass syn-call (call-message) ())
(defclass asy-call (call-message) ())
(defclass ret-call (call-message) ())
(defclass new-call (call-message) ())
(defclass del-call (call-message) ())

(defclass self-call(call-message)
  ((to-object :initarg :from-xpos)))

(defclass mutil-message (message) ())

(defclass group-message (multi-message)
  ((messages :accessor messages :initarg :messages :initform nil)))


(defgeneric push-to (msg grp-msg))

(defmethod push-to ((msg message) (grp-msg group-message))
  (progn (when msg (push msg (messages grp-msg)))
         grp-msg))

(defgeneric append-to (msg grp-msg))

(defmethod append-to (msg grp-msg)
  (progn (when msg (alexandria:appendf (messages grp-msg) (list msg)))
         grp-msg))

(defclass guard-message (message)
  ((guard :accessor guard :initarg :guard :initform "")
   (the-message :accessor the-message :initarg :the-message :initform nil)))


(defclass opt-guard (guard-message)
  ((name :initform "opt")))

(defclass loop-guard (guard-message)
  ((name :initform "loop")))

(defclass alt-group (multi-message)
  ((name :initform "alt")
   (if-message :accessor if-message :initarg :if-message :initform nil)
   (else-message :accessor else-message :initarg :else-message :initform nil)))

(defgeneric all-messages (msg))

(defmethod all-messages ((msg call-message))
  (list msg))

(defmethod all-messages ((msg guard-message))
  (all-messages (the-message msg)))

(defmethod all-messages ((msg group-message))
  (apply #'append (mapcar #'all-messages  (messages msg))))

(defmethod all-messages ((msg alt-group))
  (append (all-messages (if-message msg)) 
          (all-messages (else-message msg))))

(defmethod to-object ((msg message))
  (to-object (last (all-messages msg))))

(defparameter *context-objects* nil)
(defun find-object-by-name (n)
  (find-if #'(lambda (object)
               (string= n (name object)))
           *context-objects*))

(defun intern-object (name)
  (or (find-object-by-name name)
      (let* ((is-active (char=  #\! (char name 0)))
             (obj (make-instance 'object
                                 :name (if is-active (subseq name 1) name)
                                 :is-active is-active)))                                 
        (alexandria:appendf *context-objects* (list obj))
        (values obj t))))

(defun make-call-by-char-type (ch label from to)
  (cond ((char= ch #\=) (make-instance 'syn-call :label label :from-object from :to-object to))
        ((char= ch #\-) (make-instance 'asy-call :label label :from-object from :to-object to))
        ((char= ch #\n) (make-instance 'new-call :label label :from-object from :to-object to))
        ((char= ch #\x) (make-instance 'del-call :label label :from-object from :to-object to))
        ((char= ch #\/) (make-instance 'ret-call :label label :from-object from :to-object to))))

(defun parse-message-director (sym)
  (cl-ppcre:register-groups-bind
      (type-ch obj-name msg-label ret-label)
      ("(.+)>([^\./]+)\.([^/]+)(/.*)?" (if (stringp sym) sym (symbol-name sym)))      
    (list  (elt type-ch 0)
           obj-name
           msg-label
           ret-label)))

(defun make-by-director (from-obj director)
  (destructuring-bind (prefix obj-name msg-label ret-label) (parse-message-director director)
    (multiple-value-bind (to-obj existp) (intern-object obj-name) 
      (let ((call-msg (make-call-by-char-type prefix msg-label from-obj to-obj))
            (ret-msg (when ret-label
                       (make-instance 'ret-call
                                      :label ret-label
                                      :from-object to-obj
                                      :to-object from-obj))))
        (if (typep call-msg 'new-call)
            (if (not existp)
                (error (format nil "The name ~s is used by other." obj-name))
                (setf (new-message to-obj) call-msg)))
        (values call-msg ret-msg)))))

(defparameter *context-current-object* nil)
(defun convert-to-message (dir-or-msg)
  (if (typep dir-or-msg 'message)
      (values dir-or-msg nil)
      (make-by-director *context-current-object* dir-or-msg)))

(defun m-progn (&rest dir-or-msg-s)
  (if (null dir-or-msg-s) (make-instance 'group-message)
      (push-to (convert-to-message (car dir-or-msg-s))
               (apply 'm-progn (cdr dir-or-msg-s)))))

(defun m-chain (&rest dir-or-msg-s)
  (if (null dir-or-msg-s) (make-instance 'group-message)
      (multiple-value-bind (call ret) (convert-to-message (car dir-or-msg-s))
        (append-to ret
                   (push-to call (let ((*context-current-object* (to-object call)))
                                   (apply 'm-chain (cdr dir-or-msg-s))))))))
(defun m-opt (guard msg)
  (make-instance 'opt-guard
                 :gurad guard
                 :the-message msg))

(defun m-loop (guard msg)
  (make-instance 'loop-guard
                 :gurad guard
                 :the-message msg))

(defun m-if (guard if-msg else-msg)
  (make-instance 'alt-group
                 :ifmsg (make-instance 'guard-message
                                       :guard guard
                                       :the-message if-msg)
                 :elsemsg (make-instance 'guard-message
                                         :guard (concatenate 'string "not " guard)
                                         :the-message else-msg)))

;;原始的调试程序
(defgeneric debug-print (msg))
(defmethod debug-print ((msg message)) t)


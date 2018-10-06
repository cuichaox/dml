
(uiop:define-package :dml.seq.core
    (:mix #:cl
          #:alexandria)
  (:export #:object
           #:new-message
           #:end-message
           #:callers
           #:name
           #:is-active
           #:message
           #:messages
           #:label
           #:call-message
           #:from-object
           #:to-object
           #:syn-call
           #:asy-call
           #:ret-call
           #:new-call
           #:del-call
           #:self-call
           #:multi-message
           #:all-call-messages
           #:group-message
           #:guard-message
           #:guard
           #:frame-guard
           #:the-message
           #:opt-frame
           #:loop-frame
           #:alt-group
           #:*context-objects*
           #:&prog
           #:&chain
           #:&opt
           #:&if
           #:&loop))

(in-package :dml.seq.core)

(defclass object.bar ()
  ((call-message :accessor call-message :initarg :call-message :initform nil)
   (ret-message :accessor ret-message :initarg :ret-message :initform nil))
  (:documentation "Active strip on  life cycle Line."))

(defclass object ()
  ((name :accessor name :initarg :name :initform "")
   (is-active :accessor is-active :initarg :is-active :initform nil)
   (callers :accessor callers :initarg :callers :initform nil)
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

(defclass self-call (call-message)
  ((to-object :initarg :from-object)))

(defclass multi-message (message) ())

(defclass group-message (multi-message)
  ((messages :accessor messages :initarg :messages :initform nil)))


(defun push-to (msg grp-msg)
  (progn (when msg (push msg (messages grp-msg)))
         grp-msg))

(defun append-to (msg grp-msg)
  (progn (when msg (alexandria:appendf (messages grp-msg) (list msg)))
         grp-msg))

(defclass guard-message (message)
  ((guard :accessor guard :initarg :guard :initform "")
   (the-message :accessor the-message :initarg :the-message :initform nil)))

(defclass frame-guard (guard-message)
  ())

(defclass opt-frame (frame-guard)
  ((label :initform "opt")))

(defclass loop-frame (frame-guard)
  ((label :initform "loop")))

(defclass alt-frame (frame-message)
  ((label :initform "alt")
   (the-message :accessor if-message :initarg :if-message :initform nil)
   (else-message :accessor else-message :initarg :else-message :initform nil)))

(defgeneric all-call-messages (msg))

(defmethod all-call-messages ((msg call-message))
  (list msg))

(defmethod all-call-messages ((msg guard-message))
  (all-call-messages (the-message msg)))

(defmethod all-call-messages ((msg group-message))
  (apply #'append (mapcar #'all-call-messages  (messages msg))))

(defmethod all-call-messages ((msg alt-frame))
  (append (all-call-messages (if-message msg)) 
          (all-call-messages (else-message msg))))

(defgeneric last-objct (msg))

(defmethod last-object ((msg message))
  (to-object (car (last (all-call-messages msg)))))

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
  (cond ((eq from to)  (make-instance 'self-call :label label :from-object from))
        ((char= ch #\=) (make-instance 'syn-call :label label :from-object from :to-object to))
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
           (if ret-label (subseq ret-label 1) nil))))

(defun make-by-director (from-obj director)
  (destructuring-bind (prefix obj-name msg-label ret-label) (parse-message-director director)
    (when (equal obj-name "this") (if from-obj (setf obj-name (name from-obj))
                                               (error "call self message on nil object.")))
    (multiple-value-bind (to-obj existp) (intern-object obj-name) 
      (let* ((call-msg (make-call-by-char-type prefix msg-label from-obj to-obj))
             (ret-msg (when (and (not (typep call-msg 'self-call)) ret-label)
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

(defmacro &prog (&rest dir-or-msg-s)
  (if (null dir-or-msg-s)
      `(make-instance 'group-message)
      (let ((first (car dir-or-msg-s))
            (others (cdr dir-or-msg-s)))
        (if (stringp first)
            `(multiple-value-bind ($call $ret) (convert-to-message ,first)
               (when $ret
                 (appendf (callers (to-object $call))
                          (list (cons $call $ret))))
               (push-to $call (push-to $ret (&prog ,@others))))
            `(let (($call ,first))
               (push-to $call (&prog ,@others)))))))

(defmacro &chain (&rest dir-or-msg-s)
  (if (null dir-or-msg-s)
      `(make-instance 'group-message)
      (let ((first (car dir-or-msg-s))
            (others (cdr dir-or-msg-s)))
        (if (stringp first)
            `(multiple-value-bind ($call $ret) (convert-to-message ,first)
               (when $ret
                   (appendf  (callers (to-object $call))
                             (list (cons $call $ret))))
               (append-to $ret
                       (push-to $call
                               (let ((*context-current-object* (last-object $call)))
                                 (&chain ,@others)))))
            `(let (($call ,first))
               (push-to $call (&chain ,@others)))))))

(defmacro as-single-msg (msg)
  (if (stringp msg)
      `(&prog ,msg)
      `,msg))

(defmacro &opt (guard msg)
  `(make-instance 'opt-frame
                  :guard ,guard
                  :the-message  (as-single-msg ,msg)))

(defmacro &loop (guard msg)
  `(make-instance 'loop-frame
                  :guard ,guard
                  :the-message (as-single-msg ,msg)))

(defmacro &if (guard if-msg &optional (else-msg nil))
  (if (null else-msg)
      `(&opt ,guard ,if-msg)
      `(make-instance 'alt-frame
                      :guard ,guard
                      :the-message  (as-single-msg ,if-msg)
                      :else-message (make-instance 'guard-message
                                                   :guard (concatenate 'string)
                                                   :the-message (as-single-msg ,else-msg)))))



(require :asdf)
(require :alexandria)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defpackage #:entity-store
  (:nicknames #:es)
  (:use #:cl)
  (:export #:create
           #:entity-id
           #:add-attr-type
           #:add-entity-type
           #:get-entity-by-id
           #:get-attr-val
           #:create-entity
           #:update-entity))

(in-package #:entity-store)


(define-condition unknown-attr-type (error)
  ((attr-type :initarg :attr-type :reader attr-type))
  (:documentation "Signaled when a reference to a non-existing
attribute type is discovered."))

(define-condition missing-value-for-attr (error)
  ((attr-name :initarg :attr-name)))

(define-condition unknown-entity-type (error)
  ((entity-type :initarg :entity-type)))

(define-condition invalid-value-for-attr (error)
  ((attr-name :initarg :attr-name)
   (value :initarg :value))
  (:documentation "Signaled when a value provided for an attribute
did not make the predicate for the attribute type true."))

(define-condition unknown-attr-for-entity-type (error)
  ((attr :initarg :attr)
   (entity-type :initarg :entity-type)))

(define-condition duplicate-entity-type-name (error)
  ((name :initarg :name))
  (:documentation "Signaled when an attempt is discovered to create
an entity type with the same name as a an existing entity type."))

(defclass attr-class ()
  ((attr-type :initarg :attr-type)
   (data :initform nil
         :initarg :data)))

(defclass entity-type-class ()
  ((attrs :initarg :attrs)))

(defclass entity-store ()
  ((attr-types :initform (list))
   (entity-types :initform (make-hash-table :test #'eq))
   (entities :initform (make-hash-table :test #'eq))
   (entity-id :initform 0)))

(defclass entity-class ()
  ((id :initarg :id :reader entity-id)
   (type :initarg :type)
   (attrs :initarg :attrs)
   (entity-store :initarg :entity-store)))

(defmethod print-object ((obj entity-class) stream)

  (loop
     for attr-type-and-value being the elements of (slot-value obj 'attrs)
     for attr-type = (car attr-type-and-value)
     for attr-value = (cdr attr-type-and-value)

     do (format t "~a : ~a~%" attr-type attr-value)))


(defun get-attr-val (entity attr-name)
  (check-type entity entity-class)
  (check-type attr-name keyword)

  (let* ((attrs (slot-value entity 'attrs))
         (attr (assoc attr-name attrs)))

    (unless attr
      (error 'unknown-attr-for-entity-type
             :attr attr-name
             :entity-type (slot-value entity 'entity-type)))

    (cdr attr)))

        

(defun attr-type-by-name (entity-store name)
  (check-type entity-store entity-store)
  (check-type name keyword)

  (let* ((attr-types (slot-value entity-store 'attr-types))
         (attr-conf (assoc name attr-types)))

    (unless attr-conf
      (error 'unknown-attr-type :attr-type name))

    (cdr attr-conf)))

    

(defun create ()
  "Creates a new entity store."
  (make-instance 'entity-store))

(defun get-entity-type (entity-store key)
  (gethash key (slot-value entity-store 'entity-types)))

(defun get-entity-by-id (entity-store entity-id)
  (check-type entity-store entity-store)
  (check-type entity-id fixnum)
  (gethash entity-id (slot-value entity-store 'entities)))

(defun add-attr-type (entity-store key predicate)
  "Adds an attribute type to the given ENTITY-STORE."
  (check-type entity-store entity-store)
  (check-type key keyword)
  (check-type predicate function)

  (assert (null (assoc key (slot-value entity-store 'attr-types))))

  (push 
   (cons key predicate) 
   (slot-value entity-store 'attr-types)))


(defun add-entity-type (entity-store key attrs)
  (check-type entity-store entity-store)
  (check-type key keyword)

  (when (gethash key (slot-value entity-store 'entity-types))
    (error 'duplicate-entity-type-name
           :name key))

  (let ((new-attrs
         (loop
            for attr being the elements of attrs
            collect (let* ((attr-name (getf attr :name))
                           (attr-type (getf attr :type))
                           
                           (attr-type-conf
                            (assoc
                             attr-type
                             (slot-value entity-store 'attr-types))))
                      
                      (unless attr-type-conf
                        (error 'unknown-attr-type :attr-type attr-type))

                      (cons attr-name
                            (make-instance 'attr-class :attr-type attr-type))))))

    (let ((entity-type (make-instance 'entity-type-class :attrs new-attrs)))

      (setf (gethash key (slot-value entity-store 'entity-types))
            entity-type))))


  
(defun create-entity (entity-store key attrs)
  (check-type entity-store entity-store)
  (check-type key keyword)
  (check-type attrs list)
  (let* ((entity-type (get-entity-type entity-store key)))

    (unless entity-type
      (error 'unknown-entity-type
             :entity-type key))
         
    (let* ((attr-types (slot-value entity-store 'attr-types))
           (entity-type-attrs (slot-value entity-type 'attrs))
           (final (list)))

      (loop
         for entity-type-attr being the elements of entity-type-attrs
         for attr-name = (car entity-type-attr)
         for attr-conf = (cdr entity-type-attr)
         for attr-type = (slot-value attr-conf 'attr-type)
         for concrete-attr = (assoc attr-name attrs)
         for pred = (cdr (assoc attr-type attr-types))
         do (if (null concrete-attr)
                (error 'missing-value-for-attr
                       :attr-name attr-name)
                (let* ((concrete-value (cdr concrete-attr))
                       (res (funcall pred concrete-value nil entity-store)))

                  (if res
                      (push (cons attr-name concrete-value) final)
                      (error 'invalid-value-for-attr
                             :attr-name attr-name
                             :value concrete-value)))))

      (let ((new-entity (make-instance 'entity-class
                                       :entity-store entity-store
                                       :attrs final
                                       :type key
                                       :id (incf (slot-value entity-store 'entity-id)))))

        (setf (gethash (slot-value new-entity 'id)
                       (slot-value entity-store 'entities))
              new-entity)

        (return-from create-entity new-entity)))))


        
(defun update-entity (entity attr-key value)
  (check-type entity entity-class)
  (check-type attr-key keyword)

  (let* ((es (slot-value entity 'entity-store))
         (entity-types (slot-value es 'entity-types))
         (entity-type (gethash (slot-value entity 'type) entity-types))
         (attrs (slot-value entity-type 'attrs))
         (attr (assoc attr-key attrs)))

    (unless attr
      (error 'unknown-attr-for-entity-type
             :attr attr-key
             :entity-type entity-type))

    (let* ((abstract-attr (cdr attr))
           (attr-type-name (slot-value abstract-attr 'attr-type))
           (pred (attr-type-by-name es attr-type-name))

           (valid (funcall pred value nil es)))

      (unless valid
        (error 'invalid-value-for-attr
               :attr-name attr-key
               :value value))

      (let* ((existing-attrs (slot-value entity 'attrs))
             (existing-attr (assoc attr-key existing-attrs)))

        (setf (cdr existing-attr) value)))))

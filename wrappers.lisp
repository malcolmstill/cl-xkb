(in-package :xkb)

(define-condition context-creation-error (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Could not create the xkb context"))))

(define-condition keymap-creation-error (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Could not create the xkb context"))))

(define-condition invalid-keysym-name (warning)
  ((name :initarg :name :reader invalid-keymap-name-name))
  (:report (lambda (condition stream)
	     (format stream "Keymap name ~S is invalid"
		     (invalid-keymap-name-name condition)))))

(define-condition invalid-keysym-code (warning)
  ((keysym :initarg :keysym :reader invalid-keysym-code-code))
  (:report (lambda (condition stream)
	     (format stream "Keysym code ~S is invalid"
		     (invalid-keysym-code-code condition)))))

(defun xkb-keysym-get-name (keysym)
  (with-foreign-pointer-as-string (string-buffer #1=64)
    (when (minusp (%xkb-keysym-get-name keysym string-buffer #1#))
      (return-from xkb-keysym-get-name))))

(define-compiler-macro xkb-keysym-get-name (&whole whole keysym)
  (if (constantp keysym)
      (let ((name (xkb-keysym-get-name keysym)))
	(if name
	    name
	    (progn
	      (warn 'invalid-keysym-code :keysym keysym)
	      name)))
      whole))

(defun xkb-keysym-from-name (name &optional (flags :no-flags))
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (%xkb-keysym-from-name name flags)))
    (declare (type (unsigned-byte 32) code))
    (if (= code 0)
        nil
        code)))

(define-compiler-macro xkb-keysym-from-name (&whole whole name &optional (flags :no-flags))
  (if (and (constantp name)  (constantp flags))
      (let ((keysym (keysym-from-name name flags)))
	(if keysym
	    keysym
	    (progn
	      (warn 'invalid-keysym-name :name name)
	      keysym)))
	whole))

(defun xkb-context-new (context-flags)
  (let ((context (%xkb-context-new context-flags)))
    (when (cffi:null-pointer-p context)
      (error 'context-creation-error))
    context))

(defun xkb-keymap-new-from-names (context names flags)
  (let ((keymap (%xkb-keymap-new-from-names context names flags)))
    (when (cffi:null-pointer-p keymap)
      (error 'keymap-creation-error))
    keymap))

(defun new-keymap-from-names (ctx rules model layout variant options)
  (with-foreign-object (names '(:struct xkb-rule-names))
    (with-foreign-strings ((rules-ptr rules)
			   (model-ptr model)
			   (layout-ptr layout)
			   (variant-ptr variant)
			   (options-ptr options))
      (setf (foreign-slot-value names '(:struct xkb-rule-names) 'rules) rules-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'model) model-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'layout) layout-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'variant) variant-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'options) options-ptr)
      (xkb-keymap-new-from-names ctx names ()))))

(defmacro with-keymap-from-names ((keymap-name (context rules flags)) &body body)
  `(let ((,keymap-name (xkb-keymap-new-from-names ,context ,rules ,flags)))
     (unwind-protect
	  (progn ,@body)
       (keymap-unref ,keymap-name))))

(defmacro with-xkb-context ((context-name (flags)) &body body)
  `(let ((,context-name (xkb-context-new ,flags)))
     (unwind-protect
	  (progn ,@body)
       (xkb-context-unref ,context-name))))

(defmacro with-xkb-rule-names ((rule-name (&key (rules "")
						(model "")
						(layout "")
						(variant "")
						(options "")))
			       &body body)
  "Bind RULE-NAME to a struct xkb_rule_names object within
the context and initalize its fields using the supplied values."
  (let ((rules-ptr (gensym "rules-ptr"))
	(model-ptr (gensym "model-ptr"))
	(layout-ptr (gensym "layout-ptr"))
	(variant-ptr (gensym "variant-ptr"))
	(options-ptr (gensym "options")))
    `(cffi:with-foreign-object (,rule-name '(:struct rule-names))
       (cffi:with-foreign-strings ((,rules-ptr ,rules)
				   (,model-ptr ,model)
				   (,layout-ptr ,layout)
				   (,variant-ptr ,variant)
				   (,options-ptr ,options))
	 (setf (cffi:foreign-slot-value ,rule-name '(:struct rule-names) rules) ,rules-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) model) ,model-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) layout) ,layout-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) variant) ,variant-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) options) ,options-ptr)
	 ,@body))))

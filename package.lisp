(uiop:define-package :codewalk/package
  (:nicknames :codewalk)
  (:import-from :bordeaux-threads
                #:lock #:make-lock #:with-lock-held)
  (:import-from :alexandria
                #:if-let #:once-only)
  (:use :cl :iterate)
  (:export

   #:register-subform-types
   #:walk-subforms #:form-typecase))
(in-package :codewalk/package)

(defvar *subforms-lock* (make-lock))
(defvar *subform-types* nil)

(defun %register-subform-types (name template)
  (with-lock-held (*subforms-lock*)
    (if-let (pair (assoc name *subform-types*
                         :test #'eq))
      (setf (cdr pair) template)
      (push (cons name template) *subform-types*)))
  template)

(defmacro register-subform-types (name &body template)
  `(%register-subform-types ',name '(:form-head ,@template)))

(defparameter *funcall-template* '(:function &rest :expr))

(defun subforms-template (name)
  (if (typep name 'symbol)
      (if-let (pair (with-lock-held (*subforms-lock*)
                      (assoc name *subform-types*
                             :test #'eq)))
        (if (and (consp (cdr pair))
                 (eq (first (cdr pair)) :as))
            (subforms-template (second (cdr pair)))
            (cdr pair))
        *funcall-template*)
      (error "subforms-template name ~s is not a symbol"
             name)))

(register-subform-types block :block-name
  &body :expr)
(register-subform-types catch :catch-tag
  &body :expr)
(register-subform-types eval-when (&rest :eval-time)
  &body :expr)
(register-subform-types flet (&rest (:function-binding (&rest :variable-binding) &body :expr))
  &body :expr)
(register-subform-types function :function)
(register-subform-types go :go-tag)
(register-subform-types if :expr :expr &optional :expr)
(register-subform-types labels :as flet)
(register-subform-types let (&rest (:variable-binding :expr))
  &body :expr)
(register-subform-types let* :as let)
(register-subform-types load-time-value :expr &optional :constant)
(register-subform-types progn
  &body :expr)

(defun walk-with-template (function form template)
  (etypecase template
    (null (assert (null form)) nil)
    (cons (walk-list function form template))
    (keyword (replace-or-recurse function template form))))

(defun replace-or-recurse (function template form)
  (let* ((new (funcall function template form)))
    (if (and (eq new form) (consp form))
        (walk-subforms function form)
        new)))

(defun recurse-preserving-eq (function form template)
  (let* ((new-car (walk-with-template function (car form) (car template)))
         (new-cdr (walk-with-template function (cdr form) (cdr template))))
    (if (and (eq (car form) new-car)
             (eq (cdr form) new-cdr))
        form
        (cons new-car new-cdr))))

(defun walk-list (function form template)
  (case (first template)
    ((&rest &body) (walk-rest function form (rest template)))
    (&optional (walk-optional function form (rest template)))
    (&key (walk-key function form (rest template)))
    (otherwise (recurse-preserving-eq function form template))))

(defun walk-rest (function form template)
  (destructuring-bind (rest-type) template
    (iter (with changes = nil)
      (for subform in form)
      (for new = (walk-with-template function subform rest-type))
      (unless (eq new subform) (setf changes t))
      (collect new into updated)
      (finally (return (if changes updated form))))))

(defun walk-optional (function form template)
  (cond ((not form) nil)
        ((not template) (error "unexpected extra &optional params ~s" form))
        ((and template form) (recurse-preserving-eq function form `(,(first template) &optional ,@(rest template))))))

(defun walk-key (function form template)
  (declare (ignore function form template))
  (error "implement support for &key params, idiot"))

(defun walk-subforms (function form)
  (walk-with-template function form (subforms-template (first form))))

(defmacro form-typecase ((type form) &body clauses)
  (once-only (type form)
    `(case ,type
       ,@clauses
       (otherwise ,form))))

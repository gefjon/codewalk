(uiop:define-package :codewalk/package
  (:nicknames :codewalk)
  (:import-from :bordeaux-threads
                #:lock #:make-lock #:with-lock-held)
  (:import-from :alexandria
                #:if-let #:once-only #:parse-body #:curry #:rcurry)
  (:use :cl :iterate)
  (:export

   #:register-subform-types
   #:walk-subforms #:form-typecase))
(in-package :codewalk/package)

(deftype form () t)
(deftype generalized-boolean () t)

(deftype visitor ()
  '(function (keyword form) (values form generalized-boolean &optional)))

(deftype template ()
  '(or keyword list (function (visitor form) (values form &optional))))

(declaim (type lock *subforms-lock*))
(defvar *subforms-lock* (make-lock))
(declaim (type list *subform-types*))
(defvar *subform-types* nil)

(declaim (ftype (function (symbol template) (values template &optional))
                %register-subform-types))
(defun %register-subform-types (name template)
  (with-lock-held (*subforms-lock*)
    (if-let (pair (assoc name *subform-types*
                         :test #'eq))
      (setf (cdr pair) template)
      (push (cons name template) *subform-types*)))
  template)

(defmacro register-subform-types (name &body template)
  `(%register-subform-types ',name '(:operator ,@template)))

(declaim (type list *funcall-template*))
(defparameter *funcall-template* '(:function &rest :expr))

(declaim (ftype (function (form) (values template &optional))
                subforms-template))
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

;;; i've been trying to keep these in alphabetical order
(register-subform-types and &rest :expr)
(register-subform-types assert :expr
  &optional (&rest :place) &rest :expr)
(register-subform-types block :block-name
  &rest :expr)
(register-subform-types case :expr
  &rest (:constant &rest :expr))
(register-subform-types catch :catch-tag
  &rest :expr)
(register-subform-types ccase :as case)
(register-subform-types check-type :place :type &optional :constant)
(register-subform-types cond
  &rest (:expr &rest :expr))
(register-subform-types decf :as incf)
(register-subform-types declaim &rest :declaration)
(register-subform-types defconstant :variable-binding :expr &optional :docstring)
(register-subform-types defparameter :as defconstant)
(register-subform-types defvar :variable-binding &optional :expr :docstring)
(register-subform-types ecase :as case)
(register-subform-types eval-when (&rest :eval-time)
  &rest :expr)
;; FIXME: accept complex lambda lists?
;; FIXME: declarations and docstrings in the local function bodies?
(register-subform-types flet (&rest (:function-binding (&rest :variable-binding) &body :expr))
  &body :expr)
(register-subform-types formatter :constant)
(register-subform-types function :function)
(register-subform-types go :go-tag)
(register-subform-types ignore-errors :as :progn)
(register-subform-types incf :place &optional :expr)
(register-subform-types in-package :package-name)
(register-subform-types if :expr :expr &optional :expr)
(register-subform-types labels :as flet)
;; FIXME: accept complex lambda lists?
;; FIXME: declarations and docstrings in the local function bodies?
(register-subform-types lambda (&rest :variable-binding)
  &body :expr)
;; FIXME: accept bare symbols as let bindings?
(register-subform-types let (&rest (:variable-binding :expr))
  &body :expr)
(register-subform-types let* :as let)
(register-subform-types load-time-value :expr &optional :constant)
(register-subform-types or :as and)
(register-subform-types pop :place)
(register-subform-types progn
  &rest :expr)
(register-subform-types prog1 :expr &rest :expr)
(register-subform-types prog2 :expr :expr &rest :expr)
(register-subform-types push :expr :place)
(register-subform-types quote :constant)
(register-subform-types return :expr)
(register-subform-types return-from :block-name :expr)
(register-subform-types rotatef &rest :place)
(register-subform-types the :expr :type)
(register-subform-types throw :catch-tag :expr)
(register-subform-types unless :as when)
(register-subform-types unwind-protect :expr &rest :expr)
(register-subform-types when :expr &rest :expr)

(declaim (ftype (function (visitor form template) (values form &optional))
                walk-with-template))
(defun walk-with-template (function form template)
  (etypecase template
    (null (assert (null form)) nil)
    (function (funcall template function form))
    (cons (walk-list function form template))
    (keyword (replace-or-recurse function template form))))

(declaim (ftype (function (visitor keyword form) (values form &optional))
                replace-or-recurse))
(defun replace-or-recurse (function template form)
  (multiple-value-bind (new-form recurse-deeper-p) (funcall function template form)
    (if (and recurse-deeper-p (consp new-form)) (walk-subforms function new-form)
        new-form)))

(declaim (ftype (function (visitor form list) (values form &optional))
                walk-ordinary-pair))
(defun walk-ordinary-pair (function form template)
  (cons (walk-with-template function (car form) (car template))
        (walk-with-template function (cdr form) (cdr template))))

(declaim (ftype (function (visitor form list) (values form &optional))
                walk-list))
(defun walk-list (function form template)
  (case (first template)
    (&body (walk-body function form (rest template)))
    (&rest (walk-rest function form (rest template)))
    (&optional (walk-optional function form (rest template)))
    (&key (walk-key function form (rest template)))
    (otherwise (walk-ordinary-pair function form template))))

(defun visit-declare-form (visitor declare)
  (destructuring-bind (head &rest declarations) declare
    (assert (eq head 'declare))
    (cons head (iter (for decl in declarations)
                 (collect (walk-with-template visitor decl :declaration))))))

(declaim (ftype (function (visitor form list) (values form &optional))
                walk-body))
(defun walk-body (visitor form template)
  (multiple-value-bind (body declarations docstring) (parse-body form :documentation t)
    (append (when docstring (list (walk-with-template visitor docstring :docstring)))
            (iter (for declare in declarations)
              (collect (visit-declare-form visitor declare)))
            (walk-rest visitor body template))))

(declaim (ftype (function (visitor form list) (values form &optional))
                walk-rest))
(defun walk-rest (function form template)
  (destructuring-bind (rest-type) template
    (iter (for subform in form)
      (collect (walk-with-template function subform rest-type)))))

(declaim (ftype (function (visitor form list) (values form &optional))
                walk-optional))
(defun walk-optional (function form template)
  (cond ((not form) nil)
        ((not template) (error "unexpected extra &optional params ~s" form))
        ((and template form) (walk-ordinary-pair function form `(,(first template) &optional ,@(rest template))))))

(declaim (ftype (function (visitor form list) (values form &optional))
                walk-key))
(defun walk-key (function form template)
  (declare (ignore function form template))
  (error "implement support for &key params, idiot"))

(declaim (ftype (function (visitor form) (values form &optional))
                walk-subforms))
(defun walk-subforms (function form)
  "FUNCTION should accept two arguments, a keyword FORM-TYPE and a FORM, and should return (values NEW-FORM RECURSE-INTO-NEW-P).

If RECURSE-INTO-NEW-P is true, FUNCTION will also be applied to the subforms of NEW-FORM; if
RECURSE-INTO-NEW-P is nil (or no second value is present), it will be used unchanged."
  (walk-with-template function form (subforms-template (first form))))

(defmacro form-typecase ((type form) &body clauses)
  (once-only (type form)
    `(case ,type
       ,@clauses
       (otherwise (values ,form t)))))

;; ---------------------Q1--------------------------------
;; complete LETREC-CONT with cons,car (head),cdr (tail),null? and empty list.
;; give changes in Expression, Expval, Continuation, Apply-cont, Value-of/k

(define-datatype expression expression? 
	…
	(list-exp (exp (list-of expression?))
	(cons-exp (exp1 expression?) (exp2 expression?))
	(cdr-exp (exp1 expression?)) 
	(car-exp (exp1 expression?))
	(null?-exp (exp1 expression?))
	(emptylist-exp)
	…
)

(define-datatype expval expval? 
	…
(list-val
  (lst (list-of expval?)))	
…
)

(define-datatype continuation continuation?
	… 
	(cons1-cont
	  (exp2 expression?) 
	  (saved-env environment?)
	  (saved-cont continuation?))
	(cons2-cont 
	  (val1 expval?) 
	  (saved-cont continuation?))
	(cdr-cont
	  (saved-cont continuation?))
	(car-cont
	  (saved-cont continuation?)) 
	(null-cont
	  (saved-cont continuation?))
	
)

(define value-of/k 
	…
(null?-exp  (exp1)
(value-of/k exp1 env (null-cont cont))
(cdr-exp  (exp1)
(value-of/k exp1 env (cdr-cont cont))
(car-exp (exp1) 
	(value-of/k exp1 env (car-cont cont))
(emptylist-exp 
(apply-cont cont (list-val ‘())))
(cons-exp (exp1 exp2)
	(value-of/k exp1 env 
 	        (cons1-cont exp2 env cont)))
…
)

(define apply-cont
	…
	(null-cont (saved-cont) 
		     apply-cont saved-cont 
         (bool-val 
(null? (expval->list value))))
	(cdr-cont (saved-cont) 
	  	     apply-cont saved-cont 
         (list-val 
(cdr (expval->list value))))
	(car-cont (saved-cont) 
	    apply-cont saved-cont 
         (list-val 
(car (expval->list value))))
	(cons1-cont (exp2  saved-env saved-cont) 
                   (value-of/k exp2 saved-env (cons2-cont val saved-cont)))
	(cons2-cont (val1 saved-cont)
  	         (let ((list1 (expval->list val1))  
		     (list2 (expval-> list val)))
		  (apply-cont saved-cont (listval (cons list1 list2))))	  
	…
)

;; ---------------------Q2--------------------------------
;; https://github.com/EFanZh/EOPL-Exercises/blob/master/solutions/exercise-5.44.rkt
;An alternative to letcc and throw of the preceding exercises is to add a single procedure to the language. 
;This procedure, which in Scheme is called call-with-current-continuation, takes a one-argument procedure, p,
; and passes to p a procedure that when invoked with one argument, passes that argument to the current 
; continuation, cont. We could define call-with-current-continuation in terms of letcc and throw as follows:
let call-with-current-continuation
      = proc (p)
          letcc cont
          in (p proc (v) throw v to cont)
in ...

;Add call-with-current-continuation to the language. Then write a translator that takes the language with 
;letcc and throw and translates it into the language without letcc and throw, but with call-with-current-continuation.

;To translate a language with letcc and throw into the language without letcc and throw, just do the following:
; Translate letcc var in body into callcc(proc (var) body);
; Translate throw exp1 to exp2 into (exp2 exp1).

(define the-grammar
  '(...
    [expression ("callcc" "(" expression ")") callcc-exp]
    ...)
)
(define-datatype continuation continuation?
  [callcc-cont [cont continuation?] [saved-try-cont continuation?]]
)
;; Interpreter.
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	  [callcc-cont (saved-cont saved-try-cont) (apply-call val (cont-val saved-cont) saved-cont saved-try-cont)]
	  )))

(define value-of/k
  (lambda (exp env cont try-cont1)
    (cases expression exp
	 [callcc-exp (exp1) (value-of/k exp1 env (callcc-cont cont try-cont1) try-cont1)]
	 )))

;; ---------------------Q3--------------------------------
; Extension of CHECKED with data type tuples of the form ty1*ty2 . Must support 3 new methods:
; newTuple(exp1,exp2) , p1(exp1) and p2(exp2) , where newTuple creates a new tuple and p1 and p2
; project a tuple to its first and second component respectively.
; 1. Specify the type checking rules in CHECKED for newTuple, p1 and p2.
; 2. Specify the necessary extension of type-of for CHECKED.
; 3. What equations in INFERRED would the three new methods give?

; some example code that should run in your implementation.

let f = proc (p: (x coord: int =* y coord:int)) p:ycoord
 in (f (xcoord=3, ycoord=4))

let f = proc (p:(x:int* y:int)) p:y
 in (f (xcoord =3, ycoord=4)

let f = proc (p:(x:bool* y:int)) p:y
 in (f (xcoord =3, ycoord=4)


(define-datatype type type?
  (int-type)
  (bool-type)
  (tuple-type (e1 type?) (e2 type?))
  (proc-type (arg-type type?) (result-type type?))
  )

(define-datatype expression expression?
…
  (tuple-exp
   (exp1 expression?)
   (exp2 expression?))
   (t1-exp 
      (exp1 expression?))
   (t2-exp 
      (exp1 expression?))
  )

(define-datatype expval expval?
   …  
  (tuple-val
   (e1 expression?) (e2 expression?))
  )


// You could move this into value-of
(define expval->p1
  (lambda (v)
    (cases expval v
      (tuple-val (e1 e2) e1)
      (else (expval-extractor-error 'e1 v)))
  ))
(define expval->p2
  (lambda (v)
    (cases expval v
      (tuple-val (e1 e2) e2)
      (else (expval-extractor-error 'e2 v)))
  ))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (tuple-exp (e1 e2)
                 (tuple-val e1 e2))
      (t1-exp (tuple) (expval->p1 tuple))
      (t2-exp (tuple) (expval->p2 tuple))
…

(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (tuple-exp (e1 e2) (tuple-type (type-of e1 tenv) (type-of e2 tenv)))
      (t1-exp (t) (cases expval t
                    (tuple-val (e1 e2) (type-of e1 tenv))
                    (else (expval-extractor-error 'tuple t))))
      (t2-exp (t) (cases expval t
                    (tuple-val (e1 e2) (type-of e2 tenv))
                    (else (expval-extractor-error 'tuple t))))
…

; ----------------------seems correct solution-------------------------------------

(define-datatype type type?
	…
	(tuple-type (exp1-type type?)(exp2-type type?))
)

(define-datatype expression expression?
…
	(new-tuple-exp
		(exp1 expression?)
		(exp2 expression?)
	(p1-exp
		(exp1 expression?))
	(p2-exp
		(exp1 expression?))
  )
)

(define type-of
	… 
	(new-tuple-exp (exp1 exp2)
		(let ((ty1 (type-of exp1 tenv))
			(ty2 (type-of exp2 tenv))
		(tuple-type (ty1 ty2)))))
	(p1-exp (exp1)
		(let ((ty1 (type-of exp1 tenv)))
		   (cases type ty1
			If it’s a tuple return the type of exp1
			(tuple-type (exp1-type exp2-type)
				(exp1-type))
			(else
				(report-tuple-not-tuple-type-error … )))))
	(p2-exp (exp1)
		(let ((ty1 (type-of exp1 tenv))
		   (cases type ty1
			(tuple-type (exp1-type exp2-type)
				(exp2-type))
			(else
				(report-tuple-not-tuple-type-error … ))))))
)


(type-of exp1 tenv) = t1
(type-of exp2 tenv) = t2
---------------------------------
(type-of (new-tuple-exp exp1 exp2) tenv) = t1*t2

(type-of exp1 tenv)=t1*t2
---------------------------------
(type-of (p1-exp exp1) tenv) = t1

(type-of exp1 tenv)=t1*t2
---------------------------------
(type-of (p2-exp exp1) tenv) = t2

Equations in INFERRED:
  Type of new-tuple-exp exp1 exp2: (t1,t2)
	Type of p1-exp exp1: (t1,t2)->t1
  Type of p2-exp exp1: (t1,t2)->t2


;; ---------------------Q4--------------------------------
;; In EXCEPTIONS, using car or cdr on an empty list results in a runtime (Racket) error. Extend the language
;  to instead raise an exception when performing car or cdr on (), with exception values 11 and 12
;  respectively. You may only modify the unop-arg-cont case in apply-cont, no other changes are allowed.

 (unop-arg-cont (unop cont)
                     (if (and (equal? unop (car-unop)) (equal? val (list-val '())))
                         (apply-cont (raise1-cont cont) (num-val 11))
                         (if (and (equal? unop (cdr-unop)) (equal? val (list-val '())))
                             (apply-cont (raise1-cont cont) (num-val 12))
                             (apply-cont cont
                                 (apply-unop unop val)))))


;; ---------------------Q5--------------------------------
; Extend IMPLICIT-REFS with the address-of operator &, as well as the expressions deref (x) 
; and setref (x) (y) (as defined for EXPLICIT-REFS).
; Write out how to extend the expression definition.
; Write out how to extend the expval definition.
; Extend the value-of definition.
; Given the example program as an illustration of the extension, which evaluates to 1. 
; Write out the store after the execution of the program as precisely as possible:

let a = 3
   in let b = 4
      in let swap = proc(x) proc(y)
         let temp = deref x
            in begin; setref x = (deref y); setref y = temp; end
         in begin; (swap &a) &b; a - b; end

; Extend expressions:
  (deref-exp
   (exp1 expression?))  
  (setref-exp
   (exp1 expression?)
   (exp2 expression?))
  (&-exp
   (var symbol?))

; Extend expval:
  (ref-val
   (ref reference?))

; Extend value- of:
  	(deref-exp (exp1)
             	(let ((v1 (value-of exp1 env)))
               	(let ((ref1 (expval->ref v1)))
                 	(deref ref1))))
  	(setref-exp (exp1 exp2)
              	(let ((ref (expval->ref (value-of exp1 env))))
                	(let ((v2 (value-of exp2 env)))
                  	(begin
                    	(setref! ref v2)
                    	(num-val 23)))))
  	;; returns the pointer in the env for the var
  	(&-exp (var)
             	(ref-val (apply-env env var)))

; Extend expval: (you also need helper function for extraction)
(define expval->ref
  (lambda (v)
	(cases expval v
  	(ref-val (ref) ref)
  	(else (expval-extractor-error 'reference v)))))

;; ---------------------Q6--------------------------------
; Extend CHECKED with trees. A tree consists of a bunch of nodes which each have a left and a right subtree,
; and a bunch of leaves. You also need to support a case-construct for trees. 
; In this construct, exp1 is evaluated to either leaf or node. If it is a leaf, 
; exp2 is evaluated where 'v is bound to v. In case of a node, exp3 is evaluated where 'l is bound to left, 
; and 'r is bound to right.

 casetree exp1
  case leaf v => exp2
  case node(left, right) => exp3

;Show the changes needed in the type- and expression-datatypes.
;How does the type of expressed values need to change?
;Show the changes needed in the value-of definition.
;Show the changes needed in the type-of definition.

;; ---------------------Q7--------------------------------
; In IMPLICIT-REFS no special environment construction is needed for letrec (= extend-env-rec). 
; Provide the implementation for value-of by 
;     1) creating a reference and initializing it to a dummy value. 
;     2) create the right environment for the letrec 
;     3) build the right proc trap 
;     4) evaluate the body in the right environment.

(letrec-exp (p-name b-var p-body letrec-body)
;; Create the dummy ref
  (let ((r (newref 289))) 
;; Create a procval with an extended environnement containing the name of the procval.
   (let ((proc (proc-val (procedure b-var p-body (extend-env p-name r env)))))
;; Set the reference to this procval. As we have letrec we have to be able to recursively see the proc.
(let ((dummy (setref! r proc)))
;; Evaluate the body with the extended environnement containing the procval.
 (value-of letrec-body (extend-env p-name r env))))))

;; ---------------------Q8--------------------------------
; Add the following extensions to CLASSES:
; fieldref obj field-name: returns the value given the field of a given object.
; fieldset obj field-name exp: sets the given field of the given object to the value of the given expression.
; For each extension, give:
;            The syntax
;            The changes to the interpreter and some possible helper functions.

	   (fieldref-exp (exp name)
		     (let ((obj (value-of exp env)))
		       (deref (object->field obj name))))

	   (fieldset-exp (exp name val-exp)
			 (let* ((obj (value-of exp env))
				(val (value-of val-exp env))
				(ref (object->field obj name)))
			   (setref! ref val)))

	   )))

(define object->field
  (lambda (obj field-name)
    (let* ((fields (object->fields obj))
	   (class-name (object->class-name obj))
	   (field-names (class->field-names (lookup-class class-name))))
      (cond
       ((location field-name field-names)
	=> (lambda (n)
	     (list-ref fields n)))
       (else
	(error "object->field: can not find ~s" field-name))))))

(define object->fields
  (lambda (obj)
    (cases object obj
           (an-object (class-decl fields)
		      fields))))



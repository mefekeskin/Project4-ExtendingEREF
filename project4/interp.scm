(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require racket/trace)
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

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

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities.
        ; #####################################################

        (newvector-exp (exp1 exp2)
          (let ((len (expval->num (value-of exp1 env)))
                (val (value-of exp2 env)))
            (let ((vec (make-vector len)))
              (let loop ((i 0))
                (if (= i len)
                    (vec-val vec)
                    (begin
                      (vector-set! vec i (newref val))
                      (loop (+ i 1))))))))

        (read-vector-exp (exp1 exp2)
          (let ((vec (expval->vec (value-of exp1 env)))
                (idx (expval->num (value-of exp2 env))))
            (deref (vector-ref vec idx))))

        (update-vector-exp (exp1 exp2 exp3)
          (let ((vec (expval->vec (value-of exp1 env)))
                (idx (expval->num (value-of exp2 env)))
                (val (value-of exp3 env)))
            (begin
              (setref! (vector-ref vec idx) val)
              (num-val 23))))

        (length-vector-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (num-val (vector-length vec))))

        (swap-vector-exp (exp1 exp2 exp3)
          (let ((vec (expval->vec (value-of exp1 env)))
                (i (expval->num (value-of exp2 env)))
                (j (expval->num (value-of exp3 env))))
            (let ((ref-i (vector-ref vec i))
                  (ref-j (vector-ref vec j)))
              (let ((val-i (deref ref-i))
                    (val-j (deref ref-j)))
                (begin
                  (setref! ref-i val-j)
                  (setref! ref-j val-i)
                  (num-val 23))))))

        (copy-vector-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (let ((len (vector-length vec)))
              (let ((new-vec (make-vector len)))
                (let loop ((i 0))
                  (if (= i len)
                      (vec-val new-vec)
                      (begin
                        (vector-set! new-vec i
                          (newref (deref (vector-ref vec i))))
                        (loop (+ i 1)))))))))
        (newqueue-exp (exp1)
          (let ((cap (expval->num (value-of exp1 env))))
            (let ((vec (make-vector (+ cap 3))))
              (let loop ((i 0))
                (if (= i (+ cap 3))
                    (vec-val vec)
                    (begin
                      (vector-set! vec i (newref (num-val 0)))
                      (loop (+ i 1))))))))

        (enqueue-exp (exp1 exp2)
          (let ((vec (expval->vec (value-of exp1 env)))
                (val (value-of exp2 env)))
            (let ((size (queue-size-of vec))
                  (cap (queue-capacity-of vec)))
              (if (= size cap)
                  (eopl:error 'enqueue "stack overflow")
                  (let ((rear (queue-rear-of vec)))
                    (begin
                      (setref! (vector-ref vec (+ 3 rear)) val)
                      (setref! (vector-ref vec 2)
                               (num-val (modulo (+ rear 1) cap)))
                      (setref! (vector-ref vec 0)
                               (num-val (+ size 1)))
                      (num-val 23)))))))

        (dequeue-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (let ((size (queue-size-of vec)))
              (if (= size 0)
                  (num-val -1)
                  (let ((front (queue-front-of vec))
                        (cap (queue-capacity-of vec)))
                    (let ((val (deref (vector-ref vec (+ 3 front)))))
                      (begin
                        (setref! (vector-ref vec 1)
                                 (num-val (modulo (+ front 1) cap)))
                        (setref! (vector-ref vec 0)
                                 (num-val (- size 1)))
                        val)))))))

        (queue-size-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (num-val (queue-size-of vec))))

        (peek-queue-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (let ((front (queue-front-of vec)))
              (deref (vector-ref vec (+ 3 front))))))

        (queue-empty?-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (if (= (queue-size-of vec) 0)
                (bool-val #t)
                (bool-val #f))))

        (print-queue-exp (exp1)
          (let ((vec (expval->vec (value-of exp1 env))))
            (let ((size (queue-size-of vec))
                  (front (queue-front-of vec))
                  (cap (queue-capacity-of vec)))
              (begin
                (let loop ((i 0))
                  (if (= i size)
                      (newline)
                      (begin
                        (display
                          (expval->num
                            (deref
                              (vector-ref vec
                                (+ 3 (modulo (+ front i) cap))))))
                        (if (= i (- size 1))
                            (newline)
                            (display " "))
                        (loop (+ i 1)))))
                (num-val 23)))))

        )))
  

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE


  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
  ;; Queue layout over Part A vectors (no globals; metadata lives in the vector):
  ;;   index 0      -> current size
  ;;   index 1      -> front pointer (offset within data area)
  ;;   index 2      -> rear pointer  (offset within data area)
  ;;   indices 3..  -> circular data area of length = capacity
  ;; capacity = (length-vector vec) - 3

  (define queue-size-of
    (lambda (vec)
      (expval->num (deref (vector-ref vec 0)))))

  (define queue-front-of
    (lambda (vec)
      (expval->num (deref (vector-ref vec 1)))))

  (define queue-rear-of
    (lambda (vec)
      (expval->num (deref (vector-ref vec 2)))))

  (define queue-capacity-of
    (lambda (vec)
      (- (vector-length vec) 3)))
 
  )
  


  

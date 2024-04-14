#lang racket

(define fact (lambda (n)
               (if(= n 0)
                  1
                  (* n(fact(- n 1))))))

(define factorialenvoltorio(lambda (n)
                             (define factorialcola(lambda (n result)
                                                    (if (= n 0)
                                                        result
                                                        (factorialcola(- n 1) (* n result)))))
                             (factorialcola n 1)))
                                                                                                    

(define suma (lambda(n)(+ 1 2 n)))
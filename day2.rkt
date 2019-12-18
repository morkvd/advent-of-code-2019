#lang racket
(define program ;; answers 3716293
  "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,19,6,23,2,13,23,27,1,9,27,31,2,31,9,35,1,6,35,39,2,10,39,43,1,5,43,47,1,5,47,51,2,51,6,55,2,10,55,59,1,59,9,63,2,13,63,67,1,10,67,71,1,71,5,75,1,75,6,79,1,10,79,83,1,5,83,87,1,5,87,91,2,91,6,95,2,6,95,99,2,10,99,103,1,103,5,107,1,2,107,111,1,6,111,0,99,2,14,0,0")

(define test-program
  "1,9,10,3,2,3,11,0,99,30,40,50")

(define (initialize-memory p)
  (map string->number (string-split p ",")))

(define (safe-address? memory pointer)
  (< pointer (length memory)))

(define (instruction-99 memory pointer)
  (list "halting" memory))

(define (instruction-1 memory pointer)
  (if (safe-address? memory (+ pointer 3))
      (let ([opcode (list-ref memory pointer)]
            [arg-1  (list-ref memory (+ pointer 1))]
            [arg-2  (list-ref memory (+ pointer 2))]
            [arg-3  (list-ref memory (+ pointer 3))])
        (execute-program
         (list-set memory arg-3 (+ (list-ref memory arg-1)
                                   (list-ref memory arg-2)))
         (+ pointer 4)))
      memory))

(define (instruction-2 memory pointer)
  (if (safe-address? memory (+ pointer 3))
      (let ([opcode (list-ref memory pointer)]
            [arg-1 (list-ref memory (+ pointer 1))]
            [arg-2 (list-ref memory (+ pointer 2))]
            [arg-3 (list-ref memory (+ pointer 3))])
        (execute-program
         (list-set memory arg-3 (* (list-ref memory arg-1)
                                    (list-ref memory arg-2)))
         (+ pointer 4)))
      memory))

(define (opcode-error program pointer)
  "opcode not recognized")

(define (execute-program program pointer)
  (if (safe-address? program pointer)
      (let ([current-instruction (list-ref program pointer)])
        (cond
          [(= current-instruction 99)
           (instruction-99 program pointer)]
          [(= current-instruction 1)
           (instruction-1 program pointer)]
          [(= current-instruction 2)
           (instruction-2 program pointer)]
          [else (opcode-error program pointer)]))
      "error"))
  

(execute-program (initialize-memory program) 0)
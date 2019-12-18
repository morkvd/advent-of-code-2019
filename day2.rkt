#lang racket
(define program 
  "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,19,6,23,2,13,23,27,1,9,27,31,2,31,9,35,1,6,35,39,2,10,39,43,1,5,43,47,1,5,47,51,2,51,6,55,2,10,55,59,1,59,9,63,2,13,63,67,1,10,67,71,1,71,5,75,1,75,6,79,1,10,79,83,1,5,83,87,1,5,87,91,2,91,6,95,2,6,95,99,2,10,99,103,1,103,5,107,1,2,107,111,1,6,111,0,99,2,14,0,0")

(define test-program
  "1,9,10,3,2,3,11,0,99,30,40,50")

(define (initialize-memory p)
  (map string->number (string-split p ",")))

(define (safe-address? memory pointer)
  (< pointer (length memory)))

(define (instruction-99 memory instruction-pointer)
  (list "halting" memory))

(define (instruction-1 memory instruction-pointer)
  (if (safe-address? memory (+ instruction-pointer 3))
      (let ([opcode (list-ref memory instruction-pointer)]
            [parameter-1  (list-ref memory (+ instruction-pointer 1))]
            [parameter-2  (list-ref memory (+ instruction-pointer 2))]
            [parameter-3  (list-ref memory (+ instruction-pointer 3))])
        (execute-program
         (list-set
          memory
          parameter-3
          (+ (list-ref memory parameter-1)
             (list-ref memory parameter-2)))
         (+ instruction-pointer 4)))
      memory))

(define (instruction-2 memory instruction-pointer)
  (if (safe-address? memory (+ instruction-pointer 3))
      (let ([opcode (list-ref memory instruction-pointer)]
            [parameter-1 (list-ref memory (+ instruction-pointer 1))]
            [parameter-2 (list-ref memory (+ instruction-pointer 2))]
            [parameter-3 (list-ref memory (+ instruction-pointer 3))])
        (execute-program
         (list-set memory parameter-3 (* (list-ref memory parameter-1)
                                   (list-ref memory parameter-2)))
         (+ instruction-pointer 4)))
      memory))

(define (opcode-error program instruction-pointer)
  "opcode not recognized")

(define (execute-program program instruction-pointer)
  (if (safe-address? program instruction-pointer)
      (let ([current-instruction (list-ref program instruction-pointer)])
        (cond
          [(= current-instruction 99)
           (instruction-99 program instruction-pointer)]
          [(= current-instruction 1)
           (instruction-1 program instruction-pointer)]
          [(= current-instruction 2)
           (instruction-2 program instruction-pointer)]
          [else (opcode-error program instruction-pointer)]))
      "error"))
  

;;(execute-program (initialize-memory program) 0) ;; answers 3716293


(define (try-noun-verb memory noun verb)
  (list-set (list-set memory 1 noun) 2 verb))

(define (gravity-assist-attempt program noun verb)
  (let ([current-attempt
         (try-noun-verb
          (initialize-memory program)
          noun
          verb)])
    (execute-program current-attempt 0)))

(define (try)
  (for* ([i (build-list (length (initialize-memory program)) values)]
         [j (build-list (length (initialize-memory program)) values)])
    (if (= 19690720 (caadr (gravity-assist-attempt program i j)))
        (display (list i j (gravity-assist-attempt program i j)))
        nill)))
(try)
#lang racket

;; Automatons accept c(ad)(ad)*r

;; List[Sym, List[Sym, Sym]]
(define machine
  '((init ((c one)))
    (one ((a more)
          (d more)))
    (more ((a more)
           (d more)
           (r end)))
    (end ())))

;; List[A, B] * A -> B
(define (assv l k)
  (if (null? l) false
      (let ([hd (car l)]
            [tl (cdr l)])
        (if (eq? (car hd) k) (cadr hd)
            (assv tl k)))))

;; Simple implementation for machine as an associated list.
;; Corresponds to an interpreter for the machine.
(define (accept? machine start end stream)
  (define (accept-rec? state stream)
    (if (empty? stream) (eq? state end)
        (let* ([elem (car stream)]
               [trans (assv machine state)])
          (if (null? trans) false
              (let ([next (assv trans elem)])
                (if next
                    (accept-rec? next (cdr stream))
                    false))))))
  (accept-rec? start stream))

;; Faster implementation using conditional dispatch and tail-rec
;; functions. Note how it is specialized for the state transitions.
;; Corresponds to a compiled version of the machine.
(define (acceptm? stream)
  (define (init stream)
    (if (null? stream) false
        (case (car stream)
          ['c (one (cdr stream))]
          [else false])))
  (define (one stream)
    (if (null? stream) false
        (case (car stream)
          ['a (more (cdr stream))]
          ['d (more (cdr stream))]
          [else false])))
  (define (more stream)
    (if (null? stream) false
        (case (car stream)
          ['a (more (cdr stream))]
          ['d (more (cdr stream))]
          ['r (end (cdr stream))]
          [else false])))
  (define (end stream)
    (if (null? stream) true
        (case (car stream)
          [else false])))
  (init stream))

;; This is the only thing required to implement automatons using macros.
#| First pass: accepts all substrings, i.e. all states are accept states.
(define-syntax automaton
  (syntax-rules (: ->) ;; match these symbols literally, not as pattern vars
    [(_ init-state
        (state : (sym -> trans) ...)
        ...)
     (letrec ([state
               (λ(stream)
                 (if (null? stream) true
                     (case (car stream)
                       [(sym) (trans (cdr stream))]
                       ...
                       [else false])))]
              ...)
       init-state)]))
|#

(define-syntax process-state
  (syntax-rules (accept ->)
    [(_ accept)
     (λ(stream)
       (if (null? stream) true false))]
    [(_ (label -> target) ...)
     (λ(stream)
       (if (null? stream) false
           (case (car stream)
             [(label) (target (cdr stream))]
             ...
             [else false])))]))

(define-syntax automaton
  (syntax-rules (:)
    [(_ init-state
       (state : response ...) ...)
     (letrec ([state
                (process-state response ...)]
                ...)
              init-state)]))

;; The automaton in syntactic sugar. Expanded by macros.
(define mmachine
  (automaton start
    (start : (c -> one))
    (one : (a -> more)
           (d -> more))
    (more : (a -> more)
            (d -> more)
            (r -> end))
    (end : accept)))

(mmachine '(c a d a r))
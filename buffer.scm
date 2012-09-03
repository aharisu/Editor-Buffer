
(define-module buffer
  (use gauche.collection)
  (use gauche.sequence)
  (export 
    <buffer> 
    buf-insert buf-insert-seq
    buf-delete 
    buf-get buf-get-seq
    buf-copy!
    buf-length
    )
  )

(select-module buffer)

(define-class <buffer> () ())

(define-generic buf-insert)
(define-generic buf-insert-seq)
(define-generic buf-delete)
(define-generic buf-get)
(define-generic buf-get-seq)
(define-generic buf-copy!)
(define-generic buf-length)


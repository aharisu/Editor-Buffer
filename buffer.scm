
(define-module buffer
  (use gauche.collection)
  (use gauche.sequence)
  (export 
    <buffer> 
    buf-insert buf-insert-seq
    buf-delete 
    buf-get buf-get-seq
    buf-length
    buf-copy! buf-move!
    referencer size-of
    )
  )

(select-module buffer)

(define-class <buffer> (<sequence>) ())

(define-generic buf-insert)
(define-generic buf-insert-seq)
(define-generic buf-delete)
(define-generic buf-get)
(define-generic buf-get-seq)
(define-generic buf-length)

(define (buf-copy-or-move target tindex source beg end move?)
  (let* ([len (buf-length source)]
         [end (if (and end (<= end len)) end len)])
    (when (< 0 (- end beg))
      (buf-insert-seq target tindex (buf-get-seq source beg end))
      (when move?
        (buf-delete source beg end)))))

(define-method buf-copy! ((target <buffer>) tindex (source <buffer>) beg :optional (end #f))
  (buf-copy-or-move target tindex source beg end #f))

(define-method buf-move! ((target <buffer>) tindex (source <buffer>) beg :optional (end #f))
  (buf-copy-or-move target tindex source beg end #t))

(define-method referencer ((buf <buffer>)) buf-get)
(define-method size-of ((buf <buffer>)) (buf-length buf))


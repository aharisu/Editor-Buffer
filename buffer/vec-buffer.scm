
(define-module buffer.vec-buffer
  (use gauche.sequence)
  (use srfi-43)
  (use buffer)
  (export 
    <vec-buffer> make-vec-buffer
    buf-insert buf-insert-seq
    buf-delete 
    buf-get buf-get-seq
    buf-copy!
    buf-length
    )
  )

(select-module buffer.vec-buffer)

(define-class <vec-buffer> (<buffer>)
  (
   (expand-size :init-keyword :expand-size)
   (buf :init-keyword :buf)
   (len :init-value 0)
   ))

(define (make-vec-buffer expand-size)
  (make <vec-buffer> 
        :expand-size expand-size
        :buf (make-vector expand-size)
        ))

(define-method buf-insert ((buf <vec-buffer>) index obj)
  (check-buf buf 1)
  (let1 vec (slot-ref buf 'buf)
    (vector-copy! vec (+ index 1) vec index (- (vector-length vec) 1))
    (vector-set! vec index obj))
  (inc! (slot-ref buf 'len)))

(define-method buf-insert-seq ((buf <vec-buffer>) index seq)
  )

(define-method buf-delete ((buf <vec-buffer>) beg :optional (end #f))
  ;;not implementation
  )

(define-method buf-copy! ((target <buffer>) tindex (source <vec-buffer>) beg :optional (end #f))
  ;;not implementation
  )

(define-method buf-get ((buf <vec-buffer>) index)
  (vector-ref (slot-ref buf 'buf) index))

(define-method buf-get-seq ((buf <vec-buffer>) beg :optional (end #f))
  (vector->list (slot-ref buf 'buf) beg (or end (slot-ref buf 'len))))

(define-method buf-copy! ((target <buffer>) tindex (source <vec-buffer>) beg :optional (end #f))
  ;;not implementation
  )

(define-method buf-length ((buf <vec-buffer>))
  (slot-ref buf 'len))

(define (check-buf buf len)
  (let* ([vec (slot-ref buf 'buf)]
         [vec-len (vector-length vec)])
    (when (>= (+ (slot-ref buf 'len) len) vec-len)
      (let1 new-vec (make-vector (+ vec-len (slot-ref buf 'expand-size)))
        (vector-copy! new-vec 0 vec)
        (slot-set! buf 'buf new-vec)))))


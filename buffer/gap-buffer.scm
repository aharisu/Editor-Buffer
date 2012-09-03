
(define-module buffer.gap-buffer
  (use gauche.sequence)
  (use srfi-43)
  (use buffer)
  (export 
    <gap-buffer> make-gap-buffer
    buf-insert buf-insert-seq
    buf-delete 
    buf-get buf-get-seq
    buf-copy!
    buf-length
    )
  )

(select-module buffer.gap-buffer)

(define-class <gap-buffer> (<buffer>)
  (
   (gap-begin :init-value 0)
   (gap-end :init-keyword :gap-end)
   (block-size :init-keyword :block)
   (buf :init-keyword :buf)
   ))

(define (make-gap-buffer block-size)
  (make <gap-buffer> 
        :gap-end block-size
        :block block-size
        :buf (make-vector block-size)
        ))

(define-method buf-insert ((buf <gap-buffer>) index obj)
  (let1 index (conv-index buf index #f)
    (unless (check-buf buf index 0)
      (move-gap buf index))
    (vector-set! (slot-ref buf 'buf)
                 (slot-ref buf 'gap-begin)
                 obj)
    (inc! (slot-ref buf 'gap-begin))))

(define-method buf-insert-seq ((buf <gap-buffer>) index seq)
  (let ([seq-len (size-of seq)]
        [index (conv-index buf index #f)])
    (unless (check-buf buf index seq-len)
      (move-gap buf index))
    (let1 vec (slot-ref buf 'buf)
      (for-each
        (lambda (obj)
          (vector-set! vec (slot-ref buf 'gap-begin) obj)
          (inc! (slot-ref buf 'gap-begin)))
        seq))
    seq-len))

(define-method buf-delete ((buf <gap-buffer>) beg :optional (end #f))
  (receive (beg end)
    (conv-begin-end buf beg (or end (buf-length buf)))
    (let ([g-beg (slot-ref buf 'gap-begin)]
          [g-end (slot-ref buf 'gap-end)])
    (cond
      [(and (< beg g-beg) (>= end g-end))
       (begin0 (+ (- g-beg beg) (- end g-end))
         (slot-set! buf 'gap-begin beg)
         (slot-set! buf 'gap-end end))]
      [(< end g-beg)
       (move-gap buf end)
       (begin0 (- g-beg beg)
         (slot-set! buf 'gap-begin beg))]
      [(= beg g-gap)
       (begin0 (- end g-gap)
         (slot-set! buf 'gap-end end))]
      [(> beg g-end)
       (move-gap buf beg)
       (begin0 (- c g-end)
         (slot-set! buf 'gap-end end))]
      [else 0]))))

(define-method buf-get ((buf <gap-buffer>) index)
  (vector-ref (slot-ref buf 'buf) (conv-index buf index #t)))

(define-method buf-get-seq ((buf <gap-buffer>) beg :optional (end #f))
  (receive (beg end)
    (conv-begin-end buf beg (or end (buf-length buf)))
    (let ([g-beg (slot-ref buf 'gap-begin)]
          [g-end (slot-ref buf 'gap-end)]
          [vec (slot-ref buf 'buf)])
      (cond
        [(and (< beg g-beg) (>= end g-end))
         (letrec ([getter (lambda (i end c)
                            (if (< i end)
                              (getter (+ i 1)
                                      end
                                      (cons (vector-ref vec i) c))
                              c))])
           (reverse (getter g-end end (getter beg g-beg '()))))]
        [(or (< end g-beg) (>= beg g-end))
         (reverse (let loop ([i beg]
                             [c '()])
                    (if (< i end)
                      (loop (+ i 1)
                            (cons (vector-ref vec i) c))
                      c)))]
        [else '()]))))

(define-method buf-copy! ((target <buffer>) tindex (source <gap-buffer>) beg :optional (end #f))
  (let1 count (if end (- end beg) (- (buf-length source) beg))
    (unless (zero? count)
      (buf-insert-seq target
                      (buf-get-seq source count)
                      tindex)
      (buf-delete source beg count))))

(define-method buf-length ((buf <gap-buffer>))
  (- (vector-length (slot-ref buf 'buf)) (gap-length buf)))

(define (gap-length buf)
  (- (slot-ref buf 'gap-end) (slot-ref buf 'gap-begin)))

(define (check-buf buf index len)
  (let ([beg (slot-ref buf 'gap-begin)]
        [end (slot-ref buf 'gap-end)])
    (if (or (= beg end) (< (gap-length buf) len))
      (begin
        (expand-buf buf len)
        #f)
      (or (= index beg) (= index end)))))

(define (expand-buf buf len)
  (let* ([old-len (vector-length (slot-ref buf 'buf))]
         [new-len (+ (slot-ref buf 'block-size) 
                     old-len len)]
         [new-buf (make-vector new-len)])
    (if (zero? len)
      (slot-set! buf 'gap-begin old-len)
      (move-gap buf len))
    (vector-copy! new-buf 0 (slot-ref buf 'buf))
    (slot-set! buf 'buf new-buf)
    (slot-set! buf 'gap-end (vector-length new-buf))))

(define (move-gap buf index)
  (cond 
    [(< index (slot-ref buf 'gap-begin))
     (slot-set! buf 'gap-end (- (slot-ref buf 'gap-end)
                                (- (slot-ref buf 'gap-begin) index)))
     (vector-copy! (slot-ref buf 'buf) (slot-ref buf 'gap-end)
                   (slot-ref buf 'buf) index (slot-ref buf 'gap-begin))
     (slot-set! buf 'gap-begin index)]
    [(> index (slot-ref buf 'gap-end))
     (vector-copy! (slot-ref buf 'buf) (slot-ref buf 'gap-begin)
                   (slot-ref buf 'buf) (slot-ref buf 'gap-end) index)
     (slot-set! buf 'gap-begin (+ (slot-ref buf 'gap-begin)
                                  (- index (slot-ref buf 'gap-end))))
     (slot-set! buf 'gap-end index)]))


(define (conv-begin-end buf beg end)
  (receive (beg end)
    (let1 gl (gap-length buf)
      (cond
        [(zero? gl) (values beg end)]
        [(>= beg (slot-ref buf 'gap-begin)) (values (+ beg gl) (+ end gl))]
        [(>= end (slot-ref buf 'gap-begin)) (values beg (+ end gl))]
        [else (values beg end)]))
    (until (<= 0 beg end (vector-length (slot-ref buf 'buf)))
      (errorf "out of range ~a-~a" beg end))
    (values beg end)))


(define (conv-index buf index conv-begin?)
  (rlet1 ret (let1 gl (gap-length buf)
               (if (or (zero? gl)
                     ((if conv-begin? < <=) index (slot-ref buf 'gap-begin))) 
                 index
                 (+ index gl)))
    (until (<= 0 ret (vector-length (slot-ref buf 'buf)))
      (errorf "out of range ~a" index))))



(add-load-path "../" :relative)

(define-module buffer.gapbuffer
  (use gauche.collection)
  (use gauche.sequence)
  (use srfi-43)
  (export 
    <gapbuffer> make-gapbuffer
    gap-insert gap-insert-seq
    gap-buffer-length
    )
  )

(select-module buffer.gapbuffer)

(define-class <gap-buffer> (<buffer>)
  (
   (gap-begin :init-value 0)
   (gap-end :init-keyword :gap-end)
   (block-size :init-keyword :block)
   (buf :init-keyword :buf)
   ))

(define (make-gapbuffer block-size)
  (make <gapbuffer> 
        :gap-end size
        :block-size block-size
        :buf (make-vector block-size)
        ))

(define-method buf-insert ((buf <gap-buffer>) obj index)
  (let1 index (conv-index index #f)
    (unless (check-buf buf index 0)
      (move-gap buf index))
    (vector-set! (ref buf 'buf)
                 (ref buf 'gap-begin)
                 obj)
    (inc! (ref buf 'gap-begin))))

(define-method buf-insert-seq ((buf <gap-buffer>) seq index)
  (let ([seq-len (size-of seq)]
        [index (conv-index buf index #f)])
    (unless (check-buf buf index seq-len)
      (move-gap buf index))
    (let1 vec (ref buf 'buf)
      (for-each
        (lambda (obj)
          (vector-set! vec (ref buf 'gap-begin) obj)
          (inc! (ref buf 'gap-begin)))
        seq))
    seq-len))

(define-method buf-delete ((buf <gap-buffer>) beg :optional (count #f))
  (receive (beg end)
    (conv-begin-end beg (if count (+ beg count) (gap-buffer-length buf)))
    (let ([g-beg (ref buf 'gap-begin)]
          [g-end (ref buf 'gap-end)])
    (cond
      [(and (< beg g-beg) (>= end g-end))
       (begin0 (+ (- g-beg beg) (- end g-end))
         (ref buf 'gap-begin beg)
         (ref buf 'gap-end end))]
      [(< end g-beg)
       (move-gap buf end)
       (begin0 (- g-beg beg)
         (ref buf 'gap-begin beg))]
      [(= beg g-gap)
       (begin0 (- end g-gap)
         (ref buf 'gap-end end))]
      [(> beg g-end)
       (move-gap buf beg)
       (begin0 (- c g-end)
         (ref buf 'gap-end end))]
      [else 0]))))

(define-method buf-get ((buf <gap-buffer>) index)
  (vector-ref (ref buf 'buf) (conv-index index #t)))

(define-method buf-get-seq ((buf <gap-buffer>) beg :optional (count #f))
  (receive (beg end)
    (conv-begin-end beg (if count (+ beg count) (gap-buffer-length buf)))
    (let ([g-beg (ref buf 'gap-begin)]
          [g-end (ref buf 'gap-end)]
          [vec (ref buf 'buf)])

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

(define-method buf-copy! ((target <buffer>) tindex (source <gap-buffer>) beg :optional (count #f))
  (let1 count (if count count (- (buf-length source) beg))
    (unless (zero? count)
      (buf-insert-seq target
                      (buf-get-seq source count)
                      tindex)
      (buf-delete source beg count))))

(define-method buf-length ((buf <gap-buffer>))
  (- (vector-length (ref buf 'buf)) (gap-length buf)))

(define (gap-length buf)
  (- (ref buf 'gap-end) (ref buf 'gap-end)))

(define (check-buf buf index len)
  (let ([beg (ref buf 'gap-begin)]
        [end (ref buf 'gap-end)])
    (if (or (= beg end)
          (< (gap-length buf) len))
      (begin
        (make-large-buf buf len)
        #f)
      (= index beg end))))

(define (expand-buf buf len)
  (let* ([old-len (vector-length (ref buf 'buf))]
         [new-len (+ (ref buf 'block-size) 
                     old-len len)]
         [new-buf (make-vector new-len)])
    (if (zero? len)
      (ref buf 'gap-begin old-len)
      (move-gap buf len))
    (vector-copy! new-buf 0 (ref buf 'buf))
    (ref buf 'buf new-buf)
    (ref buf 'gap-end (vector-length new-buf))))

(define (move-gap buf index)
  (if (< index (ref buf 'gap-begin))
    (begin
      (ref buf 'gap-end (- (ref buf 'gap-end) index))
      (vector-copy! (ref buf 'buf) (ref buf 'gap-end)
                    (ref buf 'buf) index (ref buf 'gap-end))
      (ref buf 'gap-begin index))
    (begin
      (vector-copy! (ref buf 'buf) (ref buf 'gap-begin)
                    (ref buf 'buf) (ref buf 'gap-end) index)
      (ref buf 'gap-begin (+ (ref buf 'gap-begin)
                             (- index (ref buf 'gap-end))))
      (ref buf 'gap-end index))))

(define (conv-begin-end buf beg end)
  (let1 gl (gap-length buf)
    (cond
      [(zero? gl) (values beg end)]
      [(>= beg (ref buf 'gap-begin)) (values (+ beg gl) (+ end gl))]
      [(>= end (ref buf 'gap-begin)) (values beg (+ end gl))]
      [else (values beg end)])))

(define (conv-index buf index conv-begin?)
  (let1 gl (gap-length buf)
    (if (or (zero? gl)
          ((if conv-begin? >= >) index (ref buf 'gap-begin))) 
      index
      (+ index gl))))




(add-load-path "." :relative)
(use buffer.gap-buffer)
(use buffer.chunk-gap-buffer)
(use buffer.vec-buffer)
(use buffer.pt-buffer)
(use gauche.interactive)
(use math.mt-random)
(use gauche.time)

(define rand
  (let1 m (make <mersenne-twister>)
          (^n (mt-random-integer m n))))

(define (prof buf total seed)
  (let1 m (make <mersenne-twister>)
    (mt-random-set-seed! m seed)
    (time
      (dotimes [t total]
        (let ([index (mt-random-integer m (buf-length buf))]
              [count (+ 10 (mt-random-integer m 20))])
          (case (mt-random-integer m 3)
            [(0)
             (dotimes [i count] (buf-insert buf (+ index i) i))]
            [(1)
             (buf-insert-seq buf index (iota count))]
            [(2)
             (if (< (buf-length buf) (+ index count))
               (buf-delete buf index (- (buf-length buf) 1))
               (buf-delete buf index (+ index count)))]
            ))))))

(print "start")

(let ([buf1 (make-vec-buffer 50)]
      [buf2 (make-gap-buffer 50)]
      [buf3 (make-chunk-gap-buffer 50)]
      [buf4 (make-pt-buffer 50)]
      )
  (dotimes [i 100]
    (buf-insert buf1 i i)
    (buf-insert buf2 i i)
    (buf-insert buf3 i i)
    (buf-insert buf4 i i)
    )

  (let ([seed (rand 65536)]
        [total 5000])
    (prof buf1 total seed)
    (slot-set! buf1 'buf #f)

    (prof buf2 total seed)
    (slot-set! buf2 'buf #f)

    (prof buf3 total seed)

    (prof buf4 total seed)
    ))

(print "finish")

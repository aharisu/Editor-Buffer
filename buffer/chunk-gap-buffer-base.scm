
(define-module buffer.chunk-gap-buffer-base
  (use buffer)
  (export-all)
  )

(select-module buffer.chunk-gap-buffer-base)

(define-class <gb-chunk-base> ()
  (
   (buf :init-keyword :buf)
   (gap-begin :init-keyword :gap-begin)
   (gap-end :init-keyword :gap-end)
   (next :init-keyword :next)
   (prev :init-keyword :prev)
   (chunk-size :init-keyword :chunk-size)
   ))

(define (gap-len chunk)
  (- (slot-ref chunk 'gap-end) (slot-ref chunk 'gap-begin)))

(define (chunk-length chunk)
  (- (slot-ref chunk 'chunk-size) (- (slot-ref chunk 'gap-end) (slot-ref chunk 'gap-begin))))

(define (make-new-chunk chunk cline)
  (let1 c (+ 1 (quotient cline (slot-ref chunk 'chunk-size)))
    (let loop ([i 0]
               [cur chunk])
      (let1 new-chunk (make (class-of chunk)
                            :buf (make-vector (slot-ref chunk 'chunk-size))
                            :gap-begin 0
                            :gap-end (slot-ref chunk 'chunk-size)
                            :next (slot-ref cur 'next)
                            :prev cur
                            :chunk-size (slot-ref chunk 'chunk-size))
        (when (slot-ref cur 'next)
          (slot-set! (slot-ref cur 'next) 'prev new-chunk)) 
        (slot-set! cur 'next new-chunk)
        (if (< (+ i 1) c)
          (loop (+ i 1) new-chunk)
          new-chunk)))))


(define (check-buf chunk index cline)
  (let ([g-beg (slot-ref chunk 'gap-begin)]
        [g-end (slot-ref chunk 'gap-end)])
    (cond
      [(= g-beg g-end) 1] ;NON GAP
      [(< (gap-len chunk) cline) 2] ;INSUFFICIENT GAP
      [(or (= index g-beg) (= index g-end)) 0] ;check ok
      [else 3]))) ;check no

(define (move-gap chunk index)
  (let ([g-beg (slot-ref chunk 'gap-begin)]
        [g-end (slot-ref chunk 'gap-end)]
        [vec (slot-ref chunk 'buf)])
    (cond
      [(< index g-beg)
       (let ([cline (- g-beg index)]
             [index (- g-beg 1)])
         (unless (= g-beg g-end)
           (dotimes [i cline]
             (vector-set! vec (- g-end i 1) (vector-ref vec (- index i)))
             (vector-set! vec (- index i) (undefined))))
         (slot-set! chunk 'gap-begin (- g-beg cline))
         (slot-set! chunk 'gap-end (- g-end cline)))]
      [(> index g-end)
       (let1 cline (- index g-end)
         (unless (= g-beg g-end)
           (dotimes [i cline]
             (vector-set! vec (+ g-beg i) (vector-ref vec (+ g-end i)))
             (vector-set! vec (+ g-end i) (undefined))))
         (slot-set! chunk 'gap-begin (+ g-beg cline))
         (slot-set! chunk 'gap-end (+ g-end cline)))])))

(define (make-gap chunk index target-chunk)
  (let ([target-vec (slot-ref target-chunk 'buf)]
        [target-g-end (slot-ref target-chunk 'gap-end)]
        [chunk-len (slot-ref chunk 'chunk-size)]
        [vec (slot-ref chunk 'buf)])
    (do ((line chunk-len (- line 1))
         (i 1 (+ i 1)))
      ((<= line index))
      (vector-set! target-vec (- target-g-end i) (vector-ref vec (- line 1)))
      (vector-set! vec (- line 1) (undefined)))
    (slot-set! target-chunk 'gap-end
               (- target-g-end (- chunk-len index)))

    (slot-set! chunk 'gap-begin index)
    (slot-set! chunk 'gap-end (slot-ref chunk 'chunk-size))
    (- chunk-len index)))

(define (conv-index chunk line get-obj?)
  ;;????
  (if ((if get-obj? >= >) line (slot-ref chunk 'gap-begin))
  ;(if (>= line (slot-ref chunk 'gap-begin))
    (+ line (gap-len chunk))
    line))

(define (conv-begin-end chunk beg end)
  (let1 gl (gap-len chunk)
    (cond
      [(>= beg (slot-ref chunk 'gap-begin)) (values (+ beg gl) (+ end gl))]
      [(>= end (slot-ref chunk 'gap-begin)) (values beg (+ end gl))]
      [else (values beg end)])))


(define-class <chunk-gap-buffer-base> (<buffer>)
  (
   (cur :init-keyword :cur)
   (head)
   (cline :init-value 0)
   (all-cline :init-value 0)
   ))

(define-method initialize ((c <chunk-gap-buffer-base>) initargs)
  (next-method c initargs)
  (slot-set! c 'head (slot-ref c 'cur)))

(define (search-chunk buf line get-obj?)
  (let1 i (- line (slot-ref buf 'cline))
    ;;;; get-obj?
    ;(if (>= i 0)
    (if ((if get-obj? >= >) i 0)
      (let loop ([i i])
        (if (slot-ref (slot-ref buf 'cur) 'next)
          (let1 c (begin
                    (slot-set! buf 'cur (slot-ref (slot-ref buf 'cur) 'next))
                    (chunk-length (slot-ref buf 'cur)))
            (slot-set! buf 'cline (+ (slot-ref buf 'cline) c))
            ;;;; get-obj?
            ;(if (<= i c)
            (if ((if get-obj? <= <) i c)
              i
              (loop (- i c))))
          i)) 
      (let loop ([i i])
        (let1 c (chunk-length (slot-ref buf 'cur))
          ;;; ????
          ;(if (or (>= (+ i c) 0) (not (slot-ref (slot-ref buf 'cur) 'prev)))
          (if (or ((if get-obj? >= >) (+ i c) 0) (not (slot-ref (slot-ref buf 'cur) 'prev)))
            (+ i c)
            (begin
              (slot-set! buf 'cur (slot-ref (slot-ref buf 'cur) 'prev))
              (slot-set! buf 'cline (- (slot-ref buf 'cline) c))
              (loop (+ i c)))))))))

(define (delete-chunk buf chunk)
  (when (slot-ref chunk 'next)
    (slot-set! (slot-ref chunk 'next) 'prev (slot-ref chunk 'prev)))
  (if (slot-ref chunk 'prev)
    (slot-set! (slot-ref chunk 'prev) 'next (slot-ref chunk 'next))
    (slot-set! buf 'head (slot-ref chunk 'next)))
  ;;delete chunk
  ;(finalize chunk)
  )


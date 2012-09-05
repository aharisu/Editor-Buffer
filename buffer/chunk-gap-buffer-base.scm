
(define-module buffer.chunk-gap-buffer-base
  (use buffer.util)
  (export-all)
  )

(select-module buffer.chunk-gap-buffer-base)

(define-class <gb-chunk> ()
  (
   (buf :init-keyword :buf)
   (gap-begin :init-keyword :gap-begin)
   (gap-end :init-keyword :gap-end)
   (next :init-keyword :next)
   (prev :init-keyword :prev)
   (chunk-size :init-keyword :chunk-size)
   ))

(define (gap-len chunk)
  (- (@ chunk.gap-end) (@ chunk.gap-begin)))

(define (chunk-length chunk)
  (- (@ chunk.chunk-size) (- (@ chunk.gap-end) (@ chunk.gap-begin))))

(define (make-new-chunk chunk cline)
  (let ([c (+ 1 (quotient cline (@ chunk.chunk-size)))]
        [size (@ chunk.chunk-size)])
    (let loop ([i 0]
               [cur chunk])
      (let1 new-chunk (make (class-of chunk)
                            :buf (make-vector size)
                            :gap-begin 0
                            :gap-end size
                            :next (@ cur.next)
                            :prev cur
                            :chunk-size size)
        (when (@ cur.next)
          (@! cur.next.prev new-chunk))
        (@! cur.next new-chunk)
        (if (< (+ i 1) c)
          (loop (+ i 1) new-chunk)
          new-chunk)))))


(define (check-buf chunk index cline)
  (let ([g-beg (@ chunk.gap-begin)]
        [g-end (@ chunk.gap-end)])
    (cond
      [(= g-beg g-end) 1] ;NON GAP
      [(< (gap-len chunk) cline) 2] ;INSUFFICIENT GAP
      [(or (= index g-beg) (= index g-end)) 0] ;check ok
      [else 3]))) ;check no

(define (move-gap chunk index)
  (let ([g-beg (@ chunk.gap-begin)]
        [g-end (@ chunk.gap-end)]
        [vec (@ chunk.buf)])
    (cond
      [(< index g-beg)
       (let ([cline (- g-beg index)]
             [index (- g-beg 1)])
         (unless (= g-beg g-end)
           (dotimes [i cline]
             (vector-set! vec (- g-end i 1) (vector-ref vec (- index i)))
             (vector-set! vec (- index i) (undefined))))
         (@! chunk.gap-begin (- g-beg cline))
         (@! chunk.gap-end (- g-end cline)))]
      [(> index g-end)
       (let1 cline (- index g-end)
         (unless (= g-beg g-end)
           (dotimes [i cline]
             (vector-set! vec (+ g-beg i) (vector-ref vec (+ g-end i)))
             (vector-set! vec (+ g-end i) (undefined))))
         (@! chunk.gap-begin (+ g-beg cline))
         (@! chunk.gap-end (+ g-end cline)))])))

(define (make-gap chunk index target-chunk)
  (let ([target-vec (@ target-chunk.buf)]
        [target-g-end (@ target-chunk.gap-end)]
        [chunk-len (@ chunk.chunk-size)]
        [vec (@ chunk.buf)])
    (do ((line chunk-len (- line 1))
         (i 1 (+ i 1)))
      ((<= line index))
      (vector-set! target-vec (- target-g-end i) (vector-ref vec (- line 1)))
      (vector-set! vec (- line 1) (undefined)))
    (@! target-chunk.gap-end (- target-g-end (- chunk-len index)))
    (@! chunk.gap-begin index)
    (@! chunk.gap-end (@ chunk.chunk-size))
    (- chunk-len index)))

(define (conv-index chunk line get-obj?)
  ;;????
  (if ((if get-obj? >= >) line (@ chunk.gap-begin))
  ;(if (>= line (@ chunk.gap-begin))
    (+ line (gap-len chunk))
    line))

(define (conv-begin-end chunk beg end)
  (let1 gl (gap-len chunk)
    (cond
      [(>= beg (@ chunk.gap-begin)) (values (+ beg gl) (+ end gl))]
      [(>= end (@ chunk.gap-begin)) (values beg (+ end gl))]
      [else (values beg end)])))


(define-class <chunk-gap-buffer-base> ()
  (
   (cur :init-keyword :cur)
   (head)
   (cline :init-value 0)
   (all-cline :init-value 0)
   ))

(define-method initialize ((c <chunk-gap-buffer-base>) initargs)
  (next-method c initargs)
  (@! c.head (@ c.cur)))

(define (search-chunk buf line get-obj?)
  (let1 i (- line (@ buf.cline))
    ;;;; get-obj?
    ;(if (>= i 0)
    (if ((if get-obj? >= >) i 0)
      (let loop ([i i])
        (if (@ buf.cur.next)
          (let1 c (begin
                    (@! buf.cur (@ buf.cur.next))
                    (chunk-length (@ buf.cur)))
            (@inc! buf.cline c)
            ;;;; get-obj?
            ;(if (<= i c)
            (if ((if get-obj? <= <) i c)
              i
              (loop (- i c))))
          i))
      (let loop ([i i])
        (let1 c (chunk-length (@ buf.cur))
          ;;; ????
          ;(if (or (>= (+ i c) 0) (not (@ buf.cur.prev)))
          (if (or ((if get-obj? >= >) (+ i c) 0) (not (@ buf.cur.prev)))
            (+ i c)
            (begin
              (@! buf.cur (@ buf.cur.prev))
              (@dec! buf.cline c)
              (loop (+ i c)))))))))

(define (delete-chunk buf chunk)
  (when (@ chunk.next)
    (@! chunk.next.prev (@ chunk.prev)))
  (if (@ chunk.prev)
    (@! chunk.prev.next (@ chunk.next))
    (@! buf.head (@ chunk.next)))
  ;;delete chunk
  ;(finalize chunk)
  )


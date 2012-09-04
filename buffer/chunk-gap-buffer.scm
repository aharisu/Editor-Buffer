(add-load-path "../" :relative)

(define-module buffer.chunk-gap-buffer
  (use gauche.sequence)
  (use buffer)
  (use buffer.chunk-gap-buffer-base)
  (use buffer.util)
  (export 
    <chunk-gap-buffer> make-chunk-gap-buffer
    buf-insert buf-insert-seq
    buf-delete 
    buf-get buf-get-seq
    buf-copy!
    buf-length
    )
  )

(select-module buffer.chunk-gap-buffer)

(define-class <gb-chunk> (<gb-chunk-base>)
  ()
  )

(define (insert-obj chunk line obj)
  (let1 line (conv-index chunk line #f)
    (receive (num dest)
      (case (check-buf chunk line 0)
        [(1) ;NON GAP
         (make-new-chunk chunk 0)
         (if (= (@ chunk.chunk-size) line)
           (values 0 (@ chunk.next))
           (values (- 1 (make-gap chunk line (@ chunk.next))) chunk))]
        [(3) ;CHECK NO
         (move-gap chunk line)
         (values 1 chunk)]
        [(0) ;CHECK OK
         (values 1 chunk)])
      (vector-set! (@ dest.buf)
                   (@ dest.gap-begin)
                   obj)
      (@inc! dest.gap-begin)
      num)))

(define (insert-seq chunk line seq len)
  (let* ([line (conv-index chunk line #f)]
         [dec-num (case (check-buf chunk line len)
                    [(1)  ;NON GAP
                     (let1 last-made-chunk (make-new-chunk chunk (- len (gap-len chunk)))
                       (if (= (@ chunk.chunk-size) line)
                         0
                         (make-gap chunk line last-made-chunk)))]
                    [(2) ;INSUFFICIENT GAP
                     (move-gap chunk line)
                     (let1 last-made-chunk (make-new-chunk chunk (- len (gap-len chunk)))
                       (if (= (@ chunk.chunk-size) line)
                         0
                         (let ([beg (@ chunk.gap-begin)]
                               [ret (make-gap chunk (@ chunk.gap-end) last-made-chunk)])
                           (@! chunk.gap-begin beg)
                           ret)))]
                    [(3) (move-gap chunk line) 0];CHECK NO
                    [(0) 0])]) ;CHECK OK 
    (let ([target chunk]
          [num 0])
      (for-each
        (lambda (obj)
          (when (= (@ target.gap-begin) (@ target.gap-end))
            (set! target (@ target.next)))
          (vector-set! (@ target.buf)
                       (@ target.gap-begin)
                       obj)
          (@inc! target.gap-begin)
          (when (eq? target chunk)
            (inc! num)))
        seq)
      (- num dec-num))))

(define (adjust-end-line chunk beg end)
  (let1 size (@ chunk.chunk-size) 
    (if (< size end)
      size
      end)))

(define (delete-seq chunk beg end)
  (let ([vec (@ chunk.buf)]
        [g-beg (@ chunk.gap-begin)]
        [g-end (@ chunk.gap-end)]
        [undef (undefined)])
    (receive (beg end) (conv-begin-end chunk beg end)
      (let1 end (adjust-end-line chunk beg end)
        (cond
          [(and (< beg g-beg) (>= end g-end))
           (begin0 (+ (- g-beg beg) (- end g-end))
             (vector-fill! vec undef beg g-beg)
             (vector-fill! vec undef g-end end)
             (@! chunk.gap-begin beg)
             (@! chunk.gap-end end))]
          [(and (< beg g-beg) (<= g-beg end) (< end g-end))
           (begin0 (- g-beg beg)
             (vector-fill! vec undef beg g-beg)
             (@! chunk.gap-begin beg))]
          [(<= end g-beg)
           (move-gap chunk end)
           (let* ([g-beg (@ chunk.gap-begin)]
                  [ret (- g-beg beg)])
             (vector-fill! vec undef beg g-beg)
             (@! chunk.gap-begin beg)
             ret)]
          [(= beg g-end)
           (begin0 (- end g-end)
             (vector->list vec g-end end)
             (@! chunk.gap-end end))]
          [(> beg g-end)
           (move-gap chunk beg)
           (let* ([g-end (@ chunk.gap-end)]
                  [ret (- end g-end)])
             (vector->list vec g-end end)
             (@! chunk.gap-end end)
             ret)])))))

(define (get-obj chunk line)
  (vector-ref (@ chunk.buf) (conv-index chunk line #t)))

(define (get-seq chunk beg end)
  (let ([vec (@ chunk.buf)]
        [g-beg (@ chunk.gap-begin)]
        [g-end (@ chunk.gap-end)])
    (receive (beg end) (conv-begin-end chunk beg end)
      (let1 end (adjust-end-line chunk beg end)
        (cond
          [(and (< beg g-beg) (>= end g-end))
           (append! (vector->list vec beg g-beg)
                    (vector->list vec g-end end))]
          [(and (< beg g-beg) (<= g-beg end) (< end g-end))
           (vector->list vec beg g-beg)]
          [(<= end g-beg)
           (move-gap chunk end)
           (vector->list vec beg (@ chunk.gap-begin))]
          [(= beg g-end)
           (vector->list vec g-end end)]
          [(> beg g-end)
           (move-gap chunk beg)
           (vector->list vec (@ chunk.gap-end) end)])))))

(define-class <chunk-gap-buffer> (<chunk-gap-buffer-base>)
  ()
  )

(define (make-chunk-gap-buffer chunksize)
  (make <chunk-gap-buffer>
        :cur (make <gb-chunk>
                   :buf (make-vector chunksize)
                   :gap-begin 0
                   :gap-end chunksize
                   :next #f
                   :prev #f
                   :chunk-size chunksize)))

(define-method buf-insert ((buf <chunk-gap-buffer>) index obj)
  (let1 index (search-chunk buf index #f)
    (@inc! buf.cline (insert-obj (@ buf.cur) index obj))
    (@inc! buf.all-cline)))

(define-method buf-insert-seq ((buf <chunk-gap-buffer>) index seq)
  (let ([index (search-chunk buf index #f)]
        [len (size-of seq)])
    (@inc! buf.cline (insert-seq (@ buf.cur) index seq len))
    (@inc! buf.all-cline len)))

(define-method buf-delete ((buf <chunk-gap-buffer>) beg :optional (end #f))
  (let ([total-count (- (or end (buf-length buf)) beg)]
        [beg (search-chunk buf beg #t)])
    (let1 total (let loop ([beg beg]
                           [end (+ beg total-count)]
                           [total 0])
                  (let* ([cur (@ buf.cur)]
                         [cdel (delete-seq cur beg end)]
                         [total (+ total cdel)])
                    (if (or (= total total-count) (not (@ cur.next)))
                      total ;;loop end
                      (begin
                        (when (zero? (chunk-length cur))
                          (delete-chunk buf cur))
                        (@! buf.cur (@ cur.next))
                        (@inc!  buf.cline (chunk-length (@ buf.cur)))
                        (loop 0 (- end cdel) total)))))
      (@dec! buf.cline total)
      (@dec! buf.all-cline total))))

(define-method buf-get ((buf <chunk-gap-buffer>) index)
  (let1 index (search-chunk buf index #t)
    (get-obj (@ buf.cur) index)))

(define-method buf-get-seq ((buf <chunk-gap-buffer>) beg :optional (end #f))
  (let ([total-count (- (or end (buf-length buf)) beg)]
        [beg (search-chunk buf beg #t)])
    (let loop ([beg beg]
               [end (+ beg total-count)]
               [seq '()]
               [total 0])
      (let1 cur (@ buf.cur)
        (let* ([s (get-seq cur beg end)]
               [c (length s)]
               [seq (append seq s)]
               [total (+ total c)])
          (if (or (= total total-count) (not (@ cur.next)))
            seq ;;loop end
            (begin
              (@! buf.cur (@ cur.next))
              (@inc! buf.cline (chunk-length (@ buf.cur)))
              (loop 0 (- end c) seq total))))))))

(define-method buf-length ((buf <chunk-gap-buffer>))
  (@ buf.all-cline))


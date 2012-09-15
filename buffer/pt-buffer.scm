
(add-load-path "../" :relative)

(define-module buffer.pt-buffer
  (use gauche.sequence)
  (use buffer)
  (use buffer.util)
  (use buffer.chunk-gap-buffer-base)
  (use buffer.vec-buffer)
  (export 
    <pt-buffer> make-pt-buffer
    buf-insert buf-insert-seq
    buf-delete 
    buf-get buf-get-seq
    buf-length
    )
  )

(select-module buffer.pt-buffer)

(define (ptp-make entry) (cons entry (cons 0 0)))
(define (ptp-init ptp entry) 
  (ptp-entry-set! ptp entry)
  (ptp-begin-set! ptp 0)
  (ptp-end-set! ptp 0))

(define ptp-entry-set! set-car!)
(define (ptp-begin-set! ptp obj) (set-car! (cdr ptp) obj))
(define (ptp-end-set! ptp obj) (set-cdr! (cdr ptp) obj))

(define ptp-entry-ref car)
(define ptp-begin-ref (.$ car cdr))
(define ptp-end-ref (.$ cdr cdr))

(define make-entry-index cons)
(define ei-entry-set! set-car!)
(define ei-index-set! set-cdr!)
(define ei-entry-ref car)
(define ei-index-ref cdr)

(define-class <piece-table-buffer> (<chunk-gap-buffer-base>)
  (
   (cur-piece :init-value 0)
   (cpiece :init-value 0)
   (all-cpiece :init-value 0)
   ))

(define-constant entry-org 0)
(define-constant entry-add 1)
(define-constant entry-len 2)

(define (chunk-add-param chunk line entry get-obj?)
  (let1 ptp (vector-ref (@ chunk.buf) 0)
    (if (or (undefined? ptp) (ptp-entry-ref ptp))
      (let1 line (conv-index chunk line get-obj?)
        (receive (num dest)
          (case (check-buf chunk line 0)
            [(1) ;NON GAP
             (make-new-chunk chunk 0)
             (if (= (@ chunk.chunk-size) line)
               (values #t (@ chunk.next))
               (values (- 1 (make-gap chunk line (@ chunk.next))) chunk))]
            [(3) ;CHECK NO
             (move-gap chunk line)
             (values 1 chunk)]
            [(0) ;CHECK OK
             (values 1 chunk)])
          (vector-set! (@ dest.buf)
                       (@ dest.gap-begin)
                       (ptp-make entry))
          (@inc! dest.gap-begin)
          (values num (- (@ dest.gap-begin) 1))))
      (begin
        (ptp-init ptp entry)
        (values 0 0)))))

(define (chunk-del-param chunk line)
  (let ([line (conv-index chunk line #t)]
        [g-beg (@ chunk.gap-begin)]
        [g-end (@ chunk.gap-end)]
        [undef (undefined)])
    (cond
      [(= g-beg g-end)
       (vector-set! (@ chunk.buf) line undef)
       (@! chunk.gap-begin line)
       (@! chunk.gap-end (+ line 1))]
      [(= line (- g-beg 1))
       (vector-set! (@ chunk.buf) line undef)
       (@dec! chunk.gap-begin)]
      [(< line g-beg)
       (move-gap chunk (+ line 1))
       (@! chunk.gap-begin line)]
      [(= line g-end)
       (vector-set! (@ chunk.buf) line undef)
       (@inc! chunk.gap-end)]
      [(> line g-end)
       (move-gap chunk line)
       (@! chunk.gap-end (+ line 1))])))

(define (ptp-get chunk line get-obj?)
  (let1 line (conv-index chunk line get-obj?)
    (vector-ref (@ chunk.buf) line)))

(define (ptp-edit chunk line beg-index end-index get-obj?)
  (let* ([line (conv-index chunk line get-obj?)]
         [ptp (vector-ref (@ chunk.buf) line)])
    (when beg-index
      (ptp-begin-set! ptp beg-index))
    (when end-index
      (ptp-end-set! ptp end-index))))

(define (chunk-piece-len chunk line get-obj?)
  (let* ([line (conv-index chunk line #t)]
         [ptp (vector-ref (@ chunk.buf) line)])
    (- (ptp-end-ref ptp) (ptp-begin-ref ptp))))

;;
;;Piece Table Gap Buffer

(define (piece-len pt line)
  (let1 line (search-chunk pt line #t)
    (chunk-piece-len (@ pt.cur) line #t)))

(define (insert-new-param pt line entry)
  (let1 line (if (zero? line)
               (begin
                 (@! pt.cur (@ pt.head))
                 (@! pt.cline (chunk-length (@ pt.head)))
                 0)
               (search-chunk-with-piece pt line))
    (receive (num line) 
      (chunk-add-param (@ pt.cur) line entry #f)
      (cond
        [(eq? num #t)
         (@inc! pt.cline)
         (@! pt.cur (@ pt.cur.next))]
        [(<= num (@ pt.cur.chunk-size))
         (@inc! pt.cline num)])
      (@inc! pt.all-cline)
      line)))

(define (del-param pt line)
  (chunk-del-param (@ pt.cur) line)
  (@dec! pt.cline)
  (@dec! pt.all-cline)
  ;;if empty delete chunk
  (when (zero? (chunk-length (@ pt.cur)))
    (cond
      [(@ pt.cur.prev)
       (@! pt.cur (@ pt.cur.prev))
       (delete-chunk pt (@ pt.cur.next))]
      [(@ pt.cur.next)
       (@! pt.cur pt.cur.next)
       (@inc! pt.cline (chunk-length (@ pt.cur)))
       (delete-chunk pt (@ pt.cur.prev))]
      [else ;;if only one chunk is initialization
        (vector-set! (@ pt.cur.buf) 0 (ptp-make #f))
        (@! pt.cline 1)
        (@! pt.cur.gap-begin 1)])))

(define (pt-insert pt beg beg-index c)
  (@inc! pt.all-cpiece c)
  (let1 end-index (+ beg-index c)
    (receive (flg beg)
      (check-piece pt beg)
      (case flg
        [(0) ;head
         (insert-new-param pt 0 entry-add)
         (ptp-edit (@ pt.head) 0 beg-index end-index #f)
         (@! pt.cpiece c)]
        [(1) ;SP_ADD
         (let* ([line (search-chunk pt (@ pt.cur-piece) #t)]
                [ptp (ptp-get (@ pt.cur) line #t)])
           (if (= (ptp-end-ref ptp) beg-index)
             (begin
               (unless (ptp-entry-ref ptp) (ptp-entry-set! ptp entry-add))
               (ptp-end-set! ptp end-index))
             (let1 line (begin (@inc! pt.cur-piece)
                          (insert-new-param pt (@ pt.cur-piece) entry-add))
               (ptp-edit (@ pt.cur) line beg-index end-index #f))))
         (@inc! pt.cpiece c)]
        [(2) ;SP_INSERT
         (let* ([line (search-chunk pt (@ pt.cur-piece) #t)]
                [ptp (ptp-get (@ pt.cur) line #t)]
                [end (ptp-end-ref ptp)])
           (ptp-end-set! ptp (+ beg (ptp-begin-ref ptp)))
           (let1 line (insert-new-param pt (+ (@ pt.cur-piece) 1) entry-add)
             (ptp-edit (@ pt.cur) line beg-index end-index #f))
           (let1 line (insert-new-param pt (+ (@ pt.cur-piece) 2) (ptp-entry-ref ptp))
             (ptp-edit (@ pt.cur) line (+ beg (ptp-begin-ref ptp)) end #f)))
         (@inc! pt.cur-piece 2)
         (@inc! pt.cpiece c)]))))

(define (pt-del pt beg c)
  (@dec! pt.all-cpiece c)
  (receive (c beg) 
    (let loop ([c c]
               [beg (search-piece pt beg #t)])
      (let1 p-len (- (piece-len pt (@ pt.cur-piece)) beg)
        (cond
          [(>= p-len c) (values c beg)]
          [(zero? beg)
           (@! pt.cpiece (+ (- (@ pt.cpiece) p-len) (piece-len pt (+ (@ pt.cur-piece) 1))))
           (del-param pt (search-chunk pt (@ pt.cur-piece) #t))
           (loop (- c p-len) 0)]
          [else
            (let1 line (search-chunk pt (@ pt.cur-piece) #t)
              (ptp-edit (@ pt.cur) line 
                        #f
                        (+ (ptp-begin-ref (ptp-get (@ pt.cur) line #t)) beg)
                        #t)
              (@inc! pt.cur-piece)
              (@! pt.cpiece (+ (- (@ pt.cpiece) p-len) (piece-len pt (@ pt.cur-piece)))))
            (loop (- c p-len) 0)])))
    (let* ([line (search-chunk pt (@ pt.cur-piece) #t)]
           [cur (@ pt.cur)])
      (cond
        [(zero? beg)
         (if (= c (piece-len pt (@ pt.cur-piece)))
           ;;delete piece
           (begin
             (del-param pt line)
             (if (zero? (@ pt.cur-piece))
               (if (ptp-entry-ref (ptp-get cur 0 #t))
                 (@inc! pt.cpiece (piece-len pt 0))
                 (@! pt.cpiece c))
               (@dec! pt.cur-piece)))
           (begin
             ;; edit piece beg
             (ptp-edit cur line (+ (ptp-begin-ref (ptp-get cur line #t)) c) #f #t)))]
        [(= (+ beg c) (piece-len pt (@ pt.cur-piece)))
         ;;edit piece end
         (ptp-edit cur line #f (- (ptp-end-ref (ptp-get cur line #t)) c) #t)]
        [else
          ;;split piece
          (let* ([ptp (ptp-get cur line #t)]
                 [beg (+ (ptp-begin-ref ptp) beg)]
                 [end (ptp-end-ref ptp)])
            (ptp-edit cur line #f beg #t)
            (@inc! pt.cur-piece)
            (let1 line (insert-new-param pt (@ pt.cur-piece) (ptp-entry-ref ptp))
              (ptp-edit (@ pt.cur) line (+ beg c) end #t)))])
      (@dec! pt.cpiece c))))

(define (pt-get pt index)
  (let* ([index (search-piece pt index #t)]
         [i (search-chunk pt (@ pt.cur-piece) #t)]
         [ptp (ptp-get (@ pt.cur) i #t)])
    (make-entry-index (ptp-entry-ref ptp)
                      (+ index (ptp-begin-ref ptp)))))

(define (search-chunk-with-piece pt line)
  (let1 i (- line (@ pt.cline))
    (cond
      [(and (= i 1) (not (@ pt.cur)))
       (chunk-length (@ pt.cur))]
      [(> i 0)
       (let loop ([c 0]
                  [prev-i i]
                  [i i])
         (if (or (<= prev-i c) (not (@ pt.cur)))
           (begin
             (@! pt.cur (@ pt.cur))
             (let1 c (chunk-length (@ pt.cur))
               (@inc! pt.cline c)
               (loop c i (- i c))))
           prev-i))] ;loop end
      [else
        (let loop ([i i])
          (let* ([c (chunk-length (@ pt.cur))]
                 [i (+ i c)])
            (if (or (> i 0) (@ pt.cur))
              i ;loop end
              (begin
                (@! pt.cur (@ pt.cur))
                (@dec! pt.cline c)
                (loop i)))))])))

(define (check-piece pt pos)
  (if (zero? pos)
    (begin
      (@! pt.cur-piece 0)
      (@! pt.cpiece (piece-len pt 0))
      (values 0 pos))
    (let1 pos (search-piece pt pos #f)
      (cond
        [(zero? pos);SP_ADD
         (@dec! pt.cpiece (piece-len pt (@ pt.cur-piece)))
         (@dec! pt.cur-piece)
         (values 1 (piece-len pt (@ pt.cur-piece)))]
        [(= pos (piece-len pt (@ pt.cur-piece)));SP_ADD
         (values 1 pos)]
        [else ;SP_INSERT
          (values 2 pos)]))))

(define (search-piece pt pos get-obj?)
  (let1 i (- pos (@ pt.cpiece))
    (if ((if get-obj? >= >) i 0)
      (let1 last (- (@ pt.all-cline) 1)
        (let loop ([i i]);piece back from the 'cur-piece
          (if (< (@ pt.cur-piece) last)
            (let1 c (begin (@inc! pt.cur-piece)
                      (piece-len pt (@ pt.cur-piece)))
              (@inc! pt.cpiece c)
              (if ((if get-obj? <= <) i c)
                ;(if (<= i c)
                i
                (loop (- i c))))
            i)))
      (let loop ([i i]);piece ahead of the 'cur-piece
        (let1 c (piece-len pt (@ pt.cur-piece))
          (if (or ((if get-obj? >= >) (+ i c) 0) (zero? (@ pt.cur-piece)))
            ;(if (or (> (+ i c) 0) (zero? (@ pt.cur-piece)))
            (+ i c)
            (begin
              (@dec! pt.cur-piece)
              (@dec! pt.cpiece c)
              (loop (+ i c)))))))))

(define-class <pt-buffer> (<buffer>)
  (
   (entry :init-form (rlet1 vec (make-vector entry-len)
                       (vector-set! vec entry-add
                                    (make-vec-buffer 15))))
   (pt :init-keyword :pt)
   )
  )

(define (make-pt-buffer chunksize)
  (make <pt-buffer>
        :pt (make <piece-table-buffer>
                  :cur (make <gb-chunk>
                             :buf (rlet1 vec (make-vector chunksize)
                                    (vector-set! vec 0 (ptp-make #f)))
                             :gap-begin 1
                             :gap-end chunksize
                             :next #f
                             :prev #f
                             :chunk-size chunksize)
                  :cline 1
                  :all-cline 0)))

(define-method buf-insert ((buf <pt-buffer>) index obj)
  (let* ([entry (vector-ref (@ buf.entry) entry-add)]
         [len (buf-length entry)])
    (pt-insert (@ buf.pt) index len 1)
    (buf-insert entry len obj)))

(define-method buf-insert-seq ((buf <pt-buffer>) index seq)
  (let* ([entry (vector-ref (@ buf.entry) entry-add)]
         [len (buf-length entry)]
         [seq-len (size-of seq)])
    (pt-insert (@ buf.pt) index len seq-len)
    (buf-insert-seq entry len seq)))

(define-method buf-delete ((buf <pt-buffer>) beg :optional (end #f))
  (let1 end (or end (@ buf.pt.all-cpiece))
    (pt-del (@ buf.pt) beg (- end beg))))

(define-method buf-get ((buf <pt-buffer>) index)
  (let1 ei (pt-get (@ buf.pt) index)
    (if (ei-entry-ref ei)
      (ref (vector-ref (@ buf.entry) (ei-entry-ref ei))
           (ei-index-ref ei)))))

(define-method buf-get-seq ((buf <pt-buffer>) beg :optional (end #f))
  (let* ([pt (@ buf.pt)]
         [c (- (or end (@ pt.all-cpiece)) beg)])
    (begin0
      (let loop ([cell '()]
                 [i 0])
        (if (< i c)
          (let1 ei (pt-get pt (+ beg i))
            (loop (if (ei-entry-ref ei)
                    (cons (ref (vector-ref (@ buf.entry) (ei-entry-ref ei)) (ei-index-ref ei))
                          cell)
                    cell)
                  (+ i 1)))
          (reverse cell)))
      )
    ))

(define-method buf-length ((buf <pt-buffer>))
  (@ buf.pt.all-cpiece))

(use setup-download)

(define (licenses eggs repo-info)
  (pp
   (query-dependency-graph
    eggs repo-info
    (lambda (egg)
     (let ((egg-info (alist-ref egg repo-info)))
       (if egg-info
           `(,egg ,@(let ((license (license egg-info)))
                      (if license
                          `((license ,license))
                          '()))
                  ,@(let ((needs (needs egg-info)))
                      (if (pair? needs)
                          `((needs . ,needs))
                          '())))
           `(,egg ?)))))))

;;;

(define (license egg-info)
  (cond ((alist-ref 'license egg-info)
         => car)
        (else #f)))
(define (deps key egg-info)
  (cond ((alist-ref key egg-info)       ; add 'depends
         => (lambda (n)
              (map (lambda (e) (if (pair? e) (car e) e))
                   n)))
        (else '())))
(define (needs egg-info)
  (append (deps 'needs egg-info)
          (deps 'depends egg-info)))

;;;

(define (needs-dag egg repo-info)
  (let ((n (needs (alist-ref egg repo-info))))
    (cons (cons egg n)
          (append-map
           (lambda (x) (needs-dag x repo-info))
           n))))
(define (dependency-graph egg repo-info)
  (topological-sort (needs-dag egg repo-info)
                    eq?))
(define (query-dependency-graph eggs repo-info proc)
  (map proc
       (cdr
        (dependency-graph '*QUERY*
                          `((*QUERY* (needs . ,eggs))
                            . ,repo-info)))))

;;; main

(set! *repo-info* (gather-egg-information
                   (car (command-line-arguments))))

(licenses (map string->symbol (cdr (command-line-arguments)))
          *repo-info*)

(use setup-download)

(define (licenses eggs repo-info)
  (pp
   (query-dependency-graph
    eggs repo-info
    (lambda (egg egg-info)
      (if egg-info
          `(,egg ,@(cond ((license egg-info)
                          => (lambda (x)
                               `((license ,x))))
                         (else '()))
                 ,@(let ((needs (needs egg-info)))
                     (if (pair? needs)
                         `((needs . ,needs))
                         '())))
          `(,egg ?))))))

;;;kv access

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

;;;dep graph

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
  (map (lambda (egg)
         (let ((egg-info (alist-ref egg repo-info)))
           (proc egg egg-info)))
       (cdr
        (dependency-graph '*QUERY*
                          `((*QUERY* (needs . ,eggs))
                            . ,repo-info)))))

;;; main

(when (< (length (command-line-arguments))
         1)
  (fprintf (current-error-port)
           "usage: ~a <path-to-chicken-repo> <egg> [<egg> ...]\n"
           (program-name))
  (exit 1))

(set! *repo-info* (gather-egg-information
                   (car (command-line-arguments))))

(licenses (map string->symbol (cdr (command-line-arguments)))
          *repo-info*)

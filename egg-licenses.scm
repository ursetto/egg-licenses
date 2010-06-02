(use setup-download)

(define *repo-info*)

(define (licenses egg repo-info)
  (let ((egg (if (string? egg) (string->symbol egg) egg)))
    (pp
     (map (lambda (egg)
            (let ((egg-info (alist-ref egg repo-info)))
              (if egg-info
                  `(,egg ,(license egg-info) ,(needs egg-info))
                  `(,egg ?))))
          (topological-sort (needs-dag egg repo-info) eq?)))))

(define (multi-licenses eggs repo-info)
  (let ((repo-info (cons `(*QUERY* (needs . ,eggs)
                                   (license "?"))
                         repo-info)))
    (pp
     (map (lambda (egg)
            (let ((egg-info (alist-ref egg repo-info)))
              (if egg-info
                  `(,egg (license ,(license egg-info))
                         (needs . ,(needs egg-info)))
                  `(,egg ?))))
          (topological-sort (needs-dag '*QUERY* repo-info) eq?)))))

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
(define (needs-dag egg repo-info)
  (let ((n (needs (alist-ref egg repo-info))))
    (cons (cons egg n)
          (append-map
           (lambda (x) (needs-dag x repo-info))
           n))))

(set! *repo-info* (gather-egg-information
                   (cadr (command-line-arguments))))

(licenses (caddr (command-line-arguments))
          *repo-info*)

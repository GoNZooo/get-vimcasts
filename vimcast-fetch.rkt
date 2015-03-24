#lang racket/base

(require racket/match
	 racket/port
	 net/url
	 xml
	 xml/path
	 html
	 gonz/human-time)

(permissive-xexprs #t)

(define (url->xexpr base-url)
  (map xml->xexpr
       (call/input-url (string->url base-url)
		       get-pure-port
		       read-html-as-xml)))

(define (find-link xexprs format)
  (define (find-all/recursive root element-type)
    (match root
      ['() '()]
      [(list (list sub-exprs ...) exprs ...)
       (append (find-all/recursive sub-exprs element-type)
	       (find-all/recursive exprs element-type))]
      [(list expr exprs ...)
       #:when (equal? element-type expr)
       (append `((,expr ,exprs)) (find-all/recursive exprs element-type))]
      [(list expr exprs ...)
       (find-all/recursive exprs element-type)]))

  (define (link-info link)
    (match link
      [`(a (((href ,link-ref)) ,link-text))
	(cons link-text link-ref)]
      [_ (cons #f #f)]))

  (define (find-format-link links format)
    (cdr (link-info (car  (filter (lambda (link)
	      (regexp-match? (pregexp format)
			     (cdr (link-info link))))
	    links)))))
  
  (find-format-link (find-all/recursive xexprs 'a)
		    format))

(define (find-video-link url format)
  (string-append url
		 (find-link (url->xexpr url)
			    format)))

(module+ main
  (find-video-link "http://media.vimcasts.org/videos/1/" "m4v"))

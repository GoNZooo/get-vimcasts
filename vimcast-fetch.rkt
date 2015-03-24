#lang racket/base

(require racket/match
	 racket/list
	 racket/system
	 racket/port
	 net/url
	 xml
	 xml/path
	 html
	 gonz/human-time)

(permissive-xexprs #t)

(define (url->xexprs base-url)
  (map xml->xexpr
       (call/input-url (string->url base-url)
		       get-pure-port
		       read-html-as-xml)))

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


(define (find-link xexprs filetype)
  (find-filetype-link (find-all/recursive xexprs 'a)
		      filetype))

(define (link-info link)
  (match link
    [`(a (((href ,link-ref)) ,link-text))
      (cons link-text link-ref)]
    [_ (cons #f #f)]))


(define (find-filetype-link links filetype)
  (cdr (link-info (car (filter (lambda (link)
				 (regexp-match? (pregexp filetype)
						(cdr (link-info link))))
			       links)))))


(define (download-cast episode-number filetype)
  (define full-dir-url (format "~a~a/"
			       "http://media.vimcasts.org/videos/"
			       episode-number))
  (define filename (find-filetype-link (find-all/recursive (url->xexprs full-dir-url)
							   'a)
				       filetype))
  (define download-url (string-append full-dir-url filename))

  (system (format "http --download ~a --output ~a_~a"
		  download-url episode-number filename)
	  #:set-pwd? #t))

(module+ main
  (define episode-numbers (range 1 68))
  (for-each (lambda (episode-number)
	      (download-cast episode-number "m4v")
	      (sleep (human-time 1m)))
	    episode-numbers))

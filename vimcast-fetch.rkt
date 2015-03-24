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

(define (find* root element-type)
  (match root
    ['() '()]
    [(list (list sub-exprs ...) exprs ...)
     (append (find* sub-exprs element-type)
	     (find* exprs element-type))]
    [(list expr exprs ...)
     #:when (equal? element-type expr)
     (append `((,expr ,exprs)) (find* exprs element-type))]
    [(list expr exprs ...)
     (find* exprs element-type)]))

(define (find-link xexprs filetype)
  (filetype-link (find* xexprs 'a)
		      filetype))

(define (link-data link)
  (match link
    [`(a (((href ,link-ref)) ,link-text))
      (cons link-text link-ref)]
    [_ (cons #f #f)]))

(define (filetype-link links filetype)
  (cdr (link-data (car (filter (lambda (link)
				 (regexp-match? (pregexp filetype)
						(cdr (link-data link))))
			       links)))))

(define (download-cast episode-number filetype)
  (define dir-url (format "~a~a/"
			       "http://media.vimcasts.org/videos/"
			       episode-number))
  (define filename (filetype-link (find* (url->xexprs dir-url)
							   'a)
				       filetype))
  (define download-url (string-append dir-url filename))

  (system (format "http --download ~a --output ~a_~a"
		  download-url episode-number filename)
	  #:set-pwd? #t))

(module+ main
  (define episode-numbers (range 1 68))
  (for-each (lambda (episode-number)
	      (download-cast episode-number "m4v")
	      (sleep (human-time 1m)))
	    episode-numbers))

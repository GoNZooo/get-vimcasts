#lang racket/base

(require racket/match
         racket/list
         racket/system
         gonz/human-time
         gonz/html-tree)

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

(define (find-link xexprs filetype)
  (filetype-link (find* xexprs 'a)
                 filetype))

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

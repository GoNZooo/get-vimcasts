#lang racket/base

(require racket/match
         racket/list
         racket/system
         racket/file
         racket/cmdline
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

(define (download-cast episode-number filetype output-path)
  (define dir-url (format "~a~a/"
                          "http://media.vimcasts.org/videos/"
                          episode-number))
  (define filename (filetype-link (find* (url->xexprs dir-url)
                                         'a)
                                  filetype))
  (define download-url (string-append dir-url filename))

  (system (format "http --download ~a --output ~a/~a_~a"
                  download-url output-path episode-number filename)
          #:set-pwd? #t))

(define (commandline-assemble)
  (command-line
    #:program "get-vimcasts"
    #:args (output-path)

    output-path))

(module+ main
  (define episode-numbers (range 1 68))
  (define out-path (commandline-assemble))
  (make-directory* out-path)
  (for-each (lambda (episode-number)
              (download-cast episode-number "m4v" out-path)
              (sleep (human-time 10s)))
            episode-numbers))

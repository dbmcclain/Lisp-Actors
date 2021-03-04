(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2018-01-31/distinfo.txt"
                      :replace t :prompt nil)
(ql-dist:install-dist "http://s3.us-east-1.amazonaws.com/emotiq-quickdist/emotiq.txt"
                      :replace t :prompt nil)
(setf (ql-dist:preference (ql-dist:dist "emotiq"))
      (1+ (ql-dist:preference (ql-dist:dist "quicklisp"))))

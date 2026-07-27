openssl req -new -text -subj "/C=US/ST=Arizona/L=Tucson/O=Refined Audiometrics Laboratory, LLC/CN=chicken" -passout pass:{bbe37564-f7b1-11ea-82f8-787b8acbe32e} -out newreq.pem -keyout newreq.pem
openssl x509 -req -in newreq.pem -signkey newreq.pem -text -passin pass:{bbe37564-f7b1-11ea-82f8-787b8acbe32e} -out newcert.pem
cat newreq.pem newcert.pem > cert-and-key.pem
openssl dhparam -text 1024 -out dh_param_1024.pem

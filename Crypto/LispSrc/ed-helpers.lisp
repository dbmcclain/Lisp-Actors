


(loop for ix from 0 below 10
      for start from 0 by 53
      for end from 53 by 53
      collect
        (let* ((start-byte  (truncate start 64))
               (start-bit   (rem start 64))
               (end-byte    (truncate end 64))
               (end-bit     (rem end 64)))
          (if (< start-bit (- 64 53))
              (format nil "~d  x[~d] >> ~d"
                      ix start-byte start-bit)
            ;; else
            (format nil "~d  x[~d] << ~d | x[~d] >> ~d"
                    ix
                    end-byte  (- 64 start-bit)
                    start-byte start-bit))
          ))

(loop for ix from 0 below 9
      for start from 0 by 64
      for end from 64 by 64
      collect
        (let* ((start-cell  (truncate start 51))
               (start-bit   (rem start 51)))
          (list ix start-cell start-bit))
        )

(loop for ix from 0 below 10
      for bx from 0 by 53
      collect
      (list ix (truncate bx 64) (rem bx 64)))

(loop for ix from 0 below 8
      for bx from 0 by 64
      collect
      (list ix (truncate bx 53) (rem bx 53)))
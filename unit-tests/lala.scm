((lambda ($clo)
   ((closure-code $clo)
    $clo
    (define + (make-closure (lambda ($this g2 g3) ($+ g2 g3))))
    (define - (make-closure (lambda ($this g4 g5) ($- g4 g5))))
    (define * (make-closure (lambda ($this g6 g7) ($* g6 g7))))
    (define quotient (make-closure (lambda ($this g8 g9) ($quotient g8 g9))))
    (define modulo (make-closure (lambda ($this g10 g11) ($modulo g10 g11))))
    (define = (make-closure (lambda ($this g12 g13) ($= g12 g13))))
    (define < (make-closure (lambda ($this g14 g15) ($< g14 g15))))
    (define number? (make-closure (lambda ($this g16) ($number? g16))))
    ((lambda (g17) ;; x = 11
       ((lambda (g18) ;; x = x + x = 11 + 11
          ((lambda (g19) ;; y = x + x = 2200 + 2200
             ((lambda ($clo)
                ((closure-code $clo)
                 $clo
                 ((lambda ($clo)
                    ((closure-code $clo)
                     $clo
                     g18
                     g19))
                  +)))
              println))
           ((lambda (g20)
              ((lambda ($clo)
                 ((closure-code $clo)
                  $clo
                  g20
                  g20))
               +))
            2200)))
        ((lambda ($clo)
           ((closure-code $clo)
            $clo
            g17
            g17))
         +)))
     11)))


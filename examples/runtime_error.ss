; Runtime error example: dynamic dispatch can fail when an
; untyped module's method is called on the wrong object type.
; The variable x is reassigned to either an Artist or Cowboy,
; then draw() is called -- but the runtime type may not match.
; Expected output: "run-time error"
(
  (module Cowboy (class Cowboy () (method draw () 1.0)))
  (module Artist (class Artist () (method draw () 666.0)))

  (module MiniMain
    (import Cowboy)
    (import Artist)
    (class MiniMain ()
      (method main ()
        (def a (new Artist ()))
        (def c (new Cowboy ()))
        (def x 0.0)
        (if0 1.0 (x = a) (x = c))
        (x = (x --> draw ()))
        x)))

  (timport MiniMain (() ((main () Number))))

  (def obj (new MiniMain ()))
  (obj --> main ())
)

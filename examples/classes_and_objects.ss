; Typed module with a class that has fields and methods.
; Demonstrates object instantiation, field access via methods,
; and method calls.
; Expected output: 7.0
(
  (tmodule Geometry
    (class Point (x y)
      (method getX () (this --> x))
      (method getY () (this --> y))
      (method sum ()
        (def px (this --> x))
        (def py (this --> y))
        (px + py)))
    (((x Number) (y Number))
     ((getX () Number) (getY () Number) (sum () Number))))

  (timport Geometry
    (((x Number) (y Number))
     ((getX () Number) (getY () Number) (sum () Number))))

  (def a 3.0)
  (def b 4.0)
  (def p (new Point (a b)))
  (p --> sum ())
)

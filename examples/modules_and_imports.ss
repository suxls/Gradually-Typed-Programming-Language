; Multi-module system with typed and untyped modules.
; Module Mone exports an identity method.
; Module Mtwo imports Mone via timport with a shape contract,
; and wraps it in its own method.
; Expected output: 2.0
(
  (module Mone (class cone () (method m (x) x)))

  (tmodule Mtwo
    (timport Mone (() ((m (Number) Number))))
    (class ctwo () (method m (x) x))
    (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))

  (timport Mone (() ((m (Number) Number))))
  (import Mtwo)

  (def mone (new cone ()))
  (def mtwo (new ctwo ()))
  (def monetoo (mtwo --> m (mone)))
  (def two 2.0)
  (monetoo --> m (two))
)

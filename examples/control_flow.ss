; Control flow with if0 and variable reassignment.
; if0 branches on whether the condition equals 0 (truthy = 0).
; Expected output: 42.0
(
  (def flag 0.0)
  (def result 0.0)
  (def answer 42.0)
  (def wrong 99.0)
  (if0 flag (result = answer) (result = wrong))
  result
)

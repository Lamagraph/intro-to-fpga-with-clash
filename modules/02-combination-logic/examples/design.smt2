(define-fun |adder_1_bit_fv#6|
  ((state |adder_1_bit_fv_s|)) (_ BitVec 1)
  (bvor
    (|adder_1_bit_fv#3| state)
    (|adder_1_bit_fv#5| state)
  )
) ; \c_out  <1>

(define-fun |adder_1_bit_fv#10|
  ((state |adder_1_bit_fv_s|)) Bool
  (=
    (concat
      (|adder_1_bit_fv#6| state)
      (|adder_1_bit_fv#7| state)
    )
    (|adder_1_bit_fv#9| state)
  )
) ; $eq$adder_1_bit_fv.sv:14$11_Y  <2>
; yosys-smt2-assert 0 _witness_.check_assert_adder_1_bit_fv_sv_14_8 adder_1_bit_fv.sv:14.5-14.44  <3>

(define-fun |adder_1_bit_fv_a 0|
  ((state |adder_1_bit_fv_s|)) Bool
  (or
    (|adder_1_bit_fv#10| state)
    (not true)
  )
) ; _witness_.check_assert_adder_1_bit_fv_sv_14_8  <4>

(define-fun |adder_1_bit_fv_a|
  ((state |adder_1_bit_fv_s|)) Bool
  (|adder_1_bit_fv_a 0| state)
) ;  <5>

(define-fun |adder_1_bit_fv_u|
  ((state |adder_1_bit_fv_s|)) Bool
  true
) ;  <6>

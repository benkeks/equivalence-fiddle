package io.equiv.eqfiddle.ccs

import io.equiv.eqfiddle.ts.Samples

object CCSSamples {
  val ltbts1 = """
@hint "compare processes by clicking on the gutter next to the @compare statements!"

@minimize

L13(x=-200,y=500)
R13(x=300,y=500)
L13 = a.b.0 + a.0
R13 = a.b.0
@eqs "L13,R13:T,S"
@notEqs "L13,R13:CT,1F"
@compare "L13,R13"

L16(x=-200,y=400)
R16(x=300,y=400)
L16 = a.b.0 + a.(b.0 + c.0)
R16 = a.(b.0 + c.0)
@eqs "L16,R16:CT,CS"
@notEqs "L16,R16:F,1F"
@compare "L16,R16"

L21(x=-200,y=300)
R21(x=300,y=300)
L21 = a.(b.0 + c.d.0) + a.(f.0 + c.e.0)
R21 = a.(b.0 + c.e.0) + a.(f.0 + c.d.0)
@eqs "L21,R21:F,R"
@notEqs "L21,R21:FT,RT"
@compare "L21,R21"

L24(x=-200,y=200)
R24(x=300,y=200)
L24 = a.b.0 + a.c.0
R24 = a.b.0 + a.(b.0 + c.0) + a.c.0
@eqs "L24,R24:F,FT"
@notEqs "L24,R24:R,RT"
@compare "L24,R24"

L27(x=-200,y=100)
R27(x=300,y=100)
L27 = a.(b.0 + a.(b.0 + c.d.0) + a.c.e.0)
  + a.(a.c.d.0 + a.(c.e.0 + b.0))
R27 = a.(a.(b.0 + c.d.0) + a.c.e.0)
  + a.(a.c.d.0 + a.(c.e.0 + b.0) + b.0)
@eqs "L27,R27:PF"
@notEqs "L27,R27:FT,S"
@compare "L27,R27"

L31(x=-200,y=0)
R31(x=300,y=0)
L31 = a.b.c.0 + a.b.d.0
R31 = a.(b.c.0 + b.d.0)
@eqs "L31,R31:PW,RT"
@notEqs "L31,R31:S"
@compare "L31,R31"

L34(x=-200,y=-100)
L34 = a.b.c.0 + a.(b.c.0 + b.d.0)
@eqs "L34,R31:RS"
@notEqs "L34,R31:PF"
@compare "L34,R31"

L38(x=100,y=-100)
L38 = a.b.0 + a.0 + a.c.0
@eqs "L38,R24:1F"
@notEqs "L38,R24:CT,F"
@compare "L38,R24"

L42(x=-200,y=-200)
R42(x=300,y=-200)
L42 = a.b.c.0 + a.(b.c.0 + b.0)
R42 = a.(b.c.0 + b.0)
@eqs "L42,R42:2S"
@notEqs "L42,R42:B"
@compare "L42,R42"

L50(x=-200,y=-300)
R50(x=300,y=-300)
L50 = a.(b.d.0 + c.e.0) + a.(c.f.0 + b.g.0)
R50 = a.(b.d.0 + c.e.0 + c.f.0 + b.g.0)
@eqs "L50,R50:RT"
@notEqs "L50,R50:PW"
@compare "L50,R50"

"0"(x=0, y=800)
"b.0"(x=-100, y=600)
"c.0"(x=0, y=600)
"d.0"(x=100, y=600)
"e.0"(x=200, y=600)
"f.0"(x=300, y=600)
"g.0"(x=400, y=600)
"b.c.0"(x=-490, y=161)
"b.d.0"(x=-307, y=262)
"a.c.d.0 + a.c.e.0 + b.0"(x=-262, y=213)
"a.b.0 + c.d.0 + a.c.e.0"(x=595, y=201)
"c.e.0"(x=599, y=435)
"b.c.0 + b.0"(x=132, y=-10)
"b.d.0 + c.e.0"(x=-2, y=-123)
"c.f.0 + b.g.0"(x=-95, y=-37)
"c.d.0"(x=-99, y=264)
"b.0 + a.b.0 + c.d.0 + a.c.e.0"(x=-121, y=168)
"f.0 + c.d.0"(x=212, y=548)
"a.c.d.0 + a.c.e.0 + b.0 + b.0"(x=178, y=207)
"f.0 + c.e.0"(x=-304, y=600)
"c.e.0 + b.0"(x=-63, y=401)
"b.0 + c.0"(x=-49, y=475)
"b.c.0 + b.d.0"(x=84, y=171)
"b.0 + c.d.0"(x=35, y=354)
"b.0 + c.e.0"(x=100, y=479)
"b.d.0 + c.e.0 + c.f.0 + b.g.0"(x=187, y=333)
"""

  val notFailureOrSim = """
P1(x=0, y=0)
P2(x=600, y=0)
"0"(x=300, y=600)
"b.0 + d.0"(x=500, y=400)
"c.0 + d.0"(x=300, y=300)
"d.0"(x=100, y=400)
"b.0 + c.0"(x=150, y=150)

P1 = (a.(b.0 + c.0) + a.d.0)
P2 = (a.(b.0 + d.0) + a.(c.0 + d.0))
@comment "P1 is distinguished from P2 under failure and simulation preorder"
@compare "P1,P2"
"""

  val failureTraceAndImpossibleFutures = """
P1(x=100, y=100)
P2(x=600, y=100)
"b.0 + c.0"(x=50, y=250)
"0"(x=350, y=450)
"c.0"(x=550, y=300)
"b.0"(x=300, y=250)

P1 = (a.b.0 + a.(b.0 + c.0) + a.c.0)
P2 = (a.b.0 + a.c.0)
@comment "P1 is preordered to P2 by failure-trace AND impossible futures"
@compare "P1,P2"
"""

  val reviewCounterexamples = """
@comment "These examples have been mischaracterized by the original published algorithm as failure-trace-preordered." 

L1(x=0, y=0)
R1(x=600, y=0)
"0"(x=300, y=450)
"b.0"(x=200, y=250)
"a.0 + b.0"(x=600, y=300)
"b.b.0"(x=300, y=100)

L1 = a.b.0
R1 = (a.0 + a.b.b.0 + a.(a.0 + b.0))
@comment "L1 is preordered to R1 by readiness (but distinguished by failure-traces AND impossible futures)" 
@compare "L1,R1" 

L2(x=0, y=700)
R2(x=600, y=700)
"b.0 + c.0 + d.d.0"(x=100, y=600)
"a.0 + d.d.0"(x=500, y=650)
"d.0"(x=300,y=600)
"d.d.0"(x=400, y=600)
"b.b.0 + c.0 + d.d.d.0"(x=550, y=500)
"b.0 + c.c.0 + d.d.d.0"(x=550, y=600)
"c.0"(x=461, y=508)
L2 = a.(b.0 + c.0 + d.d.0)
R2 = (a.(a.0 + d.d.0) + a.(b.b.0 + c.0 + d.d.d.0) + a.(b.0 + c.c.0 + d.d.d.0))
@compare "L2, R2"

L3(x=-200, y=500)
R3(x=-100, y=500)
L3 = c.(a.a.0 + b.b.0)
R3 = c.(a.a.0 + a.0 + b.0) + c.(a.a.0 + a.0 + b.b.0)
@comment "Example where the improved pruning finds minimal formula ⟨c⟩¬⟨a⟩¬⟨a⟩⊤."
@compare "L3, R3"
"""

  val weakBisimCoupledSimParallel = """
P1A = tau.a + tau.b + tau.c
P1B = tau.a + tau.(tau.b + tau.c)
@comment "Distinguished by Weak Bisim (eq by coupledsim = weaksim + contrasim)"
@compare "P1A,P1B" 

P2A = (res!0 | (res.a.0 + res.b.0 + res.c.0)) \ {res}
P2B = (res1!res2!0 | (res1.res2.a.0 + res1.(res2.b.0 + res2.c.0))) \ {res1, res2}

@comment "More complicated variant of P1"
@compare "P2A,P2B" 

@comment "Pc, Pp Contrasim from https://arxiv.org/pdf/2108.10492.pdf" 
Pc = (pl.sp.aEats | pl.sp.bEats | pl!0 | op.sp!0) \ {pl, sp}
Pp = (pl.op.sp.aEats | pl.op.sp.bEats | pl!0 | sp!0) \ {pl, sp}

@compare "Pc,Pp"

@comment "------- layout --------" 

P2A(x=133, y=-622)
P2B(x=468, y=-615)
Pc(x=-92, y=693)
Pp(x=511, y=590)
"(pl.sp.aEats | sp.bEats | 0 | op.sp!0) \ {pl,sp}"(x=-56, y=830)
"(pl.sp.aEats | pl.sp.bEats | pl!0 | sp!0) \ {pl,sp}"(x=11, y=783)
"(sp.aEats | pl.sp.bEats | 0 | op.sp!0) \ {pl,sp}"(x=110, y=701)
"(pl.op.sp.aEats | op.sp.bEats | 0 | sp!0) \ {pl,sp}"(x=402, y=771)
"(pl.op.sp.aEats | sp.bEats | 0 | sp!0) \ {pl,sp}"(x=426, y=838)
"(op.sp.aEats | pl.op.sp.bEats | 0 | sp!0) \ {pl,sp}"(x=598, y=729)
"(pl.op.sp.aEats | bEats | 0 | 0) \ {pl,sp}"(x=414, y=908)
"(pl.op.sp.aEats | 0 | 0 | 0) \ {pl,sp}"(x=395, y=999)
"(pl.sp.aEats | sp.bEats | 0 | sp!0) \ {pl,sp}"(x=-55, y=982)
"(pl.sp.aEats | bEats | 0 | 0) \ {pl,sp}"(x=-5, y=1120)
"(pl.sp.aEats | 0 | 0 | 0) \ {pl,sp}"(x=50, y=1243)
"(sp.aEats | pl.op.sp.bEats | 0 | sp!0) \ {pl,sp}"(x=608, y=790)
"(aEats | pl.op.sp.bEats | 0 | 0) \ {pl,sp}"(x=588, y=867)
"(0 | pl.op.sp.bEats | 0 | 0) \ {pl,sp}"(x=644, y=987)
"(sp.aEats | pl.sp.bEats | 0 | sp!0) \ {pl,sp}"(x=97, y=877)
"(aEats | pl.sp.bEats | 0 | 0) \ {pl,sp}"(x=142, y=1040)
"(0 | pl.sp.bEats | 0 | 0) \ {pl,sp}"(x=260, y=1157)

"""

  val strongWeakSims = """
P0A = tau.a
P0B = a
@compare "P0A, P0B"

P00A = a.tau.tau.b
P00B = tau.a.b
@compare "P00A, P00B"

P1A = (tau.b + a)
P1B = (P1A + b)
@comment "Delay bisimilar processes" 
@compare "P1B, P1A" 

P2A = a.(tau.b + c)
P2B = (a.b + P2A)
@comment "Weakly bisimilar but not delay bisim" 
@compare "P2B, P2A" 

@comment "------- layout --------" 

"0"(x=-23, y=780)
b(x=424, y=685)
P2A(x=79, y=116)
P2B(x=421, y=149)
P1A(x=-2, y=-26)
P1B(x=391, y=-154)
"tau.b + c"(x=242, y=325)
"""

  val petersonMutex = """
ReadyAf = (readyAf!ReadyAf + setReadyAf.ReadyAf + setReadyAt.ReadyAt)
ReadyAt = (readyAt!ReadyAt + setReadyAf.ReadyAf + setReadyAt.ReadyAt)

ReadyBf = (readyBf!ReadyBf + setReadyBf.ReadyBf + setReadyBt.ReadyBt)
ReadyBt = (readyBt!ReadyBt + setReadyBf.ReadyBf + setReadyBt.ReadyBt)

TurnA = (turnA!TurnA + setTurnA.TurnA + setTurnB.TurnB)
TurnB = (turnB!TurnB + setTurnA.TurnA + setTurnB.TurnB)

A1 = setReadyAt!setTurnB!A11
A11 = (readyBf.A12 + turnA.A12)
A12 = ecA.lcA.setReadyAf!A1

B1 = setReadyBt!setTurnA!B11
B11 = (readyAf.B12 + turnB.B12)
B12 = ecB.lcB.setReadyBf!B1

Peterson = (A1 | B1 | TurnA | ReadyAf | ReadyBf) \ {readyAf, readyAt, setReadyAf, setReadyAt, readyBf, readyBt, setReadyBf, setReadyBt, turnA, turnB, setTurnA, setTurnB}
Spec = (ecA.lcA.Spec + ecB.lcB.Spec)

@compare "Peterson, Spec" 
@compare "Spec, Peterson" 
@minimize



Peterson(x=35, weakness_saturated, y=63, main, bisim_minimized)
Spec(x=-289, y=-26, main, weakness_saturated)



@section "---------- below here, there's only layout information! ----------" 

"(A1 | setTurnA!B11 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=180, y=-480)
"(setTurnB!A11 | B1 | TurnA | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-16, y=-310)
"(setTurnB!A11 | setTurnA!B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=174, y=-293)
"(A1 | B11 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=388, y=78)
"(A1 | B12 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=501, y=-465)
"(setTurnB!A11 | B12 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=490, y=267)
"(A11 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-10, y=-130)
"(A12 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-8, y=31)
"(lcA.setReadyAf!A1 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=42, y=726)
"(lcA.setReadyAf!A1 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=204, y=730)
"(setTurnB!A11 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=38, y=257)
"(A11 | B12 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=480, y=439)
"(setTurnB!A11 | lcB.setReadyBf!B1 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=673, y=271)
"(A1 | lcB.setReadyBf!B1 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=656, y=119)
"(A11 | lcB.setReadyBf!B1 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=686, y=466)
"(A11 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=189, y=409)
"(A11 | B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=305, y=-166)
"(setTurnB!A11 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=336, y=-281)
"(A11 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=290, y=-129)
"(setTurnB!A11 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=233, y=299)
"(A12 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=185, y=583)
"(A12 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=301, y=37)
"(lcA.setReadyAf!A1 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=373, y=735)
"(setReadyAf!A1 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=4, y=353)
"(A1 | B1 | TurnB | ReadyAf | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=30, y=-445)
"(setReadyAf!A1 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=172, y=356)
"(A1 | setTurnA!B11 | TurnB | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=270, y=117)
"(A1 | B1 | TurnA | ReadyAf | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-39, y=-510)
"(A1 | setReadyBf!B1 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=837, y=146)
"(setTurnB!A11 | setReadyBf!B1 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=780, y=-231)
"(A11 | setReadyBf!B1 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=837, y=461)
"(setReadyAf!A1 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=328, y=356)

"lcA.Spec"(x=-279, y=182)
"lcB.Spec"(x=-27, y=-57)
"""

  val oneShotMutex = """
ReadyAf = (readyAf!ReadyAf + setReadyAf.ReadyAf + setReadyAt.ReadyAt)
ReadyAt = (readyAt!ReadyAt + setReadyAf.ReadyAf + setReadyAt.ReadyAt)

ReadyBf = (readyBf!ReadyBf + setReadyBf.ReadyBf + setReadyBt.ReadyBt)
ReadyBt = (readyBt!ReadyBt + setReadyBf.ReadyBf + setReadyBt.ReadyBt)

TurnA = (turnA!TurnA + setTurnA.TurnA + setTurnB.TurnB)
TurnB = (turnB!TurnB + setTurnA.TurnA + setTurnB.TurnB)

A1 = setReadyAt!setTurnB!A11
A11 = (readyBf.A12 + turnA.A12)
A12 = ecA.lcA.0

B1 = setReadyBt!setTurnA!B11
B11 = (readyAf.B12 + turnB.B12)
B12 = ecB.lcB.0

Peterson = (A1 | B1 | TurnA | ReadyAf | ReadyBf) \ {readyAf, readyAt, setReadyAf, setReadyAt, readyBf, readyBt, setReadyBf, setReadyBt, turnA, turnB, setTurnA, setTurnB}

Spec = (ecA.lcA + ecB.lcB)

@compare "Peterson, Spec" 

@compare "Spec, Peterson" 


Peterson(x=7, y=-155, main, weakness_saturated, bisim_minimized)
Spec(x=926, y=-278, main, weakness_saturated)



@section "---------- below here, there's only layout information! ----------"

"(A1 | setTurnA!B11 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=181, y=-89)
"(setTurnB!A11 | B1 | TurnA | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-44, y=44)
"(setTurnB!A11 | setTurnA!B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=145, y=107)
"(A11 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=48, y=285)
"(A11 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-80, y=197)
"(setTurnB!A11 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=287, y=167)
"(A12 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-92, y=391)
"(lcA.0 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-87, y=528)
"(A1 | B11 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=367, y=-25)
"(A1 | B12 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=522, y=27)
"(A1 | lcB.0 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=666, y=148)
"(A1 | 0 | TurnA | ReadyAf | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=792, y=222)
"(0 | B1 | TurnB | ReadyAt | ReadyBf) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=-71, y=664)
"(0 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=116, y=676)
"(0 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=257, y=672)
"(A11 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=192, y=322)
"(A12 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=44, y=420)
"(lcA.0 | setTurnA!B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=57, y=536)
"(lcA.0 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=196, y=534)
"(A12 | B11 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=173, y=426)
"(A11 | B11 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=234, y=290)
"(A11 | B12 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=391, y=337)
"(setTurnB!A11 | B12 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=436, y=208)
"(A11 | lcB.0 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=521, y=376)
"(A11 | 0 | TurnB | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=665, y=428)
"(setTurnB!A11 | 0 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=716, y=345)
"(setTurnB!A11 | lcB.0 | TurnA | ReadyAt | ReadyBt) \ {readyAf,readyAt,setReadyAf,setReadyAt,readyBf,readyBt,setReadyBf,setReadyBt,turnA,turnB,setTurnA,setTurnB}"(x=579, y=266)

lcB(x=1111, y=-262)
lcA(x=933, y=-138)
"0"(x=1144, y=-126)
"""

  val diverseEquivalences = """
P1(x=400, y=150)
P2(x=600, y=150)
"0"(x=600, y=660)
c(x=700, y=400)
"a + b.c"(x=500, y=500)
"a + b + b.c"(x=600, y=400)
S0(x=600, y=300)
"a.(a + b.c) + S0"(x=450, y=300)
S1(x=400, y=400)

P1 = b.(a.(a + b.c) + S0)
P2 = (b.S0 + b.c)
S0 = a.(a + b + b.c)
S1 = a.(a + b.c)

@compare "P1,P2" 
@compare "S0,S1"

@minimize
"""

  val namedSamples = List[Samples.Example](
    Samples.Example("ltbts1",
      "Linear Time Branching Time Spectrum 1",
      ltbts1),
    Samples.Example("neither-failure-sim",
      "Neither failure nor simulation equivalent",
      notFailureOrSim),
    Samples.Example("ft-and-if",
      "FT as well as IF preordered",
      failureTraceAndImpossibleFutures),
    Samples.Example("review-counterexamples",
      "Spurious failure-trace preorderings in original algorithm",
      reviewCounterexamples),
    Samples.Example("diverse-eqs",
      "Diverse Equivalences",
      diverseEquivalences),
    Samples.Example("weak-sims",
      "Weak Bisim, Coupled, Contrasim",
      weakBisimCoupledSimParallel),
    Samples.Example("strong-weak-sims",
      "Eta, Branching, Delay Bisims",
      strongWeakSims),
    Samples.Example("peterson-mutex",
      "Peterson Mutual exclusion",
      petersonMutex),
    Samples.Example("peterson-mutex-one-shot",
      "Peterson Mutual exclusion, non-recursive",
      oneShotMutex)
  )

  def getExample(slug: String) = {
    namedSamples.find(_.slug == slug)
  }

  val default = petersonMutex

}

package de.bbisping.eqfiddle.ccs

import de.bbisping.eqfiddle.ts.Samples

object CCSSamples {
  val ltbts1 = """
@hint "compare processes by clicking on the gutter next to the @compare statements!"

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
B1f = (b1rf!B1f + b1wf.B1f + b1wt.B1t)
B1t = (b1rt!B1t + b1wf.B1f + b1wt.B1t)
B2f = (b2rf!B2f + b2wf.B2f + b2wt.B2t)
B2t = (b2rt!B2t + b2wf.B2f + b2wt.B2t)

K1 = (kr1!K1 + kw1.K1 + kw2.K2)
K2 = (kr2!K2 + kw1.K1 + kw2.K2)
P1 = b1wt!kw2!P11
P11 = (b2rf.P12 + b2rt.(kr2.P11 + kr1.P12))
P12 = enter1.exit1.b1wf!P1
P2 = b2wt!kw1!P21
P21 = (b1rf.P22 + b1rt.(kr1.P21 + kr2.P22))
P22 = enter2.exit2.b2wf!P2

Peterson = (P1 | P2 | K1 | B1f | B2f) \ {b1rf, b1wf, b1wt, b1rt, b2rf, b2wf, b2rt, b2wt, kr1, kr2, kw1, kw2}

Spec = (enter1.exit1.Spec + enter2.exit2.Spec)

@compare "Peterson, Spec"


Peterson(x=50, y=-200, main)
Spec(x=1099, y=-289, main)

"exit2.Spec"(x=1030, y=-107)
"(P1 | kw1!P21 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-1, y=-81)
"(kw2!P11 | P2 | K1 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=225, y=-176)
"(P1 | P2 | K1 | B1f | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=185, y=32)
"(kw2!P11 | kw1!P21 | K1 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=61, y=30)
"(P11 | P2 | K2 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=330, y=-59)
"(kw2!P11 | b2wf!P2 | K1 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=608, y=100)
"(P1 | P21 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-150, y=87)
"(kw2!P11 | P21 | K1 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=14, y=107)
"(P12 | P2 | K2 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=326, y=10)
"(kr2.P11 + kr1.P12 | b2wf!P2 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=437, y=336)
"(exit1.b1wf!P1 | P2 | K2 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=434, y=96)
"(P1 | kr1.P21 + kr2.P22 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-89, y=281)
"(kw2!P11 | P2 | K2 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=143, y=213)
"(kr2.P11 + kr1.P12 | P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=132, y=278)
"(kw2!P11 | kw1!P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=118, y=310)
"(kw2!P11 | kr1.P21 + kr2.P22 | K1 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=6, y=207)
"(P1 | P22 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-173, y=230)
"(P1 | exit2.b2wf!P2 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-190, y=388)
"(P11 | P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=74, y=122)
"(P1 | kw1!P21 | K2 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=290, y=423)
"""

  val oneShotMutex = """
B1f = (b1rf!B1f + b1wf.B1f + b1wt.B1t)
B1t = (b1rt!B1t + b1wf.B1f + b1wt.B1t)
B2f = (b2rf!B2f + b2wf.B2f + b2wt.B2t)
B2t = (b2rt!B2t + b2wf.B2f + b2wt.B2t)

K1 = (kr1!K1 + kw1.K1 + kw2.K2)
K2 = (kr2!K2 + kw1.K1 + kw2.K2)
P1 = b1wt!kw2!P11
P11 = (b2rf.P12 + b2rt.(kr2.P11 + kr1.P12))
P12 = enter1.exit1.0
P2 = b2wt!kw1!P21
P21 = (b1rf.P22 + b1rt.(kr1.P21 + kr2.P22))
P22 = enter2.exit2.0

Peterson = (P1 | P2 | K1 | B1f | B2f) \ {b1rf, b1wf, b1wt, b1rt, b2rf, b2wf, b2rt, b2wt, kr1, kr2, kw1, kw2}

Spec = (enter1.exit1 + enter2.exit2)

@compare "Peterson, Spec" 

@compare "Spec, Peterson"

Peterson(x=50, y=-200, main)
Spec(x=1099, y=-289, main)

"(P1 | kw1!P21 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-1, y=-81)
"(kw2!P11 | P2 | K1 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=225, y=-176)
"(kw2!P11 | kw1!P21 | K1 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=61, y=30)
"(P11 | P2 | K2 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=330, y=-59)
"(P1 | P21 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-150, y=87)
"(P12 | P2 | K2 | B1t | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=326, y=10)
"(kr2.P11 + kr1.P12 | P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=132, y=278)
"(P1 | P22 | K1 | B1f | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-173, y=230)
"(P11 | P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=74, y=122)
"0"(x=1252, y=-5)
exit2(x=1296, y=-152)
exit1(x=1095, y=-101)
"(P11 | 0 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=147, y=618)
"(kr2.P11 + kr1.P12 | 0 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=414, y=555)
"(kw2!P11 | 0 | K1 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=295, y=445)
"(0 | kw1!P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-229, y=457)
"(exit1.0 | kw1!P21 | K2 | B1t | B2t) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}"(x=-83, y=550)
"""

  val diverseEquivalences = """
P1(x=400, y=150)
P2(x=600, y=150)
"0"(x=600, y=660)
c(x=700, y=400)
"a + b.c"(x=500, y=500)
"a + b + b.c"(x=600, y=400)
"a.(a + b.c) + a.(a + b + b.c)"(x=400, y=300)
"a.(a + b + b.c)"(x=600, y=300)

P1 = b.(a.(a + b.c) + a.(a + b + b.c))
P2 = (b.a.(a + b + b.c) + b.c)

@compare "P1,P2"

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

  val default = failureTraceAndImpossibleFutures

}

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
P2A(x=0, y=300)
P2B(x=600, y=300)

P2A = (res!0 | (res.a.0 + res.b.0 + res.c.0)) \ {res}
P2B = (res1!res2!0 | (res1.res2.a.0 + res1.(res2.b.0 + res2.c.0))) \ {res1, res2}

@comment "Distinguished by Weak Bisim"
@compare "P2A,P2B"
"""

  val petersonMutex = """
B1f = b1rf!B1f + b1wf.B1f + b1wt.B1t
B1t = b1rt!B1t + b1wf.B1f + b1wt.B1t
B2f = b2rf!B2f + b2wf.B2f + b2wt.B2t
B2t = b2rt!B2t + b2wf.B2f + b2wt.B2t

K1 = kr1!K1 + kw1.K1 + kw2.K2
K2 = kr2!K2 + kw1.K1 + kw2.K2
P1 = b1wt!kw2!P11
P11 = b2rf.P12 + b2rt.(kr2.P11 + kr1.P12)
P12 = enter1.exit1.b1wf!P1
P2 = b2wt!kw1!P21
P21 = b1rf.P22 + b1rt.(kr1.P21 + kr2.P22)
P22 = enter2.exit2.b2wf!P2

Peterson = (P1 | P2 | K1 | B1f | B2f) \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}

Spec = enter1.exit1.Spec + enter2.exit2.Spec
""" // \ {b1rf,b1wf,b1wt,b1rt,b2rf,b2wf,b2rt,b2wt,kr1,kr2,kw1,kw2}


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
    Samples.Example("weak-sims",
       "Weak Bisim, Coupled, Contrasim",
         weakBisimCoupledSimParallel),
    Samples.Example("peterson-mutex",
      "Peterson Mutual exclusion",
        petersonMutex)
  )

  def getExample(slug: String) = {
    namedSamples.find(_.slug == slug)
  }

  val default = failureTraceAndImpossibleFutures

}

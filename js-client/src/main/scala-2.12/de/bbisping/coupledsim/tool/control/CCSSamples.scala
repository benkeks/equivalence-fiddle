package de.bbisping.coupledsim.tool.control

object CCSSamples {
  val ltbts1 = """
"0"(x=0, y=700)
"a.0"(x=-200, y=600)
"b.0"(x=-100, y=600)
"c.0"(x=0, y=600)
"d.0"(x=100, y=600)
"e.0"(x=200, y=600)
"f.0"(x=300, y=600)
"g.0"(x=400, y=600)

L13(x=-200,y=500) R13(x=300,y=500)
L13 = a.b.0 + a.0
R13 = a.b.0
@eqs "L13,R13:T,S"
@notEqs "L13,R13:CT,1F"

L16(x=-200,y=400) R16(x=300,y=400)
L16 = a.b.0 + a.(b.0 + c.0)
R16 = a.(b.0 + c.0)
@eqs "L16,R16:CT,CS"
@notEqs "L16,R16:F,1F"

L21(x=-200,y=300) R21(x=300,y=300)
L21 = a.(b.0 + c.d.0) + a.(f.0 + c.e.0)
R21 = a.(b.0 + c.e.0) + a.(f.0 + c.d.0)
@eqs "L21,R21:F,R"
@notEqs "L21,R21:FT,RT"

L24(x=-200,y=200) R24(x=300,y=200)
L24 = a.b.0 + a.c.0
R24 = a.b.0 + a.(b.0 + c.0) + a.c.0
@eqs "L24,R24:F,FT"
@notEqs "L24,R24:R,RT"

L27(x=-200,y=100) R27(x=300,y=100)
L27 = a.(b.0 + a.(b.0 + c.d.0) + a.c.e.0)
	+ a.(a.c.d.0 + a.(c.e.0 + b.0))
R27 = a.(a.(b.0 + c.d.0) + a.c.e.0)
	+ a.(a.c.d.0 + a.(c.e.0 + b.0) + b.0)
@eqs "L27,R27:PF"
@notEqs "L27,R27:FT,S"

L31(x=-200,y=0) R31(x=300,y=0)
L31 = a.b.c.0 + a.b.d.0
R31 = a.(b.c.0 + b.d.0)
@eqs "L31,R31:PW,RT"
@notEqs "L31,R31:S"

L34(x=-200,y=-100)
L34 = a.b.c.0 + a.(b.c.0 + b.d.0)
@eqs "L34,R31:RS"
@notEqs "L34,R31:PF"

L38(x=-100,y=-100)
L38 = a.b.0 + a.0 + a.c.0
@eqs "L38,L24:1F"
@notEqs "L38,L24:CT,F"

L42(x=-200,y=-200) R42(x=300,y=-200)
L42 = a.b.c.0 + a.(b.c.0 + b.0)
R42 = a.(b.c.0 + b.0)
@eqs "L42,R42:2S"
@notEqs "L42,R42:B"

L50(x=-200,y=-300) R50(x=300,y=-300)
L50 = a.(b.d.0 + c.e.0) + a.(c.f.0 + b.g.0)
R50 = a.(b.d.0 + c.e.0 + c.f.0 + b.g.0)
@eqs "L50,R50:RT"
@notEqs "L50,R50:PW"
"""

  val namedSamples = List[Samples.Example](
    Samples.Example("ltbts1",
	    "Linear Time Branching Time Spectrum 1",
        ltbts1)
  )

  def getExample(slug: String) = {
    namedSamples.find(_.slug == slug)
  }

  val default = ltbts1

}

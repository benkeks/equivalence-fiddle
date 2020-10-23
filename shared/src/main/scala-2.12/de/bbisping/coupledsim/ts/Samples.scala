package de.bbisping.coupledsim.ts

object Samples {

  case class Example(slug: String, name: String, code: String)

  val diamond = """
a |-x-> b
b |-y-> bot
a |-x-> c
c |-y-> bot"""

   val bug1 = """
c(x=415, y=120)
b(x=241, y=306)
bot(x=303, y=221)
d(x=297, y=119)
a(x=403, y=297)
a |-y-> b
b |-y-> bot
a |-x-> c
bot |-y-> a
bot |-y-> d
d |-x-> c
d |-y-> b"""

   val bug2 = """
c(x=240, y=109)
b(x=408, y=250)
d(x=237, y=252)
bot(x=323, y=181)
a(x=411, y=109)
a |-x-> b
b |-y-> bot
a |-x-> c
bot |-y-> a
bot |-y-> d
d |-x-> c
d |-x-> b
c |-y-> bot
"""

  val abc = """
a --> b
b --> c
c --> d
d --> e
e --> f
f --> g
g --> h
h --> i
i --> j
j --> k
k --> l
l --> m
m --> n
n --> o
o --> p
p --> q
q --> r
r --> s
s --> t
t --> u
u --> v
v --> w
w --> x
x --> y
y --> z"""

  val abc6 = """a(x=325, y=106)
b(x=396, y=158)
c(x=394, y=238)
d(x=323, y=289)
e(x=244, y=248)
f(x=221, y=159)
a |-x-> b
b |-x-> c
c |-x-> d
d |-x-> e
e |-x-> f"""

  val weakBisim1 = """

a1(x=200, y=100)
a2(x=200, y=200)
a3(x=200, y=300)
a1 |-o-> a2
a2 |-tau-> a3

b1(x=300, y=100)
b2(x=300, y=300)
b1 |-o-> b2

c1(x=400, y=100)
c2(x=400, y=200)
c3(x=400, y=300)
c1 |-tau-> c2
c2 |-o-> c3

d1(x=500, y=100)
d2(x=500, y=166)
d3(x=500, y=233)
d4(x=500, y=300)
d1 |-tau-> d2
d2 |-o-> d3
d3 |-tau-> d4

e1(x=600, y=100)
e2(x=600, y=150)
e3(x=600, y=200)
e4(x=600, y=250)
e5(x=600, y=300)
e1 |-tau-> e2
e2 |-tau-> e3
e3 |-o-> e4
e4 |-tau-> e5
"""


  val coupledSim1 = """
A0(x=200, y=100)
A1(x=100, y=200)
A2(x=200, y=200)
A3(x=300, y=200)
A0 |-tau-> A1
A1 |-a-> A1
A0 |-tau-> A2
A2 |-b-> A2
A0 |-tau-> A3
A3 |-c-> A3

B0(x=500, y=100)
B1(x=450, y=200)
B2(x=550, y=200)
B3(x=500, y=300)
B4(x=600, y=300)
B0 |-tau-> B1
B1 |-a-> B1
B0 |-tau-> B2
B2 |-tau-> B3
B2 |-tau-> B4
B3 |-b-> B3
B4 |-c-> B4

@c "coupled sim but not weak bisim"
"""

    val coupledSim2 = """
a2(x=166, y=178)
a4(x=268, y=157)
a3(x=171, y=235)
b4(x=440, y=151)
b1(x=373, y=91)
b2(x=339, y=172)
b3(x=341, y=246)
c1(x=647, y=82)
c2(x=651, y=161)
c3(x=640, y=247)
a1 |-x-> a2
a2 |-x-> a3
a1 |-x-> a4

b1 |-x-> b2
b2 |-x-> b3
b1 |-x-> b4
b4 |-tau-> b4

c1 |-x-> c2
c2 |-x-> c3

@c "properly distinguished by coupled sim but not s-coupled sim (transitivity)"
"""

   val coupledSimPhil = """Init1(x=225, y=45)
BCsit1(x=300, y=121)
Asits1(x=137, y=113)
A1(x=130, y=186)
C1(x=353, y=240)
B1(x=256, y=211)
Init2(x=549, y=37)
C2(x=429, y=246)
B2(x=508, y=217)
A2(x=615, y=179)

Init1 |-tau-> Asits1
Init1 |-tau-> BCsit1
Asits1 |-tau-> A1
BCsit1 |-tau-> B1
BCsit1 |-tau-> C1
A1 |-aEats-> A1
B1 |-bEats-> B1
C1 |-cEats-> C1

Init2 |-tau-> A2
Init2 |-tau-> B2
Init2 |-tau-> C2
A2 |-aEats-> A2
B2 |-bEats-> B2
C2 |-cEats-> C2"""


  val coupledSim1b = """
A00(x=200, y=0)
A0(x=200, y=100)
A1(x=100, y=200)
A2(x=200, y=200)
A3(x=300, y=200)
A00 |-a-> A0
A0 |-tau-> A1
A1 |-a-> A1
A0 |-tau-> A2
A2 |-b-> A2
A0 |-tau-> A3
A3 |-c-> A3

B00(x=500, y=0)
B0(x=500, y=100)
B1(x=450, y=200)
B2(x=550, y=200)
B3(x=500, y=300)
B4(x=600, y=300)
B00 |-a-> B0
B0 |-tau-> B1
B1 |-a-> B1
B0 |-tau-> B2
B2 |-tau-> B3
B2 |-tau-> B4
B3 |-b-> B3
B4 |-c-> B4

@c "coupled sim but not weak bisim (with more complex tau-a steps)"
"""

    val coupledSimPhilMin = """
P_g(x=225, y=45)
g_BC(x=300, y=121)
g_A(x=137, y=113)
A(x=130, y=186)
C(x=353, y=240)
B(x=256, y=211)
P_o(x=214, y=362)
P_t(x=81, y=283)
Troll(x=69, y=198)

P_g |-tau-> g_A
P_g |-tau-> g_BC
g_A |-tau-> A
g_BC |-tau-> B
g_BC |-tau-> C
A |-aEats-> A
B |-bEats-> B
C |-cEats-> C

P_o |-tau-> A
P_o |-tau-> B
P_o |-tau-> C

P_t |-tau-> A
P_t |-tau-> B
P_t |-tau-> C
P_t |-tau-> Troll

@c "P_o == P_g coupled sim but not weak bisim"
@c "P_o == P_t weak sim but not coupled sim"
"""

//  val coupledSim2 = """
//a0(tau, x=149, y=96)
//a1(tau, x=103, y=144)
//a2(red, x=157, y=150)
//a3(green, x=198, y=146)
//a4(blue, x=103, y=200)
//a0 --> a1
//a0 --> a2
//a0 --> a3
//a1 --> a4
//
//b0(tau, x=299, y=91)
//b1(red, x=249, y=140)
//b15(green, x=285, y=142)
//b2(tau, x=337, y=140)
//b3(green, x=309, y=180)
//b4(tau, x=369, y=180)
//b5(blue, x=372, y=226)
//b0 --> b1
//b0 --> b15
//b0 --> b2
//b2 --> b3
//b2 --> b4
//b4 --> b5
//
//@c "coupled sim but not weak bisim 2"
//"""
//
//  val coupledSim2Div = """
//a0(tau, x=149, y=96)
//a1(tau, x=103, y=144)
//a2(red, x=157, y=150)
//a3(green, x=198, y=146)
//a4(blue, x=103, y=200)
//a0 --> a1
//a0 --> a2
//a0 --> a3
//a1 --> a4
//
//b0(tau, x=299, y=91)
//b1(red, x=249, y=140)
//b15(green, x=285, y=142)
//b2(tau, x=337, y=140)
//b3(green, x=309, y=180)
//b4(tau, x=369, y=180)
//b5(blue, x=372, y=226)
//b0 --> b1
//b0 --> b15
//b0 --> b2
//b2 --> b3
//b2 --> b4
//b4 --> b5
//b4 --> b0
//
//@c "coupled sim but not divergence preserving"
//"""

    val contraSimPhil = """
B(x=332, y=328)
r_B(x=356, y=225)
A(x=175, y=344)
r1_AB(x=245, y=229)
P_r(x=216, y=116)
P1_r(x=377, y=74)
r_A(x=191, y=218)
A |-aEats-> A
B |-bEats-> B

P_r |-tau-> r_A
r_A |-grace-> A
P_r |-tau-> r_B
r_B |-grace-> B

P1_r |-tau-> r_A
P1_r |-tau-> r_B
P1_r |-grace-> r1_AB
r1_AB |-tau-> A
r1_AB |-tau-> B

@c "P_r == P1_r contrasim but not coupled sim"
"""

     val notWeakBisimCCS = """
x1(x=144, y=95)
x3(x=116, y=272)
yX(x=326, y=254)
y1(x=306, y=84)
y2(x=258, y=168)
y3(x=234, y=293)
x1 |-a-> x2
x2 |-b-> x3

y1 |-a-> y2
y1 |-tau-> yX
y2 |-b-> y3
@c "example that shows that CS and WB are no congruences for CCS."
"""

  val contraSim1 = """
A0(x=150, y=100)
A1(x=100, y=200)
A2(x=200, y=200)
A0 |-a-> A1
A1 |-x-> A1
A0 |-a-> A2
A2 |-y-> A2

B0(x=350, y=100)
B1(x=350, y=200)
B2(x=300, y=300)
B3(x=400, y=300)
B0 |-a-> B1
B1 |-tau-> B2
B1 |-tau-> B3
B2 |-x-> B2
B3 |-y-> B3

@c "contra sim but not coupled sim"
"""

  val noContraSim1 = """
A0(x=164, y=78)
B0(x=365, y=110)
A1(x=160, y=147)
B1(x=334, y=178)
A2(x=237, y=206)
A3(x=147, y=282)
A4(x=252, y=348)

A0 |-b-> A1
A1 |-c-> A2
A1 |-tau-> A3
A3 |-a-> A4

B0 |-b-> B1
B1 |-a-> A4

@c "no contra sim but single-action 'contrasim'"
"""

  val weakBisim2 = """

A1(x=100, y=0)
A2(x=100, y=100)
A5(x=200, y=200)
A3(x=100, y=200)
A4(x=100, y=300)
A1 |-a-> A2
A2 |-tau-> A3
A3 |-a-> A4
A2 |-b-> A5

B1(x=400, y=0)
B6(x=300, y=100)
B7(x=300, y=200)
B2(x=400, y=100)
B5(x=500, y=200)
B3(x=400, y=200)
B4(x=400, y=300)
B1 |-a-> B2
B2 |-tau-> B3
B3 |-a-> B4
B2 |-b-> B5
B1 |-a-> B6
B6 |-a-> B7

@c "weak sim but not branching or delay sim from figure 4.6. in San2012"
@c "where the encoding of edges-as-nodes following DP2004 does not work."
"""
    val weakBisim2b = """

A1(x=100, y=0)
A2(x=100, y=100)
A5(x=200, y=200)
A3(x=100, y=200)
A4(x=100, y=300)
A1 |-a-> A2
A2 |-tau-> A3
A3 |-a-> A4
A3 |-tau-> A4
A2 |-b-> A5

B1(x=400, y=0)
B6(x=300, y=100)
B7(x=300, y=200)
B2(x=400, y=100)
B5(x=500, y=200)
B3(x=400, y=200)
B4(x=400, y=300)
B1 |-a-> B2
B2 |-tau-> B3
B3 |-a-> B4
B3 |-tau-> B4
B2 |-b-> B5
B1 |-a-> B6
B6 |-a-> B7
B6 |-tau-> B7
B1 |-a-> B7


@c "slightly modified weak bisim 2. (counter example for naive cs-closure algorithm)"

"""

  val sim1 = """
  A1(x=109, y=129)
A2(x=87, y=195)
A3(x=154, y=191)
A4(x=138, y=260)
B1(x=269, y=103)
B2(x=269, y=159)
B3(x=252, y=267)
A1 |-a-> A2
A1 |-a-> A3
A3 |-b-> A4

B1 |-a-> B2
B2 |-b-> B3

@c "similar but not coupled sim or bisim."
"""

    val weakStepBlowUp = """
c_0(x=100, y=0)
c_1(x=200, y=100)
c_2(x=100, y=200)
c_3(x=0, y=100)
c_0 |-tau-> c_1
c_0 |-a-> c_1
c_1 |-tau-> c_2
c_1 |-b-> c_2
c_2 |-tau-> c_3
c_2 |-c-> c_3
c_3 |-tau-> c_0
c_3 |-d-> c_0

l_0(x=400, y=0)
l_1(x=500, y=100)
l_2(x=400, y=200)
l_3(x=300, y=100)
l_0 |-tau-> l_1
l_0 |-a-> l_1
l_1 |-tau-> l_2
l_1 |-b-> l_2
l_2 |-tau-> l_3
l_2 |-c-> l_3

@c "cyclic and linear systems with big weak transition blowup"
"""

  val ltbtSpectrum = """@t "separating examples from [Gla93]"

d3(x=103, y=70)
d2(x=142, y=19)
d0(x=194, y=-43)
d1(x=196, y=77)
e2(x=283, y=23)
e0(x=307, y=-37)
e3(x=221, y=74)
e1(x=351, y=79)
f0(x=445, y=-46)
f1(x=437, y=-11)
f2(x=397, y=72)
f3(x=488, y=68)
g0(x=573, y=-64)
g2(x=532, y=68)
g1(x=574, y=-15)
g3(x=621, y=63)
h0(x=732, y=-70)
h1(x=774, y=65)
h2(x=694, y=-7)
h3(x=680, y=62)
i0(x=872, y=-84)
i1(x=910, y=70)
i2(x=839, y=1)
j0(x=367, y=131)
j1(x=327, y=193)
j2(x=395, y=178)
j3(x=345, y=237)
j4(x=379, y=294)
j5(x=420, y=239)
k0(x=474, y=129)
k1(x=465, y=190)
k4(x=473, y=295)
k2(x=507, y=242)
k3(x=446, y=240)
l0(x=571, y=119)
l1(x=541, y=191)
l2(x=596, y=187)
l3(x=547, y=249)
l4(x=586, y=300)
l5(x=608, y=243)
m0(x=702, y=118)
m2(x=742, y=190)
m1(x=665, y=192)
m3(x=670, y=261)
m4(x=750, y=255)
m5(x=711, y=303)
n0(x=843, y=112)
n1(x=841, y=178)
n3(x=901, y=235)
n2(x=813, y=243)
n4(x=850, y=303)
q0(x=683, y=372)
q2(x=733, y=448)
q1(x=637, y=448)
r0(x=851, y=358)
r2(x=932, y=446)
r1(x=816, y=450)
x0(x=207, y=510)
x2(x=238, y=585)
x4(x=261, y=630)
x1(x=190, y=576)
x3(x=185, y=624)
x5(x=214, y=681)
y0(x=332, y=517)
y1(x=331, y=595)
y2(x=336, y=656)
y3(x=347, y=706)
za0(x=416, y=499)
za1(x=419, y=578)
za2(x=422, y=660)
za3(x=431, y=718)
zb0(x=495, y=500)
zb1(x=508, y=615)
zb2(x=502, y=719)
zc0(x=557, y=500)
zc1(x=565, y=618)
zc2(x=556, y=723)
zd0(x=627, y=504)
zd1(x=635, y=612)
zd2(x=632, y=724)
zd3(x=684, y=651)
ze1(x=729, y=617)
ze2(x=746, y=727)
ze0(x=709, y=506)
zf0(x=769, y=499)
zf1(x=799, y=615)
zf3(x=869, y=722)
zf2(x=790, y=703)
d0 |-a-> d1
d0 |-tau-> d2
d2 |-tau-> d3
d2 |-b-> d1
d3 |-c-> d1

e0 |-a-> e1
e0 |-tau-> e2
e2 |-tau-> e3
e0 |-tau-> e3
e2 |-b-> e1
e3 |-c-> e1

f0 |-a-> f1
f1 |-tau-> f2
f1 |-c-> f3
f2 |-b-> f3

g0 |-a-> g1
g0 |-a-> g2
g1 |-tau-> g2
g1 |-c-> g3
g2 |-b-> g3

h0 |-a-> h1
h0 |-b-> h1
h0 |-tau-> h2
h2 |-b-> h1
h2 |-tau-> h3
h3 |-c-> h1

i0 |-a-> i1
i0 |-b-> i1
i0 |-tau-> i2
i2 |-c-> i1

j0 |-a-> j1
j0 |-a-> j2
j1 |-b-> j3
j3 |-c-> j4
j2 |-b-> j3
j2 |-b-> j5
j5 |-d-> j4

k0 |-a-> k1
k1 |-b-> k2
k1 |-b-> k3
k2 |-c-> k4
k3 |-d-> k4

l0 |-a-> l1
l0 |-a-> l2
l1 |-b-> l3
l2 |-b-> l5
l5 |-c-> l4
l3 |-d-> l4

m0 |-a-> m1
m0 |-a-> m2
m1 |-tau-> m3
m2 |-tau-> m4
m3 |-b-> m5
m4 |-c-> m5

n0 |-a-> n1
n1 |-tau-> n2
n1 |-tau-> n3
n2 |-b-> n4
n3 |-c-> n4

q0 |-tau-> q1
q0 |-a-> q2
q1 |-b-> q2

r0 |-tau-> r1
r0 |-a-> r2
r0 |-b-> r2
r1 |-b-> r2

x0 |-tau-> x0
x0 |-a-> x1
x0 |-a-> x2
x1 |-b-> x3
x2 |-b-> x4
x3 |-c-> x5
x4 |-tau-> x4

y0 |-tau-> y0
y0 |-a-> y1
y1 |-b-> y2
y2 |-c-> y3

za0 |-a-> za1
za1 |-tau-> za1
za1 |-tau-> za2
za2 |-b-> za3

zb0 |-a-> zb1
zb1 |-b-> zb2

zc0 |-a-> zc1
zc1 |-b-> zc2
zc1 |-tau-> zc1

zd0 |-a-> zd1
zd1 |-b-> zd2
zd1 |-tau-> zd3
zd3 |-tau-> zd3

ze0 |-a-> ze1
ze1 |-b-> ze2
ze1 |-tau-> ze2

zf0 |-a-> zf1
zf1 |-tau-> zf2
zf1 |-b-> zf3
zf1 |-tau-> zf3
zf2 |-b-> zf3
"""

  val tacas = """
ABC1(x=182, y=115)
ABC2(x=335, y=118)
ABC3(x=547, y=408)
A(x=204, y=408)
B(x=280, y=376)
C(x=347, y=341)
F(x=288, y=519)
BC(x=366, y=250)
AC(x=309, y=227)
AB(x=259, y=189)

A |-acceptA-> F
B |-acceptB-> F
C |-acceptC-> F

ABC2 |-tau-> AC
ABC2 |-tau-> AB
ABC2 |-tau-> BC
AC |-tau-> A
AC |-tau-> C
AB |-tau-> A
AB |-tau-> B
BC |-tau-> B
BC |-tau-> C

ABC1 |-tau-> A
ABC1 |-tau-> B
ABC1 |-tau-> C

ABC3 |-tau-> A
ABC3 |-tau-> B
ABC3 |-tau-> C
ABC3 |-tau-> F
"""

  val namedSamples = List[Example](
    Example("phil",
	    "Ex 2.5f.: S_P Philosophers",
        coupledSimPhilMin),
    Example("phil-religious",
	    "Ex 2.18: P_r Religious philosophers",
       contraSimPhil),
    Example("coupled-sim-s",
	    "Ex 3.4: Coupled Sim SCS",
      coupledSim2),
    Example("cyclic-linear",
	    "Ex 3.31: Cyclic system S_C, and linear system S_L",
       weakStepBlowUp),
    Example("ltbts",
	    "Linear Time Branching Time Spectrum",
      ltbtSpectrum),
    Example("diamond",
	    "Diamond",
      diamond),
    Example("bug1",
	    "Bug 1",
      bug1),
    Example("bug2",
	    "Bug 2 – Coloring",
      bug2),
    Example("abc",
	    "Alphabet",
      abc),
    Example("weak-bisim-1",
	    "Weak Bisim 1",
      weakBisim1),
    Example("weak-bisim-2",
            "Weak Bisim 2",
      weakBisim2),
    Example("weak-bisim-2b",
            "Weak Bisim 2b",
      weakBisim2b),
    Example("weak-bisim-not-ccs",
            "Weak Bisim No CCS Congruency",
      notWeakBisimCCS),
    Example("coupled-sim-1",
	    "Coupled Sim 1",
      coupledSim1),
    Example("coupled-sim-1b",
	    "Coupled Sim 1b",
      coupledSim1b),
    Example("coupled-sim-phil",
	    "Coupled Sim Philosophers",
      coupledSimPhil),
//    Example("coupled-sim-2-div",
//	    "Coupled Sim 2 Divergence",
//      coupledSim2Div),
    Example("contra-sim-1",
	    "Contra Sim 1",
      contraSim1),
    Example("no-contra-sim-1",
      "No Contra Sim 1",
      noContraSim1),
    Example("sim-1",
	    "Sim 1",
      sim1),
    Example("tacas-2019",
	    "TACAS 2019",
      tacas)
  )

  def getExample(slug: String) = {
    namedSamples.find(_.slug == slug)
  }

  val default = diamond
}

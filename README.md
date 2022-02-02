# Model
# Static CGE model of Thailand
$Title Static model Thailand

$Ontext
Sample CGE for mpsge
31 goods, 5 energy, 2 final demand and 2 factors model
$offtext

set
sec      sector and goods        /001*031/
i(sec)   goods                   /001*031/
en(i)    energy goods            /005,006,012,019,020/
ne(i)    non-energy goods        /001*004,007*011,013*018,021*031/
j(sec)   sector                  /001*031/
ej(j)    energy sector           /005,006,012,019,020/
nj(j)    non energy sector       /001*004,007*011,013*018,021*031/
fd       final demand            /h_c,h_i,g_c,g_i,stc,exp,imp,tx_m/
*TS       'tax scenarios'     / T05, T10, T25, T50 /
;

alias(jj,j);
alias(ii,i);
alias(en,en2);
alias(ne,ne2);

parameter
el_lke(j)        elasticity of substitution between labor and energy-capital
el_ke(j)         elasticity of substitution between energy and capital
el_e(j)          elasticity of substitution among energies
el_ex(i)         elasticity of transformation
el_im(i)         elasticity of substitution

kle0(j)          total value added in sector j
ke0(j)           total energy capital composite between energy and capital
ene0(j)          total energy composite in sector j
e0(en,j)         energy input with CO2 in sector j
k0(j)            capital input in sector j
l0(j)            labor input in sector j
x0(i,j)          intermediate input in sector j
em0(en,j)        energy input as material

cmb_r(en,j)      combustion rate of energy

q0(j)            total produced goods
s0(i)            total domestic supply
d0(i)            total supply
o0(i)            total domestic output

c0(i)            final consumption by household
i0(i)            investment (fixed capital formation) by household
cg0(i)           final consumption by government
ig0(i)           investment (fixed capital formation) by government
sc0(i)           stock change
ex0(i)           export
im0(i)           import
utl0             total household consumption
ivh0             total household investment
gvc0             total government consumption
gvi0             total government investment

eh0(en)          energy with CO2 by household
wc0(i)           waste generation from household consumption
wi0(i)           waste generation from household investment

mh0              household income
mg0              government income
g_h              transfer between household and government

fs               foreign saving
pintx(i)         international price of exported goods
pintm(i)         international price of imported goods

ef_co2(en)       co2 emission factor
efa_co2(j)       co2 emission factor
co2e0(en,j)      energy related co2 emission from sector j
co2a0(j)         activity related co2 emission from sector j
co2f0(en)        co2 emission from household

end_l            labor endowment
end_l0           labor endowment fixed
end_k            capital endowment
end_co2          total endowment

tx_m(i)          import tax
tx_o(i)          output tax
tx_k(j)          capital tax
tx_l(j)          labor tax
tx_c             carbon tax

ql0(j)           quantity of labor
qk0(j)           quantity of capital
qim0(i)          quantity of import
qd0(i)           quantity of domestic demand

invh             household investment
invg             government investment
sc(i)            stock change
wc(i)            waste from household consumption
wi(i)            waste from household investment

EMS_A
EMS_EN
EMS_F
EMS              total emissions
*CT(TS) 'tax rates' / T05 0.05, T10 0.10, T25 0.25, T50 0.5 /;

parameter Thai_io(*,*);
$libinclude xlimport Thai_io Thai_2010_31_Sec.xls Thai_Data_2010_My_Work_31_Sec!B2..AR38

parameter use(*,*)       use matrix;
parameter out(*,*)       make matrix;
parameter tax(*,*)       tax;

use(i,j) = Thai_io(i,j)/1000000;
use("cap",j)=(Thai_io("202",j)+Thai_io("203",j))/1000000;
use("lab",j)=Thai_io("201",j)/1000000;

use(i,"h_c")=Thai_io(i,"301")/1000000;
use(i,"h_i")=Thai_io(i,"303")/1000000;
use(i,"g_c")=Thai_io(i,"302")/1000000;
use(i,"g_i")=0;
use(i,"stc")=Thai_io(i,"304")/1000000;
use(i,"exp")=(Thai_io(i,"305")+Thai_io(i,"306"))/1000000;
use(i,"imp")=(Thai_io(i,"401")+Thai_io(i,"404"))/1000000;
use(i,"tx_m")=(Thai_io(i,"402")+Thai_io(i,"403"))/1000000;


out(j,i)      = 0$(ord(i) ne ord(j))
              + ((Thai_io("210",j)-Thai_io("204",j))/1000000)$(ord(i) eq ord(j)) ;


out("h_c",i)= 0;
out("h_i",i)= 0;
out("tax",i)=Thai_io("204",i)/1000000;

tax("tx_o",i)=Thai_io("204",i)/1000000;
tax("tx_k",j) = 0 ;
tax("tx_l",j) = 0 ;

*-------------------------------*
*Emission factor in ton-CO2/baht*
*-------------------------------*

ef_co2("005")  = 1.34 ;
ef_co2("006")  = 0.23 ;
ef_co2("012")  = 0.07 ;
ef_co2("019")  = 0 ;
ef_co2("020")  = 0.23 ;
efa_co2(j)     = 0 ;

*-------------------------------*

el_ke(j)       = 0.7 ;
el_lke(j)      = 0.7 ;
el_e(j)        = 0.1 ;
el_ex(i)       = 2 ;
el_im(i)       = 2 ;

cmb_r(en,j)        = 1 ;
cmb_r("005","005") = 1 ;
cmb_r("005","019") = 1 ;
cmb_r("006","006") = 1 ;
cmb_r("006","011") = 1 ;
cmb_r("006","012") = 0 ;
cmb_r("006","019") = 1 ;
cmb_r("006","020") = 0 ;
cmb_r("012","006") = 1 ;
cmb_r("012","011") = 0.5 ;
cmb_r("012","012") = 1 ;
cmb_r("012","019") = 1 ;
cmb_r("012","020") = 1 ;
cmb_r("020","011") = 1 ;
cmb_r("020","012") = 1 ;
cmb_r("020","019") = 1 ;
cmb_r("020","020") = 1 ;


x0(i,j) = use(i,j);
c0(i)= use(i,"h_c");
i0(i)= use(i,"h_i");
cg0(i)= use(i,"g_c");
ig0(i)= use(i,"g_i");
sc0(i)= use(i,"stc");
im0(i)= -use(i,"imp")-use(i,"tx_m");
ex0(i)= use(i,"exp");
k0(j)= use("cap",j);
l0(j)= use("lab",j);
q0(j)= sum(i,x0(i,j))+k0(j)+l0(j);
s0(i)=sum(j,x0(i,j))+c0(i)+i0(i)+cg0(i)+ig0(i)+sc0(i);
wc0(i)=out("h_c",i);
wi0(i)=out("h_i",i);
o0(i)=sum(j,out(j,i))+wc0(i)+wi0(i);
d0(i)=o0(i)-ex0(i);

utl0=sum(i,c0(i));
ivh0=sum(i,i0(i));
gvc0=sum(i,cg0(i));
gvi0=sum(i,ig0(i));

*---------------*
*Tax calculation*
*---------------*


tx_m(i)$(im0(i))= -use(i,"tx_m")/(im0(i));
tx_o(i)         = tax("tx_o",i)/d0(i);
tx_k(j)         = tax("tx_k",j)/(k0(j)-tax("tx_k",j));
tx_l(j)         = tax("tx_l",j)/(l0(j)-tax("tx_l",j));

*---------------*

Display k0,tx_l, tx_k, tx_o;

ql0(j)          = l0(j)/(1+tx_l(j));
qk0(j)          = k0(j)/(1+tx_k(j));
qim0(i)         = im0(i)/(1+tx_m(i));
qd0(i)          = d0(i)/(1+tx_o(i));

g_h             = sum(i,tx_m(i)*qim0(i))
                 + sum(i, tx_o(i)*d0(i))
                 + sum(j, tx_k(j)*qk0(j)+tx_l(j)*ql0(j) )
                 - sum(i,cg0(i)+ ig0(i));

e0(en,j)        = x0(en,j)*cmb_r(en,j) ;
*e0(en,j)       = x0(en,j);
ene0(j)         = sum(en, e0(en,j));
ke0(j)          = k0(j)+ene0(j);
kle0(j)         = l0(j)+ke0(j);



eh0(en)         = c0(en);
invh            = ivh0;
invg            = gvi0;
sc(i)           = sc0(i);
wc(i)           = wc0(i);
wi(i)           = wi0(i);

*---------------------------*
*carbon emission calculation*
*---------------------------*
CO2e0(en,j)     = ef_co2(en)*e0(en,j) ;
CO2a0(j)        = efa_co2(j)*q0(j) ;
CO2f0(en)       = ef_co2(en)*c0(en) ;
*---------------------------*

end_k(j)        = qk0(j);
end_l           = sum(j, ql0(j)) ;
*end_l0          = sum(j, ql0(j)) ;

*----------------*
*carbon endowment*
*----------------*
end_CO2         = sum(en, sum(j, CO2e0(en,j)) + CO2f0(en)) + sum(j, CO2a0(j)) ;
*----------------*

fs              = sum(i, qim0(i) - ex0(i)) ;
pintx(i)        = 1 ;
pintm(i)        = 1 ;

mh0             = sum(j, k0(j)*(1-tx_k(j)) + l0(j)*(1-tx_l(j))) + fs + sum(i, sc0(i) + wc0(i) + wi0(i)) + g_h ;
mg0             = sum(i, tax("tx_o",i) - use(i,"tx_m")) + sum(j, tax("tx_k",j) + tax("tx_l",j)) - g_h ;

em0(en,j)       = x0(en,j)*(1-cmb_r(en,j)) ;
*tx_c          =1000000;
*cte(en,j)= co2e0(en,j)*0.5;
*cth(en)=  co2f0(en)*0.5;

Display tx_l, tx_k, use, out, CO2e0, CO2a0, CO2f0;


scalars
co2_carb co2 to carbon molecular weight conversion factor
carblim0 benchmark carbon emission rights
carblim carbon emission rights /0/
carbtax carbon tax /0/
carbon_tax carbon tax
exc_rate exchange rate baht to dollar
ra0 benchmark income level of representative agent
;

carblim0=end_CO2;

*------------*
* core model *
*------------*
$ontext
$MODEL:CGE
$PEPS:0

$SECTORS:
ACTS_OUT(j)$(q0(j))         !Activity
ACTS_IN(j)$(q0(j))          !Activity
ACTS_KLE(j)$(kle0(j))       !Activity
ACTS_KE(j)$(ke0(j))          !Activity
ACTS_ENE(j)$(ene0(j))       !Activity
ACTS_E(en,j)$(e0(en,j))     !Activity
COH$utl0                    !household consumption
COH_E(en)$eh0(en)        !household energy
IVH$ivh0                 !household investment
COG$gvc0                 !government investment
IVG$gvi0                 !government investment

export(i)           !distribution between export and domestic
import(i)           !household investment

pr_ex(i)$ex0(i)     !export
pr_im(i)$im0(i)     !import

$COMMODITIES:
PO(i)            !price of commodity in sector j

PSQ(j)$q0(j)           !
PSKLE(j)$kle0(j)         !
PSKE(j)$ke0(j)          !
PSENE(j)$ene0(j)         !
PSE(en,j)$e0(en,j)        !

PEH(en)$eh0(en)  !

PK(j)            !rent (price of existing capital)
PL               !wage (price of labor)
PCO2$carblim     !price of CO2

PU$utl0          !price of aggregated household consumption
PIVH$ivh0        !price of aggregated household investment
PGC$gvc0         !price of aggregated government consumption
PGI$gvi0         !price of aggregated government investment

PD(i)            !price of produced domestic goods i
PS(i)            !price of supplied goods i
PEX(i)$(ex0(i))  !price of exported goods i
PIM(i)$(im0(i))  !price of imported goods i
EXR              !exchange rate


$CONSUMERS:
MH               !income
GOV              !government

$auxiliary:
ctax$carbtax     !tax on aggregate carbon emissions              !


$PROD:ACTS_OUT(j)$(q0(j) )         t:0
o:PO(i)          q:out(j,i)      p:1
i:PSQ(j)         q:q0(j)         p:1

$PROD:ACTS_IN(j)$(q0(j) )          s:0
o:PSQ(j)         q:q0(j)         p:1
i:PS(ne)         q:x0(ne,j)      p:1
i:PSKLE(j)       q:kle0(j)       p:1
i:PCO2           q:CO2a0(j)      p:0
i:PS(en)         q:em0(en,j)     p:1


$PROD:ACTS_KLE(j)$(kle0(j) )         s:el_lke(j)
o:PSKLE(j)        q:kle0(j)       p:1
i:PL              q:ql0(j)        p:(1+tx_l(j))   A:GOV   T:tx_l(j)
i:PSKE(j)         q:ke0(j)        p:1

$PROD:ACTS_KE(j)$(ke0(j) )          s:el_ke(j)      a:0
o:PSKE(j)        q:ke0(j)        p:1
i:PSENE(j)       q:ene0(j)       p:1
i:PK(j)          q:qk0(j)        p:(1+tx_k(j))   A:GOV   T:tx_k(j)   a:

$PROD:ACTS_ENE(j)$(ene0(j) )         s:el_e(j)
o:PSENE(j)       q:ene0(j)      p:1
i:PSE(en,j)      q:e0(en,j)     p:1

$PROD:ACTS_E(en,j)$(e0(en,j) )       s:0
o:PSE(en,j)      q:e0(en,j)       p:1
i:PS(en)         q:e0(en,j)       p:1
i:PCO2$carblim   q:(co2e0(en,j))  p:1


$PROD:export(i)  t:el_ex(i)
o:PEX(i)         q:ex0(i)        p:1
o:PD(i)          q:d0(i)         p:1
i:PO(i)          q:o0(i)         p:1

$PROD:import(i)  s:el_im(i)
o:PS(i)          q:s0(i)         p:1
i:PIM(i)         q:qim0(i)       p:(1+tx_m(i))   A:GOV   T:tx_m(i)
i:PD(i)          q:d0(i)         p:(1+tx_o(i))   A:GOV   T:tx_o(i)

$PROD:pr_ex(i)$ex0(i)
o:EXR            q:(ex0(i)*pintx(i))
i:PEX(i)         q:(ex0(i))

$PROD:pr_im(i)$im0(i)
o:PIM(i)         q:(qim0(i))
i:EXR            q:(qim0(i)*pintm(i))

$prod:COH$utl0   s:0    a:1  b:1
O:PU$utl0        q:utl0          p:1
i:PS(ne)         q:c0(ne)        p:1      a:
I:PEH(en)        q:eh0(en)       p:1      b:

$prod:COH_E(en)$eh0(en)  s:0
O:PEH(en)        q:eh0(en)       p:1
I:PS(en)         q:c0(en)        p:1
I:PCO2$carblim   q:co2f0(en)     p:1

$prod:IVH$ivh0   s:0
O:PIVH$ivh0      q:ivh0          p:1
I:PS(ne)         q:i0(ne)        p:1

$prod:COG$gvc0   s:0
O:PGC$gvc0       q:gvc0          p:1
I:PS(i)          q:cg0(i)        p:1

$prod:IVG$gvi0   s:0
O:PGI$gvi0       q:gvi0          p:1
I:PS(i)          q:ig0(i)        p:1

$demand:MH
D:PU             q:utl0
E:PIVH           q:(-invh)
E:PK(j)$end_k(j) q:end_k(j)
E:PL             q:end_l
E:EXR            q:(fs)
E:PS(i)          q:(-sc(i))
E:PO(i)          q:wc(i)
E:PO(i)          q:wi(i)
E:PL             q:g_h

$demand:GOV      s:0
D:PGC            q:gvc0
D:PGI            q:(gvi0)
E:PCO2$carblim   q:carblim       r:ctax$carbtax
E:PL             q:(-g_h)

$constraint:ctax$carbtax
pco2=e=carbtax;

$report:
V:Q(j)           O:PSQ(j)        PROD:ACTS_IN(j)
V:XN(i,j)        I:PS(i)         PROD:ACTS_IN(j)
V:K(j)           I:PK(j)         PROD:ACTS_KE(j)
V:L(j)           I:PL            PROD:ACTS_KLE(j)
V:CO2a(j)        I:PCO2          PROD:ACTS_IN(j)
V:XE(en,j)       I:PSE(en,j)     PROD:ACTS_ENE(j)
V:CO2e(en,j)     I:PCO2          PROD:ACTS_E(en,j)
V:CO2f(en)       I:PCO2          PROD:COH_E(en)
V:QCN(ne)         i:PS(ne)      PROD:COH
V:QCE(en)         i:PS(en)      PROD:COH_E(en)

$offtext
$SYSINCLUDE MPSGESET CGE

*-----------------*
*End of core model*
*-----------------*


*---------------------*
*Benchmark replication*
*---------------------*
ctax.l   = 0;
carblim=carblim0;
*---------------------*
$INCLUDE CGE.GEN
SOLVE CGE USING MCP;



* now suppress listing to save memory
$offlisting
$offsymxref offsymlist
options
limrow = 0
limcol = 0
solprint = off
sysout = off
;



*---------------*
*Policy analysis*
*---------------*

sets
         iter    iteration over level of carbon constraint/iter1 * iter20/
;

parameters
results, LAB_S, CO2_E, CO2_H, CO2_E0, CO2_H0 ,C_H, C_G, I_H, I_G, EXP, IMP, TXM, STC, GDP_carbtax, GDP_original, EMS0;

loop(iter,
* perform benchmark solve first before computing distorted equilibrium
         carblim = carblim0;
         carbtax = 0;

$INCLUDE CGE.GEN
SOLVE CGE USING MCP;

*policy solves with carbon taxes at $5/ton increments

*-----------------------------*
*insert carbon tax in $/tonCO2*
*-----------------------------*
         carbon_tax =  25 * (ord(iter)-1);
*-----------------------------*

*exchange rate baht to one dollar*
         exc_rate = 30;
*--------------------------------*
         carbtax = carbon_tax * exc_rate/ 1000;
         carblim=carblim0$(carbtax=0);
         carblim$carbtax = 1;

$INCLUDE CGE.GEN
SOLVE CGE USING MCP;

EMS_A= sum(j,CO2a.l(j));
EMS_EN= sum((en,j),CO2e.l(en,j));
EMS_F= sum(en,CO2f.l(en));
EMS= EMS_A+EMS_EN+EMS_F;

LAB_S   = sum(j, L.l(j));
CO2_E   = sum((en,j), CO2e.l(en,j));
CO2_H   = sum(en, CO2f.l(en));
CO2_E0   = sum((en,j), CO2e0(en,j));
CO2_H0   = sum(en, CO2f0(en));
EMS0     = CO2_E0+CO2_H0;
C_H     = sum(ne, QCN.l(ne))+ sum(en, QCE.l(en))   ;
C_G     = sum(i,  COG.l*cg0(i)) ;
I_H     = sum(ne, IVH.l*i0(ne)) ;
I_G     = sum(i,  IVG.l*ig0(i)) ;
EXP     = sum(i,  pr_ex.l(i)*ex0(i)) ;
IMP     = sum(i,  pr_im.l(i)*qim0(i)) ;
TXM     = sum(i,  pr_im.l(i)*qim0(i)*tx_m(i)) ;
STC     = sum(i,  sc(i)) ;


GDP_carbtax = C_H + C_G + I_H + I_G + EXP - IMP - TXM + STC ;
GDP_original = sum(i, c0(i) + cg0(i) + i0(i) + ig0(i) + sc(i) + ex0(i) -im0(i)) ;

results(iter, "carbon tax ($/tCO2)")     = carbon_tax;
results(iter, "carbon tax (baht/tCO2)")  = carbtax*1000;
results(iter, "energy emissions")        = CO2_E;
results(iter, "household emissions")     = CO2_H;
results(iter, "Total emissions")         = EMS ;
results(iter, "emissions % change")      = (EMS0-EMS)/EMS0*100;
results(iter, "labor supply")            = LAB_S;
results(iter, "GDP")                     = GDP_carbtax;
results(iter, "GDP % change")            = (GDP_original-GDP_carbtax)/GDP_original*100;


);

Display results;

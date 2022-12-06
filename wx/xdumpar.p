/*xdumpar.p*/
/*rensa XINLADD.P OCH XUTLADD.P skapa ny prog XINLADD.P via XLOADallt_D.P xin.i 
skapa ny prog XUTLADD.P via XDUMPALLT_D.P xUT.i
*/
DEF VARIABLE kommando AS CHARACTER.
kommando = "-db sundn9 -S 2516 -H 194.132.143.8 -N TCP -U ELPAO -P 'KAGGEN'".
CONNECT VALUE(kommando) NO-ERROR.                
{LDALIAS8.I}
RUN XUTLADD.P. 
DISCONNECT VALUE(LDBNAME(1)).
kommando = "-db tom -S 2538 -H 194.132.143.8 -N TCP -U ELPAO -P 'KAGGEN'".
CONNECT VALUE(kommando) NO-ERROR.                
{VERALIAS.I}
RUN XINLADD.P.



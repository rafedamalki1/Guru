/*XAPPEL0.P*/
{LDALIAS8.I}
CREATE SERVER Guru.Konstanter:apphand.
appcon = Guru.Konstanter:apphand:CONNECT("-S appelpool -H Ntserver1 -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
   
IF NOT Guru.Konstanter:appcon THEN MESSAGE "DU FICK NU EN MASSA FEL MEDDELANDEN.  
   DESSA MEDDELANDEN INNEB�R ATT DU KOMMER INTE ATT K�RA GURU MED H�GSTA FART, 
   MEN ALLT FUNGERAR �ND�. KONTAKTA SYSTEM ANSVARIG." VIEW-AS ALERT-BOX. 
ELSE MESSAGE "OK" VIEW-AS ALERT-BOX. 
RUN WSTART.W.
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand.

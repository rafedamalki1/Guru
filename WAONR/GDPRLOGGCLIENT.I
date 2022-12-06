
/*------------------------------------------------------------------------
    File        : GDPRLOGGCLIENT.I
    Purpose     : 
                 Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + pkod.
Guru.GlobalaVariabler:GDPRtyp = "EP".  EPOST                
Guru.GlobalaVariabler:GDPRtyp = "GPL". GPL 
Guru.GlobalaVariabler:GDPRtyp = "DP". DARWINPLUS
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + STRING(MARKAGARE.MARKNR).
   Guru.GlobalaVariabler:GDPRtyp = "M". 
   {GDPRLOGGCLIENT.I}
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   {GDPRLOGGCLIENT.I}
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.
{GDPRLOGGCLIENT.I}
   IF AVAILABLE eposttemp THEN Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + eposttemp.EPOST + "," +  eposttemp.MEDD .
   Guru.GlobalaVariabler:GDPRtyp = "EP". 
   {GDPRLOGGCLIENT.I}
       Description :  .\Guru\Konstanter:GDPRRESET(pkod).
                 eller
    Author(s)   : Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + pkod.
    Created     : Mon Jan 15 13:21:12 CET 2018
    Notes       :
   FOR EACH mval WHERE NO-LOCK:
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + STRING(mval.MARKNR).
   END.   
   Guru.GlobalaVariabler:GDPRtyp = "M". 
   {GDPRLOGGCLIENT.I}    
       
       
  ----------------------------------------------------------------------*/
RUN GDPRLOGGCLIENT.P (THIS-PROCEDURE:NAME).


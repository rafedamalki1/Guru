
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

CREATE SERVER Guru.Konstanter:apphand.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "GSOL" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appguru -H guru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
ELSE IF FORETAG.FORETAG = "NORD" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appguru -H nordkraft -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN",""). 
END.
ELSE IF FORETAG.FORETAG = "GRAN" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appgran -H granguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN",""). 
END.
ELSE IF FORETAG.FORETAG = "GADM" THEN DO:
   IF LDBNAME(1) = "UTBI" THEN DO:
      Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S apputbi -H granguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
   END. 
   ELSE 
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appadm -H granguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
ELSE IF FORETAG.FORETAG = "GSYD" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appgsyd -H granguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
ELSE IF FORETAG.FORETAG = "GRIT" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appgrit -H granguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
ELSE IF FORETAG.FORETAG = "GREN" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appgren -H granguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
ELSE IF FORETAG.FORETAG = "SUND" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appsund -H beredning1 -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
ELSE IF FORETAG.FORETAG = "ELPA" THEN DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appelpool -H Ntserver1 -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
END.
IF NOT Guru.Konstanter:appcon THEN DO:
   MESSAGE 
   "DU FICK NU EN MASSA FEL MEDDELANDEN." SKIP
   "DESSA MEDDELANDEN INNEBÄR ATT DU KOMMER INTE ATT KÖRA GURU MED HÖGSTA FART," SKIP 
   "MEN ALLT FUNGERAR ÄNDÅ. KONTAKTA SYSTEM ANSVARIG." VIEW-AS ALERT-BOX. 
END.
IF Guru.Konstanter:appcon THEN DO:                           
   RUN XTIDF.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
END.
ELSE DO:
   RUN XTIDF.P. 
END.
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.

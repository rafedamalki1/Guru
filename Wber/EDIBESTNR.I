/* EDIBESTNR.I*/
/*IF nyttnr = 0 THEN  DO:
   nyttnr = levtrp2temp.BESTNR.
END.*/

IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "GKAL" {GLOBVES.I} THEN DO:
   FILL-IN-BESTNR = ediben + STRING(beredningtemp.AONR) + "." + STRING(beredningtemp.DELNR) + "." + STRING(nyttnr).
END.
ELSE IF Guru.Konstanter:globforetag = "BORL" THEN DO:
   FILL-IN-BESTNR = ediben + STRING(beredningtemp.AONR) + "." + STRING(beredningtemp.DELNR) + "-" + STRING(nyttnr).
END.
ELSE IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
   FILL-IN-BESTNR = ediben + STRING(beredningtemp.AONR) + STRING(beredningtemp.DELNR,"99") + "-" + STRING(nyttnr).
END.
ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
   FILL-IN-BESTNR = ediben + STRING(beredningtemp.AONR) + "." + STRING(beredningtemp.DELNR) + "-" + STRING(nyttnr).
END.
ELSE DO:
   FILL-IN-BESTNR = STRING(beredningtemp.AONR) + "." + STRING(beredningtemp.DELNR,Guru.Konstanter:varforetypchar[1]) + "-" + STRING(levtrp2temp.BESTNR).   
END.



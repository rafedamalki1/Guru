/*PLANTIDHMT.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{ANMARKD.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
{TIDUTTTNEW.I}
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

DEFINE VARIABLE tempint AS INTEGER NO-UNDO.
DEFINE VARIABLE tempstr AS CHARACTER NO-UNDO.
DEFINE VARIABLE slutvecko AS INTEGER NO-UNDO.
DEFINE VARIABLE startvecko AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO. 

IF Guru.Konstanter:varforetypval[8] NE 0 THEN DO: 
   tempint = 1.
   DO WHILE tempint LE Guru.Konstanter:varforetypval[8] + 2:
      ASSIGN
      tempstr = tempstr + "="
      tempint = tempint + 1.
   END.
END.
ELSE DO: 
   tempstr = "========".
   tempint = 9.
END.

str="===================================================================================".
FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND
PLANNRTAB.ARTAL = artalvar NO-LOCK NO-ERROR.
IF AVAILABLE PLANNRTAB THEN DO:
   CREATE tidut.
   SUBSTRING(tidut.UT,60) = STRING(TODAY).
   IF PLANNRTAB.FASTAPLANNR = FALSE THEN DO:
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gutfk) + " " + CAPS(Guru.Konstanter:gomrk) + ":".
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PLANNRTAB.OMRADE
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE OMRADETAB THEN DO:
      SUBSTRING(tidut.UT,22) = OMRADETAB.NAMN.
   END.
   END.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gplk) + "          :".
   SUBSTRING(tidut.UT,22) = PLANNRTAB.PLANNR.
   SUBSTRING(tidut.UT,30) = "ÅRTAL:".
   SUBSTRING(tidut.UT,37) = STRING(PLANNRTAB.ARTAL).
   IF PLANNRTAB.PLANNRAVDATUM NE ? THEN DO:
      IF PLANNRTAB.PLANNRAVDATUM NE 01/01/91 THEN DO:
         SUBSTRING(tidut.UT,44) = "AVSLUTAT:".
         SUBSTRING(tidut.UT,54) = STRING(PLANNRTAB.PLANNRAVDATUM).
      END.   
   END.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gaonamnk) + "  :".
   SUBSTRING(tidut.UT,22) = PLANNRTAB.ORT.
   CREATE tidut.
   ASSIGN tidut.UT = str.
   IF PLANNRTAB.FASTAPLANNR = FALSE THEN DO:
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gbestk)
   SUBSTRING(tidut.UT,20) = ":".
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PLANNRTAB.BESTID
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE OMRADETAB THEN DO:
      SUBSTRING(tidut.UT,22) = OMRADETAB.NAMN.
   END.
   ELSE DO:
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = PLANNRTAB.BESTID
      USE-INDEX BEST NO-LOCK NO-ERROR.
      IF AVAILABLE BESTTAB THEN DO:
         SUBSTRING(tidut.UT,30) = BESTTAB.BESTNAMN.
      END.
   END.
   END.
   regdatum = DATE(STRING(PLANNRTAB.SLUTVNR)).
   RUN REGDAG.P.
   RUN REGVEC.P.
   slutvecko = regvnr.
   regdatum = DATE(STRING(PLANNRTAB.STARTVNR)).
   RUN REGDAG.P.
   RUN REGVEC.P.
   startvecko = regvnr.
   
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = STRING(CAPS(Guru.Konstanter:gdebk),"XXXXXXXXXX") + "      :".
   SUBSTRING(tidut.UT,22) = PLANNRTAB.PRISTYP.
   CREATE tidut.
   ASSIGN tidut.UT = str.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = "ANLÄGGNINGSNR   :".
   SUBSTRING(tidut.UT,22) = STRING(PLANNRTAB.ANLNR, "x(15)").
   SUBSTRING(tidut.UT,44) = "TRAKT.ZON:".
   SUBSTRING(tidut.UT,61) = STRING(PLANNRTAB.TRAKTAMENTE).
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gartk) + ":".
   SUBSTRING(tidut.UT,22) = STRING(PLANNRTAB.ARBARTKOD).
   SUBSTRING(tidut.UT,44) = CAPS(Guru.Konstanter:gpriok) + ":".
   SUBSTRING(tidut.UT,61) = STRING(PLANNRTAB.PKOD).
   CREATE tidut.
   ASSIGN tidut.UT = str.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = "START VECKA     :".
   SUBSTRING(tidut.UT,22) = STRING(startvecko).
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = "SLUT VECKA      :".
   SUBSTRING(tidut.UT,22) = STRING(slutvecko).
   CREATE tidut.
   ASSIGN tidut.UT = str.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gberek).
   SUBSTRING(tidut.UT,20) = ":".
   SUBSTRING(tidut.UT,22) = PLANNRTAB.BEREDARE.
   SUBSTRING(tidut.UT,44) = CAPS(Guru.Konstanter:garbak).
   SUBSTRING(tidut.UT,59) = ":".
   SUBSTRING(tidut.UT,61) = PLANNRTAB.ARBANSVARIG.
   CREATE tidut.
   ASSIGN tidut.UT = str.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = "ANMÄRKNING      :".
   IF PLANNRTAB.ANM NE "" THEN DO:
   RUN anm_UI.
   END.
   CREATE tidut.
   ASSIGN tidut.UT = str.
   FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
   CREATE tidut.
   SUBSTRING(tidut.UT,4) = "KONTOSTRÄNG".
   SUBSTRING(tidut.UT,22) = KBENAMNING.K1.
   SUBSTRING(tidut.UT,29) = KBENAMNING.K2.
   SUBSTRING(tidut.UT,36) = KBENAMNING.K3.
   SUBSTRING(tidut.UT,43) = KBENAMNING.K4.
   SUBSTRING(tidut.UT,50) = KBENAMNING.K5.
   SUBSTRING(tidut.UT,57) = "PROCENT".
   CREATE tidut.
   ASSIGN tidut.UT = "====================.======.======.======.======.======.=======.===================".
   FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND
   PLANKONTO.ARTAL = PLANNRTAB.ARTAL USE-INDEX PLANKONT NO-LOCK:
   CREATE tidut.
   SUBSTRING(tidut.UT,22) = PLANKONTO.K1.
   SUBSTRING(tidut.UT,29) = PLANKONTO.K2.
   SUBSTRING(tidut.UT,36) = PLANKONTO.K3.
   SUBSTRING(tidut.UT,43) = PLANKONTO.K4.
   SUBSTRING(tidut.UT,50) = PLANKONTO.K5.
   SUBSTRING(tidut.UT,57) = STRING(PLANKONTO.SATS%).
   END.
   CREATE tidut.
   ASSIGN tidut.UT = str.
   CREATE tidut.
   CREATE tidut.
   ASSIGN tidut.UT = "============================.=======.===.=========================================".
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,4) = "KALK. KOPPLADE TILL " +  CAPS(Guru.Konstanter:gplk)
   SUBSTRING(tidut.UT,30) = "KALKNR"
   SUBSTRING(tidut.UT,38) = "TYP"
   SUBSTRING(tidut.UT,41) = "BENÄMNING".
   CREATE tidut.
   ASSIGN tidut.UT = "============================.=======.===.=========================================".
   OPEN QUERY kalkq FOR EACH KALKAONR WHERE KALKAONR.PLANNR = PLANNRTAB.PLANNR AND
   KALKAONR.ARTAL = PLANNRTAB.ARTAL USE-INDEX AONR NO-LOCK.
   GET FIRST kalkq NO-LOCK.
   DO WHILE AVAILABLE(KALKAONR):
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KALKAONR.KALKNR AND KALKHUV.OMRADE = KALKAONR.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,30) = STRING(KALKHUV.KALKNR,">>>>>>9")
         SUBSTRING(tidut.UT,38) = STRING(KALKHUV.TYPKALK)
         SUBSTRING(tidut.UT,41) = SUBSTRING(KALKHUV.BENAMNING,1,30).
      END.
      GET NEXT kalkq NO-LOCK.
   END.
   
   CREATE tidut.
   ASSIGN tidut.UT = str.
   IF PLANNRTAB.KOPPAO = TRUE THEN DO:
   CREATE tidut.
   CREATE tidut.
   ASSIGN tidut.UT = "============================." + tempstr + ".=====.=========================================".
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,4) = CAPS(Guru.Konstanter:gaok) + " KOPPLADE TILL " + CAPS(Guru.Konstanter:gplk)
   SUBSTRING(tidut.UT,30) = CAPS(Guru.Konstanter:gaok)
   SUBSTRING(tidut.UT,(30 + tempint)) = CAPS(Guru.Konstanter:gdelnrk) 
   SUBSTRING(tidut.UT,(30 + tempint + 7)) = "ORT / BENÄMNING".
   CREATE tidut.
   ASSIGN tidut.UT = "============================." + tempstr + ".=====.=========================================".
   OPEN QUERY aoq FOR EACH AONRTAB WHERE
   AONRTAB.PLANNR = PLANNRTAB.PLANNR AND
   AONRTAB.ARTAL = PLANNRTAB.ARTAL NO-LOCK.
   GET FIRST aoq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,30) = AONRTAB.AONR
      SUBSTRING(tidut.UT,(30 + tempint)) = STRING(AONRTAB.DELNR,Guru.Konstanter:varforetypchar[1])
      SUBSTRING(tidut.UT,(30 + tempint + 7)) = SUBSTRING(AONRTAB.ORT,1,30).
      GET NEXT aoq NO-LOCK.
   END.
   CLOSE QUERY aoq.
   END.
END.
   
PROCEDURE anm_UI :
   ASSIGN
   retvar = 1
   ednum = 1
   ednum3 = LENGTH(PLANNRTAB.ANM)
   retvar = INDEX(PLANNRTAB.ANM,CHR(10),ednum)
   edtecken = 44
   edtext = PLANNRTAB.ANM
   tidtext = "".
   {ANMARK2.I}
END PROCEDURE.

PROCEDURE anmark_UI :        
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.
   IF anmark = 1 THEN DO:                        
      ASSIGN            
      SUBSTRING(tidut.UT,17) = ":"   
      SUBSTRING(tidut.UT,18) = SUBSTRING(edtext,ednum,edtecken).
      CREATE tidut.
   END.  
   ELSE IF anmark = 2 THEN DO:                
      ASSIGN            
      SUBSTRING(tidut.UT,17) = ":"   
      SUBSTRING(tidut.UT,18) = tidtext.
      CREATE tidut.
   END.   
   ELSE IF anmark = 3 THEN DO:           
      ASSIGN           
      SUBSTRING(tidut.UT,17) = ":"    
      SUBSTRING(tidut.UT,18) = SUBSTRING(edtext,1 + ednum2 * edtecken,edtecken).
      CREATE tidut.
   END.
END PROCEDURE.

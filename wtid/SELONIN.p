/*SELONIN.P*/

DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE mspar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bstrang AS CHARACTER NO-UNDO.
DEFINE VARIABLE bant AS DECIMAL NO-UNDO.
DEFINE VARIABLE pnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE akod AS CHARACTER NO-UNDO.
DEFINE VARIABLE tavt AS CHARACTER NO-UNDO.
DEFINE VARIABLE spdat AS CHARACTER NO-UNDO.
{TIDUTTT.I}
DEFINE INPUT PARAMETER ifil AS CHARACTER.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
/*IF Guru.Konstanter:globforetag = "elpa" THEN DO: 
   str = "/PRO9/guru/apptemp/".   
END.*/

{AMERICANEUROPEAN.I}
INPUT FROM VALUE(ifil) NO-ECHO.  
REPEAT:
  CREATE tidut.
  ASSIGN.
  IMPORT UNFORMATTED tidut.     
END.
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa"  THEN str = str.
ELSE DO:   
   FOR EACH tidut:
     IF tidut.UT = "" THEN DELETE tidut.
   END.
END.
/* så här kördes det hos sundsvall tom 20051005*/
IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "CElpa"  THEN DO:
   akod = "".
   FOR EACH tidut:
      IF SUBSTRING(tidut.UT,8,1) = "-"  THEN DO:
         pnr = SUBSTRING(tidut.UT,2,6) + SUBSTRING(tidut.UT,9,4).
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = pnr NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.
            IF AVAILABLE ANSTFORMTAB THEN akod = ANSTFORMTAB.KOD.
            tavt = PERSONALTAB.TRAAVTAL.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
         END.
      END.
      IF SUBSTRING(tidut.UT,24,1) = "/" THEN DO:
         mspar = SUBSTRING(tidut.UT,20,52).
         SUBSTRING(tidut.UT,40,52)= SUBSTRING(mspar,1,52).
         SUBSTRING(tidut.UT,20,20)= "                    ".
         IF akod NE ""  THEN DO:
            FIND FIRST LONTILL WHERE LONTILL.KOD = akod AND LONTILL.VILART = SUBSTRING(tidut.UT,2,4) NO-LOCK NO-ERROR.
         END.
         ELSE DO:         
            FIND FIRST LONTILL WHERE LONTILL.VILART = SUBSTRING(tidut.UT,2,4) NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE LONTILL THEN DO:
            IF LONTILL.LONTILLAGG = "0116" THEN DO:
               SUBSTRING(tidut.UT,20,8) = SUBSTRING(tidut.UT,8,8).
               SUBSTRING(tidut.UT,8,8) = "        ".
            END.
            ELSE IF LONTILL.ERSATTNING > 0 THEN DO:
               SUBSTRING(tidut.UT,20,8) = STRING(LONTILL.ERSATTNING,">>>>9.99").
            END.
            ELSE IF LONTILL.ENHET = "KR" THEN DO:
               SUBSTRING(tidut.UT,30,8) = SUBSTRING(tidut.UT,8,8).
               SUBSTRING(tidut.UT,8,8) = "        ".
            END.
         END.
         ELSE DO:
            IF tavt NE ""  THEN DO:
               FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = tavt AND TRAKTATAB.VILART = SUBSTRING(tidut.UT,2,4) NO-LOCK NO-ERROR.
            END.
            ELSE DO:         
               FIND FIRST TRAKTATAB WHERE TRAKTATAB.VILART = SUBSTRING(tidut.UT,2,4) NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE TRAKTATAB THEN DO:
               IF TRAKTATAB.ERSATTNING > 0 THEN DO:               
                  SUBSTRING(tidut.UT,20,8) = STRING(TRAKTATAB.ERSATTNING,">>>>9.99").
               END.
            END.
         END.         
      END.
   END.
END.
{EUROPEANAMERICAN.I}
{GDPRLOGGCLIENT.I}

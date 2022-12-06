/*KUNDUTLUL.P FRÅN GURU TILL FIL*/
{NAMNDB.I}
{TIDUTTT.I}
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE STREAM eko.
DEFINE STREAM ekospar.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
IF namndb() = "UTBI" THEN RETURN.

ASSIGN
nrcol[1] = 1
nrcol[2] = 2
nrcol[3] = 3
nrcol[4] = 4
nrcol[5] = 5
nrcol[6] = 6

 
breddantal = 6   
bredd[1] = 11
bredd[2] = 30
bredd[3] = 35
bredd[4] = 35
bredd[5] = 35
bredd[6] = 30.

ASSIGN
i = 2.     
utnr[nrcol[1]] = 1.
DO WHILE i <= breddantal:             
   utnr[i] = utnr[i - 1] + bredd[i - 1].            
   i = i + 1.
END.


OPEN QUERY aoq FOR EACH BESTTAB /*WHERE BESTTAB.KUNDPRISF = 1*/ NO-LOCK.
GET FIRST aoq NO-LOCK.    
DO WHILE AVAILABLE(BESTTAB):
   DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK.    
      ASSIGN BESTTAB.KUNDPRISF = 0.      
      RUN aonrut_UI.
   END.   
   GET NEXT aoq NO-LOCK.     
END.
PROCEDURE aonrut_UI:      
   FOR EACH JURPERS NO-LOCK:
      EMPTY TEMP-TABLE tidut NO-ERROR. 
      IF FORETAG.FORETAG = "ELPA" THEN DO:                                  
         prognamn = CAPS("d:\DELAD\PRO9s\EXPORT\kundguru" + STRING(TODAY,"99999999") + ".txt"). 
         prognamnk = CAPS("d:\DELAD\PRO9s\EXKOPIA\kundguru.txt"). 
      END.
      IF FORETAG.FORETAG = "LULE" THEN DO:                                  
         prognamn = CAPS("D:\ELPOOL\DELAD\PRO9S\EXPORT\kundguru" + JURPERS.JUDID + STRING(TODAY,"99999999") + ".txt"). 
         prognamnk = CAPS("D:\ELPOOL\DELAD\PRO9S\EXKOPIA\kundguru" + JURPERS.JUDID + ".txt"). 
      END.
      CREATE tidut.   
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = BESTTAB.VIBESTID
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = BESTTAB.BESTNAMN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = BESTTAB.ADRESS
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = BESTTAB.FAKADRESS 
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(BESTTAB.PNR,"999 99") + " " + BESTTAB.ORT 
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "".                
      OUTPUT STREAM eko TO VALUE(prognamn) APPEND.
      OUTPUT STREAM ekospar TO VALUE(prognamnk) APPEND.
      FOR EACH tidut:
         PUT STREAM eko UNFORMATTED
         tidut.UT AT 1 SKIP.
         PUT STREAM ekospar UNFORMATTED
         tidut.UT AT 1 SKIP.
      END.
      OUTPUT STREAM eko CLOSE. 
      OUTPUT STREAM ekospar CLOSE.
   END.
END PROCEDURE.

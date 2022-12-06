/*AONRUT.P AONUMMER FRÅN GURU TILL FIL*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{TIDUTTT.I}
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT FORETAG.FORETAG).
{AMERICANEUROPEAN.I}
IF FORETAG.FORETAG = "ELPA" THEN DO: 
   prognamn = "\\pc112\DELAD\PRO9\EXPORT\aout" + STRING(TODAY,"999999") + ".d". 
   prognamnk = "\\pc112\DELAD\PRO9\EXKOPIA\aout" + STRING(TODAY,"999999") + ".d". 
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
    /*ny kolumn*/
   breddantal = 8   /*antal kolumner*/
   bredd[1] = 6
   bredd[2] = 3
   bredd[3] = 4
   bredd[4] = 4
   bredd[5] = 4
   bredd[6] = 4
   bredd[7] = 4
   bredd[8] = 8.
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.
END.

OPEN QUERY aoq FOR EACH AONRTAB WHERE AONRTAB.AUTOREG = TRUE 
USE-INDEX AONR NO-LOCK.
GET FIRST aoq NO-LOCK.    
DO WHILE AVAILABLE(AONRTAB):
   DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK.    
      ASSIGN AONRTAB.AUTOREG = FALSE.
      RUN aonrut_UI.
   END.
   GET NEXT aoq NO-LOCK.     
END.
OS-COPY VALUE(prognamn) VALUE(prognamnk).  
{EUROPEANAMERICAN.I}
PROCEDURE aonrut_UI:      
   IF FORETAG.FORETAG = "GRAN" OR FORETAG.FORETAG = "GKAL" THEN DO:
      IF AONRTAB.DELNR > 0 THEN RETURN.
   END.
   IF AONRTAB.PRISTYP = "FRÅNVARO." THEN RETURN.
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN do :
      avdatum = ?.
   END.
   ELSE DO:
      avdatum = AONRTAB.AONRAVDATUM.      
   END.
   OPEN QUERY akq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = AONRTAB.AONR  AND 
   AONRKONTKOD.DELNR = AONRTAB.DELNR NO-LOCK. 
   GET FIRST akq NO-LOCK.
   DO WHILE AVAILABLE(AONRKONTKOD):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = AONRTAB.AONR
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(AONRTAB.DELNR,Guru.Konstanter:varforetypchar[1])
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = AONRKONTKOD.K1 
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = AONRKONTKOD.K2
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = AONRKONTKOD.K3
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = AONRKONTKOD.K4    
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = AONRKONTKOD.K5.
      IF avdatum NE ? THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(AONRTAB.AONRAVDATUM,"99999999").
      GET NEXT akq NO-LOCK.
   END.                  
   OUTPUT TO VALUE(prognamn).
   FOR EACH tidut:
      PUT UNFORMATTED tidut.UT AT 1 CHR(13).   
   END.
   OUTPUT CLOSE.
END PROCEDURE.

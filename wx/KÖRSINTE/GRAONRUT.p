/*GRAONRUT.P AONUMMER FRÅN GURU TILL FIL*/
{TIDUTTT.I}
DEFINE TEMP-TABLE tidutK2
   FIELD UT AS CHARACTER FORMAT "X(132)".
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE VARIABLE breddantalK2 AS INTEGER NO-UNDO.
DEFINE VARIABLE utnrK2 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE breddK2 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolK2 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE aofil AS CHARACTER NO-UNDO.
DEFINE VARIABLE kontfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE filut AS CHARACTER NO-UNDO.
DEFINE VARIABLE filutkopia AS CHARACTER NO-UNDO.
DEFINE VARIABLE formappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG. 
IF globforetag = "ELPA" THEN DO: 
   aofil = "Ek4.txt".  
   kontfil = "EKtokompl.txt".
   prognamn = "\\pc112\DELAD\PRO9\guru\EXPORT\". 
   prognamnk = "\\pc112\DELAD\PRO9\guru\EXKOPIA\". 
END.
ELSE IF globforetag = "GRAN" THEN DO:
   aofil =   "k4.txt".
   kontfil = "Ktokompl.txt".
   prognamn = "\\GRANGURU\guru_ser\server\PRO9s\EXPORT\".
   prognamnk = "\\GRANGURU\guru_ser\server\PRO9s\EXPORT\EXKOPIA\".    
END.
ASSIGN
nrcol[1] = 1
nrcol[2] = 2
nrcol[3] = 3
nrcol[4] = 4
nrcol[5] = 5
 /*ny kolumn*/
breddantal = 5   /*antal kolumner*/
bredd[1] = 1
bredd[2] = 3
bredd[3] = 20
bredd[4] = 30
bredd[5] = 1.
ASSIGN
i = 2.     
utnr[nrcol[1]] = 1.
DO WHILE i <= breddantal:             
   utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
   i = i + 1.
END.
ASSIGN
nrcolK2[1] = 1
nrcolK2[2] = 2
nrcolK2[3] = 3
nrcolK2[4] = 4
nrcolK2[5] = 5
nrcolK2[6] = 6
nrcolK2[7] = 7
nrcolK2[8] = 8
nrcolK2[9] = 9
nrcolK2[10] = 10
nrcolK2[11] = 11
nrcolK2[12] = 12  /*ny kolumn*/
breddantalK2 = 12   /*antal kolumner*/
breddK2[1] = 3
breddK2[2] = 20
breddK2[3] = 20
breddK2[4] = 20
breddK2[5] = 20
breddK2[6] = 20
breddK2[7] = 20
breddK2[8] = 20
breddK2[9] = 20
breddK2[10] = 20
breddK2[11] = 10
breddK2[12] = 10.

ASSIGN
i = 2.     
utnrK2[nrcolK2[1]] = 1.
DO WHILE i <= breddantalK2:             
   utnrK2[i] = utnrK2[i - 1] + breddK2[i - 1] + 1.            
   i = i + 1.
END.
{AMERICANEUROPEAN.I}

OPEN QUERY aoq FOR EACH AONRTAB WHERE AONRTAB.AUTOREG = TRUE 
USE-INDEX AONR NO-LOCK.
GET FIRST aoq NO-LOCK.    
DO WHILE AVAILABLE(AONRTAB):
   RUN aonrut_UI.
   GET NEXT aoq NO-LOCK.     
END.
{EUROPEANAMERICAN.I}
PROCEDURE aonrut_UI:      
   FOR EACH tidut:
      DELETE tidut.
   END.
   FOR EACH tidutK2:
      DELETE tidutK2.
   END.
   IF globforetag = "GRAN"  OR globforetag = "ELPA" THEN DO:
      IF AONRTAB.DELNR > 0 THEN DO TRANSACTION:
         GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
         IF LOCKED(AONRTAB) = FALSE THEN AONRTAB.AUTOREG = FALSE.    
         RETURN.
      END.      
   END. 
   /*ej debi..*/
   IF AONRTAB.PRISTYP = "FRÅNVARO." THEN DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
      IF LOCKED(AONRTAB) = FALSE THEN AONRTAB.AUTOREG = FALSE.     
      RETURN.
   END.
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:
      avdatum = ?.
   END.
   ELSE DO:
      avdatum = AONRTAB.AONRAVDATUM.      
   END.
   CREATE tidut.
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO TRANSACTION:
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "A".
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
      IF LOCKED(AONRTAB) = FALSE THEN AONRTAB.AUTOREG = FALSE.          
   END.
   ELSE IF AONRTAB.AONRAVDATUM + 31 > TODAY THEN DO:
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "A".
   END.
   ELSE DO TRANSACTION:
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "I".
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
      IF LOCKED(AONRTAB) = FALSE THEN AONRTAB.AUTOREG = FALSE.      
   END.         
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "4"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = AONRTAB.AONR 
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = AONRTAB.ORT 
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Y".
   ASSIGN
   filut = ""
   filutkopia = "".
   IF globforetag = "ELPA" THEN formappvar = "N9\".
   ELSE IF globforetag = "GRAN" THEN formappvar = "N9\".
   ASSIGN
   filut = prognamn + formappvar 
   filutkopia = prognamnk + formappvar.
   IF SEARCH(filut) = ? THEN DO:
      OS-CREATE-DIR VALUE(filut).
   END.
   IF SEARCH(filutkopia) = ? THEN DO:
      OS-CREATE-DIR VALUE(filutkopia).
   END.
   ASSIGN
   filut = filut + aofil 
   filutkopia = filutkopia + aofil.
   OUTPUT TO VALUE(filut) APPEND.
   FOR EACH tidut:
      PUT UNFORMATTED tidut.UT AT 1 SKIP.   
   END.
   OUTPUT CLOSE.
   OUTPUT TO VALUE(filutkopia) APPEND.
   FOR EACH tidut:
      PUT UNFORMATTED tidut.UT AT 1 SKIP.   
   END.
   OUTPUT CLOSE.
   OPEN QUERY akq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = AONRTAB.AONR  AND 
   AONRKONTKOD.DELNR = AONRTAB.DELNR NO-LOCK. 
   GET FIRST akq NO-LOCK.
   IF AVAILABLE AONRKONTKOD THEN DO:
      IF INTEGER(AONRKONTKOD.K1) >= 1000 THEN filut = filut.
      ELSE DO:
         CREATE tidutK2.
         ASSIGN
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[1]]) = "4"
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[2]]) = AONRTAB.AONR 
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[4]]) = STRING(INTEGER(AONRKONTKOD.K1),"999") 
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[11]]) = STRING(TODAY,"99999999")
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[12]]) = "20201231".
         RUN utfil_UI.        
      END.
   END.
   ELSE DO:
      IF INTEGER(AONRTAB.OMRADE) >= 1000 THEN filut = filut.
      ELSE DO:
         CREATE tidutK2.
         ASSIGN
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[1]]) = "4"
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[2]]) = AONRTAB.AONR 
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[4]]) = STRING(INTEGER(AONRTAB.OMRADE),"999") 
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[11]]) = STRING(TODAY,"99999999")
         SUBSTRING(tidutK2.UT,utnrK2[nrcolK2[12]]) = "20201231".
         RUN utfil_UI.        
      END.
   END.
END PROCEDURE.
PROCEDURE utfil_UI:
   ASSIGN
   filut = REPLACE(filut,aofil," ") 
   filutkopia = REPLACE(filutkopia,aofil," ").
   ASSIGN
   filut = TRIM(filut) + TRIM(kontfil) 
   filutkopia = TRIM(filutkopia) + TRIM(kontfil).
   OUTPUT TO VALUE(filut) APPEND.
   FOR EACH tidutK2:
      PUT UNFORMATTED tidutK2.UT AT 1 SKIP.   
   END.
   OUTPUT CLOSE.
   OUTPUT TO VALUE(filutkopia) APPEND.
   FOR EACH tidutK2:
      PUT UNFORMATTED tidutK2.UT AT 1 SKIP.   
   END.
   OUTPUT CLOSE.
END PROCEDURE.

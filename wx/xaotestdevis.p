
/*GKAONDEVIS.P AONUMMER FRÅN GURU TILL FIL*/
DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)"
   FIELD JUD AS CHARACTER.
DEFINE TEMP-TABLE tidutstart
   FIELD UT AS CHARACTER FORMAT "X(132)".
DEFINE TEMP-TABLE tidutslut
   FIELD UT AS CHARACTER FORMAT "X(132)".
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnrstart AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnrslut AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolstart AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolslut AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE VARIABLE aofil AS CHARACTER NO-UNDO.
DEFINE VARIABLE kontfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE filut AS CHARACTER NO-UNDO.
DEFINE VARIABLE filutkopia AS CHARACTER NO-UNDO.
DEFINE VARIABLE formappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE jupers AS CHARACTER  NO-UNDO.
DEFINE VARIABLE anr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cklockan AS CHARACTER NO-UNDO.
DEFINE VARIABLE aostartdatum AS DATE NO-UNDO.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
cklockan = REPLACE(STRING(TIME,"hh:mm:ss"),":","").

/*
formappvar = "M7\". 800 småland SEFAB 0880630   och Småländsk = 180

formappvar = "N6\". 100      "GKEAB"  0880610  Elnät = 100
  
*/   

DEFINE QUERY aoq FOR AONRTAB.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG. 
IF globforetag = "ELPA" THEN DO:        
   ASSIGN
   prognamn = "\\pc012\D\DELAD\PRO9\guru\EXPORT\"
   prognamnk = "\\pc012\D\DELAD\PRO9\guru\EXKOPIA\". 
END.
ASSIGN
aofil = "aonrdevis" + STRING(TODAY,"99999999") + cklockan + ".txt"
kontfil = "aokontondevis" + STRING(TODAY,"99999999") + cklockan + ".txt".
ASSIGN
nrcolstart[1] = 1
nrcolstart[2] = 2
nrcolstart[3] = 3
nrcolstart[4] = 4
nrcolstart[5] = 5
nrcolstart[6] = 6
nrcolstart[7] = 7
nrcolstart[8] = 8.
ASSIGN
nrcolslut[1] = 1
nrcolslut[2] = 2
nrcolslut[3] = 3
nrcolslut[4] = 4
nrcolslut[5] = 5
nrcolslut[6] = 6
nrcolslut[7] = 7
nrcolslut[8] = 8
nrcolslut[9]  = 9
nrcolslut[10] = 10
nrcolslut[11] = 11
nrcolslut[12] = 12
nrcolslut[13] = 13
nrcolslut[14] = 14
nrcolslut[15] = 15
nrcolslut[16] = 16.
ASSIGN
nrcol[1] = 1
nrcol[2] = 2
nrcol[3] = 3
nrcol[4] = 4
nrcol[5] = 5
nrcol[6] = 6
nrcol[7] = 7
nrcol[8] = 8
nrcol[9]  = 9
nrcol[10] = 10
nrcol[11] = 11
nrcol[12] = 12
nrcol[13] = 13.
ASSIGN
breddantal = 8   /*antal kolumner*/
bredd[1] = 2
bredd[2] = 3
bredd[3] = 5
bredd[4] = 3
bredd[5] = 6
bredd[6] = 6
bredd[7] = 3
bredd[8] = 222.
ASSIGN
i = 2.     
utnrstart[1] = 1.
DO WHILE i <= breddantal:             
   utnrstart[i] = utnrstart[i - 1] + bredd[i - 1].            
   i = i + 1.
END.
ASSIGN
breddantal = 16   /*antal kolumner*/
bredd[1] = 2
bredd[2] = 10
bredd[3] = 10
bredd[4] = 10
bredd[5] = 10 
bredd[6] = 18 
bredd[7] = 18 
bredd[8]  = 18 
bredd[9]  = 10 
bredd[10] = 10 
bredd[11] = 10 
bredd[12] = 10 
bredd[13] = 18 
bredd[14] = 18 
bredd[15] = 10 
bredd[16] = 68. 
ASSIGN
i = 2.     
utnrslut[1] = 1.
DO WHILE i <= breddantal:             
   utnrslut[i] = utnrslut[i - 1] + bredd[i - 1].            
   i = i + 1.
END.

ASSIGN
breddantal = 13   /*antal kolumner*/
bredd[1] = 2
bredd[2] = 9
bredd[3] = 7
bredd[4] = 6
bredd[5] = 6 
bredd[6] = 53 
bredd[7] = 1 
bredd[8]  = 10 
bredd[9]  = 71 
bredd[10] = 25 
bredd[11] = 6 
bredd[12] = 6 
bredd[13] = 48. 
ASSIGN
i = 2.     
utnr[1] = 1.
DO WHILE i <= breddantal:             
   utnr[i] = utnr[i - 1] + bredd[i - 1].            
   i = i + 1.
END.


jupers = "800".
EMPTY TEMP-TABLE tidut NO-ERROR.
RUN aonrtab_UI (INPUT jupers).
RUN filut_UI.

PROCEDURE filut_UI:  
   ASSIGN
   formappvar = "M7\".
   aofil = "tHANDELprojektnr" + STRING(TODAY,"99999999") + cklockan + ".txt".
   RUN utfil_UI (INPUT "800").
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN ftp_UI. 
   formappvar = "N6\".
   aofil = "tELNATprojektnr" + STRING(TODAY,"99999999") + cklockan + ".txt".
   RUN utfil_UI (INPUT "100").
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN ftp_UI. 
                   
END PROCEDURE.
PROCEDURE ftp_UI:
   /*InGuru InHR InEconoma*/

   RUN FTPFILE.P  (INPUT "gep", INPUT "vsf17ggr", INPUT TRUE, INPUT 1,
                   INPUT filut, INPUT "/InEconoma/" + aofil,                
                   INPUT "192.121.243.27", OUTPUT TABLE felmeddtemp).                     
   FOR EACH felmeddtemp:
      DISPLAY felmeddtemp.
   END.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
END PROCEDURE.

PROCEDURE startpost_UI:
   DEFINE INPUT PARAMETER id AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER finnsej AS LOGICAL NO-UNDO.
   FIND FIRST tidut WHERE tidut.JUD = id NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidut THEN DO:
      finnsej = TRUE.
      RETURN.
   END.
   CREATE tidutstart.
   ASSIGN   
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[1]]) = "10"
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[2]]) = "430"
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[3]]) = "08806"
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[4]]) = "GUR"
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[5]]) = STRING(TODAY,"999999") 
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[6]]) = cklockan
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[7]]) = "PRO"
   SUBSTRING(tidutstart.UT,utnrstart[nrcolstart[8]]) = "".      
END PROCEDURE.
PROCEDURE slutpost_UI:
   DEFINE INPUT PARAMETER id AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hjantal AS INTEGER NO-UNDO.
   DEFINE VARIABLE hjnoll AS CHARACTER NO-UNDO.
   hjantal = 0.
   REPEAT:
      hjantal = hjantal + 1.
      IF hjantal > utnrslut[nrcolslut[15]] THEN LEAVE.
      hjnoll = hjnoll + "0".
   END.
   hjantal = 0.
   FOR EACH tidut WHERE tidut.JUD = id:
      hjantal = hjantal + 1.
   END.
   CREATE tidutslut.
   ASSIGN   
   SUBSTRING(tidutslut.UT,utnrslut[nrcolslut[1]]) = "50"
   SUBSTRING(tidutslut.UT,utnrslut[nrcolslut[2]]) = hjnoll
   SUBSTRING(tidutslut.UT,utnrslut[nrcolslut[15]]) = STRING(hjantal,"9999999999")
   SUBSTRING(tidutslut.UT,utnrslut[nrcolslut[16]]) = "".
   
END PROCEDURE.
PROCEDURE aonrtab_UI:  
   DEFINE INPUT PARAMETER jpers AS CHARACTER.
   OPEN QUERY aoq FOR EACH AONRTAB WHERE AONRTAB.AUTOREG = TRUE 
   USE-INDEX AONR NO-LOCK.
   GET FIRST aoq NO-LOCK.    
   DO WHILE AVAILABLE(AONRTAB):      
      RUN aonrut_UI.
      GET NEXT aoq NO-LOCK.     
   END.
END PROCEDURE.           

PROCEDURE aonrut_UI:      
   DEFINE VARIABLE hjnoll AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hjantal AS INTEGER NO-UNDO.   
   IF AONRTAB.DELNR > 0 THEN DO TRANSACTION:
      
      RETURN.
   END.      
   
   

   /*ej debi..*/
   IF AONRTAB.PRISTYP = "FRÅNVARO." THEN DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
      RETURN.
   END.
   FIND FIRST AONRTIDLAGE WHERE 
   AONRTIDLAGE.AONR = AONRTAB.AONR AND 
   AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
   AONRTIDLAGE.IDTIDLAG = "AOUPPLAGT"
   NO-LOCK NO-ERROR.                         
   IF AVAILABLE AONRTIDLAGE THEN aostartdatum = AONRTIDLAGE.DATUM1.
   IF aostartdatum = ? THEN aostartdatum = TODAY.
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:
      avdatum = ?.
   END.
   ELSE DO:
      avdatum = AONRTAB.AONRAVDATUM.      
   END.
   IF avdatum = ? THEN avdatum = 01/01/2025.   
   hjantal = 0.
   REPEAT:
      hjantal = hjantal + 1.
      IF LENGTH(AONRTAB.AONR) + hjantal > 10 THEN LEAVE.
      hjnoll = hjnoll + "0".
   END.
   /*mejl jan Carlsson 20040813
   Vid kontoöverföring till ekonomi gäller tvärtom, nollor innan kontodelen.
   */
   aonrvar = hjnoll + AONRTAB.AONR.
   CREATE tidut.
   tidut.JUD = jupers.
   ASSIGN 
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "21"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "".
   IF tidut.JUD = "100" THEN SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "0880610".
   IF tidut.JUD = "800" THEN SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "0880630".
   ASSIGN 
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(aostartdatum,"999999")
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(avdatum,"999999")
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = ""
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "E"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = aonrvar
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = SUBSTRING(AONRTAB.ORT,1,25)
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(aostartdatum,"999999")
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(avdatum,"999999")     
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "".

   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO TRANSACTION:   
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
      END.
   ELSE IF AONRTAB.AONRAVDATUM > TODAY + 31 THEN DO:      
   END.
   ELSE DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
   END.         
      
END PROCEDURE.
PROCEDURE utfil_UI:   
   DEFINE INPUT PARAMETER jp AS CHARACTER NO-UNDO.
   ASSIGN
   filut = ""
   filutkopia = "".
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
   EMPTY TEMP-TABLE tidutstart NO-ERROR.
   EMPTY TEMP-TABLE tidutslut NO-ERROR. 
   RUN startpost_UI (INPUT jp,OUTPUT musz).
   IF musz = FALSE THEN DO:  
      OUTPUT TO VALUE(filut) APPEND CONVERT TARGET "iso8859-1".
      FIND FIRST tidutstart NO-LOCK NO-ERROR.
      PUT UNFORMATTED tidutstart.UT AT 1 SKIP.         
      FOR EACH tidut WHERE tidut.JUD = jp:
         PUT UNFORMATTED tidut.UT AT 1 SKIP.         
      END.
      RUN slutpost_UI (INPUT jp).
      FIND FIRST tidutslut NO-LOCK NO-ERROR.
      PUT UNFORMATTED tidutslut.UT AT 1 SKIP.         
      OUTPUT CLOSE.

      OUTPUT TO VALUE(filutkopia) APPEND CONVERT TARGET "iso8859-1".
      PUT UNFORMATTED tidutstart.UT AT 1 SKIP.         
      FOR EACH tidut WHERE tidut.JUD = jp:
         PUT UNFORMATTED tidut.UT AT 1 SKIP.         
      END.
      PUT UNFORMATTED tidutslut.UT AT 1 SKIP.         
      OUTPUT CLOSE.
      FOR EACH tidut WHERE tidut.JUD = jp:
         DELETE tidut.
      END.
   END.
END PROCEDURE.


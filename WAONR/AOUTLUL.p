/*AOUTLUL.P FRÅN GURU TILL FIL*/
{TIDUTTT.I}
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.

DEFINE VARIABLE fbreddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE futnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE fbredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE fnrcol AS INTEGER EXTENT 50 NO-UNDO.


DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE STREAM eko.
DEFINE STREAM ekospar.
DEFINE STREAM ekolule.
DEFINE VARIABLE aoxmlbuffh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE aoxmltt NO-UNDO
  FIELD AONR AS CHARACTER
  FIELD DELNR AS INTEGER FORMAT "999"
  FIELD ORT AS CHARACTER
  FIELD JUDID AS CHARACTER
  INDEX AONR JUDID AONR DELNR.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO.  
prognamnque = "D:\elpool\delad\PRO9S\autotid.txt". 
aoxmlbuffh = TEMP-TABLE aoxmltt:DEFAULT-BUFFER-HANDLE.   


FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
IF LDBNAME(1) = "UTBI" THEN RETURN.

ASSIGN
nrcol[1] = 1
nrcol[2] = 2
nrcol[3] = 3
nrcol[4] = 4
nrcol[5] = 5

breddantal = 5   
bredd[1] = 6
bredd[2] = 3
bredd[3] = 40
bredd[4] = 4
bredd[5] = 8.

ASSIGN
i = 2.     
utnr[nrcol[1]] = 1.
DO WHILE i <= breddantal:             
   utnr[i] = utnr[i - 1] + bredd[i - 1].            
   i = i + 1.
END.


/*ASSIGN
fnrcol[1] = 1
fnrcol[2] = 2
fnrcol[3] = 3
fnrcol[4] = 4
fnrcol[5] = 5
fbreddantal = 5   
fbredd[1] = 50
fbredd[2] = 100
fbredd[3] = 1.
fbredd[4] = 8.
fbredd[5] = 8.

ASSIGN
i = 2.     
futnr[fnrcol[1]] = 1.
DO WHILE i <= fbreddantal:             
   futnr[i] = futnr[i - 1] + fbredd[i - 1].            
   i = i + 1.
END.
*/


OPEN QUERY aoq FOR EACH AONRTAB WHERE AONRTAB.AUTOREG = TRUE 
USE-INDEX AONR NO-LOCK.
GET FIRST aoq NO-LOCK.    
DO WHILE AVAILABLE(AONRTAB):
   DO TRANSACTION:
      GET CURRENT aoq EXCLUSIVE-LOCK NO-WAIT.    
      IF LOCKED(AONRTAB) = FALSE THEN AONRTAB.AUTOREG = FALSE.  
              
      RUN aonrut_UI.
      RUN flexaout_UI.
   END.
   GET NEXT aoq NO-LOCK.     
END.
/*
IF FORETAG.FORETAG = "ELPA" THEN DO: 
   prognamn = CAPS("d:\DELAD\PRO9s\EXPORT\projguru" + STRING(TODAY,"99999999") + ".xml"). 
    
END.
IF FORETAG.FORETAG = "LULE" THEN DO: 
   prognamn = CAPS("D:\ELPOOL\DELAD\PRO9S\EXPORT\projguru" + JURPERS.JUDID + STRING(TODAY,"99999999") + ".xml"). 
    
END.

aoxmlbuffh:WRITE-XML("file", prognamn,FALSE,"iso8859-1").
*/ 
PROCEDURE flexaout_UI :
   /*
   C:\FilesFromGuru\AO01 -> importsökväg i Flex HRM \\flex\FilesFromGuru$\AO01
C:\FilesFromGuru\AO02 -> importsökväg i Flex HRM \\flex\FilesFromGuru$\AO02
C:\FilesFromGuru\AO03 -> importsökväg i Flex HRM \\flex\FilesFromGuru$\AO03
C:\FilesFromGuru\AO04 -> importsökväg i Flex HRM \\flex\FilesFromGuru$\AO04
   */
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE stdatum AS DATE NO-UNDO.   
   
   FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = AONRTAB.AONR  AND 
   AONRKONTKOD.DELNR = AONRTAB.DELNR NO-LOCK NO-ERROR.
   IF AVAILABLE AONRKONTKOD THEN DO:
      IF AONRKONTKOD.K2 = "JA" THEN RETURN.
   END.
   IF AONRTAB.OMRADE = "" THEN RETURN.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND  AONRTIDLAGE.IDTIDLAG = "AOUPPLAGT" NO-LOCK NO-ERROR.
   stdatum = ?.
   IF AVAILABLE AONRTIDLAGE THEN DO:
    /*  stdatum = AONRTIDLAGE.DATUM1.*/
    /*LULEÅ vill kunna skriva tid from 1:a i månaden  Lena 20220216*/
      stdatum  = DATE(MONTH(AONRTIDLAGE.DATUM1),01,YEAR(AONRTIDLAGE.DATUM1)).
          
   END.
   IF stdatum = ? THEN DO:
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND TIDREGITAB.DELNR = AONRTAB.DELNR 
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         stdatum = TIDREGITAB.DATUM.
      END.
      ELSE stdatum = ?.
   END.
   IF NOT AVAILABLE JURPERS THEN DO:
      RETURN.
   END.
   IF FORETAG.FORETAG = "ELPA" THEN DO: 
      prognamn = CAPS("d:\DELAD\PRO9s\EXPORT\Flexao" + STRING(TODAY,"99999999")). 
      prognamn = "d:\DELAD\PRO9s\EXPORT\AO" + JURPERS.JUDID.     
   END.
   IF FORETAG.FORETAG = "LULE" THEN DO:
      
     
      
     /*Anders Olsson Elpool i Umeå AB  5 apr 2019 17:14:33 
       prognamn = "\\flex\FilesFromGuru$\AO" + JURPERS.JUDID .
     */
     prognamn = "D:\ELPOOL\DELAD\PRO9S\export\proj\AO" + JURPERS.JUDID.
    
      
      prognamn = CAPS(prognamn + "\Flexao" + JURPERS.JUDID + STRING(TODAY,"99999999")) + ".txt".
      /*
      IF SEARCH(prognamn) NE ? THEN  prognamn = CAPS(prognamn + "\Flexao" + JURPERS.JUDID + STRING(TODAY,"99999999")). 
      ELSE DO:
         OUTPUT TO VALUE(prognamnque) APPEND.
         PUT prognamn " saknas " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
         OUTPUT CLOSE.
      END.   
      */
   END.
   CREATE tidut.
   ASSIGN
   tidut.UT = AONRTAB.AONR + STRING(AONRTAB.DELNR,"999").
   tidut.UT = tidut.UT + ";" + REPLACE(AONRTAB.ORT,";","").
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:
      tidut.UT = tidut.UT + ";" + "0".
      IF stdatum = ? THEN DO: 
         tidut.UT = tidut.UT + ";;;2".
      END.   
      ELSE tidut.UT = tidut.UT + ";" +  STRING(stdatum,"99999999") + ";;2".
   END.
   ELSE DO: 
      tidut.UT = tidut.UT + ";" + "2".
      IF stdatum = ? THEN tidut.UT = tidut.UT + ";".
      ELSE tidut.UT = tidut.UT + ";" + STRING(stdatum,"99999999").
      tidut.UT = tidut.UT + ";" + STRING(AONRTAB.AONRAVDATUM,"99999999") + ";2".
   END.
   /*   
   prognamn = "d:\DELAD\PRO9s\EXPORT\FL.TXT".           
   */
   OUTPUT STREAM eko TO VALUE(prognamn) CONVERT TARGET "UTF-8" APPEND.
   PUT STREAM eko UNFORMATTED
   tidut.UT AT 1 SKIP.
   DELETE tidut.
   OUTPUT STREAM eko CLOSE. 
   /*
   prognamn2 = REPLACE(prognamn,"\\flex\FilesFromGuru$\","D:\ELPOOL\DELAD\PRO9S\EXPORT\"). 
   OUTPUT STREAM ekolule TO VALUE(prognamn2) CONVERT TARGET "UTF-8" APPEND.
   PUT STREAM ekolule UNFORMATTED
   tidut.UT AT 1 SKIP.
   DELETE tidut.
   OUTPUT STREAM ekolule CLOSE. 
   */
END PROCEDURE.

PROCEDURE aonrut_UI: 
   FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = AONRTAB.AONR  AND 
   AONRKONTKOD.DELNR = AONRTAB.DELNR NO-LOCK NO-ERROR.
   IF AVAILABLE AONRKONTKOD THEN DO:
      IF AONRKONTKOD.K2 = "JA" THEN RETURN.
   END.
   IF AONRTAB.OMRADE = "" THEN RETURN.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF FORETAG.FORETAG = "ELPA" THEN DO: 
      prognamn = CAPS("d:\DELAD\PRO9s\EXPORT\projguru" + STRING(TODAY,"99999999") + ".txt"). 
      prognamnk = CAPS("d:\DELAD\PRO9s\EXKOPIA\projguru.txt"). 
   END.
   IF FORETAG.FORETAG = "LULE" THEN DO: 
      prognamn = CAPS("D:\ELPOOL\DELAD\PRO9S\EXPORT\projguru" + JURPERS.JUDID + STRING(TODAY,"99999999") + ".txt"). 
      prognamnk = CAPS("D:\ELPOOL\DELAD\PRO9S\EXKOPIA\projguru" + JURPERS.JUDID + ".txt"). 
   END.
   
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = AONRTAB.AONR
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(AONRTAB.DELNR,"999").
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = SUBSTRING(AONRTAB.ORT,1,40).
   IF AVAILABLE AONRKONTKOD THEN DO:
      IF AONRKONTKOD.K1 NE "" THEN DO:
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = AONRKONTKOD.K1.
      END.    
   END.   
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Pågående".
   END.   
   ELSE DO:
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(AONRTAB.AONRAVDATUM,"99999999").            
   END.   
   
   OUTPUT STREAM eko TO VALUE(prognamn) APPEND.
   OUTPUT STREAM ekospar TO VALUE(prognamnk) APPEND.
   PUT STREAM eko UNFORMATTED
   tidut.UT AT 1 SKIP.
   PUT STREAM ekospar UNFORMATTED
   tidut.UT AT 1 SKIP.
   DELETE tidut.
   OUTPUT STREAM eko CLOSE. 
   OUTPUT STREAM ekospar CLOSE. 
   
END PROCEDURE.

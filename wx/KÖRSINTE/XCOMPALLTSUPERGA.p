/*XCOMPALLTSUPERGA.p
KÖR COPM PÅ GURUANDERS 
define variable vvv as Start.ZipElpool no-undo.
vvv = new Start.ZipElpool().
vvv:MakeZip("c:\temp","c:\t.zip").
C:\DELAD\PRO11\DLC\bin\prowin32.exe -ininame C:\delad\PRO11\GURU\OE11s.ini -pf C:\delad\PRO11\GURU\oekalk.pf -assemblies C:\DELAD\PRO11\wrk_oemgmt\GURU11 -p XCOMPALLTSUPER.p
C:\DELAD\Pro116\dlc\bin\prowin.exe -ininame C:\delad\PRO116\GURU\OE116s.ini -pf C:\delad\PRO11\GURU\oekalk.pf -assemblies C:\DELAD\PRO11\wrk_oemgmt\GURU11 -p XCOMPALLTSUPER.p

*/
{XCOMP116.I}
DEFINE VARIABLE comp112 AS LOGICAL NO-UNDO.
DEFINE VARIABLE allasvar AS LOGICAL.
DEFINE VARIABLE mappsvar AS LOGICAL.
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE searchdir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE savedir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tempsearchdir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE wcguruwtidir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cdirnames AS CHARACTER NO-UNDO.
DEFINE VARIABLE delfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE curdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE statok AS INTEGER NO-UNDO.
DEFINE VARIABLE numcdir AS INTEGER NO-UNDO.
DEFINE VARIABLE dirnr AS INTEGER NO-UNDO.
DEFINE VARIABLE webforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER  LABEL "File" NO-UNDO.


DEFINE VARIABLE filserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(60)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE webdate AS DATE NO-UNDO.
DEFINE VARIABLE iCOMP AS INTEGER NO-UNDO.

DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE lager AS CHARACTER NO-UNDO.
DEFINE VARIABLE GuruComp AS LOGICAL FORMAT "guru/ekg" LABEL "Guru eller EKG" NO-UNDO.

DEFINE VARIABLE filnamnR AS CHARACTER NO-UNDO.
DEFINE VARIABLE totprogram AS INTEGER NO-UNDO.
DEFINE VARIABLE oshelp  AS CHARACTER NO-UNDO.
totprogram = 2567.
DISPLAY totprogram.
  MESSAGE "Kör prog på GURUANDERS"
  VIEW-AS ALERT-BOX.
IF PROGRESS = "FULL" THEN DO:
   SESSION:DEBUG-ALERT = YES.
END. 

GuruComp  = TRUE.  
DEFINE TEMP-TABLE rensatemp NO-UNDO
   FIELD NIVA AS INTEGER
   FIELD SERCHMAPPKOMP AS CHARACTER.
DEFINE BUFFER rensatempbuff FOR rensatemp.
   
DEFINE TEMP-TABLE kompdir NO-UNDO
   FIELD SERCHMAPPKOMP AS CHARACTER
   FIELD SAVEMAPPKOMP AS CHARACTER
   FIELD MAPPKOMP AS CHARACTER
   FIELD NIVA AS INTEGER
   FIELD KLAR AS LOGICAL
   INDEX MAPPKOMP NIVA SERCHMAPPKOMP MAPPKOMP.
DEFINE VARIABLE pcvar AS CHARACTER NO-UNDO.   
DEFINE BUFFER kompdirbuff FOR kompdir.
SESSION:DEBUG-ALERT = YES.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/.
OUTPUT TO VALUE(utfil).
PUT TODAY .
OUTPUT CLOSE.
pcvar = "pc122".
searchdir = "\\" + pcvar + "\delad\pro116\GuruAnders\"  /*Sökväg till de filer som ska kompileras.*/.
savedir = "\\" + pcvar + "\delad\pro116\GURU\KOMP11\".   /*Sökväg till de kompilerade filerna.*/.
webdate = 01/01/89.

UPDATE webdate felkoll.                                                                                  
RUN mappkomp_UI (INPUT "W").
ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.

UPDATE webdate.
/*  
REPEAT:
    
   IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
   IF LDBNAME(1) = ? THEN LEAVE.
END.
*/                                                                                
RUN mappkomp_UI (INPUT "C").

ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.

UPDATE webdate. 
DEBUGGER:SET-BREAK(). 
RUN clsmappar_UI.
DEBUGGER:SET-BREAK().
PROCEDURE  mappkomp_UI :
   DEFINE INPUT PARAMETER mapptyp AS CHARACTER NO-UNDO.
  
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn BEGINS mapptyp AND attrlist = "D" THEN DO: 
         /*Sparar alla mappnamn i en textsträng.*/
         IF cdirnames NE "" THEN DO:
            cdirnames = cdirnames + "," + filnamn.
         END.
         ELSE DO: 
            cdirnames = filnamn.
         END.
         numcdir = numcdir + 1.
         IF mapptyp = "c" OR mapptyp = "W" THEN DO:
            IF filnamn = "wx" OR curdir = "WWEB"  THEN.
            /*Skapar en ny mapp för varje funnen 'c'-mapp*/
            ELSE DO:
               IF mapptyp = "W" THEN DO: 
                  IF filnamn = "WTID" THEN OS-CREATE-DIR VALUE(savedir + filnamn) NO-ERROR.
               END.   
               ELSE OS-CREATE-DIR VALUE(savedir + filnamn) NO-ERROR.
            END.  
            statok = OS-ERROR.
            
            IF statok = 2 THEN DO:
               /*dir fanns*/
            END.
            ELSE IF statok NE 0 THEN DO:
               MESSAGE "Directory not created. System Error #" statok.
               DISPLAY savedir + filnamn. 
               RETURN.
            END. 
         END.                                                                                        
      END.
   END.
   INPUT CLOSE. 
   /*Rensar tidigare innehåll i filen.*/
   
   mappsvar = TRUE.
   MESSAGE "Vill du kompilera alla mappar " + mapptyp + "?" 
   VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL UPDATE allasvar.
   CASE allasvar:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
      END.
      OTHERWISE RETURN.
   END CASE.   
   /*Lopar genom varje c-mapp*/
   DO WHILE dirnr LE numcdir:
      /*Plockar ut namnet för aktuell mapp*/
      curdir = ENTRY(dirnr,cdirnames,",").
      IF curdir = "wx" OR curdir = "WWEB"  THEN.
      ELSE DO:
         tempsearchdir = searchdir + curdir + "\".
         /*Sätter denna mapp som input*/
         IF allasvar = FALSE THEN DO:
            MESSAGE "Vill du kompilera mappen: " + curdir + "?" 
               VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE mappsvar.
         END.
         
         IF mappsvar = TRUE THEN DO:
            INPUT FROM OS-DIR(tempsearchdir) NO-ECHO.
            REPEAT:
               /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
               SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 dirlist VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 attrlist.
               IF filnamn MATCHES "*.p" OR filnamn MATCHES "*.w"  THEN DO:
                  /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
                  FILE-INFO:FILE-NAME = filnamn.
                  IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                     comp112 = TRUE.  
                     IF comp112 = TRUE THEN DO:
                         iCOMP = iCOMP + 1.
                         MESSAGE curdir " Compile file: " + filnamn + " into " + savedir.
                         OUTPUT TO VALUE(utfil) APPEND. 
                         IF felkoll = TRUE THEN PUT filnamn SKIP.
                         /*MESSAGE "Compiled file: " + filnamn + "  ,Destination Directory: " + tempsavedir. */
                         /*Anders Olsson Elpool i Umeå AB  24 jan 2017 16:17:41 
                         till komp11 resp komp10  dir 
                         */
           
                         
                         COMPILE VALUE(dirlist) SAVE INTO VALUE(savedir).
                         OUTPUT CLOSE.
                         filnamnR = REPLACE(filnamn,".P",".r").
                         filnamnR = REPLACE(filnamnR,".w",".r"). 
                         
                     END.
                  END.   
               END.
               ELSE IF filnamn = "." THEN.           
               ELSE IF filnamn = ".." THEN.   
               ELSE IF filnamn MATCHES "*.i" OR filnamn MATCHES "*.r" THEN.  
               ELSE DO: 
                  IF attrlist = "D" THEN.
                  ELSE DO:  
                                       END.           
               END.                  
            END.
            INPUT CLOSE.
         END.
      END.
      dirnr = dirnr + 1.
   END.
   DISPLAY iCOMP.
END PROCEDURE.

PROCEDURE clsmappar_UI :
   EMPTY TEMP-TABLE kompdir NO-ERROR. 
   DEFINE VARIABLE antalniv AS INTEGER NO-UNDO.
   antalniv = 1.    
   MESSAGE "Vill du kompilera alla CLSmappar ?" 
   VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL UPDATE allasvarcls AS LOGICAL.
   CASE allasvarcls:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
      END.
      OTHERWISE RETURN.
   END CASE.   
   
   searchdir = searchdir + "2GURU\".
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3  ^ attrlist.
      /* om filen är en mapp.*/
      IF attrlist = "D" THEN DO: 
         IF filnamn = "." THEN NEXT.
         IF filnamn = ".." THEN NEXT.
         IF filnamn MATCHES '*TesterTester*' THEN NEXT.
        
         OS-CREATE-DIR VALUE(savedir + "2Guru\") NO-ERROR.
        
         CREATE kompdir.
         ASSIGN
         kompdir.NIVA = antalniv
         kompdir.SERCHMAPPKOMP = searchdir + filnamn + "\"
         kompdir.SAVEMAPPKOMP = savedir  /* + filnamn + "\"*/
         kompdir.MAPPKOMP = filnamn.
         filnamn = "".
         /*
         OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP) NO-ERROR.
         */
         OS-CREATE-DIR VALUE(savedir + "2Guru\" + kompdir.MAPPKOMP) NO-ERROR.
         statok = OS-ERROR.
         
         IF statok = 2 THEN DO:
            /*dir fanns*/
         END.
         ELSE IF statok NE 0 THEN DO:
            MESSAGE "Directory not created. System Error #" statok.
            DISPLAY savedir + kompdir.MAPPKOMP. 
            RETURN.
         END.
         
         /*Sparar alla mappnamn i en textsträng.*/
      END.   

   END.
  INPUT CLOSE.
   
   REPEAT:
      FIND FIRST kompdir WHERE kompdir.NIVA = antalniv NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE kompdir THEN LEAVE.
      FOR EACH kompdir  WHERE kompdir.NIVA = antalniv NO-LOCK:
         INPUT FROM OS-DIR(kompdir.SERCHMAPPKOMP) NO-ECHO.
         REPEAT:
            SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3  ^ attrlist.
            /*Kolla om filen är en mapp.*/
            IF attrlist = "D" THEN DO: 
               IF filnamn = "." THEN NEXT.
               IF filnamn = ".." THEN NEXT.
               IF filnamn MATCHES '*TesterTester*' THEN NEXT.
               IF filnamn MATCHES '*Mark*' THEN NEXT.
               IF filnamn MATCHES '*Arende*' THEN NEXT.
               CREATE kompdirbuff.
               ASSIGN
               kompdirbuff.NIVA = antalniv + 1
               kompdirbuff.SERCHMAPPKOMP = kompdir.SERCHMAPPKOMP + filnamn + "\"
               kompdirbuff.SAVEMAPPKOMP = kompdir.SAVEMAPPKOMP /*+ filnamn + "\"*/
               kompdirbuff.MAPPKOMP = filnamn.
               filnamn = "".
               /*Sparar alla mappnamn i en textsträng.*/
               /*
               OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP + "\" + kompdirbuff.MAPPKOMP) NO-ERROR.
               */
               OS-CREATE-DIR VALUE(savedir + "2Guru\" + kompdir.MAPPKOMP + "\" + kompdirbuff.MAPPKOMP) NO-ERROR.
               statok = OS-ERROR.
               
               IF statok = 2 THEN DO:
                  /*dir fanns*/
               END.
               ELSE IF statok NE 0 THEN DO:
                  MESSAGE "Directory not created. System Error #" statok.
                  DISPLAY savedir + kompdir.MAPPKOMP + "\" + kompdirbuff.MAPPKOMP. 
                  RETURN.
               END.
                  
            END.
         END.
         INPUT CLOSE.
      END.
      antalniv = antalniv + 1.      
   END.
     
   mappsvar = TRUE.
   FOR EACH kompdir WHERE kompdir.KLAR = FALSE:
      /*Plockar ut namnet för aktuell mapp*/
      IF allasvarcls = FALSE THEN DO:  
          MESSAGE "Vill du kompilera mappen: " kompdir.SERCHMAPPKOMP "?" 
          VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE mappsvar.
      END.
      ELSE mappsvar = TRUE.    
      IF mappsvar = TRUE THEN DO:
         INPUT FROM OS-DIR(kompdir.SERCHMAPPKOMP) NO-ECHO.
         REPEAT:
            /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
            SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3  dirlist VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 attrlist.
            IF filnamn MATCHES "*.cls" THEN DO:
               /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn).
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  /*
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(kompdir.SAVEMAPPKOMP).
                  */
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(savedir + "2Guru\").
                  
                  OUTPUT CLOSE.
               END.                            
            END.
            ELSE IF filnamn MATCHES "*.p" THEN DO:
                /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn).
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                 
                  
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  /*
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                  */
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(savedir + "2Guru\" + kompdir.MAPPKOMP).
                  
                  OUTPUT CLOSE.
               END.                            
            END.
            ELSE IF filnamn = "." THEN.           
            ELSE IF filnamn = ".." THEN.   
            ELSE IF filnamn MATCHES "*.i" OR filnamn MATCHES "*.r" THEN.   
            ELSE DO:
               IF attrlist = "D" THEN.
               ELSE DO:  
                  kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                /*
                  OS-COPY VALUE(dirlist) VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                  */
                  OS-COPY VALUE(dirlist) VALUE(savedir + "2Guru\" + kompdir.MAPPKOMP).
                     
               END.   
            END.   
         END.   
         INPUT CLOSE.
      END.
      kompdir.KLAR = TRUE.      
   END.
  
   DISPLAY iCOMP.
   
END PROCEDURE.
PROCEDURE Rensa_UI :
   DEFINE VARIABLE delantalniv AS INTEGER NO-UNDO.
   DEFINE VARIABLE mappdel AS CHARACTER NO-UNDO.
   mappdel = "\WebGuru\GuruOnWeb\2Guru\".
   delantalniv = 1. 
   INPUT FROM OS-DIR("\\" + pcvar + mappdel) NO-ECHO.
   REPEAT:
      SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF attrlist = "D" THEN DO: 
         IF filnamn = "." THEN NEXT.
         IF filnamn = ".." THEN NEXT.
         CREATE rensatemp.
         ASSIGN  
         rensatemp.NIVA = delantalniv
         rensatemp.SERCHMAPPKOMP = "\\" + pcvar + mappdel + filnamn + "\".                                                                                  
      END.
   END.
   INPUT CLOSE.
   REPEAT:
      FIND FIRST rensatemp WHERE rensatemp.NIVA = delantalniv NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE rensatemp THEN LEAVE.
      FOR EACH rensatemp  WHERE rensatemp.NIVA = delantalniv NO-LOCK:
         INPUT FROM OS-DIR(rensatemp.SERCHMAPPKOMP) NO-ECHO.
         REPEAT:
            SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3  ^ attrlist.
            /*Kolla om filen är en mapp.*/
            IF attrlist = "D" THEN DO: 
               IF filnamn = "." THEN NEXT.
               IF filnamn = ".." THEN NEXT.
               IF filnamn MATCHES '*TesterTester*' THEN NEXT.
               IF filnamn MATCHES '*Mark*' THEN NEXT.
               IF filnamn MATCHES '*Arende*' THEN NEXT.
               
               CREATE rensatempbuff.
               ASSIGN
               rensatempbuff.NIVA = delantalniv + 1
               rensatempbuff.SERCHMAPPKOMP = rensatemp.SERCHMAPPKOMP + filnamn + "\".
                  
            END.
         END.
         INPUT CLOSE.
      END.
      delantalniv = delantalniv + 1.      
   END.
   mappdel = "\WebGuru\GuruOnWeb\".
   delantalniv = 1. 
   INPUT FROM OS-DIR("\\" + pcvar + mappdel) NO-ECHO.
   REPEAT:
      SET filnamn VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn BEGINS "c" AND attrlist = "D" THEN DO: 
         IF filnamn = "." THEN NEXT.
         IF filnamn = ".." THEN NEXT.
         CREATE rensatemp.
         ASSIGN  
         rensatemp.SERCHMAPPKOMP = "\\" + pcvar + mappdel + filnamn + "\".                                                                                  
      END.
   END.
   INPUT CLOSE.
   
   
   FOR EACH rensatemp WHERE NO-LOCK:
      rensatemp.SERCHMAPPKOMP = rensatemp.SERCHMAPPKOMP + "*.*".   
   END. 
   
   
   mappdel = "\WebGuru\GuruOnWeb\".
   CREATE rensatemp.
   rensatemp.SERCHMAPPKOMP = "\\" + pcvar + "\WebGuru\Komp11\*.*".
   CREATE rensatemp.                          
   rensatemp.SERCHMAPPKOMP = "\\" + pcvar + mappdel + "ctid\bilder\*.*".
   CREATE rensatemp. 
   rensatemp.SERCHMAPPKOMP = "\\" + pcvar + mappdel + "ctid\Manualer\*.*".  
   CREATE rensatemp.
   rensatemp.SERCHMAPPKOMP = "\\" + pcvar + mappdel + "wtid\*.*".   
   
   OUTPUT TO VALUE ("\\" + pcvar + "\WebGuru\rens.bat"). 
   
   FOR EACH  rensatemp:
      PUT UNFORMATTED  "DEL /Q " rensatemp.SERCHMAPPKOMP SKIP.  
      DELETE rensatemp. 
   END.
   OUTPUT CLOSE. 
   OS-COMMAND  VALUE("\\" + pcvar + "\WebGuru\rens.bat"). 
   
     
END PROCEDURE.


RUN OPENDOC.P (utfil,"","",NO).

/*XCOMPALLTSUPER.p*/

/*
 KOMPILERING W MAPPAR
 ====================
 ALLA FILER I W-MAPPAR FÖR SERVER HAMNAR I SAMMA MAPP - WTID
 FLYTTAR ANDRA TYPER AV FILER TILL WTID
 
 KOMPILERING C MAPPAR
 ====================
 ALLA C MAPPAR, KLIENTPROGRAM HAMNAR I C-MAPPAR FÖR RESPTIV CAB FIL OBS CSTART OCH CSMART HANMNAR I SAMMA MAPP
 FLYTTAR ANDRA TYPER AV FILER RÄTT
 
 
 KOMPILERING CLS
 ===============
 CLS FILER HAMNAR I RÄTT MAPPAR 
 COMPILERING AV *.P FILER KRÄVER RÄTT SPARMAPP  
 FLYTTAR ANDRA FILER KRÄVER RÄTT SPARMAPP
 
 
 EXTRA UPPGIFTER
 ===============
 FLYTTAR BILDER

 FLYTTAR MANUALER
 
 TOSHIBAN kompilerar över alla  \\pc122\WebGuru\GuruOnWeb\2Guru\Controls program för rätt excel version

 
 
 VID KÖRNING AV WEBCLIENT PÅ SERVER KAN MAN FLYTTA ALLA PROGRAM TILL RÄTTA MAPPAR VIA  
 wtid\VALDBSERVERMULTI.w OBS JUST NU ENDAST FÖR V 11.6
   

MAPPEN WTID/LADDA MÅSTE BEHANDLAS SEPARAT  MEN DET BEHÖVS BARA VID FÖRÄNDRINGAR I DESSA PROGRAM 
DESSA FILER SKA XCODE 
C:\delad\pro116\dlc\bin\xcode -d D:\WebGuru\Xcode\Ladda *.i *.P 
FILERNA LIGGER I KLARTEXT I D:\WebGuru\Xcode OCH KRYPTERAS TILL D:\WebGuru\Xcode\Ladda
SEN FLYTTAS DESSA MANUELT TILL D:\WebGuru\GuruOnWeb\wtid\Ladda

*/

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
DEFINE VARIABLE webbdir AS CHARACTER NO-UNDO.

DEFINE VARIABLE filnamnR AS CHARACTER NO-UNDO.
DEFINE VARIABLE totprogram AS INTEGER NO-UNDO.
DEFINE VARIABLE oshelp  AS CHARACTER NO-UNDO.
totprogram = 2567.
DISPLAY totprogram.
IF PROVERSION BEGINS "11" THEN.
ELSE DO:
   MESSAGE "fel PROVERSION" PROVERSION
   VIEW-AS ALERT-BOX.
   RETURN. 
END.  
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
FUNCTION mappkoll RETURNS  LOGICAL 
   (INPUT filname AS CHARACTER ):
   IF filnamn = "." THEN RETURN TRUE.
   IF filnamn = ".." THEN RETURN TRUE.
   IF filnamn MATCHES '*TesterTester*' THEN RETURN TRUE.
   IF filnamn MATCHES '*Mark*' THEN RETURN TRUE.
   IF filnamn MATCHES '*Arende*' THEN RETURN TRUE.
   IF filnamn MATCHES '*ControlsTXX*' THEN RETURN TRUE.
   RETURN FALSE. 
END FUNCTION.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/.
OUTPUT TO VALUE(utfil).
PUT TODAY .
OUTPUT CLOSE.

pcvar = "pc122".
searchdir = "\\" + pcvar + "\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/.
IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO:
   savedir = "\\" + pcvar + "\WebGuru\GuruOnWeb\WTID\".
   webbdir = "\\" + pcvar + "\WebGuru\GuruOnWeb\".   /*Sökväg till de kompilerade filerna.*/
   
END.   
/*
lager = "\\Elpooweb-2\e\lager".
*/
MESSAGE "Obs! Rensar du måste du köra XCOMPWEBcontrols.p" skip
"Rensa?" 
VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE rens AS LOGICAL.
CASE rens:
   WHEN TRUE THEN DO:
      RUN Rensa_UI.    
   END.
   WHEN FALSE THEN DO:
   END.
   
END CASE.  
webdate = 01/01/89.
IF SUBSTRING(PROVERSION,1,2) = "10" THEN. 
ELSE DO:
   UPDATE webdate felkoll.                                                                                  
   RUN mappkomp_UI (INPUT "W").
END.   
ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.

IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
   UPDATE webdate felkoll.
END.   
ELSE UPDATE webdate.  
REPEAT:
   IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
   IF LDBNAME(1) = ? THEN LEAVE.
END.
                                                                                
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
   IF mapptyp = "c" THEN DO:
      MESSAGE "Nyheter?"
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE nht AS LOGICAL.
      CASE nht:
         WHEN TRUE THEN DO:
            OUTPUT TO VALUE(webbdir + "nyheter.txt").
            PUT UNFORMATTED  TODAY SKIP.
            OUTPUT CLOSE.
           
         END.
         WHEN FALSE THEN DO:
         END.
         
      END CASE.   
        
   END.
  
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
                   /* det ska bara finnas en mapp på servern*/
                  IF filnamn = "WTID" THEN OS-CREATE-DIR VALUE(webbdir + filnamn) NO-ERROR.
               END.   
               ELSE OS-CREATE-DIR VALUE(webbdir + filnamn) NO-ERROR.
            END.  
            statok = OS-ERROR.
            
            IF statok = 2 THEN DO:
               /*dir fanns*/
            END.
            ELSE IF statok NE 0 THEN DO:
               MESSAGE "Directory not created. System Error #" statok.
               DISPLAY webbdir + filnamn. 
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
                     iCOMP = iCOMP + 1.
                     MESSAGE curdir " Compile file: " + filnamn + " into " + savedir.
                     /*ALLA FILER I W-MAPPAR FÖR SERVER HAMNAR I SAMMA MAPP - WTID*/
                     IF mapptyp = "w" THEN DO:
                        OUTPUT TO VALUE(utfil) APPEND. 
                        IF felkoll = TRUE THEN PUT filnamn SKIP.
                        COMPILE VALUE(dirlist) SAVE INTO VALUE(savedir).
                        OUTPUT CLOSE.
                     END.
                     /*ALLA C MAPPAR, KLIENTPROGRAM HAMNAR I C-MAPPAR FÖR RESPTIV CAB FIL
                     VID KÖRNING AV WEBCLIENT PÅ SERVER KAN MAN FLYTTA ALLA PROGRAM TILL RÄTTA MAPPAR VIA  
                     wtid\VALDBSERVERMULTI.w*/
                     IF mapptyp = "c" THEN DO:
                         COMPILE VALUE(dirlist) SAVE INTO VALUE(webbdir + curdir).
                         
                     END.
                  END.   
               END.
               ELSE IF filnamn = "." THEN.           
               ELSE IF filnamn = ".." THEN.   
               ELSE IF filnamn MATCHES "*.i" OR filnamn MATCHES "*.r" THEN.  
               ELSE DO: 
                  IF attrlist = "D" THEN.
                  ELSE DO:  
                     /*FLYTTAR ANDRA TYPER AV FILER RÄTT*/  
                     IF mapptyp = "W" THEN DO:
                        OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).    
                     END.
                     IF mapptyp = "c" THEN DO:
                        OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(webbdir + curdir + "\" + filnamn).
                        
                     END.
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
         IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
            OS-CREATE-DIR VALUE(webbdir + "2Guru\") NO-ERROR.
            IF filnamn MATCHES '*Guru' THEN. 
            ELSE IF filnamn MATCHES '*Helpers' THEN.
            ELSE NEXT.
         END.  
         OS-CREATE-DIR VALUE(webbdir + "2Guru\") NO-ERROR.
         
         CREATE kompdir.
         ASSIGN
         kompdir.NIVA = antalniv
         kompdir.SERCHMAPPKOMP = searchdir + filnamn + "\"
         kompdir.SAVEMAPPKOMP = savedir  /* + filnamn + "\"*/
         kompdir.MAPPKOMP = filnamn.
         filnamn = "".
         
         OS-CREATE-DIR VALUE(webbdir + "2Guru\" + kompdir.MAPPKOMP) NO-ERROR.
         statok = OS-ERROR.
         
         IF statok = 2 THEN DO:
            /*dir fanns*/
         END.
         ELSE IF statok NE 0 THEN DO:
            MESSAGE "Directory not created. System Error #" statok.
            DISPLAY webbdir + kompdir.MAPPKOMP. 
            RETURN.
         END.
         
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
               
               OS-CREATE-DIR VALUE(webbdir + "2Guru\" + kompdir.MAPPKOMP + "\" + kompdirbuff.MAPPKOMP) NO-ERROR.
               statok = OS-ERROR.
               
               IF statok = 2 THEN DO:
                  /*dir fanns*/
               END.
               ELSE IF statok NE 0 THEN DO:
                  MESSAGE "Directory not created. System Error #" statok.
                  DISPLAY webbdir + kompdir.MAPPKOMP + "\" + kompdirbuff.MAPPKOMP. 
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
                  /*KOMPILERING CLS*/
                  iCOMP = iCOMP + 1.                  
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(webbdir + "2Guru\").
                  
                  OUTPUT CLOSE.
               END.                            
            END.
            ELSE IF filnamn MATCHES "*.p" THEN DO:
                /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn).
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                 
                  /*COMPILERING *.P*/
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(webbdir + "2Guru\" + kompdir.MAPPKOMP).
                  
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
                  /*FLYTTAR ANDRA FILER */
                  OS-COPY VALUE(dirlist) VALUE(webbdir + "2Guru\" + kompdir.MAPPKOMP).
                     
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
         IF mappkoll(filnamn) = TRUE THEN NEXT.
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
               IF mappkoll(filnamn) = TRUE THEN NEXT. 
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
         IF mappkoll(filnamn) = TRUE THEN NEXT.
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


/*FLYTTAR BILDER*/
guruwtidir = "\\" + pcvar + "\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = webbdir + "ctid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
/*FLYTTAR MANUALER*/
guruwtidir = "\\" + pcvar + "\delad\pro9\guru\ctid\Manualer\*.*".
wcguruwtidir = webbdir + "ctid\Manualer\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

/*KGMPILERAR EXCEL PROG MED RÄTT VERSION*/
IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO: 
   MESSAGE "Starta Controls på TOSHIBAN och vänta till den är färdig." SKIP 
   "Ikon Controlls" skip
   "Detta för bakåt kompatibelt för de cls program som berör Excel mm" SKIP 
   "se XCOMPWEBcontrols.p"
   VIEW-AS ALERT-BOX.
  
END.
RUN OPENDOC.P (utfil,"","",NO).
IF SUBSTRING(PROVERSION,1,2) = "10" THEN .
ELSE DO:
   DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
   DEFINE VARIABLE efel AS CHARACTER NO-UNDO.
   DEFINE VARIABLE TextPtr AS MEMPTR NO-UNDO.
   SET-SIZE(TextPtr) =  60000.
   MESSAGE "Vill du skicka mail ?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO UPDATE fragval AS LOGICAL.
   IF fragval = TRUE THEN DO: 
      RUN EPOSTMAPI.P (INPUT  "Martin.Johansson@one-nordic.se",
                    INPUT  "",
                    INPUT  "",
                    INPUT  "Ny version av Guru",
                    INPUT  TextPtr,                 
                    INPUT  "",
                    INPUT  "elpool.ume@elpool.se",
                    OUTPUT skick,
                    OUTPUT efel
                    ).
      SET-SIZE(TextPtr) = 0. 
      IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.
   END.
END.    
MESSAGE "Skriv in Realsedatum i GDPR åtgärder"
VIEW-AS ALERT-BOX.
utfil = "\\SERVER05\GuruProjekt\Fasta\GDPR\GDPR åtgärder.docx".

RUN OPENDOC.P (utfil,"","",NO).
/*XCOMPALLTZIP.P*/
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
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(25)" LABEL "File" NO-UNDO.
DEFINE VARIABLE filserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(60)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE webdate AS DATE NO-UNDO.
DEFINE VARIABLE iCOMP AS INTEGER NO-UNDO.
DEFINE VARIABLE serverdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE lager AS CHARACTER NO-UNDO.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/.
OS-DELETE VALUE(utfil) NO-ERROR.
searchdir = "C:\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/.
IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
   savedir = "C:\delad\PRO10\guru\komp10\".   /*Sökväg till de kompilerade filerna.*/.
   serverdir = "\\192.168.95.101\delad\PRO10\serverapp\server\".
END.   
lager = "\\elpool-kopior\e$\lager".
webdate = 01/01/89.

UPDATE webdate felkoll.                                                                                  
RUN mappkomp_UI (INPUT "C").
ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.
UPDATE webdate.                                                                                  
RUN mappkomp_UI (INPUT "W").
PROCEDURE  mappkomp_UI :
   DEFINE INPUT PARAMETER bb AS CHARACTER NO-UNDO.
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn BEGINS bb AND attrlist = "D" THEN DO: 
         /*Sparar alla mappnamn i en textsträng.*/
         IF cdirnames NE "" THEN DO:
            cdirnames = cdirnames + "," + filnamn.
         END.
         ELSE DO: 
            cdirnames = filnamn.
         END.
         numcdir = numcdir + 1.                                                                                        
      END.
   END.
   INPUT CLOSE. 
   /*Rensar tidigare innehåll i filen.*/
   
   mappsvar = TRUE.
   MESSAGE "Vill du kompilera alla mappar " + bb + "?" 
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE allasvar.
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
               SET filnamn dirlist attrlist.
               IF filnamn MATCHES "*.p" OR filnamn MATCHES "*.w"  THEN DO:
                  /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
                  FILE-INFO:FILE-NAME = filnamn.
                  IF FILE-INFO:FILE-MOD-DATE >= webdate THEN DO:
                     iCOMP = iCOMP + 1.
                     
                      MESSAGE curdir " Compile file: " + filnamn + " into " + savedir.
                      OUTPUT TO VALUE(utfil) APPEND. 
                      IF felkoll = TRUE THEN PUT filnamn SKIP.
                      /*MESSAGE "Compiled file: " + filnamn + "  ,Destination Directory: " + tempsavedir. */
                      COMPILE VALUE(dirlist) SAVE INTO VALUE(savedir).
                      OUTPUT CLOSE.
                      filserver = REPLACE(filnamn,".P",".r").
                      filserver = REPLACE(filserver,".w",".r").
                      OS-COPY VALUE(savedir + filserver) VALUE(serverdir + filserver). 
                      OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(lager + "\" + filnamn).            
                  END.
               END.
               ELSE IF filnamn MATCHES "*.i" THEN DO:           
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(lager + "\" + filnamn).  
               END.   
               ELSE IF filnamn MATCHES "*.wrx" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).            
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).            
               END.
               ELSE IF filnamn MATCHES "*.ocx" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.
               ELSE IF filnamn MATCHES "*.doc" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.
               ELSE IF filnamn MATCHES "*.xls" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.
               IF filnamn MATCHES "*.exe" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.               
            END.
            INPUT CLOSE.
         END.
      END.
      dirnr = dirnr + 1.
   END.
   DISPLAY iCOMP.
END PROCEDURE.

guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\PC112\delad\pro9\guru\komp9\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\PC112\delad\pro10\guru\komp10\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = serverdir + "bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
/*Öppnar upp loggfilen i word.*/
RUN OPENDOC.P (utfil,"","",NO).

/* 
1 SKAPA EN ZIP AV ALLT I KOMP10 //savedir 
DEN SKA NAMNGES //
2. FRÅGA OM DU VILL KÖRA NY SINGEL VERSION //logical (boolean)
3 OM JA STARTA SELF EXT
*/


PROCEDURE zippaALLT :
    DEFINE INPUT  PARAMETER ziptill AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER zipNamn  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER zipfiler  AS CHARACTER NO-UNDO.        
    
    DEFINE VARIABLE wzzip AS CHARACTER NO-UNDO.    
    
    DEFINE VARIABLE zipString AS CHARACTER NO-UNDO.
    wzzip = "C:\Program Files\WinZip\wzzip.exe". /*Länk till WinZip programmet med kommandot wzzip*/
    IF SEARCH(wzzip) = ? THEN wzzip = "C:\Program\WinZip\wzzip.exe".
    IF SEARCH(wzzip) = ? THEN RETURN.

    IF SEARCH(wzzip) = ? THEN RETURN.
    
    zipString = wzzip + " " + ziptill + zipNamn + " " + zipfiler.
    
    OS-COMMAND VALUE(zipString).
    
    
END PROCEDURE.


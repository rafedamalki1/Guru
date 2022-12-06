/*XCOMPALLT.P

define variable vvv as Start.ZipElpool no-undo.
vvv = new Start.ZipElpool().
vvv:MakeZip("c:\temp","c:\t.zip").

*/
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
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(58)" LABEL "File" NO-UNDO.
DEFINE VARIABLE filserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(60)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE webdate AS DATE NO-UNDO.
DEFINE VARIABLE iCOMP AS INTEGER NO-UNDO.
DEFINE VARIABLE serverdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE lager AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE kompdir NO-UNDO
   FIELD SERCHMAPPKOMP AS CHARACTER
   FIELD SAVEMAPPKOMP AS CHARACTER
   FIELD MAPPKOMP AS CHARACTER
   FIELD NIVA AS INTEGER
   FIELD KLAR AS LOGICAL
   INDEX MAPPKOMP NIVA SERCHMAPPKOMP MAPPKOMP.
DEFINE BUFFER kompdirbuff FOR kompdir.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/.
OS-DELETE VALUE(utfil) NO-ERROR.
searchdir = "C:\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/.
IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
   savedir = "C:\delad\PRO10\guru\komp10\".   /*Sökväg till de kompilerade filerna.*/.
   /*demok appserver*/
   serverdir = "\\webguru\delad\PRO10\serverapp\server\".
END.
IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO:
   savedir = "\\PC112\delad\PRO11\guru\komp11\".   /*Sökväg till de kompilerade filerna.*/.
   /*demok appserver*/
   serverdir = "\\webguru\delad\PRO11\serverapp\server\".
END.   
/*
lager = "\\Elpooweb-2\e\lager".
*/
webdate = 01/01/89.

UPDATE webdate felkoll.                                                                                  
RUN mappkomp_UI (INPUT "C").
ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.
UPDATE webdate.                                                                                  
RUN mappkomp_UI (INPUT "W").

ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.
UPDATE webdate. 
      
                                                                                 
IF SUBSTRING(PROVERSION,1,2) = "10" THEN.                                                                                 
else RUN clsmappar_UI.
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
                      /*
                      OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(lager + "\" + filnamn).     
                      */       
                  END.
               END.
               ELSE IF filnamn MATCHES "*.i" THEN DO:           
                  /*
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(lager + "\" + filnamn).
                  */  
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
               ELSE IF filnamn MATCHES "*.dotx" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.
               ELSE IF filnamn MATCHES "*.xls" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(savedir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.
               ELSE IF filnamn MATCHES "*.xlsx" THEN DO:
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(serverdir + filnamn).
               END.
               IF filnamn MATCHES "*.exe" THEN DO:
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
                  OS-COPY VALUE(tempsearchdir +  filnamn) VALUE(serverdir +  filnamn).
               END.     
               IF filnamn MATCHES "*.pdf" THEN DO:
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

PROCEDURE clsmappar_UI :
   EMPTY TEMP-TABLE kompdir NO-ERROR. 
   DEFINE VARIABLE antalniv AS INTEGER NO-UNDO.
   antalniv = 1.    
   MESSAGE "Vill du kompilera CLSmappar " "?" 
   VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE allasvar.
   IF allasvar = FALSE THEN RETURN. 
   searchdir = searchdir + "2GURU\".
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /* om filen är en mapp.*/
      IF attrlist = "D" THEN DO: 
         IF filnamn BEGINS "." THEN.
         ELSE IF filnamn MATCHES '*TesterTester*' THEN .
         ELSE DO: 
            CREATE kompdir.
            ASSIGN
            kompdir.NIVA = antalniv
            kompdir.SERCHMAPPKOMP = searchdir + filnamn + "\"
            kompdir.SAVEMAPPKOMP = savedir  /* + filnamn + "\"*/
            kompdir.MAPPKOMP = filnamn.
            filnamn = "".
            /*Sparar alla mappnamn i en textsträng.*/
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
            SET filnamn ^ attrlist.
            /*Kolla om filen är en mapp.*/
            IF attrlist = "D" THEN DO: 
               IF filnamn BEGINS "." THEN.
               ELSE IF filnamn MATCHES '*TesterTester*' THEN .
               ELSE DO: 
                  CREATE kompdirbuff.
                  ASSIGN
                  kompdirbuff.NIVA = antalniv + 1
                  kompdirbuff.SERCHMAPPKOMP = kompdir.SERCHMAPPKOMP + filnamn + "\"
                  kompdirbuff.SAVEMAPPKOMP = kompdir.SAVEMAPPKOMP /*+ filnamn + "\"*/
                  kompdirbuff.MAPPKOMP = filnamn.
                  filnamn = "".
                  /*Sparar alla mappnamn i en textsträng.*/
               END.   
            END.
         END.
         INPUT CLOSE.
      END.
      antalniv = antalniv + 1.      
   END.
   /*
  FOR EACH kompdir:
     DISPLAY kompdir.SERCHMAPPKOMP FORMAT "X(60)" kompdir.SAVEMAPPKOMP FORMAT "X(60)" kompdir.MAPPKOMP FORMAT "X(60)".
  END.
  */   
   mappsvar = TRUE.
   FOR EACH kompdir WHERE kompdir.KLAR = FALSE:
      /*Plockar ut namnet för aktuell mapp*/
      MESSAGE "Vill du kompilera mappen: " kompdir.SERCHMAPPKOMP "?" 
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE mappsvar.
      IF mappsvar = TRUE THEN DO:
                  /*
         OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP) NO-ERROR.
         */
         INPUT FROM OS-DIR(kompdir.SERCHMAPPKOMP) NO-ECHO.
         
        
         REPEAT:
            /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
            SET filnamn dirlist VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 attrlist.
            IF filnamn MATCHES "*.cls" THEN DO:
               /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
              FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn).
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(kompdir.SAVEMAPPKOMP).
                  OUTPUT CLOSE.
               END.                            
            END.
            IF filnamn MATCHES "*.p" THEN DO:
                /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn).
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                 
                  OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP) NO-ERROR.
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                  OUTPUT CLOSE.
               END.                            
            END.
            IF filnamn MATCHES "*.resx" THEN DO:
               kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                  /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP) NO-ERROR.   
               OS-COPY VALUE(dirlist) VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                                              
            END.  
            IF filnamn MATCHES "*.pdf" THEN DO:
               kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                  /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP) NO-ERROR.   
               OS-COPY VALUE(dirlist) VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                                              
            END.           
         END.
         INPUT CLOSE.
      END.
      kompdir.KLAR = TRUE.      
   END.
  
   DISPLAY iCOMP.
   
END PROCEDURE.

guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\PC112\delad\pro10\guru\komp10\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\PC112\delad\pro11\guru\komp11\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = serverdir + "bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).


guruwtidir = "\\PC112\delad\pro9\guru\ctid\Manualer\*.*".
wcguruwtidir = "\\PC112\delad\pro10\guru\komp10\Manualer\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

guruwtidir = "\\PC112\delad\pro9\guru\ctid\Manualer\*.*".
wcguruwtidir = "\\PC112\delad\pro11\guru\komp11\Manualer\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).




/*Öppnar upp loggfilen i word.*/

RUN OPENDOC.P (utfil,"","",NO).

 

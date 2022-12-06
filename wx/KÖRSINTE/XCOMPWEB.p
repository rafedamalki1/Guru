/*XCOMPWEB.P*/

/*************************************************************************
*  Programsyfte:  Kompilerar alla *.p och *.w filer som ligger i en C-mapp
*                 under 'searchdir' sökvägen. De kompilerade filerna hamnar
*                 under 'savedir' + den C-mapp filerna ligger i.
*
*  Exempelvis kan en kompileringsträng se ut så här:
*     COMPILE \\Pc112\delad\pro9\guru\Caonr\Emedd.w SAVE INTO
*                \\Server3\D\delad\PRO9\guruweb\Caonr. 
*
*  Resultatet av ovanstående kommandorad blir en .r-fil (Emedd.r) som hamnar
*  under \\Server3\D\delad\PRO9\guruweb\Caonr.  
*  Senast ändrad: 2003-02-13 (Elpool i Umeå AB, Mikael Eriksson )
**************************************************************************/
SESSION:SUPPRESS-WARNINGS = TRUE. 
DEFINE VARIABLE allasvar AS LOGICAL.
DEFINE VARIABLE mappsvar AS LOGICAL.
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE searchdir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE savedir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tempsearchdir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE wcguruwtidir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tempsavedir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cdirnames AS CHARACTER NO-UNDO.
DEFINE VARIABLE delfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE curdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE statok AS INTEGER NO-UNDO.
DEFINE VARIABLE numcdir AS INTEGER NO-UNDO.
DEFINE VARIABLE dirnr AS INTEGER NO-UNDO.
DEFINE VARIABLE webforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE webdate AS DATE NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(70)" LABEL "File" NO-UNDO.
DEFINE VARIABLE filnamnR AS CHARACTER NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(70)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iCOMP AS INTEGER NO-UNDO.
DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE net AS CHARACTER NO-UNDO.
DEFINE VARIABLE GuruComp AS LOGICAL FORMAT "guru/ekg" LABEL "Guru eller EKG" NO-UNDO.
GuruComp  = true.
DEFINE TEMP-TABLE kompdir NO-UNDO
   FIELD SERCHMAPPKOMP AS CHARACTER
   FIELD SAVEMAPPKOMP AS CHARACTER
   FIELD MAPPKOMP AS CHARACTER
   FIELD NIVA AS INTEGER
   INDEX MAPPKOMP NIVA SERCHMAPPKOMP MAPPKOMP.
DEFINE BUFFER kompdirbuff FOR kompdir.
REPEAT:
   IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
   IF LDBNAME(1) = ? THEN LEAVE.
END.
SESSION:DEBUG-ALERT = YES.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/
searchdir = "\\PC112\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/
savedir = "\\webguru\delad\PRO9\guruweb".   /*Sökväg till de kompilerade filerna.*/
IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
   savedir = "\\webguru\delad\PRO10\guruweb".   /*Sökväg till de kompilerade filerna.*/
END.
IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO:
   savedir = "C:\DELAD\PRO11\GURUWEB".   /*Sökväg till de kompilerade filerna.*/
   update GuruComp.
   if GuruComp = false then  savedir = "C:\DELAD\PRO11\EKGWEB".
END.   
net = "\".
/*
UPDATE net.
*/
savedir = savedir + net.
/*
IF net = "net\" THEN savedir = "\\ELPOOWEB-2\delad\pro" + SUBSTRING(PROVERSION,1,2) + "\GURUWEBNET\".
*/
webdate = 01/01/89.
UPDATE webdate felkoll.
RUN mappkomp_UI.
ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1.
UPDATE webdate. 
IF SUBSTRING(PROVERSION,1,2) = "10" THEN RUN clsmappar_UI.                                                                                 
else RUN clsmappar_UI.
 
PROCEDURE mappkomp_UI : 
   OS-COPY VALUE(searchdir + "nyheter.txt") VALUE(savedir + "\" + "nyheter.txt").
   OS-COPY VALUE(searchdir + "gurumed.txt") VALUE(savedir + "\" + "gurumed.txt").
   /*
   IF webforetag = "lule" THEN savedir = "\\acer-bärbar\luleweb\".
   */
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn BEGINS "c" AND attrlist = "D" THEN DO: 
         /*Sparar alla mappnamn i en textsträng.*/
         IF cdirnames NE "" THEN DO:
            cdirnames = cdirnames + "," + filnamn.
         END.
         ELSE DO: 
            cdirnames = filnamn.
         END.
         numcdir = numcdir + 1.
         tempsavedir = savedir + filnamn.
         /*Skapar en ny mapp för varje funnen 'c'-mapp*/
         OS-CREATE-DIR VALUE(tempsavedir) NO-ERROR.
         statok = OS-ERROR.
         
         IF statok = 2 THEN DO:
            /*dir fanns*/
         END.
         ELSE IF statok NE 0 THEN DO:
            MESSAGE "Directory not created. System Error #" statok.
            RETURN.
         END.                                                                                 
         
      END.
   END.
   INPUT CLOSE. 
   /*Rensar tidigare innehåll i filen.*/
   OS-DELETE VALUE(utfil).
   mappsvar = TRUE.
   OUTPUT TO VALUE(utfil) APPEND.
   PUT STRING(TIME,"HH:MM:SS") AT 1 SKIP.
   OUTPUT CLOSE.
   MESSAGE "Vill du kompilera alla mappar?"       
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
      tempsearchdir = searchdir + curdir + "\".
      tempsavedir = savedir + curdir.
      /*Sätter denna mapp som input*/
      IF allasvar = FALSE THEN DO:
         MESSAGE "Vill du kompilera mappen: " + curdir + "?" 
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE mappsvar.
      END.
      IF mappsvar = TRUE THEN DO:
         /*
         INPUT FROM OS-DIR(tempsavedir) NO-ECHO.
         REPEAT:
            /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
            SET filnamn dirlist attrlist.
            IF filnamn MATCHES "*.r" THEN DO:
               OS-DELETE VALUE(dirlist).
            END.
         END.
         INPUT CLOSE.
         */
         INPUT FROM OS-DIR(tempsearchdir) NO-ECHO.
         REPEAT:
            /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
            SET filnamn dirlist attrlist.
            IF filnamn MATCHES "*.p" OR filnamn MATCHES "*.w"  THEN DO: 
               /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               FILE-INFO:FILE-NAME = filnamn.
               IF FILE-INFO:FILE-MOD-DATE >= webdate THEN DO:
                  iCOMP = iCOMP + 1.
                  message "Compile file: " + filnamn + " into directory " + tempsavedir.
                  OUTPUT TO VALUE(utfil) APPEND.
                   /*MESSAGE "Compiled file: " + filnamn + "  ,Destination Directory: " + tempsavedir. */
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(tempsavedir).
                  filnamnR = REPLACE(filnamn,".P",".r").
                  filnamnR = REPLACE(filnamnR,".w",".r"). 
                 
                  IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO:
                     guruwtidir = "c:\DELAD\PRO11\GURU\KOMP11\" + filnamnR.
                     
                     OS-COPY  VALUE(tempsavedir + "\" + filnamnR) VALUE(guruwtidir).
                  END.   
                  
                  
                  OUTPUT CLOSE.
               END.
            END.
            ELSE IF filnamn MATCHES "*.wrx" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).            
            END.
            ELSE IF filnamn MATCHES "*.ocx" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
            ELSE IF filnamn MATCHES "*.doc" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
            ELSE IF filnamn MATCHES "*.dotx" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
            ELSE IF filnamn MATCHES "*.xls" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
            ELSE IF filnamn MATCHES "*.xlsx" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
            ELSE IF filnamn MATCHES "*.exe" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
            ELSE IF filnamn MATCHES "*.sdf" THEN DO:
               OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
            END.
         END.
         INPUT CLOSE.
      END.
      dirnr = dirnr + 1.
   END.
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
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF attrlist = "D" THEN DO: 
         IF filnamn BEGINS "." THEN.
         ELSE DO: 
            IF filnamn MATCHES '*TesterTester*' THEN NEXT.
            IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
               IF filnamn MATCHES '*Guru' THEN. 
               ELSE IF filnamn MATCHES '*Helpers' THEN.
               ELSE NEXT.
            END.     
            CREATE kompdir.
            ASSIGN
            kompdir.NIVA = antalniv
            kompdir.SERCHMAPPKOMP = searchdir + filnamn + "\"
            kompdir.SAVEMAPPKOMP = savedir + "2Guru\"  /*+ filnamn + "\"*/
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
            /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
            IF attrlist = "D" THEN DO: 
               IF filnamn BEGINS "." THEN.
               ELSE DO: 
                  IF filnamn MATCHES '*TesterTester*' THEN NEXT.
                  IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
                     IF filnamn MATCHES '*Guru' THEN. 
                     ELSE IF filnamn MATCHES '*Helpers' THEN.
                     ELSE NEXT.
                  END.    
                  /*
                  IF filnamn MATCHES '*TesterTester*' THEN .
                  ELSE DO:
                     */
                     CREATE kompdirbuff.
                     ASSIGN
                     kompdirbuff.NIVA = antalniv + 1
                     kompdirbuff.SERCHMAPPKOMP = kompdir.SERCHMAPPKOMP + filnamn + "\"
                     kompdirbuff.SAVEMAPPKOMP = kompdir.SAVEMAPPKOMP  /*+ filnamn + "\"*/
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
   mappsvar = TRUE.
   FOR EACH kompdir:
      /*Plockar ut namnet för aktuell mapp*/
      MESSAGE "Vill du kompilera mappen: " kompdir.SERCHMAPPKOMP "?" 
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE mappsvar.
      IF mappsvar = TRUE THEN DO:
       
         OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP) NO-ERROR.
         
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
                  IF SUBSTRING(PROVERSION,1,2) = "10" THEN DO:
                     IF filnamn = "DatabaseManager.cls" THEN NEXT.
                     IF filnamn = "Module.cls" THEN NEXT.
                     IF filnamn = "ModuleManager.cls" THEN NEXT.
                     IF filnamn = "Root.cls" THEN NEXT.  
                     IF filnamn = "Window.cls" THEN NEXT.
                     IF filnamn = "WindowManager.cls" THEN NEXT.
                     IF filnamn = "DVLink.cls" THEN NEXT.
                     IF filnamn = "Functions.cls" THEN NEXT.
                     
                  END.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP) NO-ERROR.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(kompdir.SAVEMAPPKOMP).
                  filnamnR = REPLACE(filnamn,".cls",".r").
                  IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO:
                     guruwtidir = "c:\DELAD\PRO11\GURU\KOMP11\" + kompdir.MAPPKOMP.
                     OS-COPY  VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP + filnamnR) VALUE(guruwtidir + filnamnR).
                  END.   
                  OUTPUT CLOSE.
               END.                            
            END.
            IF filnamn MATCHES "*.p" THEN DO:
                /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
              
               FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn).
               
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + kompdir.SAVEMAPPKOMP.
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                  OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP) NO-ERROR.
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                  filnamnR = REPLACE(filnamn,".P",".r").
                  IF SUBSTRING(PROVERSION,1,2) = "11" THEN DO:
                     guruwtidir = "c:\DELAD\PRO11\GURU\KOMP11\" + kompdir.MAPPKOMP.
                     OS-COPY  VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP + filnamnR) VALUE(guruwtidir + filnamnR).
                  END.
                  
                  OUTPUT CLOSE.
               END.                            
            END.
            IF filnamn MATCHES "*.resx" OR 
            filnamn MATCHES "*.wrx" OR
            filnamn MATCHES "*.ocx" OR 
            filnamn MATCHES "*.doc" OR  
            filnamn MATCHES "*.xls" OR 
            filnamn MATCHES "*.xlsx" OR 
            filnamn MATCHES "*.exe" OR 
            filnamn MATCHES "*.sdf" OR 
            filnamn MATCHES "*.pdf" 
            THEN DO:
               kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ).
               OS-CREATE-DIR VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP) NO-ERROR. 
               OS-COPY VALUE(dirlist) VALUE(kompdir.SAVEMAPPKOMP + kompdir.MAPPKOMP).
                                             
            END.  
                       
         END.
         INPUT CLOSE.
      END. 
      DELETE kompdir.       
   END.
  
   DISPLAY iCOMP.
   
END PROCEDURE.

/*bilder till webclient*/ 

guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "C:\DELAD\PRO11\GURUWEB\ctid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "C:\DELAD\PRO11\EKGWEB\ctid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\webguru\delad\PRO10\guruweb\ctid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

guruwtidir = "\\PC112\delad\pro9\guru\ctid\Manualer\*.*".
wcguruwtidir = "\\webguru\delad\PRO10\guruweb\ctid\Manualer\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).

guruwtidir = "\\PC112\delad\pro9\guru\ctid\Manualer\*.*".
wcguruwtidir = "C:\DELAD\PRO11\GURUWEB\ctid\Manualer\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
/*
 
 
DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
conappvar = "-AppService Awebcom -H 192.168.95.22 -S 5222".
CREATE SERVER Guru.Konstanter:apphand.
appcon = Guru.Konstanter:apphand:CONNECT(conappvar) NO-ERROR.
IF Guru.Konstanter:appcon = TRUE THEN RUN XCOMPWEBcontrols.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
ELSE DO:
   MESSAGE "Kan inte kompilera controls!"
   VIEW-AS ALERT-BOX.
END.     
*/

MESSAGE "Starta Controls på TOSHIBAN och vänta till den är färdig." skip
"Om nej så flyttas inte programmen till web wtid och web controls"
VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE allasvar.
   CASE allasvar:
      WHEN TRUE THEN DO:
         guruwtidir = "\\PC112\DELAD\PRO11\GURU\KOMP11".
         wcguruwtidir = "\\PC112\delad\PRO11\guruweb\wtid".
         guruwtidir = 'robocopy "' + guruwtidir + '" "' + wcguruwtidir + '" *.* /mir'.
         OS-COMMAND SILENT VALUE(guruwtidir).
                  
      END.
      WHEN FALSE THEN DO:
      END.
      
   END CASE.   



/*
guruwtidir = "\\PC112\delad\pro9\guru\KOMP11\Manualer\*.*".
wcguruwtidir = "C:\DELAD\PRO11\GURUWEB\ctid\Manualer\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
*/
/*
guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\webguru\delad\PRO10\guruwebnet\ctid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
*/

/*Öppnar upp loggfilen i word.*/
DISPLAY iCOMP.
RUN OPENDOC.P (utfil,"","",NO).

/*XCOMPWEBDATE.P*/

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
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(25)" LABEL "File" NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(60)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
REPEAT:
   IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
   IF LDBNAME(1) = ? THEN LEAVE.
END.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/
searchdir = "\\PC112\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/
savedir = "\\192.168.95.101\delad\PRO9\guruweb\".   /*Sökväg till de kompilerade filerna.*/
UPDATE webdate.
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
MESSAGE "Vill du kompilera alla mappar?" 
   VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE allasvar.
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
      INPUT FROM OS-DIR(tempsavedir) NO-ECHO.
      REPEAT:
         /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
         SET filnamn dirlist attrlist.
         IF filnamn MATCHES "*.r" THEN DO:
            OS-DELETE VALUE(dirlist).
         END.
      END.
      INPUT CLOSE.
      INPUT FROM OS-DIR(tempsearchdir) NO-ECHO.
      REPEAT:
         /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
         SET filnamn dirlist attrlist.
         IF filnamn MATCHES "*.p" OR filnamn MATCHES "*.w"  THEN DO:
            /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
            FILE-INFO:FILE-NAME = filnamn.
            IF FILE-INFO:FILE-MOD-DATE >= webdate THEN DO:
               MESSAGE "Compile file: " + filnamn + " into directory " + tempsavedir.
               OUTPUT TO VALUE(utfil) APPEND.
                /*MESSAGE "Compiled file: " + filnamn + "  ,Destination Directory: " + tempsavedir. */
               COMPILE VALUE(dirlist) SAVE INTO VALUE(tempsavedir).
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
         ELSE IF filnamn MATCHES "*.xls" THEN DO:
            OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(tempsavedir + "\" + filnamn).
         END.
      END.
      INPUT CLOSE.
   END.
   dirnr = dirnr + 1.
END.
/*demok*/
guruwtidir = "\\PC112\delad\pro9\guru\komp9\*.*".
wcguruwtidir = "\\192.168.95.101\delad\PRO9\guru\wtid\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".

OS-COMMAND SILENT VALUE(guruwtidir).
guruwtidir = "\\PC112\delad\pro9\guru\komp9\bilder\*.*".
wcguruwtidir = "\\192.168.95.101\delad\PRO9\guru\wtid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
guruwtidir = "\\PC112\delad\pro9\guru\ctid\bilder\*.*".
wcguruwtidir = "\\192.168.95.101\delad\PRO9\guruweb\ctid\bilder\".
IF webforetag = "lule" THEN  wcguruwtidir = "\\acer-bärbar\luleweb\ctid\bilder\".
guruwtidir = "xcopy " + guruwtidir + " " + wcguruwtidir + " /c/d/f/s/e/y ".
OS-COMMAND SILENT VALUE(guruwtidir).
/*Öppnar upp loggfilen i word.*/
RUN OPENDOC.P (utfil,"","",NO).

/*XCOMPGRAN.P*/

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
DEFINE VARIABLE cdirnames AS CHARACTER NO-UNDO.
DEFINE VARIABLE delfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE curdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE statok AS INTEGER NO-UNDO.
DEFINE VARIABLE numcdir AS INTEGER NO-UNDO.
DEFINE VARIABLE dirnr AS INTEGER NO-UNDO.
DEFINE VARIABLE webforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(25)" LABEL "File" NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(60)" LABEL "Directory" NO-UNDO.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/.
OS-DELETE VALUE(utfil) NO-ERROR.
searchdir = "\\PC112\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/.
savedir = "\\pc112\delad\PRO9\ckomp\".   /*Sökväg till de kompilerade filerna.*/.
RUN mappkomp_UI (INPUT "C").
ASSIGN
cdirnames = ""
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.doc" /*Logfil för kompilering.*/
searchdir = "\\PC112\delad\pro9\guru\"  /*Sökväg till de filer som ska kompileras.*/
savedir = "\\pc112\delad\PRO9\wkomp\".   /*Sökväg till de kompilerade filerna.*/
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
   MESSAGE "Vill du kompilera alla mappar" + bb + "?" 
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
                  
                  MESSAGE curdir " Compile file: " + filnamn + " into " + savedir.
                  OUTPUT TO VALUE(utfil) APPEND.
                   /*MESSAGE "Compiled file: " + filnamn + "  ,Destination Directory: " + tempsavedir. */
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(savedir).
                  OUTPUT CLOSE.
               END.
               ELSE IF filnamn MATCHES "*.wrx" THEN DO:
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(savedir + "\" + filnamn).            
               END.
               ELSE IF filnamn MATCHES "*.ocx" THEN DO:
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(savedir + "\" + filnamn).
               END.
               ELSE IF filnamn MATCHES "*.doc" THEN DO:
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(savedir + "\" + filnamn).
                  
               END.
               ELSE IF filnamn MATCHES "*.xls" THEN DO:
                  OS-COPY VALUE(tempsearchdir + "\" + filnamn) VALUE(savedir + "\" + filnamn).
               END.
            END.
            INPUT CLOSE.
         END.
      END.
      dirnr = dirnr + 1.
   END.
END PROCEDURE.
/*Öppnar upp loggfilen i word.*/
RUN OPENDOC.P (utfil,"","",NO).


/*Rensaelpoolkopior.p*/
/* körs ej längre  ersätts av SFTPELPOOLGURU.P*/
/*se även Rensawww2.p */
/*Anders Olsson Elpool i Umeå AB  17 aug 2021 10:37:39 
 körs inte
 */
{AMERICANEUROPEAN.I}
DEFINE TEMP-TABLE filtemp NO-UNDO
FIELD KOPDIR AS CHARACTER
FIELD KOPFIL AS CHARACTER
FIELD NEWNAME  AS CHARACTER.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE datoruser AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE cvnr AS CHARACTER NO-UNDO.
RUN INLOAPI.P (OUTPUT datoruser, OUTPUT outdatornamn).
regdatum = TODAY. 
RUN  REGVEC.P.
cvnr = "V" + SUBSTRING(STRING(regvnr,"999"),2,2).
prognamnque = "c:\delad\PRO11S\autotid.txt". 
prognamnque2 = "C:\delad\PRO11S\autotidkop.txt".
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF DAY(TODAY) = 28 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
/*
OS-DELETE "C:\DELAD\pro9\guru\MOBILE\WRK\pasguru\openedge\wtid\" RECURSIVE.
OS-DELETE "C:\DELAD\pro9\guru\MOBILE\wrk_oemgmt\GURUKOPIA\" RECURSIVE.
*/
IF outdatornamn = "SERVER05" THEN.
ELSE DO:
   IF outdatornamn = "PC122" THEN DO:
     RUN AUTOELPSALJ.p.
     RUN ZIPGURUMAPP.p.
   END.
END.   
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
/*Flyttar PCbackupp till ftp*/ 
IF outdatornamn = "PC122" THEN QUIT.
IF WEEKDAY(TODAY) = 5 THEN DO:
  
   RUN backuppkoll_UI (INPUT "\\PC112\BckPC122\PC122c\",INPUT "pc122c" + cvnr)   .  /*Sökväg till de filer*/
   RUN backuppkoll_UI (INPUT "\\PC112\BckServer05\Server05c\",INPUT "server05" + cvnr).
   RUN backuppkoll_UI (INPUT "\\PC112\BckPC218\PC218c\",INPUT "pc218c" + cvnr).
   /*
   RUN backuppkoll_UI (INPUT "\\BACKUPSTATION1\Volume_1\pc112c\",INPUT "pc112c" + cvnr)   .  /*Sökväg till de filer*/
   RUN backuppkoll_UI (INPUT "\\BACKUPSTATION2\Volume_1\server04\",INPUT "server05" + cvnr).
   RUN backuppkoll_UI (INPUT "\\BACKUPSTATION2\Volume_2\pc208c\",INPUT "pc208c" + cvnr).
   */
   RUN BackFlytt.
END.
ELSE DO: 
   DEFINE VARIABLE  sourcefilename AS CHARACTER NO-UNDO.
   sourcefilename = "\\SERVER05\ftp\TillBerget\".
   OS-DELETE VALUE(sourcefilename) RECURSIVE.
   OS-CREATE-DIR VALUE(sourcefilename).  
   
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT "delete TillBerget " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.

   

/*
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION1\Volume_2\pc112d\").
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION1\Volume_2\pc120d\").
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION1\Volume_2\pc208d\").
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION2\Volume_1\Webbguru\").
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION2\Volume_1\server04\").
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION2\Volume_2\pc120c\").
RUN backuppkoll_UI (INPUT "\\BACKUPSTATION2\Volume_2\pc208c\").

RUN backuppkoll_UI (INPUT "\\BACKUPST1VIT\Volume_1\pc112c\").  /*Sökväg till de filer*/
RUN backuppkoll_UI (INPUT "\\BACKUPST1VIT\Volume_2\pc112d\").
RUN backuppkoll_UI (INPUT "\\BACKUPST1VIT\Volume_2\pc120d\").
RUN backuppkoll_UI (INPUT "\\BACKUPST1VIT\Volume_2\pc208d\").
/*
RUN backuppkoll_UI (INPUT "\\BACKUPST2\Volume_1\Webbguru\").
*/
RUN backuppkoll_UI (INPUT "\\BACKUPST2VIT\Volume_1\server04\").
RUN backuppkoll_UI (INPUT "\\BACKUPST2VIT\Volume_2\pc120c\").
RUN backuppkoll_UI (INPUT "\\BACKUPST2VIT\Volume_2\pc208c\").
*/
{EUROPEANAMERICAN.I}
QUIT.
PROCEDURE BackFlytt :
   DEFINE VARIABLE ftpdir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dirlist AS CHARACTER NO-UNDO.
   DEFINE VARIABLE serverwtidir AS CHARACTER NO-UNDO.
   /*flyttar*/
   FOR EACH filtemp WHERE NO-LOCK:
      dirlist = filtemp.KOPDIR + filtemp.KOPFIL.
      serverwtidir = "\\SERVER05\ftp\TillBerget\" + filtemp.NEWNAME.
      IF outdatornamn = "SERVER05" THEN serverwtidir = "C:\inetpub\ftproot\TillBerget\" + filtemp.NEWNAME. 
      
      OS-COPY VALUE(dirlist) VALUE(serverwtidir).  
      /*
      ftpdir = 'robocopy "' + dirlist + '" "' + serverwtidir.
      ftpdir = 'robocopy "' + dirlist + '" "' + serverwtidir + '" *.* /E'.
      OS-COPY VALUE(dirlist) VALUE(serverwtidir).  
      
      OS-COMMAND SILENT VALUE(ftpdir).
      */
   END.
END PROCEDURE.
PROCEDURE backuppkoll_UI :
   DEFINE INPUT  PARAMETER  searchdir AS CHARACTER NO-UNDO. 
   DEFINE INPUT  PARAMETER ftpname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kolldat AS DATE NO-UNDO. 
   DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(70)" LABEL "File" NO-UNDO.
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE kolldatvar AS CHARACTER NO-UNDO.
   IF WEEKDAY(TODAY) = 5 THEN.
   ELSE RETURN.
   kolldat = ?.
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn MATCHES "*.MRIMG" THEN DO: 
         FILE-INFO:FILE-NAME = searchdir + filnamn.
         /*
         kolldatvar = STRING(FILE-INFO:FILE-MOD-DATE).
         */
         IF kolldat = ? THEN DO: 
            kolldat = FILE-INFO:FILE-MOD-DATE.
            FIND FIRST filtemp WHERE filtemp.KOPDIR = searchdir NO-LOCK NO-ERROR.
            IF NOT AVAILABLE filtemp THEN DO:
               CREATE filtemp.
            END.   
            ASSIGN
            filtemp.KOPDIR = searchdir
            filtemp.KOPFIL = filnamn.
            filtemp.NEWNAME = ftpname + ".MRIMG".
         END.    
         IF kolldat < FILE-INFO:FILE-MOD-DATE THEN  DO:
            kolldat = FILE-INFO:FILE-MOD-DATE.
            FIND FIRST filtemp WHERE filtemp.KOPDIR = searchdir NO-LOCK NO-ERROR.
            IF NOT AVAILABLE filtemp THEN DO:
               CREATE filtemp.
            END.   
            ASSIGN
            filtemp.KOPDIR = searchdir
            filtemp.KOPFIL = filnamn.
            filtemp.NEWNAME = ftpname + ".MRIMG".        
         END.                      
      END.
   END. 
   INPUT CLOSE.
   OUTPUT TO "c:\protemp11\filer.txt" APPEND.
   PUT UNFORMATTED TODAY  " "  searchdir SKIP.
   OUTPUT CLOSE.
   
   /*tar bort */
   /*
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn MATCHES "*.MRIMG" THEN DO: 
         FILE-INFO:FILE-NAME = searchdir + filnamn.
         IF FILE-INFO:FILE-MOD-DATE < TODAY - 24 THEN DO:
            IF kolldat > FILE-INFO:FILE-MOD-DATE THEN DO:
               OUTPUT TO "c:\protemp11\filer.txt" APPEND.
               PUT UNFORMATTED searchdir + filnamn " Tas bort" SKIP. 
               OUTPUT CLOSE.
               OS-DELETE VALUE(searchdir + filnamn) NO-ERROR.               
            END.   
         END.
         ELSE DO:
            OUTPUT TO "c:\protemp11\filer.txt" APPEND.
            PUT UNFORMATTED searchdir + filnamn " Sparas " SKIP. 
            OUTPUT CLOSE.
         END.      
      END.
   END.
   INPUT CLOSE.
   */      
END PROCEDURE.
 
/*
MESSAGE SESSION:PIXELS-PER-ROW 
SESSION:PIXELS-PER-ROW
VIEW-AS ALERT-BOX.
I-ppc = SESSION:PIXELS-PER-COL
I-ppr = SESSION:PIXELS-PER-ROW
I-multc = DECIMAL(I-ppc) / 8.0
I-multr = DECIMAL(I-ppr) / 24.0.
FONT-TABLE:GET-TEXT-WIDTH-PIXELS.


oForm:AutoScaleDimensions = NEW System.Drawing.SizeF(96, 96). 
oForm:AutoScaleMode = System.Windows.Forms.AutoScaleMode:Dpi.
*/       

/*FTPDELFILE_OSC.p*/
DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
  
DEFINE INPUT PARAMETER motagandemaskin AS CHARACTER NO-UNDO.  /*ftp maskin */
DEFINE INPUT PARAMETER portremot AS INTEGER NO-UNDO.  /*ftp maskin */
DEFINE INPUT PARAMETER remotLogin  AS CHARACTER NO-UNDO.  /*login*/               
DEFINE INPUT PARAMETER remotpasswd AS CHARACTER NO-UNDO.  /*lösen*/
DEFINE INPUT PARAMETER motagandekat AS CHARACTER NO-UNDO.  /*underkatalog */
DEFINE INPUT PARAMETER motagandeextra AS CHARACTER NO-UNDO.  /*extra kommandon som ska köras, tex browsa ner i strukturen */
DEFINE INPUT PARAMETER ftpcommando_filnamn  AS CHARACTER NO-UNDO.  /* namn på ftpfil */ 
DEFINE INPUT PARAMETER remotfilnamn AS CHARACTER NO-UNDO. /* Filen på ftpservern */
DEFINE INPUT PARAMETER binardata AS LOGICAL NO-UNDO. /* Binärdata */

DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.

DEFINE STREAM ftpst.
DEFINE VARIABLE vl_command_line AS CHAR NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE vl_filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_slask AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_tag AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_status AS INTEGER NO-UNDO.
DEFINE VARIABLE antali AS INTEGER.

IF INDEX(motagandemaskin,":") > 0 THEN DO:
   portremot = INTEGER(SUBSTRING(motagandemaskin,INDEX(motagandemaskin,":") + 1)).
   motagandemaskin = SUBSTRING(motagandemaskin,1,INDEX(motagandemaskin,":") - 1).
END.   
  
/* Del */
OUTPUT STREAM ftpst TO VALUE(ftpcommando_filnamn + ".ftp").

PUT STREAM ftpst UNFORMATTED "open " motagandemaskin + " " + STRING(portremot) SKIP.
PUT STREAM ftpst UNFORMATTED remotLogin skip
                       remotpasswd skip.
                       
IF motagandekat <> "" THEN
   PUT STREAM ftpst UNFORMATTED "cd " + motagandekat SKIP.
   
IF motagandeextra <> "" THEN DO:
   DO cnt = 1 TO NUM-ENTRIES(motagandeextra):
      PUT STREAM ftpst UNFORMATTED ENTRY(cnt,motagandeextra) SKIP.
   END. 
END.

IF binardata THEN
   PUT STREAM ftpst UNFORMATTED "binary" SKIP.   
   
PUT STREAM ftpst UNFORMATTED "del " remotfilnamn SKIP.
PUT STREAM ftpst UNFORMATTED "bye" SKIP.

OUTPUT STREAM ftpst CLOSE.

vl_command_line = "ftp -s:" + ftpcommando_filnamn + ".ftp -i >" + ftpcommando_filnamn + ".out". 
/*** Kör skriptet som du nyss byggt ihop ***/

OS-COMMAND  VALUE(vl_command_line).

RUN FTPCHECK_OSC.p (ftpcommando_filnamn + ".out", FALSE, OUTPUT TABLE felmeddtemp). /* Läser loggfilen och returnerar felmedd */
/* RUN FTPLOG_OSC.p (vl_mall, "GET", vl_status). Ifall man vill ha ett program som visar upp loggar skapar vi en tabell med status på ftp */

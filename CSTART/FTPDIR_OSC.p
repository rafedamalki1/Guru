/*FTPDIR_OSC.p*/
DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.
/*DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.

*/

DEFINE INPUT PARAMETER motagandemaskin AS CHARACTER NO-UNDO.  /*ftp maskin */
DEFINE INPUT PARAMETER portremot AS INTEGER NO-UNDO.  /*ftp maskin */
DEFINE INPUT PARAMETER remotLogin  AS CHARACTER NO-UNDO.  /*login*/               
DEFINE INPUT PARAMETER remotpasswd AS CHARACTER NO-UNDO.  /*l�sen*/
DEFINE INPUT PARAMETER motagandekat AS CHARACTER NO-UNDO.  /*underkatalog */
DEFINE INPUT PARAMETER motagandeextra AS CHARACTER NO-UNDO.  /*extra kommandon som ska k�ras, tex browsa ner i strukturen */
DEFINE INPUT PARAMETER ftpcommando_filnamn  AS CHARACTER NO-UNDO.  /* namn p� ftpfil */ 
DEFINE INPUT PARAMETER filurval AS CHARACTER NO-UNDO. /* Om man vill filtrera ut filer att lista */
DEFINE INPUT PARAMETER dirformat  AS INTEGER NO-UNDO.  /* hur f�r vi tillbaka diren? */

DEFINE OUTPUT PARAMETER TABLE FOR provag.

DEFINE STREAM ftpst.
DEFINE VARIABLE vl_command_line AS CHAR NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE vl_filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_slask AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_tag AS CHARACTER NO-UNDO.

IF INDEX(motagandemaskin,":") > 0 THEN DO:
   portremot = INTEGER(SUBSTRING(motagandemaskin,INDEX(motagandemaskin,":") + 1)).
   motagandemaskin = SUBSTRING(motagandemaskin,1,INDEX(motagandemaskin,":") - 1).
END. 

IF filurval = "" THEN
   filurval = "*.*".

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

PUT STREAM ftpst UNFORMATTED "dir" SKIP.
PUT STREAM ftpst UNFORMATTED "bye" SKIP.

OUTPUT STREAM ftpst CLOSE.

vl_command_line = "ftp -i -s:" + ftpcommando_filnamn + ".ftp " + " >" + ftpcommando_filnamn + ".out".
   
/*** K�r skriptet som du nyss byggt ihop ***/



OS-COMMAND SILENT VALUE(vl_command_line).

/*** Kolla status p� outputfilen ***/

INPUT FROM VALUE(ftpcommando_filnamn + ".out").

/* H�r kan man f� olika format p� resultatet beroende p� vilken ftp-server man anropar, s� detta beh�ver byggas p� allteftersom fler l�ggs till. Skicka in vilken det �r om det �r annat �n default */
CASE dirformat:
   WHEN ? OR WHEN 0 THEN
   DO:  
        REPEAT:
            IMPORT vl_tag     vl_slask vl_slask    vl_slask vl_slask vl_slask vl_slask vl_slask vl_filename.
                  /* -rw-r--r-- 1         ftp         ftp      8        Jun      08       14:07    DAGJ11234545.txt */
   
            if vl_filename = "" THEN
                NEXT.
            ELSE
            DO:
                /*** H�r kan man filtrera ut om man m�ste skilja ut olika generationer av filer och d� vill man oftast ta den f�rsta generationen, man kan �ven filtrera ut p� andra kolumner i filen  */
                IF vl_tag = "-rw-r--r--" AND vl_filename MATCHES filurval THEN /* Vi b�rjar med att lista alla filer oavsett */
                DO:
                    CREATE provag.
                    ASSIGN
                    provag.VAGNR = 0 /* Inneh�ller inga kataloger i filnamnet */
                    provag.VAG = vl_filename.
                                        
                END.
            END.
        END.    
    END.
   
END /* CASE */.

/******** EXEMPEL *****

ftp> open www.guruonweb.se 21
Connected to www.guruonweb.se.
220-FileZilla Server 0.9.60 beta
220-written by Tim Kosse (Tim.Kosse@gmx.de)
220 Please visit https://filezilla-project.org/
202 UTF8 mode is always enabled. No need to send this command.
User (www.guruonweb.se:(none)): 
331 Password required for elpao

230 Logged on
ftp> cd elpao
250 CWD successful. "/elpao" is current directory.
ftp> dir
200 Port command successful
150 Opening data channel for directory listing of "/elpao"
-rw-r--r-- 1 ftp ftp          62976 May 17 10:13 36737.xls
-rw-r--r-- 1 ftp ftp       40771584 May 25 08:47 AvCad.mdb
-rw-r--r-- 1 ftp ftp          21999 Jun 01 15:12 c++error.png
-rw-r--r-- 1 ftp ftp              8 Jun 08 14:07 DAGJ11234545.txt
-rw-r--r-- 1 ftp ftp              4 Jun 08 14:07 DAGJ1123454599999999.txt
-rw-r--r-- 1 ftp ftp              7 Jun 08 14:07 DAGJ112347777.txt
-rw-r--r-- 1 ftp ftp           6041 Jun 07 11:49 GKALEKOUTDEVIStest.r
-rw-r--r-- 1 ftp ftp           6144 May 06 14:27 Thumbs.db
226 Successfully transferred "/elpao"
ftp: 527 bytes received in 0.02Seconds 23.95Kbytes/sec.
ftp> bye
221 Goodbye


******** SLUT EXEMPEL H�MTA AVISERING LCD ******/


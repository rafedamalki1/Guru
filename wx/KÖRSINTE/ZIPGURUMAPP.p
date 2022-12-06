/*ZIPGURUMAPP.p*/
/*ZIPPAR C:\delad\pro9\guru\ FTP TILL BERGET PÅ FREDAGAR.*/
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE felmeddftptemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL AS INTEGER.
DEFINE VARIABLE datoruser AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.   
RUN INLOAPI.P (OUTPUT datoruser, OUTPUT outdatornamn).
   
  
IF outdatornamn = "pc122" THEN RUN zippaALLT (INPUT "d:\delad\guruZIP\", INPUT "GURUA" + STRING(TODAY,"999999") + ".zip", INPUT "C:\delad\pro116\GuruAnders").
IF outdatornamn = "pc218" THEN RUN zippaALLT (INPUT "d:\delad\guruZIP\", INPUT "GURUL" + STRING(TODAY,"999999") + ".zip", INPUT "C:\pro116\GuruLena").
/*Anders Olsson Elpool i Umeå AB  17 jan 2019 09:29:05 
 Rensar ftp mapp, men fn kommer det inga filer dit.
*/


RUN FlyttaBkc (INPUT "\\Server05\ftp\fortum\*.*", INPUT "\\server05\fortum\").


QUIT.

PROCEDURE zippaALLT :
   DEFINE INPUT  PARAMETER ziptill AS CHARACTER NO-UNDO. 
   DEFINE INPUT  PARAMETER zipNamn  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER zipfiler  AS CHARACTER NO-UNDO.        
   DEFINE VARIABLE zippfil AS CLASS Modules.Global.ZipElpool NO-UNDO.
   DEFINE VARIABLE wzzip AS CHARACTER NO-UNDO.    
   DEFINE VARIABLE FtpResult AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE FtpNet AS Helpers.FtpNet.
   
   zippfil = NEW Modules.Global.ZipElpool().
   zippfil:MakeZip(zipfiler, ziptill +  zipNamn).
   OUTPUT TO VALUE(ziptill + "ftpresult.txt") APPEND.
      PUT UNFORMATTED TODAY " " STRING(TIME,"HH:MM:SS") " ZipFtp startar" SKIP.
   OUTPUT CLOSE.  
   ftpNet = NEW Helpers.FtpNet().
   prognamn =  ziptill +  zipNamn.
   prognamn2 = "ftp://www2.guruonweb.se/Upload/" + zipNamn.
   FtpResult = ftpNet:Skicka(CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), CHR(115) + CHR(117) + CHR(112) + CHR(101) + CHR(114) + CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110),prognamn2,prognamn).
 
   OUTPUT TO VALUE(ziptill + "ftpresult.txt") APPEND.
      PUT UNFORMATTED TODAY " " STRING(TIME,"HH:MM:SS") " FTP klar-" + FtpResult  " " zipNamn  SKIP.
   OUTPUT CLOSE. 
   
END PROCEDURE.

PROCEDURE FlyttaBkc :
   DEFINE INPUT  PARAMETER sourcefilename AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER copyfilename AS CHARACTER NO-UNDO.
   DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO. 
   guruwtidir = "xcopy " + sourcefilename + " " + copyfilename + " /c/d/f/s/e/y ".
   OS-COMMAND SILENT VALUE(guruwtidir). 
   sourcefilename = SUBSTRING(sourcefilename,1,INDEX(sourcefilename,"*") - 1).
   OS-DELETE VALUE(sourcefilename) RECURSIVE.
   OS-CREATE-DIR VALUE(sourcefilename).  
END PROCEDURE.

/*
    RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(115) + CHR(117) + CHR(112) + CHR(101) + CHR(114) + CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT true, INPUT 2,
                     INPUT ziptill +  zipNamn, INPUT zipNamn,
                     INPUT "www2.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(115) + CHR(117) + CHR(112) + CHR(101) + CHR(114) + CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT true, INPUT 2,
                     INPUT ziptill +  zipNamn, INPUT "Upload\" + zipNamn,
                     INPUT "{www2app.I}", OUTPUT TABLE felmeddftptemp).                  
  */

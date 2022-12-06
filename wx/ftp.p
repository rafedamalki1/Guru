/*ftp.p*/
   DEFINE VARIABLE ftpnet AS Helpers.FtpNet.
   DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO.
   DEFINE VARIABLE FtpResult AS CHARACTER NO-UNDO.
   prognamnque = "C:\delad\pro9s\ftp.txt".
   ftpnet = new Helpers.FtpNet().
   RUN ut_UI (INPUT "FTP av-" + "for9"). 
   FtpResult = ftpnet:Skicka(CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79), "kaggen","ftp://ftp.guruonweb.se/fortum/fors9.bck","C:\delad\pro9s\DBKOPIA\fors9.bck").
   MESSAGE FtpResult
   VIEW-AS ALERT-BOX.
   RUN ut_UI (INPUT "FTP klar-" + "for9").
PROCEDURE ut_UI:
   DEFINE INPUT PARAMETER uttxt AS CHARACTER FORMAT "X(20)" NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED uttxt " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.   
   
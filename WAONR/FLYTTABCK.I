
/*------------------------------------------------------------------------
    File        : FLYTTABCK.I
    Purpose     : 

    Syntax      :

    Description : 




    Author(s)   : 
    Created     : Wed May 10 11:00:03 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
 
IF Computer_LanIP = Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(109)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(134)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(76)) THEN DO:
   prognamnque = "d:\elpool\delad\pro9s\flytt.txt".
   
   
   RUN ut_UI ("flyttar bck oneno").
   RUN FlyttaBkc (INPUT "d:\elpool\delad\pro9s\DBKOPIA\", INPUT "E:\GuruBck\").
   
    
END.
IF Computer_LanIP = {forsdb.I} THEN DO:
   prognamnque = "C:\delad\pro9s\flytt.txt".
   RUN ut_UI ("flyttar bck FORTUM").
   RUN FlyttaBkc (INPUT "C:\delad\pro9s\DBKOPIA\", INPUT "\\" + {server05ftp.I} + "\Backup\Fortumbackup\").
   
    
END.
IF Computer_LanIP = {www2db.I} THEN DO:
   prognamnque = "C:\delad\pro10s\flytt.txt".
    RUN ut_UI ("flyttar bck www2").
   RUN FlyttaBkc (INPUT "C:\delad\pro10s\DBKOPIA\", INPUT "\\" + {server05ftp.I} + "\www2backup\").
   RUN ut_UI("flyttar guruzip WWW2").
   RUN FlyttaBkc (INPUT "C:\GURUZIP\", INPUT "\\" + {server05ftp.I} + "\Guruzip\db\").
   
   RUN FlyttaBkc (INPUT "C:\GURUZIP\", INPUT "\\" + {server05ftp.I} + "\Guruzip\").
END.
IF Computer_LanIP = {egnadb.I} THEN DO:
   prognamnque = "C:\delad\pro10s\flytt.txt".
   RUN ut_UI ("flyttar bck EGNA").
   RUN FlyttaBkc (INPUT "C:\delad\Pro10s\dbkopia\", INPUT "\\" + Guru.Konstanter:InternNr(STRING(192)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(168)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(7)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(100)) + "\Backup\Egnabackup\").   
END.

PROCEDURE FlyttaBkc :
   DEFINE INPUT  PARAMETER sourcefilename AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER copyfilename AS CHARACTER NO-UNDO.
   DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO. 
   
   guruwtidir = "robocopy " + sourcefilename + " " + copyfilename + " *.* /E /mir ".
   RUN ut_UI (guruwtidir).
   OS-COMMAND SILENT VALUE(guruwtidir). 
   
    sourcefilename = "DEL " + sourcefilename + "*.BCK".
    RUN ut_UI (sourcefilename).
    DEBUGGER:SET-BREAK().
   OS-COMMAND SILENT VALUE(sourcefilename).
  
  /*
   guruwtidir = "xcopy " + sourcefilename + " " + copyfilename + " /c/d/f/s/e/y ".
  
  OS-DELETE VALUE(sourcefilename) .
   sourcefilename = REPLACE(sourcefilename,"DBKOPIA\","DBKOIPIA*").
   OS-CREATE-DIR VALUE("D:\Elpool\delad\pro9s\HEJ").
   OS-CREATE-DIR VALUE(sourcefilename).
   MESSAGE sourcefilename
 VIEW-AS ALERT-BOX. 
   MESSAGE OS-ERROR
   VIEW-AS ALERT-BOX.
    */
   
  
END PROCEDURE.



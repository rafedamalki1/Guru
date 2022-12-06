
/*------------------------------------------------------------------------
    File        : AutoBytlosenStart.p
    Purpose     : 

    Syntax      : C:\delad\pro116\dlc\bin\prowin.exe -ininame C:\delad\PRO116\GURU\OE116s.ini -pf C:\delad\PRO116\GURU\AUTOBYTL.pf -assemblies C:\DELAD\PRO116\wrk_oemgmt\GURU11

    Description : 

    Author(s)   : 
    Created     : Wed Aug 12 10:09:01 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE datoruser AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
prognamnque = "c:\delad\PRO11S\AutoBytlosen.txt". 

OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
 
IF DAY(TODAY) = 28 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.


RUN AutoBytlosen.w.

QUIT.
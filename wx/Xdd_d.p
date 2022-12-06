/*Xdd_D.P*/
DEFINE VARIABLE lvar      AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE lvar#     AS INTEGER             NO-UNDO.
DEFINE VARIABLE i         AS INTEGER             NO-UNDO.
DEFINE VARIABLE codepage  AS CHARACTER           NO-UNDO init "UNDEFINED".
DEFINE VARIABLE irecs     AS INTEGER             NO-UNDO.
DEFINE VARIABLE d-ldbname AS CHARACTER           NO-UNDO.
DEFINE VARIABLE d-was     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE numformat AS CHARACTER           NO-UNDO.
DEFINE VARIABLE maptype   AS CHARACTER           NO-UNDO.

DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE prog_namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER  NO-UNDO.
define temp-table ttb_dump
        field db        as character
        field tbl       as character
        field DUMP       as character
        index upi is primary unique db tbl.

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER  
   INDEX PRO IS PRIMARY PROGNAMN.
codepage = "iso8859-1".
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "NORD" THEN DO: 
   prog_namn = "/eko1/guru/export/".
   kommandoprog = "/eko1/guru/export/aoin.txt".  
END.    
ELSE IF FORETAG.FORETAG = "VORD" OR FORETAG.FORETAG = "VSYD" OR
FORETAG.FORETAG = "VOST" OR FORETAG.FORETAG = "VAST" THEN DO:
   prog_namn = "/guru/export/".
   kommandoprog = "/guru/export/aoin.txt".   
END. 
ELSE DO: 
   prog_namn = "C:\delad\PRO8\GURU\export\".
   kommandoprog = "C:\delad\PRO8\GURU\export\aoin.txt".
  
END. 
OPEN QUERY fq FOR EACH _FILE NO-LOCK.
GET FIRST fq NO-LOCK.
DO WHILE AVAILABLE(_FILE):
   RUN ladda1_UI.     
   GET NEXT fq NO-LOCK.
END.
DEF VAR txt AS CHARACTER.
FOR EACH ttb_dump:   
  /* txt = "INPUT FROM " + prog_namn + ttb_dump.DUMP + ".d" + " NO-ECHO .".*/
   txt = prog_namn + ttb_dump.DUMP +  ".d" .
   RUN "XUT.I" ttb_dump.tbl txt prog_namn.
   /*  FUNKAR EJ I RUN TIME KOMPILERAD.*/
   /*OM {XIN.I ttb_dump.tbl} GER TTB_DUMP.TBL PÅ ALLA ETTOR.*/                    
END.
PROCEDURE ladda1_UI:   
   CREATE ttb_dump.
   ASSIGN
   ttb_dump.db = LDBNAME("DICTDB")
   ttb_dump.tbl = _FILE._FILE-NAME.
   ttb_dump.DUMP = _FILE._DUMP-NAME.   
END PROCEDURE.


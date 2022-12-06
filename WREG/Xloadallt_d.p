/*XLOADallt_D.P */
DEFINE VARIABLE lvar      AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE lvar#     AS INTEGER             NO-UNDO.
DEFINE VARIABLE i         AS INTEGER             NO-UNDO.
DEFINE VARIABLE codepage  AS CHARACTER           NO-UNDO init "UNDEFINED".
DEFINE VARIABLE irecs     AS INTEGER             NO-UNDO.
DEFINE VARIABLE d-ldbname AS CHARACTER           NO-UNDO.
DEFINE VARIABLE d-was     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE numformat AS CHARACTER           NO-UNDO.
DEFINE VARIABLE maptype   AS CHARACTER           NO-UNDO.
DEFINE VARIABLE vart AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE prog_namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER  NO-UNDO.
DEFINE VARIABLE koll AS CHARACTER NO-UNDO.
define temp-table ttb_dump
        field db        as character
        field tbl       as character
        field DUMP       as character
        index upi is primary unique db tbl.

codepage = "iso8859-1".
prog_namn = "d:\elpool\delad\pro9s\import\". /*var finns data*/
vart = "c:\delad\pro9s\export\XINLADD.P".    /*vart ska XINLADD.P hamna*/
OPEN QUERY fq FOR EACH _file NO-LOCK.
GET FIRST fq NO-LOCK.
DO WHILE AVAILABLE(_file):
   IF _file._FILE-NAME BEGINS "_" THEN kommando = kommando.
   ELSE IF _file._FILE-NAME BEGINS "SYS" THEN  kommando = kommando.
   ELSE RUN ladda1_UI.     
   GET NEXT fq NO-LOCK.
END.
DEF VAR txt AS CHARACTER.
OUTPUT TO VALUE(vart) .
PUT UNFORMATTED "DEFINE VARIABLE finnsfil AS CHARACTER NO-UNDO." SKIP.  
PUT UNFORMATTED 'SESSION:NUMERIC-FORMAT = "AMERICAN".' SKIP.
PUT UNFORMATTED "RUN ladda_UI." SKIP.   
PUT UNFORMATTED "RUN ladda2_UI." SKIP.   
PUT UNFORMATTED "RUN ladda3_UI." SKIP.   
PUT UNFORMATTED "RUN ladda4_UI." SKIP.
PUT UNFORMATTED "RUN ladda5_UI." SKIP.
PUT UNFORMATTED "RUN ladda6_UI." SKIP.
PUT UNFORMATTED "PROCEDURE ladda_UI:" SKIP.  
OUTPUT CLOSE. 
FOR EACH ttb_dump:
   koll = "SEARCH('" + prog_namn + ttb_dump.DUMP + ".d" +  "').".
   txt = "INPUT FROM " + prog_namn + ttb_dump.DUMP + ".d" + " NO-ECHO .".
   IF ttb_dump.DUMP = "kalkspec" THEN DO:
      OUTPUT TO VALUE(vart) APPEND.
      PUT UNFORMATTED "END PROCEDURE." SKIP.   
      PUT UNFORMATTED "PROCEDURE ladda2_UI:" SKIP.   
      OUTPUT CLOSE.
      /*
      ttb_dump.tbl = "K1".
      */
   END.
   /*
   ELSE IF ttb_dump.DUMP = "kalkyl" THEN DO: 
      ttb_dump.tbl = "K2".
   END.
   */
   IF ttb_dump.DUMP = "faktmoms" THEN DO:
      OUTPUT TO VALUE(vart) APPEND.
      PUT UNFORMATTED "END PROCEDURE." SKIP.   
      PUT UNFORMATTED "PROCEDURE ladda3_UI:" SKIP.   
      OUTPUT CLOSE.      
   END. 
   IF ttb_dump.DUMP = "mtrlber" THEN DO:
      OUTPUT TO VALUE(vart) APPEND.
      PUT UNFORMATTED "END PROCEDURE." SKIP.   
      PUT UNFORMATTED "PROCEDURE ladda4_UI:" SKIP.   
      OUTPUT CLOSE.      
   END.    
   IF ttb_dump.DUMP = "berord" THEN DO:
      OUTPUT TO VALUE(vart) APPEND.
      PUT UNFORMATTED "END PROCEDURE." SKIP.   
      PUT UNFORMATTED "PROCEDURE ladda5_UI:" SKIP.   
      OUTPUT CLOSE.      
   END. 
   IF ttb_dump.DUMP = "pabordn" THEN DO:
      OUTPUT TO VALUE(vart) APPEND.
      PUT UNFORMATTED "END PROCEDURE." SKIP.   
      PUT UNFORMATTED "PROCEDURE ladda6_UI:" SKIP.   
      OUTPUT CLOSE.      
   END.
   RUN "XIN.I" ttb_dump.tbl txt prog_namn vart koll.
   /*  FUNKAR EJ I RUN TIME KOMPILERAD.*/
   /*OM {XIN.I ttb_dump.tbl} GER TTB_DUMP.TBL PÅ ALLA ETTOR.*/                    
END.
OUTPUT TO VALUE(vart) APPEND.
PUT UNFORMATTED "END PROCEDURE." SKIP.   
OUTPUT CLOSE. 

PROCEDURE ladda1_UI:   
   CREATE ttb_dump.
   ASSIGN
   ttb_dump.db = LDBNAME("DICTDB")
   ttb_dump.tbl = _FILE._FILE-NAME.
   ttb_dump.DUMP = _FILE._DUMP-NAME.   
END PROCEDURE.

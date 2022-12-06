/*XLADDA_DF.P*/
DEFINE VARIABLE program AS CHARACTER FORMAT "X(35)".
DEFINE VARIABLE db AS INTEGER LABEL "VILKEN DATABAS" .
DEFINE VARIABLE id AS CHARACTER.
DEFINE VARIABLE password AS CHARACTER.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.

DISPLAY "ANGE 1 FÖR vord" SKIP
        "ANGE 2 FÖR vsyd" SKIP
        "ANGE 3 FÖR VOST" SKIP
        "ANGE 4 FÖR vast" SKIP
        "ANGE 5 FÖR VUTBI" SKIP
        WITH FRAME CC.
UPDATE db WITH FRAME CC.       
IF db = 1 THEN DO:
   kommandoprog = "procopy /guru/db9/vord /guru/db9/ba/vord".  
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VORD") THEN DO :      
      CONNECT -db /guru/db9/vord -1.  
   END.
END.   
ELSE IF db = 2 THEN DO:
   kommandoprog = "procopy /guru/db9/vsyd /guru/db9/ba/vsyd".
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VSYD") THEN DO :               
      CONNECT -db /guru/db9/vsyd -1. 
   END.
END.
ELSE IF db = 3 THEN DO:
   kommandoprog = "procopy /guru/db9/vost /guru/db9/ba/vost".
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VOST") THEN DO :               
      CONNECT -db /guru/db9/vost -1. 
   END.
END.
ELSE IF db = 4 THEN DO:
   kommandoprog = "procopy /guru/db9/vast /guru/db9/ba/vast".
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VAST") THEN DO :               
      CONNECT -db /guru/db9/vast -1. 
   END.
END.
ELSE IF db = 5 THEN DO:
   kommandoprog = "procopy /guru/db9/vutbi /guru/db9/ba/vutbi". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VUTBI") THEN DO :               
      CONNECT -db /guru/db9/vutbi -1. 
   END.
END.

{LDALIAS8.I}
UPDATE id
password WITH FRAME CC1.
IF NOT SETUSERID(id,password,"DICTDB") THEN QUIT.


UPDATE program.
/*RUN VALUE(program).*/
RUN prodict/load_df.p (INPUT program) .
PAUSE .
quit.

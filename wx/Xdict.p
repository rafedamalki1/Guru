/*Xdict.p.P*/
DEFINE VARIABLE program AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE id AS CHARACTER.
DEFINE VARIABLE password AS CHARACTER.
DEFINE VARIABLE db AS INTEGER LABEL "VILKEN DATABAS" .
DISPLAY "ANGE 1 FÖR VORD" 
        "ANGE 2 FÖR VSYD" 
        "ANGE 3 FÖR VOST" 
        "ANGE 4 FÖR VAST" 
        "ANGE 5 FÖR VUTBI"
        "ANGE 6 FÖR VATT" 
        "ANGE 11 FÖR esnord" 
        "ANGE 12 FÖR esadm" 
        "ANGE 13 FÖR eseta"
        "ANGE 14 FÖR esmal" 
        "ANGE 15 FÖR ESAB"
        WITH FRAME CC 1 col.
UPDATE db WITH FRAME CC.       


IF db = 1 THEN DO:
   kommandoprog = "procopy /guru/db9/vord /guru/db9/ba/vord".  
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VORD") THEN DO :      
      CONNECT -db /guru/db/vord -1.  
   END.
END.   
ELSE IF db = 2 THEN DO:
   kommandoprog = "procopy /guru/db9/vsyd /guru/db9/ba/vsyd".
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("VSYD") THEN DO :               
      CONNECT -db /guru/db/vsyd -1. 
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
ELSE IF db = 6 THEN DO:
   kommandoprog = "procopy /guru/db9/vatt /guru/db9/ba/vatt". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("vatt") THEN DO :               
      CONNECT -db /guru/db9/vatt -1. 
   END.
END.

ELSE IF db = 11 THEN DO:
   kommandoprog = "procopy /u10/guru/db/esnord /u12/guru/db/ba/esnord". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("esnord") THEN DO :               
      CONNECT -db /u10/guru/db/esnord -1. 
   END.
END.
ELSE IF db = 12 THEN DO:
   kommandoprog = "procopy /u10/guru/db/esadm /u12/guru/db/ba/esadm". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("esadm") THEN DO :               
      CONNECT -db /u10/guru/db/esadm -1. 
   END.
END.
ELSE IF db = 13 THEN DO:
   kommandoprog = "procopy /u10/guru/db/eta /u12/guru/db/ba/eta". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("eta") THEN DO :               
      CONNECT -db /u10/guru/db/eta -1. 
   END.
END.
ELSE IF db = 14 THEN DO:
   kommandoprog = "procopy /u10/guru/db/esmal /u12/guru/db/ba/esmal". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("esmal") THEN DO :               
      CONNECT -db /u10/guru/db/esmal -1. 
   END.
END.
ELSE IF db = 15 THEN DO:
   kommandoprog = "procopy /u10/guru/db/esab /u12/guru/db/ba/esab". 
   OS-COMMAND VALUE(kommandoprog).
   IF NOT CONNECTED("esab") THEN DO :               
      CONNECT -db /u10/guru/db/esab -1. 
   END.
END.
UPDATE id
password WITH FRAME CC1.
IF SETUSERID(id,password,"DICTDB") THEN RUN dict.p.
ELSE QUIT.
quit.

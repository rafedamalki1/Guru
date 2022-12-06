/*XSTARTVE.P*/
DEFINE VARIABLE program AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE db AS INTEGER LABEL "VILKEN DATABAS" .
DEFINE VARIABLE id AS CHARACTER.
DEFINE VARIABLE password AS CHARACTER.

DISPLAY "ANGE 1 FÖR vord" SKIP
        "ANGE 2 FÖR vsyd" SKIP
        "ANGE 3 FÖR VOST" SKIP
        "ANGE 4 FÖR vast" SKIP
        "ANGE 5 FÖR VUTBI" SKIP
        "ANGE 6 FÖR VATT" SKIP
        "ANGE 11 FÖR esnord" SKIP
        "ANGE 12 FÖR esadm" SKIP
        "ANGE 13 FÖR eseta" SKIP
        "ANGE 14 FÖR esmal" SKIP
        "ANGE 15 FÖR ESAB" SKIP
        WITH FRAME CC  1 COL.
      UPDATE db WITH FRAME CC.       
      IF db = 1 THEN DO:
         IF NOT CONNECTED("vord") THEN DO :      
            CONNECT -db vord -S 2603 -H thnuguru -N tcp. 
         END.
      END.   
      ELSE IF db = 2 THEN DO:
         IF NOT CONNECTED("vsyd") THEN DO :               
            CONNECT -db vsyd -S 2605 -H thnuguru -N tcp . 
         END.
      END.
      ELSE IF db = 3 THEN DO:
         IF NOT CONNECTED("VOST") THEN DO :               
      CONNECT -db vost -S 2607 -H thnuguru -N tcp . 
   END.
END.
ELSE IF db = 4 THEN DO:
   IF NOT CONNECTED("vast") THEN DO :               
      CONNECT -db vast -S 2609 -H thnuguru -N tcp . 
   END.
END.
ELSE IF db = 5 THEN DO:
   IF NOT CONNECTED("VUTBI") THEN DO :               
      CONNECT -db vutbi -S 2601 -H thnuguru -N tcp . 
   END.
END.
/*
ELSE IF db = 6 THEN DO:
   IF NOT CONNECTED("VATT") THEN DO :
      CONNECT -db vatt -S 2611 -H thnuguru -N tcp. 
   END.
END.

ELSE IF db = 11 THEN DO:
   IF NOT CONNECTED("esnord") THEN DO :               
      CONNECT -db esnord -S esnord8 -H elpaso -N tcp . 
   END.
END.
ELSE IF db = 12 THEN DO:
   IF NOT CONNECTED("esadm") THEN DO :               
      CONNECT -db esadm -S esadmd8 -H elpaso -N tcp . 
   END.
END.
ELSE IF db = 13 THEN DO:
   IF NOT CONNECTED("eta") THEN DO :               
      CONNECT -db eta -S eseta8 -H elpaso -N tcp . 
   END.
END.
ELSE IF db = 14 THEN DO:
   IF NOT CONNECTED("esmal") THEN DO :               
      CONNECT -db esmal -S esmal8 -H elpaso -N tcp . 
   END.
END.
ELSE IF db = 15 THEN DO:
   IF NOT CONNECTED("esab") THEN DO :               
      CONNECT -db esab -S esab -H elpaso -N tcp . 
   END.
END.
*/
{LDALIAS8.I}
UPDATE id
password WITH FRAME CC1.
IF SETUSERID(id,password,"DICTDB") THEN do:

   UPDATE program.
   RUN VALUE(program).
   PAUSE .
end.   
RETURN.

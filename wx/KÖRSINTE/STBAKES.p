/*STBAKES.P*/
DEFINE INPUT PARAMETER dbval AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER progval AS CHARACTER NO-UNDO.
IF dbval = "esnord" THEN DO:
   CONNECT -db esnord -S esnord8 -H elpaso.sydkraft.se -N tcp NO-ERROR.                        
END.
ELSE IF dbval = "eta" THEN DO:
   CONNECT -db eta -S eseta8 -H elpaso.sydkraft.se-N tcp NO-ERROR. 
   
END.   
ELSE IF dbval = "esadm" THEN DO:
   CONNECT -db esadm -S esadmd8 -H elpaso.sydkraft.se -N tcp NO-ERROR.    
END.
{LDALIAS8.I}
IF CONNECTED("esnord") THEN DO:
   RUN VALUE(progval).
END.
ELSE IF CONNECTED("eta") THEN DO:
   RUN VALUE(progval).
END.
ELSE IF CONNECTED("esadm") THEN DO:
   RUN VALUE(progval).
END.

IF CONNECTED("esnord") THEN DO:      
   DISCONNECT esnord NO-ERROR.    
END.
IF CONNECTED("eta") THEN DO: 
   DISCONNECT eta NO-ERROR. 
END.
IF CONNECTED("esadm") THEN DO:
   DISCONNECT esadm NO-ERROR. 
END.

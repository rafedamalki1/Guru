/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: FRIKALKKOLL.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.02.05 09:34 ELPAO   
     Modified: 2009.02.05 09:45 ELPAO    
     Modified: 2009.10.15 11:25 ELPAO    
     Modified: 
     ENDAST PÅ SERVERSIDAN
*/    
DEFINE VARIABLE frikalk AS LOGICAL NO-UNDO.
DEFINE VARIABLE friextra AS HANDLE NO-UNDO.
DEFINE VARIABLE frihamta AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET friextra. 
RUN VISAFKALAPP.P PERSISTENT SET frihamta. 
RUN kalkarstart_UI IN frihamta (INPUT KALKAONR.KALKNR).
EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
CREATE inextradatatemp.          
ASSIGN
inextradatatemp.PROGRAM = "FRIKALK2"                   
inextradatatemp.HUVUDCH  = ?            
inextradatatemp.HUVUDINT = KALKAONR.KALKNR.
RUN finnsextra_UI IN friextra (INPUT TABLE inextradatatemp,OUTPUT frikalk).
EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
IF frikalk = TRUE THEN DO:
   RUN hmtafri_UI IN frihamta (INPUT KALKAONR.KALKNR, OUTPUT TABLE efastkalktemp,OUTPUT TABLE kalkbeftemp,OUTPUT TABLE tidutrubrik).   
   RUN friupp_UI.
END.
IF VALID-HANDLE(frihamta) THEN DELETE PROCEDURE frihamta.
IF VALID-HANDLE(friextra) THEN DELETE PROCEDURE friextra.
IF frikalk = TRUE THEN RETURN.

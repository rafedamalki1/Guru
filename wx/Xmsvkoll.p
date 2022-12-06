/*Xmsvkoll.p   byt verkamhet på gamla projekt .Flytta även kalkyler och uppföljning. SUNDSVALL*/
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
FOR EACH tidregitab WHERE datum GE 01/01/2010 AND tidregitab.tidlog = true EXCLUSIVE-LOCK:
   ASSIGN tidregitab.overtidtill = "MSV" tidregitab.pris = 0 tidregitab.veckokord = "".

END.


/*Xmsvkoll.p   byt verkamhet p� gamla projekt .Flytta �ven kalkyler och uppf�ljning. SUNDSVALL*/
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
FOR EACH tidregitab WHERE datum GE 01/01/2010 AND tidregitab.tidlog = true EXCLUSIVE-LOCK:
   ASSIGN tidregitab.overtidtill = "MSV" tidregitab.pris = 0 tidregitab.veckokord = "".

END.


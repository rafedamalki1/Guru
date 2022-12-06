   /*BERKALKSTART2.I*/
   DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.                      /*EXTRATABHMT*/
   DEFINE VARIABLE totmtrlspar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE totmtrlspar2 AS DECIMAL NO-UNDO.
   /*KALKBER*/
   
   IF NOT VALID-HANDLE(fbestapph) THEN RUN EXTRATABHMT.P PERSISTENT SET fbestapph.
   totmtrlspar = totmtrl.
   totmtrl = 0.
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   /*KALKYL-BEREDNING SUNDSVALL*/
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "KALKBER"                   
   inextrakopptemp.KOPPLACHAR1 = FASTSPEC.OMRADE
   inextrakopptemp.KOPPLAINT1 = FASTSPEC.KALKNR
   inextrakopptemp.KOPPLACHAR2 = ?
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp).        
   FIND FIRST extrakopptemp WHERE NO-LOCK NO-ERROR.
   FIND FIRST BEREDNING WHERE BEREDNING.OMRADE = extrakopptemp.KOPPLACHAR2 AND 
   BEREDNING.BERNR = extrakopptemp.KOPPLAINT2 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE BEREDNING THEN DO:

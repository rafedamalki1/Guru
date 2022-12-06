/*BERKALKSLUT.I*/
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   totmtrlspar2 = totmtrl.
   totalt = totalt + totmtrl.
   totmtrl = totmtrlspar + totmtrlspar2. 
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph NO-ERROR.

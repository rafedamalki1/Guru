
/*------------------------------------------------------------------------
    File        : chbermtrl.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 21 14:28:15 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
OUTPUT TO c:\chber.txt.
FOR EACH bermtrl WHERE BERMTRL.AONR = "49" AND BERMTRL.OMRADE = "swe" NO-LOCK:
   FIND FIRST berval WHERE  berval.AONR = BERMTRL.AONR AND berval.OMRADE = BERMTRL.OMRADE AND  BERVAL.NUM = BERMTRL.NUM AND 
   BERVAL.skapNUM = BERMTRL.skapNUM  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BERVAL THEN DO:
      PUT UNFORMATTED BERMTRL.AONR "$" BERMTRL.OMRADE "$" BERMTRL.NUM "$" BERMTRL.SKAPNUM "$" BERMTRL.ENR "$" " finns inte" SKIP. 
   END.   
   
END.
OUTPUT CLOSE.

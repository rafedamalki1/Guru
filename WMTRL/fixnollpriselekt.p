
/*------------------------------------------------------------------------
    File        : fixnollpriselekt.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri May 29 16:13:06 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
OUTPUT TO C:\RÄTTFELPRIS.TXT.
FOR EACH INKADRESS  WHERE INKADRESS.BESTDATUM GE 05/12/2015 USE-INDEX AONR NO-LOCK:
   FOR EACH INKMTRL WHERE INKMTRL.INKBESTID = INKADRESS.INKBESTID AND INKMTRL.LEVKODINK  = "1" AND INKMTRL.PRISINK = 0 AND INKMTRL.INKANTAL > 0 EXCLUSIVE-LOCK:
      FIND FIRST MTRL WHERE MTRL.LEVKOD = "1" AND MTRL.Enr = INKMTRL.ENR AND MTRL.KALKNR = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:         
         ASSIGN INKMTRL.PRIS = MTRL.NPRIS INKMTRL.PRISINK = MTRL.NPRIS.              
      END.   
      PUT UNFORMATTED "1"   + " " +  INKMTRL.AONRAONR  + " " + STRING(INKMTRL.AONRDELNR)   + " " +  STRING(INKMTRL.INKBESTID)  + " " +  STRING(INKADRESS.BESTDATUM)  + " " +  INKMTRL.LEVNAMN  + " " + INKMTRL.ENR + " " +   STRING(INKMTRL.INKANTAL)   + "     " +   STRING(INKMTRL.PRISINK) + " " +   STRING(MTRL.NPRIS)   SKIP.
   END.
   FOR EACH INKMTRL WHERE INKMTRL.INKBESTID = INKADRESS.INKBESTID AND INKMTRL.LEVKODINK  = "11" AND INKMTRL.PRISINK = 0 AND INKMTRL.INKANTAL > 0 EXCLUSIVE-LOCK:
      FIND FIRST MTRL WHERE MTRL.LEVKOD = "1" AND MTRL.Enr = INKMTRL.ENR AND MTRL.KALKNR = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:         
         ASSIGN INKMTRL.PRIS = MTRL.NPRIS INKMTRL.PRISINK = MTRL.NPRIS.              
      END.   
      PUT UNFORMATTED "11"   + " " +  INKMTRL.AONRAONR  + " " + STRING(INKMTRL.AONRDELNR)   + " " +  STRING(INKMTRL.INKBESTID)  + " " +  STRING(INKADRESS.BESTDATUM)  + " " +  INKMTRL.LEVNAMN  + " " + INKMTRL.ENR + " " +   STRING(INKMTRL.INKANTAL)   + "     " +   STRING(INKMTRL.PRISINK) + " " +   STRING(MTRL.NPRIS)   SKIP.
   END.
      
END.

/*------------------------------------------------------------------------
    File        : BerskapDubbStopp.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 30 12:44:59 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

      FIND FIRST berskaptemp WHERE berskaptemp.KTYPKOD = BERSKAP.KTYPKOD AND berskaptemp.KOD = BERSKAP.KOD  
      AND berskaptemp.ENR = BERSKAP.ENR 
      AND berskaptemp.LEVKOD = BERSKAP.LEVKOD 
      NO-LOCK NO-ERROR.
      IF AVAILABLE berskaptemp THEN.
      ELSE DO:   
         CREATE berskaptemp.
         BUFFER-COPY BERSKAP TO berskaptemp.
         berskaptemp.BERSKAPROW = ROWID(BERSKAP).
      END.   
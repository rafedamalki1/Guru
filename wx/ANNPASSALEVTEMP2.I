
/*------------------------------------------------------------------------
    File        : ANNPASSALEVTEMP2.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Mar 26 16:27:22 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      LttHbuff:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(32),NO-LOCK) NO-ERROR.
      IF LttHbuff:AVAILABLE THEN DO:
         Guru.Konstanter:LeverantorTTh:BUFFER-CREATE().
         Guru.Konstanter:LeverantorTTh:BUFFER-COPY(LttHbuff).
         LttHbuff:BUFFER-DELETE().
      END.
      LttHbuff:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(7),NO-LOCK) NO-ERROR.
      IF LttHbuff:AVAILABLE THEN DO:
         Guru.Konstanter:LeverantorTTh:BUFFER-CREATE().
         Guru.Konstanter:LeverantorTTh:BUFFER-COPY(LttHbuff).
         LttHbuff:BUFFER-DELETE().
      END.      
   END.  

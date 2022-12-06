
/*------------------------------------------------------------------------
    File        : ANNPASSALEVTEMP1.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Mar 26 16:26:15 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

   DEFINE VARIABLE Ltth AS HANDLE NO-UNDO.
   DEFINE VARIABLE LttHbuff AS HANDLE NO-UNDO.
   CREATE TEMP-TABLE Ltth.
   Ltth:CREATE-LIKE(Guru.Konstanter:LeverantorTTh).
   Ltth:TEMP-TABLE-PREPARE("Ltt").
   LttHbuff  = Ltth:DEFAULT-BUFFER-HANDLE. 
   
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(32),NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN DO:
         
         LttHbuff:BUFFER-CREATE().
         LttHbuff:BUFFER-COPY(Guru.Konstanter:LeverantorTTh).
         Guru.Konstanter:LeverantorTTh:BUFFER-DELETE().
      END.
      Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(7),NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN DO:
         
         LttHbuff:BUFFER-CREATE().
         LttHbuff:BUFFER-COPY(Guru.Konstanter:LeverantorTTh).
         Guru.Konstanter:LeverantorTTh:BUFFER-DELETE().
      END.      
   END.

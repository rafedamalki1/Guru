
/*------------------------------------------------------------------------
    File        : UTBBEREDNINGUID.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 08 11:32:25 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

   DEFINE VARIABLE listnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE mrID AS ROWID NO-UNDO.
   DEFINE VARIABLE beredningh AS HANDLE NO-UNDO.
   CREATE BUFFER beredningh FOR TABLE "BEREDNING".
   beredningh:FIND-FIRST("WHERE BERNR = " + STRING(BerValdNr) + " AND OMRADE = " + QUOTER(BerValdaOmr) ,NO-LOCK) NO-ERROR.
   IF beredningh:AVAILABLE THEN DO:
      listnr = beredningh:BUFFER-FIELD("UID"):BUFFER-VALUE. 
   END.


/*------------------------------------------------------------------------
    File        : VattenFallPrisStopp.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 13 09:38:18 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

IF Guru.Konstanter:globforetag = "VAST" THEN DO: 
   IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN.
   ELSE IF Guru.Konstanter:HuvudLeverantor = Guru.SharedVariable:ValdmtrlLeverantor THEN.
   ELSE DO:
      
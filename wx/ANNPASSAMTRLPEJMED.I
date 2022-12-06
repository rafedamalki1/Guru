
/*------------------------------------------------------------------------
    File        : ANNPASSAMTRLPEJMED.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Mar 26 16:30:10 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

IF Guru.Konstanter:globforetag = "GRAN" AND dynbuffh:BUFFER-FIELD("LEVKODINK"):BUFFER-VALUE = "32"  THEN.
ELSE IF Guru.Konstanter:globforetag = "GRAN" AND dynbuffh:BUFFER-FIELD("LEVKODINK"):BUFFER-VALUE = "7"  THEN.
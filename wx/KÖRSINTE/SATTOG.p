/*SATTOG.P*/
DEFINE SHARED VARIABLE datsover AS CHARACTER FORMAT "X(1)" LABEL "OVERTIDUTTAG" NO-UNDO.  
DEFINE SHARED VARIABLE datsovernr AS CHARACTER LABEL "ÖVER/KOMP" NO-UNDO.  
IF datsovernr = "1" THEN datsover = "K".
ELSE IF datsovernr = "0" THEN datsover = "Ö".
ELSE IF datsovernr = "3" THEN datsover = "I".
ELSE IF datsovernr = "2" THEN datsover = "L". 
ELSE IF datsovernr = "" THEN datsover = "".
    

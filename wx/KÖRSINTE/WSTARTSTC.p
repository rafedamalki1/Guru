/*WSTARTSTC.P*/

DEFINE VARIABLE nyaprog AS LOGICAL NO-UNDO.
nyaprog = TRUE.
/*
REPEAT:
  
   RUN WSTART.W (INPUT "",INPUT "classer",INPUT-OUTPUT nyaprog).
    
   IF nyaprog = FALSE THEN LEAVE.
END.
*/

RUN WSTART.W (INPUT "",INPUT "classer",INPUT-OUTPUT nyaprog).

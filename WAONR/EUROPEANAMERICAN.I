/*EUROPEANAMERICAN.I*/
/*
&IF DEFINED(GLNF) &THEN &SCOPED-DEFINE GLNF
&ELSE &SCOPED-DEFINE GLNF DEFINE VARIABLE numformat AS CHARACTER NO-UNDO.
&ENDIF
{&GLNF}
  
&IF DEFINED(GLNF) EQ 0 &THEN 
&GLOBAL-DEFINE GLNF 
DEFINE VARIABLE numformat AS CHARACTER NO-UNDO.
&ENDIF
*/  

IF Guru.GlobalaVariabler:numformat = "" THEN SESSION:NUMERIC-FORMAT = "AMERICAN". 
ELSE SESSION:NUMERIC-FORMAT = Guru.GlobalaVariabler:numformat. 



/*PERSSEKW.I*/     
IF Guru.Konstanter:persekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME {&FRAME-NAME}. 
/*IF Guru.Konstanter:persekvar[2] = TRUE THEN ENABLE BTN_UPP WITH FRAME {&FRAME-NAME}.*/
ENABLE BTN_UPP WITH FRAME {&FRAME-NAME}.
IF Guru.Konstanter:persekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
IF Guru.Konstanter:persekvar[4] = TRUE THEN ENABLE BTN_VISAP WITH FRAME {&FRAME-NAME}.  
IF Guru.Konstanter:persekvar[5] = TRUE THEN ENABLE BTN_SCH WITH FRAME {&FRAME-NAME}.
IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
   ENABLE BTN_SCH WITH FRAME {&FRAME-NAME}.
END.
/*IF Guru.Konstanter:persekvar[6] = TRUE THEN ENABLE BTN_VECS WITH FRAME {&FRAME-NAME}. */
/*IF Guru.Konstanter:persekvar[6] = TRUE THEN ENABLE BTN_RUL WITH FRAME {&FRAME-NAME}. */
IF Guru.Konstanter:persekvar[7] = TRUE THEN ENABLE BTN_ARB WITH FRAME {&FRAME-NAME}.              
/*IF Guru.Konstanter:persekvar[8] = TRUE THEN ENABLE BTN_VISA WITH FRAME {&FRAME-NAME}.              
IF Guru.Konstanter:persekvar[9] = TRUE THEN ENABLE BTN_VISA WITH FRAME {&FRAME-NAME}.           */
IF Guru.Konstanter:persekvar[10] = TRUE THEN ENABLE BTN_DEBPR WITH FRAME {&FRAME-NAME}.             
/*IF Guru.Konstanter:persekvar[11] = TRUE THEN ENABLE BTN_MAXOM WITH FRAME {&FRAME-NAME}.*/             

     

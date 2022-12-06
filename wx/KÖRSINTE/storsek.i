/*STORSEK.I KÖRS INTE */ 
xhop = "STOR".  
PROCEDURE nextguru_UI:
   FIND FIRST XSEK WHERE XSEK.MENYVART = xhop AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
END PROCEDURE.   

IF storlogvar = FALSE THEN DO:
   storlogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:storsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:storsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:storsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:storsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:storsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:storsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:storsekvar[7] = XSEK.SEK[7].
END.
IF Guru.Konstanter:storsekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME {&FRAME-NAME}.       
IF Guru.Konstanter:storsekvar[2] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.          
IF Guru.Konstanter:storsekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
IF Guru.Konstanter:storsekvar[4] = TRUE THEN ENABLE BTN_VIS WITH FRAME {&FRAME-NAME}.
IF globforetag = "SVEN" OR globforetag = "SEKG" OR globforetag = "MALU" OR globforetag = "BIRK" THEN DO:
   ASSIGN
   BTN_RAPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_ADM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_LAS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
END.   
ELSE DO:    
   IF Guru.Konstanter:storsekvar[5] = TRUE THEN ENABLE BTN_RAPP WITH FRAME {&FRAME-NAME}.              
   IF Guru.Konstanter:storsekvar[6] = TRUE THEN ENABLE BTN_ADM WITH FRAME {&FRAME-NAME}.              
   IF Guru.Konstanter:storsekvar[7] = TRUE THEN ENABLE BTN_LAS WITH FRAME {&FRAME-NAME}.           
END.
       
     

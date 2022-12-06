/*STORSEKW.I*/ 
xhop = "STOR".  
IF Guru.Konstanter:storsekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME {&FRAME-NAME}.       
IF Guru.Konstanter:storsekvar[2] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.          
IF Guru.Konstanter:storsekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
IF Guru.Konstanter:storsekvar[4] = TRUE THEN ENABLE BTN_VIS WITH FRAME {&FRAME-NAME}.
IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" OR Guru.Konstanter:globforetag = "MALU" OR Guru.Konstanter:globforetag = "BIRK" THEN DO:
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
       
     

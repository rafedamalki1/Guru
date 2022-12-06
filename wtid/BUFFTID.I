/*BUFFTID.I*/
   BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
   
   IF allatider = 1 THEN DO:
      BTN_NYALLA:LABEL IN FRAME {&FRAME-NAME} = "  Buffert~nregistrering".
      IF Guru.Konstanter:globforetag = "XELPA"  THEN DO:
         BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         IF status-stopp = FALSE THEN ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.
      END.      
   END.
   

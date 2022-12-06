/*ARTALBORT.I*/
status-ok = RAD_PERIOD:DELETE("Visning per år").   
IF RAD_PERIOD = 0 THEN RAD_PERIOD = 2.
IF RAD_PERIOD = 1 THEN RAD_PERIOD = 2.
DISPLAY RAD_PERIOD WITH FRAME {&FRAME-NAME}.
APPLY "VALUE-CHANGED" TO RAD_PERIOD.
DEFINE TEMP-TABLE laddaartemp NO-UNDO
   FIELD ARTAL AS INTEGER FORMAT "9999"
   INDEX ARTAL ARTAL.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN LADDAAR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE laddaartemp).
   END.
   ELSE DO:
      RUN LADDAAR.P
      (OUTPUT TABLE laddaartemp).
   END.             
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0").   
    /*LADDAR ÅR I CMB_ARTAL*/   
   FIND FIRST laddaartemp NO-ERROR.
   IF AVAILABLE laddaartemp THEN DO:
      FOR EACH laddaartemp:
         status-ok = CMB_ARTAL:ADD-LAST(STRING(laddaartemp.ARTAL,"9999")).      
         ASSIGN
         CMB_ARTAL:SCREEN-VALUE = STRING(laddaartemp.ARTAL,"9999"). 
      END.
   END. 
   ELSE DO:
      status-ok = CMB_ARTAL:ADD-LAST(STRING(YEAR(TODAY),"9999")). 
      CMB_ARTAL:SCREEN-VALUE = STRING(YEAR(TODAY),"9999"). 
   END.
   
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL   
   FILL-IN-AR = INPUT CMB_ARTAL.    
   /*CMB_ARTAL:HIDDEN = TRUE.*/
   ON VALUE-CHANGED OF CMB_ARTAL IN FRAME {&FRAME-NAME} /* Årtal */
   DO:                           
      ASSIGN
      CMB_ARTAL = INPUT CMB_ARTAL              
      FILL-IN-AR = INPUT CMB_ARTAL.            
      IF YEAR(TODAY) = CMB_ARTAL THEN DO:      
         FILL-IN-STOPPDAT = TODAY.
      END.
      ELSE DO:
         FILL-IN-STOPPDAT = DATE(12,31,CMB_ARTAL).
      END.      
      FILL-IN-STARTDAT = DATE(01,01,CMB_ARTAL).
      DISPLAY FILL-IN-STARTDAT FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.      
   END.
   APPLY "VALUE-CHANGED" TO CMB_ARTAL.

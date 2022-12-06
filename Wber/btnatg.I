/*BTNATG.I*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   IF NOT VALID-HANDLE(btnatgh) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.    
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,SEL_UPPATG:HANDLE IN FRAME FRAME-ATG).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KORATG:HANDLE IN FRAME FRAME-ATG). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_AVBATG:HANDLE IN FRAME FRAME-ATG). 
      RUN BERATGM.P PERSISTENT SET btnatgh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
   END.  
   RUN main_UI IN btnatgh (INPUT vart). 
   RUN frame_UI (INPUT "ATG").

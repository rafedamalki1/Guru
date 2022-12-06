
/* VALDBMAIN.I*/
   C-Win:TITLE = "Start av Guru " + PROVERSION  + " " + PROGRESS.
   BTN_START:LOAD-IMAGE ("BILDER\xbtn_ok.gif") NO-ERROR.
   BTN_AVB:LOAD-IMAGE ("BILDER\xbtn_avs.gif") NO-ERROR.                  
   RUN enable_UI. 
   IF NUM-RESULTS("{&BROWSE-NAME}") = 1 THEN DO:
      APPLY "CHOOSE" TO BTN_START.
      APPLY "CLOSE" TO THIS-PROCEDURE. 
      LEAVE MAIN-BLOCK.    
   END.   
  
   status-ok = BRW_VDB:DESELECT-FOCUSED-ROW().
     

 
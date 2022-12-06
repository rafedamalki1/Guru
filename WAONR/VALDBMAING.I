

/* VALDBMAING.I*/
  C-Win:TITLE = "Start av Guru " + PROVERSION  + " " + PROGRESS.
   BTN_START:LOAD-IMAGE ("BILDER\xbtn_ok.gif") NO-ERROR.
   BTN_AVB:LOAD-IMAGE ("BILDER\xbtn_avs.gif") NO-ERROR.                  
   RUN enable_UI.
   IF runvar = "FSUND" OR runvar = "FMISV" OR runvar = "FSNAT"  OR runvar = "FELPA" THEN DO:
      
      IF runvar = "FSUND" THEN DO:
         /*ENERGI + ELNÄT*/
         FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = "SUNDn9" NO-ERROR.
      END.
      ELSE IF runvar = "FMISV" THEN DO:
         /*ENERGI + ELNÄT*/
         FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = "misv" NO-ERROR.
      END.
      ELSE IF runvar = "FSNAT" THEN DO:
         /*ENERGI + ELNÄT*/
         FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = "SUNDNAT" NO-ERROR.
      END.
      
      ELSE FIND FIRST valdbtemp WHERE NO-ERROR.
      RUN flexsund_UI.
      /*
      APPLY "CHOOSE" TO BTN_START.
      */
      APPLY "CLOSE" TO THIS-PROCEDURE. 
      LEAVE MAIN-BLOCK. 
   END.
   IF runvar = "SELNDEPA" THEN DO:
      FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = "SUNDNAT" NO-ERROR.
      RUN flexsund_UI.
      /*
      APPLY "CHOOSE" TO BTN_START.
      */
      APPLY "CLOSE" TO THIS-PROCEDURE. 
      LEAVE MAIN-BLOCK. 
   END.
   IF NUM-RESULTS("{&BROWSE-NAME}") = 1 THEN DO:
      APPLY "CHOOSE" TO BTN_START.
      APPLY "CLOSE" TO THIS-PROCEDURE. 
      LEAVE MAIN-BLOCK.    
   END.   
  
   status-ok = BRW_VDB:DESELECT-FOCUSED-ROW().
     

 
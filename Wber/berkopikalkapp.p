/* berkopikalkapp.p */

{BBLABEL.I}
PROCEDURE getLabels_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR bblabeltemp.
   
   FOR EACH BBENAMNING NO-LOCK:
      CREATE bblabeltemp.
      ASSIGN 
         bblabeltemp.F1      = BBENAMNING.B1     
         bblabeltemp.F2      = BBENAMNING.B2     
         bblabeltemp.F3      = BBENAMNING.B3     
         bblabeltemp.F4      = BBENAMNING.B4     
         bblabeltemp.F5      = BBENAMNING.B5     
         bblabeltemp.F6      = BBENAMNING.B6     
         bblabeltemp.ID1     = BBENAMNING.ID1    
         bblabeltemp.ID2     = BBENAMNING.ID2    
         bblabeltemp.FRI3    = "Fri id"
         bblabeltemp.KONSKOD = BBENAMNING.KONSKOD.
   END.
END PROCEDURE.
/*BUTCOL.I*/
DEFINE VARIABLE pixvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE startradvar AS DECIMAL NO-UNDO.
PROCEDURE getsizf_UI :
   DEFINE OUTPUT PARAMETER x-mult AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER y-mult AS DECIMAL NO-UNDO.
   ASSIGN
   x-mult = x-multi
   y-mult = y-multi.
END PROCEDURE.
PROCEDURE buttrow_UI :
   /*RADJUSTERING*/
   {BTNROW.I}
END PROCEDURE.
PROCEDURE buttrow2_UI :
   /*SÄTTER STARTRAD*/
   {BTNROW2.I}
END PROCEDURE.

PROCEDURE buttcol_UI :
   /*NÄR BILDEN ÄR DIT SATT VIA UIB STORLEK 7.25 ELLER 7.5 TESTA*/
   /*FÖRSTA KNAPPEN LIGGER RÄTT*/
   {BTNCOL.I}
END PROCEDURE.
PROCEDURE buttcolm_UI :
   {BTNCOL2.I}
END PROCEDURE.
PROCEDURE buttcolh_UI :
   /*HÖGERJUSTERA KNAPPARNA*/
   {BTNCOLH.I}
END PROCEDURE.
PROCEDURE getFullVersion EXTERNAL "versioninfo.dll" :
   DEFINE OUTPUT PARAM pcver AS CHARACTER. 
   DEFINE INPUT PARAM pilen AS LONG.
   DEFINE RETURN PARAM iret AS LONG.
END.
PROCEDURE rowplus_UI :
   DEFINE INPUT PARAMETER blefth AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER brighth AS HANDLE NO-UNDO.
   DEFINE VARIABLE framehelph AS HANDLE NO-UNDO.
   DEFINE VARIABLE frmh AS DECIMAL NO-UNDO.
   RETURN.

   
   /*
   framehelph = brighth:FRAME.
   FIND FIRST frametemp WHERE frametemp.FRMHAND = brighth:FRAME NO-LOCK NO-ERROR.
   IF AVAILABLE frametemp THEN DO:
      /*
      IF blefth = ? THEN DO:
         brighth:ROW = brighth:ROW * (frametemp.FRMHAND:HEIGHT-PIXELS / frametemp.FHOJD).             
      END.
      ELSE brighth:ROW = blefth:ROW + blefth:HEIGHT-CHARS + 0.1 * (frametemp.FRMHAND:HEIGHT-PIXELS / frametemp.FHOJD).             
      */
      brighth:ROW = brighth:ROW * (frametemp.FRMHAND:HEIGHT-PIXELS / frametemp.FHOJD).             
   END.
   
   FIND FIRST frametemp WHERE frametemp.FRMHAND = brighth:FRAME NO-LOCK NO-ERROR.
   IF AVAILABLE frametemp THEN DO:
      frmh = framehelph:HEIGHT-PIXELS / frametemp.FHOJD.
      IF blefth = ? THEN DO:
         brighth:Y = brighth:Y * (Guru.SharedVariable:globmaxforand).             
      END.
      ELSE brighth:Y = brighth:Y + 0.1 * (Guru.SharedVariable:globmaxforand).             
   END.
     brighth:Y = brighth:Y * (Guru.SharedVariable:globmaxforand).
   */
   
   

             
END PROCEDURE.

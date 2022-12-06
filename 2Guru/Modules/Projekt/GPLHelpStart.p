/*GPLHelpStart.p*/
DEFINE VARIABLE InitAppHand       AS HANDLE  NO-UNDO. /*Används för att kolla om det finns liggare / starta ny liggare*/

PROCEDURE ConGPL_UI:
   IF Guru.Konstanter:appcon THEN DO:
      RUN GPLBER.p PERSISTENT SET InitAppHand ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN GPLBER.p PERSISTENT SET InitAppHand (INPUT Guru.Konstanter:globanv).
   END.
END PROCEDURE.

PROCEDURE DisConGPL_UI:
    IF VALID-HANDLE(InitAppHand) THEN DO: 
       RUN avslutagpl_UI IN InitAppHand.
       DELETE PROCEDURE InitAppHand.
    END.   
    InitAppHand = ?.
END PROCEDURE. 


PROCEDURE gpl_UI :
   DEFINE INPUT PARAMETER aonrvar AS CHARACTER.
   DEFINE INPUT PARAMETER delnrvar AS INTEGER.
   DEFINE VARIABLE startgplroot AS Guru.Root NO-UNDO.
   
   DEFINE VARIABLE fanns AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE plidvar AS INTEGER NO-UNDO. 
   
   RUN ConGPL_UI.

   RUN finnsgpl_UI IN InitAppHand 
   (INPUT aonrvar, INPUT delnrvar,
    OUTPUT fanns, OUTPUT plidvar).
   

   IF fanns THEN. ELSE DO:      
      MESSAGE "Det finns ingen personalliggare för detta projekt, vill du starta en? " 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE starta AS LOGICAL.
   
      IF starta = TRUE THEN DO:
         RUN nygpl_UI IN InitAppHand 
         (INPUT aonrvar, INPUT delnrvar, 
          OUTPUT plidvar).
      END.
      ELSE RETURN.
   END.
   RUN DisConGPL_UI.
   
   startgplroot = NEW Guru.Root().
   startgplroot:StartGPLDb(aonrvar, delnrvar, plidvar).
   startgplroot:StartGPL().
  
   
   DELETE OBJECT startgplroot NO-ERROR.

END PROCEDURE.

PROCEDURE gpltt_UI :
   /*
   DEFINE OUTPUT PARAMETER TABLE FOR ttGPLAKTIVITET.
   DEFINE OUTPUT PARAMETER TABLE FOR ttGPLHUVUD.
   RETURN.
   */
   RETURN.
   
END PROCEDURE.

/*
     Filename: XPERSISTENTTEST.I
      Created: 03.08.0013 14:29ELPAO     
     Modified: 
*/

   /* Följande rader räknar antalet persistent procedures */
   DEFINE VARIABLE temph AS HANDLE NO-UNDO.
   DEFINE VARIABLE nexth AS HANDLE NO-UNDO.
   DEFINE VARIABLE counter AS INTEGER NO-UNDO.
/*    DEFINE VARIABLE counter2 AS INTEGER NO-UNDO. */
   
   temph = SESSION:FIRST-PROCEDURE.
   DO WHILE VALID-HANDLE(temph):
      counter = counter + 1.
      nexth = temph:NEXT-SIBLING.
/*       IF THIS-PROCEDURE:CURRENT-WINDOW = temph:CURRENT-WINDOW THEN DO: */
/*          counter2 = counter2 + 1.                                      */
/*          DELETE PROCEDURE temph.                                       */
/*       END.                                                             */
      MESSAGE temph:FILE-NAME.
      temph = nexth.
   END.
   MESSAGE counter.

/*    temph = SESSION:FIRST-CHILD.                                                                */
/*    DO WHILE VALID-HANDLE(temph):                                                               */
/*       counter = counter + 1.                                                                   */
/*       MESSAGE counter ": " temph:FILE-NAME temph:PRIVATE-DATA.                                 */
/* /*       MESSAGE counter ": " temph:FILE-NAME temph:REMOTE temph:REMOTE CURRENT-WINDOW SKIP */ */
/* /*          temph:PUBLISHED-EVENTS.                                                         */ */
/*       temph = temph:NEXT-SIBLING.                                                              */
/*    END.                                                                                        */
/*    MESSAGE counter.                                                                            */

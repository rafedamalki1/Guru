/*BRWSOK.I*/
DEFINE VARIABLE brwvar AS INTEGER NO-UNDO.     
DEFINE VARIABLE brwopen AS LOGICAL EXTENT 10 NO-UNDO.
/* DEFINE VARIABLE dynok AS LOGICAL NO-UNDO. */
DEFINE VARIABLE sokok AS LOGICAL NO-UNDO.
/* DEFINE VARIABLE firsttime AS LOGICAL NO-UNDO. */
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE multi AS LOGICAL NO-UNDO.
DEFINE VARIABLE brwwh AS WIDGET-HANDLE.
/* DEFINE VARIABLE brwwah AS WIDGET-HANDLE EXTENT 10 NO-UNDO. */
/* DEFINE VARIABLE brwwgh AS WIDGET-HANDLE EXTENT 10 NO-UNDO. */
DEFINE VARIABLE brwsortvar AS INTEGER NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
/* DEFINE VARIABLE nydynbrwh AS HANDLE NO-UNDO. */
/* DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.  */
/* DEFINE VARIABLE colh AS WIDGET-HANDLE NO-UNDO. */
/* DEFINE VARIABLE dynfalth AS HANDLE NO-UNDO. */
/* DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.   */
/* DEFINE VARIABLE inkommandosortquery AS CHARACTER NO-UNDO. */
DEFINE VARIABLE descvar AS LOGICAL EXTENT 10 NO-UNDO.
/* DEFINE VARIABLE faltnamn AS CHARACTER NO-UNDO. */
DEFINE VARIABLE startmultisok AS CHARACTER NO-UNDO.

/* PROCEDURE openbdyn_UI :                                                                                      */
/*    DEFINE INPUT PARAMETER tabvar AS CHARACTER NO-UNDO.                                                       */
/* /*    MESSAGE brwwah[brwvar] brwwgh[brwvar] descvar[brwvar]. */                                              */
/*    IF brwwah[brwvar] NE ? THEN DO:                                                                           */
/*       IF brwwgh[brwvar] = brwwah[brwvar] THEN DO:                                                            */
/*          IF descvar[brwvar] = TRUE THEN descvar[brwvar] = FALSE.                                             */
/*          ELSE descvar[brwvar] = TRUE.                                                                        */
/*       END.                                                                                                   */
/*       ELSE descvar[brwvar] = FALSE.                                                                          */
/*       brwwgh[brwvar] = brwwah[brwvar].                                                                       */
/*    END.                                                                                                      */
/*    ELSE brwwah[brwvar] = brwwgh[brwvar].                                                                     */
/*    IF brwwah[brwvar] = ? THEN DO:                                                                            */
/*       kommandosortquery = "FOR EACH " + tabvar + " NO-LOCK.".                                                */
/*    END.                                                                                                      */
/*    ELSE DO:                                                                                                  */
/*       IF descvar[brwvar] = FALSE THEN                                                                        */
/*       kommandosortquery = "FOR EACH " + tabvar + " NO-LOCK BY " + brwwah[brwvar]:NAME + ".".                 */
/*       ELSE kommandosortquery = "FOR EACH " + tabvar + " NO-LOCK BY " + brwwah[brwvar]:NAME + " DESCENDING.". */
/*    END.                                                                                                      */
/*    dynok = dynbrwh:QUERY-PREPARE(kommandosortquery).                                                         */
/*    IF dynok THEN dynbrwh:QUERY-OPEN().                                                                       */
/*    ELSE DO:                                                                                                  */
/*       MESSAGE "Kunde inte preparera BRW_Query!" VIEW-AS ALERT-BOX.                                           */
/*       RETURN.                                                                                                */
/*    END.                                                                                                      */
/* END PROCEDURE.                                                                                               */


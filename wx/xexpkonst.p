/*KOPIERING AV EN KONSTRUKTION FRÅN EN KONSTRUKTIONSGRUPP TILL EN ANNAN*/

DEFINE BUFFER KBUFF FOR KONSTRUKTION.
DEFINE BUFFER KBUFF2 FOR KONSTVAL.
DEFINE BUFFER KBUFF3 FOR MTRLBER.
DEFINE VARIABLE ORDVAR AS INTEGER NO-UNDO.
DEFINE VARIABLE fran AS CHARACTER NO-UNDO.
DEFINE VARIABLE till AS CHARACTER NO-UNDO.
DEFINE VARIABLE frankons AS INTEGER NO-UNDO.
DEFINE VARIABLE tillkons AS INTEGER NO-UNDO.
/*
från              till
N33/12mf.         N33/12mfK
N83/12kx          N83/12kxK
N33/12A.          N33/12AK
SS2_24A.          SS2_24AK
N33/24x
N33_24A.
SS2/12A.
*/
fran = "S2/12A..".
frankons = 12.
till = "S2/12A.".
tillkons = 30.

FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = fran AND KONSTRUKTION.KONSKOD = frankons NO-LOCK:
   FIND LAST KBUFF USE-INDEX ORDNING NO-LOCK NO-ERROR.
   ORDVAR = KBUFF.ORDNING + 1.
   CREATE KBUFF.
   BUFFER-COPY KONSTRUKTION TO KBUFF.
   ASSIGN
   KBUFF.KTYPKOD = till
   KBUFF.KONSKOD = tillkons
   KBUFF.ORDNING = ORDVAR.   
END.
/*kolla att bbenamning stämmer mellan konstgruktionsgrupperna*/
FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = frankons AND KONSTVAL.KTYPKOD = fran NO-LOCK:
   FIND FIRST KBUFF2 WHERE KBUFF2.KONSKOD = tillkons AND KBUFF2.KVALKOD = KONSTVAL.KVALKOD AND 
   KBUFF2.BB = KONSTVAL.BB NO-LOCK NO-ERROR.
   IF AVAILABLE KBUFF2 THEN ORDVAR = KBUFF2.ORDNING.
   ELSE DO:
      FIND LAST KBUFF2 WHERE KBUFF2.KONSKOD = tillkons AND
      KBUFF2.BB = KONSTVAL.BB USE-INDEX ORD2 NO-LOCK NO-ERROR.
      ORDVAR = KBUFF2.ORDNING + 1.
   END.
   CREATE KBUFF2.
   BUFFER-COPY KONSTVAL TO KBUFF2.
   ASSIGN
   KBUFF2.KTYPKOD = till
   KBUFF2.KONSKOD = tillkons
   KBUFF2.ORDNING = ORDVAR.   
END.
FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = fran NO-LOCK:
   CREATE KBUFF3.
   BUFFER-COPY MTRLBER TO KBUFF3.
   ASSIGN
   KBUFF3.KTYPKOD = till.   
END.




/*KOPIERING AV KONSTVAL FRÅN EN KONSTRUKTION TILL EN ANNAN DÄR DET EJ ÄR SAMMA KONSTRUKTIONSGRUPP
SAMT MATERIEL*/
/* FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = 33 AND                                                         */
/* KONSTVAL.KTYPKOD = "N33_24x" AND KONSTVAL.ORDNING >= 14 AND KONSTVAL.BB = "Lspfördelning/nollkl" NO-LOCK: */
/*    FIND FIRST KBUFF2 WHERE KBUFF2.KONSKOD = 12 AND KBUFF2.KVALKOD = KONSTVAL.KVALKOD AND                  */
/*    KBUFF2.BB = KONSTVAL.BB NO-LOCK NO-ERROR.                                                              */
/*    IF AVAILABLE KBUFF2 THEN ORDVAR = KBUFF2.ORDNING.                                                      */
/*    ELSE DO:                                                                                               */
/*       FIND LAST KBUFF2 WHERE KBUFF2.KONSKOD = 12 AND                                                      */
/*       KBUFF2.BB = KONSTVAL.BB USE-INDEX ORD2 NO-LOCK NO-ERROR.                                            */
/*       ORDVAR = KBUFF2.ORDNING + 1.                                                                        */
/*    END.                                                                                                   */
/*    CREATE KBUFF2.                                                                                         */
/*    BUFFER-COPY KONSTVAL TO KBUFF2.                                                                        */
/*    ASSIGN                                                                                                 */
/*    KBUFF2.KTYPKOD = "Komp12kv"                                                                            */
/*    KBUFF2.KONSKOD = 12                                                                                    */
/*    KBUFF2.ORDNING = ORDVAR.                                                                               */
/*                                                                                                           */
/*    FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = "N33_24x" AND MTRLBER.F2 = KONSTVAL.KVALKOD                   */
/*    NO-LOCK:                                                                                               */
/*       CREATE KBUFF3.                                                                                      */
/*       BUFFER-COPY MTRLBER TO KBUFF3.                                                                      */
/*       ASSIGN                                                                                              */
/*       KBUFF3.KTYPKOD = "Komp12kv".                                                                        */
/*    END.                                                                                                   */
/* END.                                                                                                      */


/*KOPIERING AV MATERIEL FRÅN EN KONSTRUKTION TILL EN ANNAN DÄR DET ÄR SAMMA KONSTVAL OCH DÄR KONSTVAL ÄR TRUE*/
/* FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = 33 AND                                                         */
/* KONSTVAL.KTYPKOD = "N33_24A" AND KONSTVAL.ORDNING >= 14 AND KONSTVAL.BB = "Lspfördelning/nollkl" NO-LOCK: */
/*    FOR EACH KBUFF2 WHERE KBUFF2.KONSKOD = 33 AND KBUFF2.KVALKOD = KONSTVAL.KVALKOD AND                    */
/*    KBUFF2.BB = KONSTVAL.BB AND KBUFF2.KOPP = TRUE AND KBUFF2.KTYPKOD NE "N33_24A" NO-LOCK:                */
/*       FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KBUFF2.KTYPKOD AND MTRLBER.F2 = KBUFF2.KVALKOD             */
/*       EXCLUSIVE-LOCK:                                                                                     */
/*          DELETE MTRLBER.                                                                                  */
/*       END.                                                                                                */
/*       FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = "N33_24A" AND MTRLBER.F2 = KONSTVAL.KVALKOD                */
/*       NO-LOCK:                                                                                            */
/*          CREATE KBUFF3.                                                                                   */
/*          BUFFER-COPY MTRLBER TO KBUFF3.                                                                   */
/*          ASSIGN                                                                                           */
/*          KBUFF3.KTYPKOD = KBUFF2.KTYPKOD.                                                                 */
/*       END.                                                                                                */
/*    END.                                                                                                   */
/* END.                                                                                                      */

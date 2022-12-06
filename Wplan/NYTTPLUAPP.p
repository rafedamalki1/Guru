/*NYTTPLUAPP.P*/
/*Laddar temptabeller åt nyttplanu.W */
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{PLANNRTEMP.I}
{AVTPLANTEMP.I}
{ANSVPLANTEMP.I}
{AUTOMREGTEMP.I}
{DIRDEF.I}
&Scoped-define NEW 
&Scoped-define SHARED  
{BESTKUNDALLT.I}

DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER nyplanvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR bestkundallt.
DEFINE OUTPUT PARAMETER TABLE FOR ansvplantemp.
DEFINE OUTPUT PARAMETER TABLE FOR plankonttemp.
DEFINE OUTPUT PARAMETER TABLE FOR automregtemp.
DEFINE OUTPUT PARAMETER TABLE FOR kbenamntemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR upplantemp.

DEFINE VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE BUFFER ansvaobuff FOR ANSVAONR.

/*Ladda*/ 
IF vart = 1 THEN DO:
   EMPTY TEMP-TABLE automregtemp NO-ERROR.    
   OPEN QUERY auq FOR EACH AUTOMREG USE-INDEX PRISTYPER NO-LOCK.
   GET FIRST auq NO-LOCK.
   DO WHILE AVAILABLE(AUTOMREG):
      CREATE automregtemp.
      BUFFER-COPY AUTOMREG TO automregtemp.
      GET NEXT auq NO-LOCK.         
   END.
   CLOSE QUERY auq.
   
/*Gör om fornamn till aonr och plan     */             
/*    FIND FIRST ANSVAONR WHERE ANSVAONR.FORNAMN = "AONR" NO-LOCK NO-ERROR. */
/*    IF NOT AVAILABLE ANSVAONR THEN DO:                                    */
/*       OPEN QUERY apq FOR EACH ANSVAONR NO-LOCK.                          */
/*       GET FIRST apq NO-LOCK.                                             */
/*       DO WHILE AVAILABLE (ANSVAONR):                                     */
/*          DO TRANSACTION:                                                 */
/*             GET CURRENT apq EXCLUSIVE-LOCK.                              */
/*             ASSIGN                                                       */
/*             ANSVAONR.FORNAMN = "AONR"                                    */
/*             ANSVAONR.EFTERNAMN = "".                                     */
/*             CREATE ansvaobuff.                                           */
/*             BUFFER-COPY ANSVAONR TO ansvaobuff.                          */
/*             ansvaobuff.FORNAMN = "PLAN".                                 */
/*          END.                                                            */
/*          GET NEXT apq NO-LOCK.                                           */
/*       END.                                                               */
/*    END.                                                                  */
/*    CLOSE QUERY apq.                                                      */

/*    FOR EACH ansvplantemp:                                                                           */
/*       DELETE ansvplantemp.                                                                          */
/*    END.                                                                                             */
/*    OPEN QUERY ansq FOR EACH ANSVAONR WHERE ANSVAONR.FORNAMN = "PLAN" NO-LOCK.                       */
/*    GET FIRST ansq NO-LOCK.                                                                          */
/*    DO WHILE AVAILABLE(ANSVAONR):                                                                    */


   EMPTY TEMP-TABLE ansvplantemp NO-ERROR.    
   OPEN QUERY ansq FOR EACH ANSVAONR NO-LOCK,
   EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANSVAONR.PERSONALKOD NO-LOCK.
   GET FIRST ansq NO-LOCK.
   DO WHILE AVAILABLE(ANSVAONR):
      CREATE ansvplantemp.
      ASSIGN 
      ansvplantemp.NAMN = PERSONALTAB.EFTERNAMN + " " + PERSONALTAB.FORNAMN
      ansvplantemp.FORNAMN = PERSONALTAB.FORNAMN 
      ansvplantemp.EFTERNAMN = PERSONALTAB.EFTERNAMN 
      ansvplantemp.PERSONALKOD = ANSVAONR.PERSONALKOD. 
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      GET NEXT ansq NO-LOCK.
   END.
   CLOSE QUERY ansq.
   EMPTY TEMP-TABLE kbenamntemp NO-ERROR.    
   OPEN QUERY kbq FOR EACH KBENAMNING USE-INDEX KBEN NO-LOCK.
   GET FIRST kbq NO-LOCK.
   DO WHILE AVAILABLE(KBENAMNING):
      CREATE kbenamntemp.
      BUFFER-COPY KBENAMNING TO kbenamntemp.
      GET NEXT kbq NO-LOCK.
   END.
   CLOSE QUERY kbq.
   IF nyplanvar = FALSE THEN DO:
      FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar and
      upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF AVAILABLE upplantemp THEN DO:
         EMPTY TEMP-TABLE plankonttemp NO-ERROR.          
         OPEN QUERY plkq FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = plannrvar AND 
         PLANKONTO.ARTAL = artalvar NO-LOCK.
         GET FIRST plkq NO-LOCK.
         DO WHILE AVAILABLE(PLANKONTO):
            CREATE plankonttemp.
             BUFFER-COPY PLANKONTO TO plankonttemp.
            plankonttemp.RECTIDVIS = RECID(PLANKONTO).
            GET NEXT plkq NO-LOCK.
         END.
         CLOSE QUERY plkq.         
      END.
   END.
   
   {GDPRLOGGCLIENT.I}
   RETURN.
END.  


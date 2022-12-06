DEFINE TEMP-TABLE avd_temp
   FIELD AVDELNINGNR LIKE STORDISTRIKT.AVDELNINGNR   
   FIELD NAMN LIKE STORDISTRIKT.NAMN
   INDEX AVD IS PRIMARY AVDELNINGNR.

DEFINE INPUT PARAMETER TABLE FOR avd_temp.
DEFINE INPUT PARAMETER valfore AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER distvar LIKE STORDISTRIKT.DISTRIKTID NO-UNDO.   
DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER alla AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER totkunder AS INTEGER NO-UNDO.

   {muswait.i}   
   IF valfore = TRUE THEN DO:
      FOR EACH avd_temp USE-INDEX AVD:
         OPEN QUERY dq FOR EACH STORDISTRIKT WHERE 
         STORDISTRIKT.AVDELNINGNR = avd_temp.AVDELNINGNR AND 
         STORDISTRIKT.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
         GET FIRST dq NO-LOCK.
         DO WHILE AVAILABLE(STORDISTRIKT):
            distvar = STORDISTRIKT.DISTRIKTID.
            RUN distrikt_UI.
            GET NEXT dq NO-LOCK.
         END.
         CLOSE QUERY dq.
      END.         
   END.
   ELSE DO:
      IF alla = TRUE THEN DO:
         /*OUTPUT TO  C:\A\Störning\GAMLAKUND.TXT.*/     
         OPEN QUERY kq FOR EACH KUNDSTOR WHERE 
         KUNDSTOR.ARTAL = YEAR(bdatum) USE-INDEX DISTRIKT NO-LOCK.
         GET FIRST kq NO-LOCK.
         DO WHILE AVAILABLE(KUNDSTOR):
            /*ta bara med de distrikt som har rapporterat in störningar. Några filer har varit felaktiga så enbart KUNDSTOR har kommit in Lena 20171214 */            
            FIND FIRST STORNINGSTAB WHERE  YEAR(STORNINGSTAB.HDATUM) = KUNDSTOR.ARTAL AND  STORNINGSTAB.DISTRIKTID = KUNDSTOR.DISTRIKTID NO-LOCK NO-ERROR.
            IF AVAILABLE STORNINGSTAB THEN DO:      
      
               
               ASSIGN
               totkunder = totkunder + KUNDSTOR.ANTALKUNDER.
               /*PUT KUNDSTOR.DISTRIKTID KUNDSTOR.ANTALKUNDER  TOTKUNDER SKIP.*/
            END.   
            GET NEXT kq NO-LOCK.
         END.
         CLOSE QUERY kq.
         /*OUTPUT CLOSE.*/
      END.
      ELSE DO:
         RUN distrikt_UI.         
      END.
   END.

PROCEDURE distrikt_UI:
   OPEN QUERY kq FOR EACH KUNDSTOR WHERE KUNDSTOR.DISTRIKTID = distvar AND
   KUNDSTOR.ARTAL = YEAR(bdatum) USE-INDEX DISTRIKT NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KUNDSTOR):      
      /*ta bara med de distrikt som har rapporterat in störningar. Några filer har varit felaktiga så enbart KUNDSTOR har kommit in Lena 20171214 */            
      FIND FIRST STORNINGSTAB WHERE  YEAR(STORNINGSTAB.HDATUM) = KUNDSTOR.ARTAL AND  STORNINGSTAB.DISTRIKTID = distvar NO-LOCK NO-ERROR.
      IF AVAILABLE STORNINGSTAB THEN DO:      
         ASSIGN      
         totkunder = totkunder + KUNDSTOR.ANTALKUNDER.
      END.   
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
END PROCEDURE.

/*VISKONT.I*/
   DEFINE VARIABLE k1var AS CHARACTER NO-UNDO.
   DEFINE VARIABLE k2var AS CHARACTER NO-UNDO.
   DEFINE VARIABLE k3var AS CHARACTER NO-UNDO.
   DEFINE VARIABLE k4var AS CHARACTER NO-UNDO.
   DEFINE VARIABLE k5var AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varaonr LIKE AONRTAB.AONR NO-UNDO.
   DEFINE VARIABLE vardelnr LIKE AONRTAB.DELNR NO-UNDO.
   DEFINE VARIABLE varomr LIKE AONRTAB.OMRADE NO-UNDO.
   DEFINE VARIABLE orevar AS LOGICAL NO-UNDO.   
   DEFINE VARIABLE frimusz AS LOGICAL NO-UNDO.   
   EMPTY TEMP-TABLE viskonttemp NO-ERROR. 
   orevar = FALSE.
   GET FIRST BRW_K1 NO-LOCK.
   IF NOT AVAILABLE FAKTKUNDKONTO THEN DO:
      RETURN.
   END.
   IF AVAILABLE FAKTKUNDKONTO THEN DO:
      FIND FIRST FAKTFRIA WHERE FAKTFRIA.FAKTNR = FAKTKUNDKONTO.FAKTNR AND
      FAKTFRIA.FDELNR = FAKTKUNDKONTO.FDELNR AND FAKTFRIA.FAKTURERAD = TRUE AND 
      FAKTFRIA.TYP BEGINS "FAKT" NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTFRIA THEN DO:
         IF FAKTFRIA.VFAKTNR = 0 THEN frimusz = FALSE.
         ELSE frimusz = TRUE.
         
      END.           
      IF frimusz = TRUE THEN RUN OMKONT.P (INPUT 1,INPUT FAKTKUNDKONTO.FAKTNR,INPUT FAKTKUNDKONTO.FDELNR,OUTPUT TABLE viskonttemp).
      ELSE DO:
         DO WHILE AVAILABLE(FAKTKUNDKONTO):   
            /*har man inga aonr får man inte moms på kundkonto*/
          
            FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 1 AND 
            viskonttemp.AONR = FAKTKUNDKONTO.AONR AND viskonttemp.DELNR = FAKTKUNDKONTO.DELNR AND
            viskonttemp.KONTO = KUNDFODRAN.KUNDKONTO AND viskonttemp.MOTPART = MOTPART.MOTPART
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE viskonttemp THEN DO:
               CREATE viskonttemp.
               IF varaonr = FAKTKUNDKONTO.AONR AND vardelnr = FAKTKUNDKONTO.DELNR THEN musz = musz.
               ELSE DO:
                  FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTKUNDKONTO.AONR AND 
                  AONRTAB.DELNR = FAKTKUNDKONTO.DELNR NO-LOCK NO-ERROR.
                  /*har man inga aonr får man inte moms på kundkonto*/
                  ASSIGN 
                  k1var = ""
                  k2var = ""
                  k3var = ""
                  k4var = ""
                  k5var = ""
                  varomr = AONRTAB.OMRADE
                  varaonr = AONRTAB.AONR          
                  vardelnr = AONRTAB.DELNR.     
                  FIND FIRST FAKTAONRKONTO WHERE 
                  FAKTAONRKONTO.FDELNR = 0 AND
                  FAKTAONRKONTO.FAKTNR = FAKTKUNDKONTO.FAKTNR AND
                  FAKTAONRKONTO.VFAKTNR = FAKTKUNDKONTO.VFAKTNR AND 
                  FAKTAONRKONTO.AONR = FAKTKUNDKONTO.AONR AND 
                  FAKTAONRKONTO.DELNR = FAKTKUNDKONTO.DELNR
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE FAKTAONRKONTO THEN DO:
                     ASSIGN
                     k1var = FAKTAONRKONTO.K1  
                     k2var = FAKTAONRKONTO.K2  
                     k3var = FAKTAONRKONTO.K3  
                     k4var = FAKTAONRKONTO.K4  
                     k5var = FAKTAONRKONTO.K5.  
                  END.
                  ELSE DO:
                     FIND FIRST AONRKONTKOD WHERE 
                     AONRKONTKOD.AONR = FAKTKUNDKONTO.AONR AND 
                     AONRKONTKOD.DELNR = FAKTKUNDKONTO.DELNR
                     NO-LOCK NO-ERROR.
                     IF AVAILABLE FAKTAONRKONTO THEN DO:
                        ASSIGN
                        k1var = AONRKONTKOD.K1   
                        k2var = AONRKONTKOD.K2   
                        k3var = AONRKONTKOD.K3   
                        k4var = AONRKONTKOD.K4   
                        k5var = AONRKONTKOD.K5.  
                     END.
                  END.
               END.
               ASSIGN
               viskonttemp.OMRADE = varomr.
               IF orevar = FALSE THEN DO:
                  orevar = TRUE.
                  ASSIGN
                  viskonttemp.ORVAR = TRUE
                  viskonttemp.DEBET = FAKTURERAD.ORESUTJ.
               END.   
               ASSIGN
               viskonttemp.K1 = k1var
               viskonttemp.K2 = k2var
               viskonttemp.K3 = k3var
               viskonttemp.K4 = k4var
               viskonttemp.K5 = k5var.
            END.
            ASSIGN 
            viskonttemp.ORDNING = 1
            viskonttemp.AONR = FAKTKUNDKONTO.AONR      
            viskonttemp.DELNR = FAKTKUNDKONTO.DELNR 
            viskonttemp.VDELNR = STRING(FAKTKUNDKONTO.DELNR,Guru.Konstanter:varforetypchar[1])      
            viskonttemp.KONTO = KUNDFODRAN.KUNDKONTO      
            viskonttemp.MOTPART = MOTPART.MOTPART      
            viskonttemp.DEBET = viskonttemp.DEBET + FAKTKUNDKONTO.BELOPP
            viskonttemp.KREDIT = 0.      
            
          
            GET NEXT BRW_K1 NO-LOCK.
         END.
      END.
   END.
   IF frimusz = TRUE THEN frimusz = frimusz.
   ELSE DO:
      GET FIRST BRW_K2 NO-LOCK.
      DO WHILE AVAILABLE(FAKTINTAKTKONT):
         FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 3 AND 
         viskonttemp.AONR = FAKTINTAKTKONT.AONR AND viskonttemp.DELNR = FAKTINTAKTKONT.DELNR AND
         viskonttemp.KONTO = INTAKTTAB.INTAKTKONTO AND viskonttemp.MOTPART = MOTPART.MOTPART
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE viskonttemp THEN DO:
            CREATE viskonttemp.
            IF varaonr = FAKTINTAKTKONT.AONR AND vardelnr = FAKTINTAKTKONT.DELNR THEN musz = musz.
            ELSE DO:
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTINTAKTKONT.AONR AND
               AONRTAB.DELNR = FAKTINTAKTKONT.DELNR NO-LOCK NO-ERROR.
               ASSIGN 
               varomr = AONRTAB.OMRADE
               varaonr = AONRTAB.AONR
               vardelnr = AONRTAB.DELNR.
            END.
            viskonttemp.OMRADE = varomr.
         END.
         ASSIGN 
         viskonttemp.ORDNING = 3
         viskonttemp.AONR = FAKTINTAKTKONT.AONR      
         viskonttemp.DELNR = FAKTINTAKTKONT.DELNR      
         viskonttemp.VDELNR = STRING(FAKTINTAKTKONT.DELNR,Guru.Konstanter:varforetypchar[1]) 
         viskonttemp.KONTO = INTAKTTAB.INTAKTKONTO      
         viskonttemp.MOTPART = MOTPART.MOTPART      
         viskonttemp.DEBET = 0 
         viskonttemp.KREDIT = viskonttemp.KREDIT + FAKTINTAKTKONT.BELOPP.  
           
         GET NEXT BRW_K2 NO-LOCK.
      END. 
   END.
   orevar = FALSE.
   IF FAKTURERAD.ORESUTJ NE 0 THEN DO:
      FIND FIRST viskonttempbuff WHERE viskonttempbuff.ORVAR = TRUE NO-LOCK NO-ERROR.
      IF Guru.Konstanter:varforetypval[15] NE  0 THEN DO:
         CREATE viskonttemp.         
         ASSIGN 
         viskonttemp.ORDNING = 2.5
         viskonttemp.KONTO = STRING(Guru.Konstanter:varforetypval[15])
         viskonttemp.MOTPART = ""
         viskonttemp.DEBET = 0 
         viskonttemp.KREDIT = FAKTURERAD.ORESUTJ
         viskonttemp.K1 = viskonttempbuff.K1
         viskonttemp.K2 = viskonttempbuff.K2
         viskonttemp.K3 = viskonttempbuff.K3
         viskonttemp.K4 = viskonttempbuff.K4
         viskonttemp.K5 = viskonttempbuff.K5.
        
      END.
   END.
   IF frimusz = TRUE THEN frimusz = frimusz.
   ELSE DO:
      GET FIRST BRW_K4 NO-LOCK.
      DO WHILE AVAILABLE(FAKTMOMS):
         FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 2 AND 
         viskonttemp.KONTO = MOMSTAB.MOMSKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE viskonttemp THEN DO:
            CREATE viskonttemp.         
         END.
         IF orevar = FALSE THEN DO:
            IF Guru.Konstanter:varforetypval[15] = 0 THEN DO:
               orevar = TRUE.
               viskonttemp.KREDIT = FAKTURERAD.ORESUTJ.
            END.        
         END.    
         ASSIGN 
         viskonttemp.ORDNING = 2
         viskonttemp.KONTO = MOMSTAB.MOMSKOD      
         viskonttemp.MOTPART = ""
         viskonttemp.DEBET = 0 
         viskonttemp.KREDIT = viskonttemp.KREDIT + FAKTMOMS.MOMSBELOPP.  
         IF SUBSTRING(FAKTMOMS.FRITEXT,150,1) = "" OR SUBSTRING(FAKTMOMS.FRITEXT,150,1) = "0" THEN DO:
            FIND FIRST vismomstemp WHERE vismomstemp.AONR = FAKTMOMS.AONR AND
            vismomstemp.DELNR = FAKTMOMS.DELNR NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST vismomstemp WHERE vismomstemp.AONR = FAKTMOMS.AONR AND
            vismomstemp.DELNR = FAKTMOMS.DELNR AND
            vismomstemp.MOTPARTID = INTEGER(SUBSTRING(FAKTMOMS.FRITEXT,150))              
            NO-ERROR.
         END.
         IF NOT AVAILABLE vismomstemp THEN CREATE vismomstemp.
         ASSIGN
         vismomstemp.AONR = FAKTMOMS.AONR 
         vismomstemp.DELNR = FAKTMOMS.DELNR
         vismomstemp.MOMS = vismomstemp.MOMS + FAKTMOMS.MOMSBELOPP.
         IF SUBSTRING(FAKTMOMS.FRITEXT,150,1) = "" OR SUBSTRING(FAKTMOMS.FRITEXT,150,1) = "0" THEN DO:            
         END.
         ELSE DO:
            vismomstemp.MOTPARTID = INTEGER(SUBSTRING(FAKTMOMS.FRITEXT,150)).                          
         END.
         GET NEXT BRW_K4 NO-LOCK.
      END.
      RELEASE vismomstemp NO-ERROR.
      RELEASE viskonttemp NO-ERROR.
      /*har man inga aonr får man inte moms på kundkonto*/ 
      
      /*
      OPEN QUERY vmq FOR EACH vismomstemp,
      EACH viskonttemp WHERE viskonttemp.ORDNING = 1 AND
      viskonttemp.AONR = vismomstemp.AONR AND viskonttemp.DELNR = vismomstemp.DELNR.
      GET FIRST vmq.
      DO WHILE AVAILABLE(vismomstemp):                       
         viskonttemp.DEBET = viskonttemp.DEBET + vismomstemp.MOMS.            
         GET NEXT vmq.
      END.    
      CLOSE QUERY vmq.   
      */
      FOR EACH vismomstemp:
         FIND FIRST MOTPART WHERE MOTPART.MOTPARTID = vismomstemp.MOTPARTID NO-LOCK NO-ERROR.
         IF AVAILABLE MOTPART THEN DO:
            ASSIGN
            vismomstemp.MOTPART = MOTPART.MOTPART.
         END.
      END.
      OPEN QUERY vmq FOR EACH vismomstemp, 
      EACH viskonttemp WHERE viskonttemp.ORDNING = 1 AND
      viskonttemp.AONR = vismomstemp.AONR AND viskonttemp.DELNR = vismomstemp.DELNR AND 
      viskonttemp.MOTPART = vismomstemp.MOTPART.
      GET FIRST vmq.
      DO WHILE AVAILABLE(vismomstemp):                       
         viskonttemp.DEBET = viskonttemp.DEBET + vismomstemp.MOMS.            
         GET NEXT vmq.
      END.    
      CLOSE QUERY vmq.   
      
   END.
   IF FAKTURERINGSTYP.TIDIGAREFAKT = TRUE AND FAKTURERINGSTYP.SLUT = TRUE THEN DO: 
      FIND FIRST faktureradbuff WHERE faktureradbuff.FAKTNR = FAKTPLAN.FAKTNR AND
      faktureradbuff.VFAKTNR NE 0 NO-LOCK NO-ERROR.
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = faktureradbuff.FAKTTYPID NO-LOCK NO-ERROR.         
      RUN styrintakt_UI.                  
   END.
   /*
   FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND
   FAKTURERAD.FDELNR = FAKTURERAD.FDELNR NO-LOCK NO-ERROR.
   */
   FIND FIRST FAKTURERINGSTYP WHERE 
   FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
   CREATE viskonttemp.
   ASSIGN
   viskonttemp.ORDNING = 99
   viskonttemp.AONR = "Summa:"
   viskonttemp.DEBET = 0 
   viskonttemp.KREDIT = 0.
   RELEASE viskonttemp.
   FIND FIRST viskonttempbuff WHERE viskonttempbuff.ORDNING = 99 NO-ERROR.   
   OPEN QUERY BRW_VIS FOR EACH viskonttemp WHERE viskonttemp.ORDNING NE 99.
   GET FIRST BRW_VIS.
   DO WHILE AVAILABLE(viskonttemp):      
      IF viskonttemp.DEBET = 0 AND viskonttemp.KREDIT = 0 THEN DELETE viskonttemp.
      ELSE DO:
         FIND FIRST viskonttempbuff WHERE viskonttempbuff.ORDNING = 99 NO-ERROR.   
         ASSIGN       
         viskonttempbuff.DEBET = viskonttempbuff.DEBET + viskonttemp.DEBET 
         viskonttempbuff.KREDIT = viskonttempbuff.KREDIT + viskonttemp.KREDIT.               
      END.
      GET NEXT BRW_VIS.
   END.
   

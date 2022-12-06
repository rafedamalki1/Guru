/*VISKONTK.I*/
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
   IF NOT AVAILABLE FAKTKUNDKONTOKRED THEN DO:
      RETURN.
   END.
   IF AVAILABLE FAKTKUNDKONTOKRED THEN DO:
      FIND FIRST FAKTFRIAKRED WHERE FAKTFRIAKRED.FAKTNR = FAKTKUNDKONTOKRED.FAKTNR AND
      FAKTFRIAKRED.FDELNR = FAKTKUNDKONTOKRED.FDELNR AND FAKTFRIAKRED.FAKTURERAD = TRUE AND 
      FAKTFRIAKRED.TYP BEGINS "FAKT" NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTFRIAKRED THEN DO:
         IF FAKTFRIAKRED.VKREDIT = 0 THEN frimusz = FALSE.
         ELSE frimusz = TRUE.
         
      END.           
      IF frimusz = TRUE THEN RUN OMKONT.P (INPUT 2,INPUT FAKTKUNDKONTOKRED.FAKTNR,INPUT FAKTKUNDKONTOKRED.FDELNR,OUTPUT TABLE viskonttemp).
      ELSE DO:
         DO WHILE AVAILABLE(FAKTKUNDKONTOKRED):
          
            FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 1 AND 
            viskonttemp.AONR = FAKTKUNDKONTOKRED.AONR AND viskonttemp.DELNR = FAKTKUNDKONTOKRED.DELNR AND
            viskonttemp.KONTO = KUNDFODRAN.KUNDKONTO AND viskonttemp.MOTPART = MOTPART.MOTPART
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE viskonttemp THEN DO:
               CREATE viskonttemp.
               IF varaonr = FAKTKUNDKONTOKRED.AONR AND vardelnr = FAKTKUNDKONTOKRED.DELNR THEN musz = musz.
               ELSE DO:
                  FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTKUNDKONTOKRED.AONR AND 
                  AONRTAB.DELNR = FAKTKUNDKONTOKRED.DELNR NO-LOCK NO-ERROR.
                  ASSIGN 
                  k1var = ""
                  k2var = ""
                  k3var = ""
                  k4var = ""
                  k5var = ""
                  varomr = AONRTAB.OMRADE
                  varaonr = AONRTAB.AONR
                  vardelnr = AONRTAB.DELNR.  
                  FIND FIRST FAKTAONRKONTOKRED WHERE 
                  FAKTAONRKONTOKRED.FDELNR = 0 AND
                  FAKTAONRKONTOKRED.FAKTNR = FAKTKUNDKONTOKRED.FAKTNR AND
                  FAKTAONRKONTOKRED.VKREDIT = FAKTKUNDKONTOKRED.VKREDIT AND 
                  FAKTAONRKONTOKRED.AONR = FAKTKUNDKONTOKRED.AONR AND 
                  FAKTAONRKONTOKRED.DELNR = FAKTKUNDKONTOKRED.DELNR
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE FAKTAONRKONTOKRED THEN DO:
                     ASSIGN
                     k1var = FAKTAONRKONTOKRED.K1  
                     k2var = FAKTAONRKONTOKRED.K2  
                     k3var = FAKTAONRKONTOKRED.K3  
                     k4var = FAKTAONRKONTOKRED.K4  
                     k5var = FAKTAONRKONTOKRED.K5.  
                  END.
                  ELSE DO:
                     FIND FIRST AONRKONTKOD WHERE 
                     AONRKONTKOD.AONR = FAKTKUNDKONTOKRED.AONR AND 
                     AONRKONTKOD.DELNR = FAKTKUNDKONTOKRED.DELNR
                     NO-LOCK NO-ERROR.
                     IF AVAILABLE FAKTAONRKONTOKRED THEN DO:
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
                  viskonttemp.DEBET = FAKTKRED.ORESUTJ.
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
            viskonttemp.AONR = FAKTKUNDKONTOKRED.AONR      
            viskonttemp.DELNR = FAKTKUNDKONTOKRED.DELNR 
            viskonttemp.VDELNR = STRING(FAKTKUNDKONTOKRED.DELNR,Guru.Konstanter:varforetypchar[1])      
            viskonttemp.KONTO = KUNDFODRAN.KUNDKONTO      
            viskonttemp.MOTPART = MOTPART.MOTPART      
            viskonttemp.DEBET = viskonttemp.DEBET + FAKTKUNDKONTOKRED.BELOPP
            viskonttemp.KREDIT = 0.
           
            GET NEXT BRW_K1 NO-LOCK.
         END.
      END.
   END.
   IF frimusz = TRUE THEN frimusz = frimusz.
   ELSE DO:
      GET FIRST BRW_K2 NO-LOCK.
      DO WHILE AVAILABLE(FAKTINTAKTKONTKRED):
         FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 3 AND 
         viskonttemp.AONR = FAKTINTAKTKONTKRED.AONR AND viskonttemp.DELNR = FAKTINTAKTKONTKRED.DELNR AND
         viskonttemp.KONTO = INTAKTTAB.INTAKTKONTO AND viskonttemp.MOTPART = MOTPART.MOTPART
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE viskonttemp THEN DO:
            CREATE viskonttemp.
            IF varaonr = FAKTINTAKTKONTKRED.AONR AND vardelnr = FAKTINTAKTKONTKRED.DELNR THEN musz = musz.
            ELSE DO:
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTINTAKTKONTKRED.AONR AND
               AONRTAB.DELNR = FAKTINTAKTKONTKRED.DELNR NO-LOCK NO-ERROR.
               ASSIGN 
               varomr = AONRTAB.OMRADE
               varaonr = AONRTAB.AONR
               vardelnr = AONRTAB.DELNR.
            END.
            viskonttemp.OMRADE = varomr.
         END.
         ASSIGN 
         viskonttemp.ORDNING = 3
         viskonttemp.AONR = FAKTINTAKTKONTKRED.AONR      
         viskonttemp.DELNR = FAKTINTAKTKONTKRED.DELNR      
         viskonttemp.VDELNR = STRING(FAKTINTAKTKONTKRED.DELNR,Guru.Konstanter:varforetypchar[1]) 
         viskonttemp.KONTO = INTAKTTAB.INTAKTKONTO      
         viskonttemp.MOTPART = MOTPART.MOTPART      
         viskonttemp.DEBET = 0 
         viskonttemp.KREDIT = viskonttemp.KREDIT + FAKTINTAKTKONTKRED.BELOPP.
         GET NEXT BRW_K2 NO-LOCK.
      END.   
   END.
   orevar = FALSE.
   IF FAKTKRED.ORESUTJ NE 0 THEN DO:
      FIND FIRST viskonttempbuff WHERE viskonttempbuff.ORVAR = TRUE NO-LOCK NO-ERROR.
      IF Guru.Konstanter:varforetypval[15] NE  0 THEN DO:
         CREATE viskonttemp.         
         ASSIGN 
         viskonttemp.ORDNING = 2.5
         viskonttemp.KONTO = STRING(Guru.Konstanter:varforetypval[15])
         viskonttemp.MOTPART = ""
         viskonttemp.DEBET = 0 
         viskonttemp.KREDIT = FAKTKRED.ORESUTJ
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
      DO WHILE AVAILABLE(FAKTMOMSKRED):
         FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 2 AND 
         viskonttemp.KONTO = MOMSTAB.MOMSKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE viskonttemp THEN DO:
            CREATE viskonttemp.
         END.
         IF Guru.Konstanter:varforetypval[15] = 0 THEN DO:
            IF orevar = FALSE THEN DO:
               orevar = TRUE.                  
               viskonttemp.KREDIT = FAKTKRED.ORESUTJ.
            END.
         END.
         ASSIGN 
         viskonttemp.ORDNING = 2
         viskonttemp.KONTO = MOMSTAB.MOMSKOD      
         viskonttemp.MOTPART = ""
         viskonttemp.DEBET = 0 
         viskonttemp.KREDIT = viskonttemp.KREDIT + FAKTMOMSKRED.MOMSBELOPP.
         IF SUBSTRING(FAKTMOMSKRED.FRITEXT,150,1) = "" OR SUBSTRING(FAKTMOMSKRED.FRITEXT,150,1) = "0" THEN DO:
            FIND FIRST vismomstemp WHERE vismomstemp.AONR = FAKTMOMSKRED.AONR AND
            vismomstemp.DELNR = FAKTMOMSKRED.DELNR NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST vismomstemp WHERE vismomstemp.AONR = FAKTMOMSKRED.AONR AND
            vismomstemp.DELNR = FAKTMOMSKRED.DELNR AND
            vismomstemp.MOTPARTID = INTEGER(SUBSTRING(FAKTMOMSKRED.FRITEXT,150))              
            NO-ERROR.
         END.

         
         IF NOT AVAILABLE vismomstemp THEN CREATE vismomstemp.
         ASSIGN
         vismomstemp.AONR = FAKTMOMSKRED.AONR 
         vismomstemp.DELNR = FAKTMOMSKRED.DELNR
         vismomstemp.MOMS = vismomstemp.MOMS + FAKTMOMSKRED.MOMSBELOPP. 
         IF SUBSTRING(FAKTMOMSKRED.FRITEXT,150,1) = "" OR SUBSTRING(FAKTMOMSKRED.FRITEXT,150,1) = "0" THEN DO:            
         END.
         ELSE DO:
            vismomstemp.MOTPARTID = INTEGER(SUBSTRING(FAKTMOMSKRED.FRITEXT,150)).                          
         END.
         GET NEXT BRW_K4 NO-LOCK.
      END.       
      RELEASE vismomstemp NO-ERROR.
      RELEASE viskonttemp NO-ERROR.
      /*
      OPEN QUERY vmq FOR EACH vismomstemp,
      EACH viskonttemp WHERE viskonttemp.ORDNING = 1 AND
      viskonttemp.AONR = vismomstemp.AONR AND viskonttemp.DELNR = vismomstemp.DELNR.
      GET FIRST vmq.
      DO WHILE AVAILABLE(viskonttemp):           
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
   FIND FIRST FAKTURERINGSTYP WHERE 
   FAKTURERINGSTYP.FAKTTYPID = FAKTKRED.FAKTTYPID NO-LOCK NO-ERROR.
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
         ASSIGN       
         viskonttempbuff.DEBET = viskonttempbuff.DEBET + viskonttemp.DEBET 
         viskonttempbuff.KREDIT = viskonttempbuff.KREDIT + viskonttemp.KREDIT.      
      END.
      GET NEXT BRW_VIS.
   END.
   

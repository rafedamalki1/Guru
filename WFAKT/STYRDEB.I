/*STYRDEB.I*/   
   DEFINE VARIABLE varaonr LIKE AONRTAB.AONR NO-UNDO.
   DEFINE VARIABLE vardelnr LIKE AONRTAB.DELNR NO-UNDO.
   DEFINE VARIABLE varomr LIKE AONRTAB.OMRADE NO-UNDO.      
   IF FAKTURERAD.VFAKTNR NE 0 THEN DO:
      OPEN QUERY fakintq FOR EACH FAKTINTAKTKONT WHERE 
      FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND 
      FAKTINTAKTKONT.VFAKTNR NE FAKTURERAD.VFAKTNR NO-LOCK, 
      EACH INTAKTTAB WHERE INTAKTTAB.INTAKTID = FAKTINTAKTKONT.INTAKTID NO-LOCK, 
      EACH MOTPART WHERE MOTPART.MOTPARTID = FAKTINTAKTKONT.MOTPARTID NO-LOCK.
   END.
   ELSE DO:   
      OPEN QUERY fakintq FOR EACH FAKTINTAKTKONT WHERE 
      FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND 
      FAKTINTAKTKONT.VFAKTNR NE 0 NO-LOCK, 
      EACH INTAKTTAB WHERE INTAKTTAB.INTAKTID = FAKTINTAKTKONT.INTAKTID NO-LOCK, 
      EACH MOTPART WHERE MOTPART.MOTPARTID = FAKTINTAKTKONT.MOTPARTID NO-LOCK.
   END.   
   GET FIRST fakintq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONT): 
      IF FAKTINTAKTKONT.VFAKTNR = 0 THEN musz = musz.
      ELSE DO:
         FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 98 AND 
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
         viskonttemp.ORDNING = 98
         viskonttemp.AONR = FAKTINTAKTKONT.AONR      
         viskonttemp.DELNR = FAKTINTAKTKONT.DELNR      
         viskonttemp.VDELNR = STRING(FAKTINTAKTKONT.DELNR,Guru.Konstanter:varforetypchar[1]) 
         viskonttemp.KONTO = INTAKTTAB.INTAKTKONTO      
         viskonttemp.MOTPART = MOTPART.MOTPART
         viskonttemp.DEBET = viskonttemp.DEBET + FAKTINTAKTKONT.BELOPP 
         viskonttemp.KREDIT = 0.

         
         FIND FIRST viskonttemp WHERE viskonttemp.ORDNING = 3 AND 
         viskonttemp.AONR = FAKTINTAKTKONT.AONR AND viskonttemp.DELNR = FAKTINTAKTKONT.DELNR AND
         viskonttemp.MOTPART = MOTPART.MOTPART
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE viskonttemp THEN DO:
            CREATE viskonttemp.
            viskonttemp.KONTO = INTAKTTAB.INTAKTKONTO.      
         END.      
         ASSIGN 
         viskonttemp.ORDNING = 3
         viskonttemp.AONR = FAKTINTAKTKONT.AONR      
         viskonttemp.DELNR = FAKTINTAKTKONT.DELNR      
         viskonttemp.VDELNR = STRING(FAKTINTAKTKONT.DELNR,Guru.Konstanter:varforetypchar[1]) 
         viskonttemp.MOTPART = MOTPART.MOTPART            
         viskonttemp.KREDIT = viskonttemp.KREDIT + FAKTINTAKTKONT.BELOPP.         
      END.
      GET NEXT fakintq NO-LOCK.
   END.

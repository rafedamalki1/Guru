/*PLANNRAPP.P*/
/* Uför operationer åt PLANNRU.W */
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{RHMT.I}
{DHMT.I}
{VHMT.I}
{PLANNRTEMP.I}
{DIRDEF.I}
DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR plannrtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valplantemp.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE BUFFER planbuff FOR PLANNRTAB.
/*bort_UI*/
IF vart = 1 THEN DO:
   /*Ta bort konton*/ 
   FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND 
   PLANNRTAB.ARTAL = artalvar NO-LOCK NO-ERROR.
   DEFINE QUERY bq FOR PLANKONTO.
   OPEN QUERY bq FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND
   PLANKONTO.ARTAL = PLANNRTAB.ARTAL USE-INDEX PLANKONT NO-LOCK.        
   DO TRANSACTION:
      GET FIRST bq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(PLANKONTO):      
         DELETE PLANKONTO.
         GET NEXT bq EXCLUSIVE-LOCK.                 
      END.
   END. 
   CLOSE QUERY bq.
   /*Ta bort uppdelning*/          
   IF PLANNRTAB.UPPNR = TRUE THEN DO TRANSACTION:
      FIND FIRST UPPDELA WHERE UPPDELA.PLANNR = PLANNRTAB.PLANNR AND
      UPPDELA.ARTAL = PLANNRTAB.ARTAL USE-INDEX PLAN EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE UPPDELA THEN DELETE UPPDELA.            
      OPEN QUERY kq 
      FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND
      PLANKONTO.ARTAL = PLANNRTAB.ARTAL + 1 USE-INDEX PLAN NO-LOCK.            
      GET FIRST kq EXCLUSIVE-LOCK.                       
      DO WHILE AVAILABLE(PLANKONTO):
         DELETE PLANKONTO.
         GET NEXT kq EXCLUSIVE-LOCK.                  
      END.
      CLOSE QUERY kq.
      FIND FIRST planbuff WHERE planbuff.PLANNR = PLANNRTAB.PLANNR AND
      planbuff.ARTAL = PLANNRTAB.ARTAL + 1 
      USE-INDEX PLAN EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE planbuff THEN DELETE planbuff.                                                                                               
   END.   
   /*Ta bort kopplingen till kalkyler*/  
   DEFINE QUERY fq FOR FASTSPEC.
   OPEN QUERY fq FOR EACH FASTSPEC WHERE FASTSPEC.PLANNR = PLANNRTAB.PLANNR AND
   FASTSPEC.ARTAL = PLANNRTAB.ARTAL USE-INDEX PLAN NO-LOCK.
   DO TRANSACTION:
      GET FIRST fq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(FASTSPEC):                   
         IF AVAILABLE FASTSPEC THEN DO:
            ASSIGN FASTSPEC.PLANNR = ?
            FASTSPEC.ARTAL = ?.
         END.    
         GET NEXT fq EXCLUSIVE-LOCK.
      END.
   END.
   CLOSE QUERY fq.              
   DO TRANSACTION:
      FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND 
      PLANNRTAB.ARTAL = artalvar EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE PLANNRTAB THEN DELETE PLANNRTAB. 
      FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
      valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF AVAILABLE valplantemp THEN DELETE valplantemp. 
      FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar 
      AND plannrtemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF AVAILABLE plannrtemp THEN DELETE plannrtemp.
   END.  
END.
/*Ta bort uppdel*/
IF vart = 2 THEN DO:
   FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND 
   PLANNRTAB.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF AVAILABLE PLANNRTAB THEN DO:
      IF PLANNRTAB.UPPNR = TRUE THEN DO TRANSACTION:
         FIND FIRST UPPDELA WHERE UPPDELA.PLANNR = PLANNRTAB.PLANNR AND
         UPPDELA.ARTAL = PLANNRTAB.ARTAL USE-INDEX PLAN EXCLUSIVE-LOCK NO-ERROR.
         DELETE UPPDELA.
         OPEN QUERY uq
         FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND
         PLANKONTO.ARTAL = PLANNRTAB.ARTAL + 1 USE-INDEX PLAN NO-LOCK.
         GET FIRST uq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(PLANKONTO):
            DELETE PLANKONTO.
            GET NEXT uq EXCLUSIVE-LOCK.
         END.
         CLOSE QUERY uq.
         FIND FIRST planbuff WHERE planbuff.PLANNR = PLANNRTAB.PLANNR AND
         planbuff.ARTAL = PLANNRTAB.ARTAL + 1
         USE-INDEX PLAN EXCLUSIVE-LOCK NO-ERROR.
         DELETE planbuff.
         FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
         valplantemp.ARTAL = artalvar + 1  NO-LOCK NO-ERROR.
         IF AVAILABLE valplantemp THEN DELETE valplantemp.
         FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar AND 
         plannrtemp.ARTAL = artalvar + 1  NO-LOCK NO-ERROR.
         IF AVAILABLE plannrtemp THEN DELETE plannrtemp.
         FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND 
         PLANNRTAB.ARTAL = artalvar EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN
         PLANNRTAB.UPP = FALSE
         PLANNRTAB.UPPNR = FALSE.
         FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
         valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
         IF AVAILABLE valplantemp THEN DO:
            ASSIGN
            valplantemp.UPP = FALSE
            valplantemp.UPPNR = FALSE.
         END.
         FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar AND 
         plannrtemp.ARTAL = artalvar NO-LOCK NO-ERROR.
         IF AVAILABLE plannrtemp THEN DO:
            ASSIGN
            plannrtemp.UPP = FALSE
            plannrtemp.UPPNR = FALSE.
         END.
      END.
      ELSE DO TRANSACTION:         
         FIND FIRST UPPDELA WHERE UPPDELA.PLANNR = PLANNRTAB.PLANNR AND
         UPPDELA.ARTAL = PLANNRTAB.ARTAL - 1 USE-INDEX PLAN EXCLUSIVE-LOCK NO-ERROR.
         DELETE UPPDELA.
         OPEN QUERY dq
         FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND
         PLANKONTO.ARTAL = PLANNRTAB.ARTAL USE-INDEX PLAN NO-LOCK.
         GET FIRST dq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(PLANKONTO):
            DELETE PLANKONTO.
            GET NEXT dq EXCLUSIVE-LOCK.
         END.
         CLOSE QUERY dq.
         FIND FIRST planbuff WHERE planbuff.PLANNR = PLANNRTAB.PLANNR AND
         planbuff.ARTAL = PLANNRTAB.ARTAL - 1
         USE-INDEX PLAN EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN
         planbuff.UPP = FALSE
         planbuff.UPPNR = FALSE.
         FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
         valplantemp.ARTAL = PLANNRTAB.ARTAL - 1 NO-LOCK NO-ERROR.
         IF AVAILABLE valplantemp THEN DO:
            ASSIGN
            valplantemp.UPP = FALSE
            valplantemp.UPPNR = FALSE.
         END.
         FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar AND 
         plannrtemp.ARTAL = PLANNRTAB.ARTAL - 1 NO-LOCK NO-ERROR.
         IF AVAILABLE plannrtemp THEN DO:
            ASSIGN
            plannrtemp.UPP = FALSE
            plannrtemp.UPPNR = FALSE.
         END.
         FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND 
         PLANNRTAB.ARTAL = artalvar EXCLUSIVE-LOCK NO-ERROR.
         DELETE PLANNRTAB.
         FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
         valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
         IF AVAILABLE valplantemp THEN DELETE valplantemp.
         FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar AND 
         plannrtemp.ARTAL = artalvar NO-LOCK NO-ERROR.
         IF AVAILABLE plannrtemp THEN DELETE plannrtemp.
      END.
   END.
END.

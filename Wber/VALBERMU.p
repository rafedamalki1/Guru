   /*VALBERMU.P RÄTT ORDNING PÅ KONSTRUKTIONERNA*/
{KONVALTEMP.I} 
{KONVAL2TEMP.I}    

DEFINE TEMP-TABLE kon_id2
   FIELD NUM LIKE BERVAL.NUM
   FIELD GRUPP LIKE BERVAL.KONSKOD
   FIELD FORNR LIKE BERID.FORNR
   FIELD LINNR LIKE BERID.LINNR
   FIELD NATNR LIKE BERID.NATNR
   FIELD FRI1 LIKE BERID.FRI1
   FIELD FRI2 LIKE BERID.FRI2 
   FIELD XKORD LIKE BERID.XKORD
   FIELD YKORD LIKE BERID.YKORD
   FIELD ENDKOMB LIKE BERID.ENDKOMB
   FIELD FRI3 LIKE BERID.FRI3
   FIELD A LIKE BERID.A
   FIELD B LIKE BERID.B
   FIELD C LIKE BERID.C
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX FRI FRI1 FRI2 ASCENDING
   INDEX X XKORD ASCENDING.      
   
   
DEFINE TEMP-TABLE ord_temp2
   FIELD NUM LIKE BERORD.NUM
   FIELD ORD LIKE BERORD.ORD
   INDEX NUM NUM ASCENDING.
        
   
DEFINE BUFFER konbuff FOR kon_val2.   
DEFINE INPUT-OUTPUT PARAMETER kombnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valaonr99 LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade99 LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR kon_val2.
EMPTY TEMP-TABLE kon_val2  NO-ERROR. 
   /*VALDA KONSTRUKTIONER*/   
   OPEN QUERY ordq FOR EACH BERORD WHERE BERORD.AONR = valaonr99 AND
   BERORD.OMRADE = valomrade99 USE-INDEX ORD NO-LOCK.
   GET FIRST ordq NO-LOCK.
   DO WHILE AVAILABLE(BERORD):
      CREATE ord_temp2.
      ASSIGN 
      ord_temp2.NUM = BERORD.NUM
      ord_temp2.ORD = BERORD.ORD.
      GET NEXT ordq NO-LOCK.
   END.          
   CLOSE QUERY ordq.
   
   OPEN QUERY berq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr99 AND
   BERVAL.OMRADE = valomrade99 USE-INDEX OMR NO-LOCK.
   GET FIRST berq NO-LOCK.
   DO WHILE AVAILABLE(BERVAL):
      CREATE kon_val2.
      ASSIGN 
      kon_val2.BERAONR = valaonr99
      kon_val2.OMRADE = valomrade99
      kon_val2.GRUPP = BERVAL.KONSKOD 
      kon_val2.F1 = BERVAL.KTYPKOD
      kon_val2.F2 = BERVAL.F2 
      kon_val2.F3 = BERVAL.F3 
      kon_val2.F4 = BERVAL.F4 
      kon_val2.F5 = BERVAL.F5 
      kon_val2.F6 = BERVAL.F6 
      kon_val2.NUM = BERVAL.NUM
      kon_val2.ID = BERVAL.ID
      kon_val2.UPPLAG = BERVAL.UPPLAG
      kon_val2.KSKAP = BERVAL.KSKAP
      kon_val2.TYP = BERVAL.TYP
      kon_val2.SKAPNUM = BERVAL.SKAPNUM
      kon_val2.ANMARK = BERVAL.ANMARK.
      IF kon_val2.KSKAP = FALSE THEN DO:
         FIND FIRST ord_temp2 WHERE ord_temp2.NUM = kon_val2.NUM
         USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE ord_temp2 THEN DO:
            kon_val2.ORD = ord_temp2.ORD.
         END.
      END.
      GET NEXT berq NO-LOCK.
   END.          
   CLOSE QUERY berq.
   OPEN QUERY berqid FOR EACH BERID WHERE BERID.AONR = valaonr99 AND
   BERID.OMRADE = valomrade99 USE-INDEX OMR NO-LOCK.
   GET FIRST berqid NO-LOCK.
   DO WHILE AVAILABLE(BERID): 
      FIND FIRST kon_val2 WHERE kon_val2.NUM = BERID.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
      CREATE kon_id2.
      ASSIGN      
      kon_id2.NUM = BERID.NUM 
      kon_id2.GRUPP = kon_val2.GRUPP
      kon_id2.FORNR = BERID.FORNR 
      kon_id2.LINNR = BERID.LINNR 
      kon_id2.NATNR = BERID.NATNR 
      kon_id2.FRI1 = BERID.FRI1 
      kon_id2.FRI2 = BERID.FRI2
      kon_id2.XKORD = BERID.XKORD
     
      kon_id2.ENDKOMB = BERID.ENDKOMB
      kon_id2.FRI3 = BERID.FRI3
      kon_id2.A = BERID.A
      kon_id2.B = BERID.B
      kon_id2.C = BERID.C. 
      GET NEXT berqid NO-LOCK.
   END.  
   CLOSE QUERY berqid.           
   FOR EACH kon_val2 WHERE kon_val2.KSKAP = FALSE:
      IF kon_val2.ID = TRUE THEN DO:
         FIND FIRST kon_id2 WHERE kon_id2.NUM = kon_val2.NUM
         USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE kon_id2 THEN DO:
            IF kon_id2.FRI2 = ? THEN kon_val2.ID2 = kon_id2.NATNR.
            ELSE kon_val2.ID2 = STRING(kon_id2.FRI2).
            kon_val2.EXTRA1 = kon_id2.FRI3.  
            FIND FIRST konbuff WHERE konbuff.NUM = kon_val2.NUM AND
            konbuff.KSKAP = TRUE NO-LOCK NO-ERROR.
            IF AVAILABLE konbuff THEN DO:
               kon_val2.EXTRA = "+" + " " + kon_id2.FRI3.
            END.
            ELSE DO:
               kon_val2.EXTRA = "  " + kon_id2.FRI3.
            END.
         END.
      END.
      ELSE DO:      
         FIND FIRST konbuff WHERE konbuff.NUM = kon_val2.NUM AND
         konbuff.KSKAP = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE konbuff THEN kon_val2.EXTRA = "+".
      END.
   END.
   DEFINE VARIABLE xkordvar AS INTEGER NO-UNDO.
   REPEAT:
      FIND FIRST kon_id2 WHERE kon_id2.XKORD NE ? NO-LOCK NO-ERROR.
      IF AVAILABLE kon_id2 THEN DO:
         xkordvar = kon_id2.XKORD.
         kombnr = kombnr + 1.
         FOR EACH kon_id2 WHERE kon_id2.XKORD = xkordvar,
         EACH kon_val2 WHERE kon_val2.NUM  = kon_id2.NUM: 
             kon_val2.SKAPNUM = kombnr.         
             IF AVAILABLE kon_id2 THEN DELETE kon_id2.
         END.         
      END.
      ELSE LEAVE.
   END.

    /* Lena testar att lägga in samma som i inköp med val av linor och kablar per upplag*/
   /*Linor/kablar*/
   FIND FIRST BEREDNING WHERE BEREDNING.BERAONR = valaonr99 AND BEREDNING.OMRADE = valomrade99
   NO-LOCK NO-ERROR.
   OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = BEREDNING.BERAONR AND 
   BERLINKAB.OMRADE = valomrade99 AND BERLINKAB.KORTKOD = ? AND BERLINKAB.UPPLAG NE ? USE-INDEX OMR NO-LOCK.
   GET FIRST linq NO-LOCK.
   DO WHILE AVAILABLE(BERLINKAB):
      FIND FIRST BERUPP WHERE BERUPP.AONR = BEREDNING.BERAONR AND BERUPP.OMRADE = valomrade99 AND 
      BERUPP.UPPLAG = BERLINKAB.UPPLAG NO-LOCK NO-ERROR.
      IF BERUPP.ANTALRADER <= 100 THEN DO:      
         FIND FIRST kon_val2 WHERE kon_val2.BERAONR = BEREDNING.BERAONR AND kon_val2.OMRADE = valomrade99 AND
         kon_val2.GRUPP = 1000 AND kon_val2.F1 = "LIN/KAB" AND kon_val2.EXTRA1 = ("UPPLAG:" + STRING(BERLINKAB.UPPLAG))
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kon_val2 THEN DO:      
            CREATE kon_val2.
            ASSIGN 
            kon_val2.BERAONR = BEREDNING.BERAONR
            kon_val2.OMRADE = valomrade99
            kon_val2.GRUPP = 1000 
            kon_val2.F1 = "LIN/KAB"
            kon_val2.F2 = STRING(BERLINKAB.UPPLAG)
            kon_val2.EXTRA1 = "UPPLAG:" + STRING(BERLINKAB.UPPLAG)
            kon_val2.UPPLAG = BERLINKAB.UPPLAG.      
         END.             
      END.
      ELSE DO:
         FIND FIRST kon_val2 WHERE kon_val2.BERAONR = BEREDNING.BERAONR AND kon_val2.OMRADE = valomrade99 AND
         kon_val2.GRUPP = 1000 AND kon_val2.F1 = "LIN/KAB" AND kon_val2.EXTRA1 = ("UPPLAG:" + STRING(BERLINKAB.UPPLAG))
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kon_val2 THEN DO:      
            CREATE kon_val2.
            ASSIGN 
            kon_val2.BERAONR = BEREDNING.BERAONR
            kon_val2.OMRADE = valomrade99
            kon_val2.GRUPP = 1000 
            kon_val2.F1 = "LIN/KAB"
            kon_val2.F2 = STRING(BERLINKAB.UPPLAG)
            kon_val2.F6 = STRING(BERUPP.ANTALRADER)
            kon_val2.EXTRA1 = "UPPLAG:" + STRING(BERLINKAB.UPPLAG)
            kon_val2.UPPLAG = BERLINKAB.UPPLAG
            kon_val2.NUM = 10000 + BERLINKAB.UPPLAG.      
         END.             
      END.
      GET NEXT linq NO-LOCK.
   END.      
   CLOSE QUERY linq.

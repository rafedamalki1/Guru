   /*BERUPPF.p*/
DEFINE VARIABLE totmtrl LIKE FASTKALK.MATERIEL NO-UNDO.

DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.

/*BEREDNING*/
{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I} 
/*SLUT BEREDNING*/

FIND FIRST BEREDNING WHERE BEREDNING.BERNR = INTEGER(valaonr) AND 
BEREDNING.OMRADE = valomrade USE-INDEX BERNR NO-LOCK NO-ERROR.
IF BEREDNING.AONR NE ? THEN DO:   

   OPEN QUERY fastq FOR EACH KALKAONR WHERE
   KALKAONR.AONR = BEREDNING.AONR AND
   KALKAONR.DELNR = BEREDNING.DELNR NO-LOCK,
   EACH FASTSPEC WHERE FASTSPEC.KALKNR = KALKAONR.KALKNR NO-LOCK.
   GET FIRST fastq NO-LOCK.
   DO WHILE AVAILABLE(FASTSPEC):
      FIND FIRST KALKUPP WHERE KALKUPP.KALKNR = FASTSPEC.KALKNR
      USE-INDEX KALKNR NO-LOCK NO-ERROR.
      IF AVAILABLE KALKUPP THEN DO:
         IF KALKUPP.TYP = 3 THEN DO TRANSACTION:            
            FIND FIRST KALKNATT WHERE KALKNATT.DELNR = FASTSPEC.KALKNR
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE KALKNATT THEN DO:               
               RUN LISTPROG.P
               (INPUT BEREDNING.BERAONR, INPUT BEREDNING.OMRADE, OUTPUT TABLE mtrl_temp, OUTPUT TABLE lin_upp, 
               OUTPUT TABLE lin_temp).               
               
               FOR EACH mtrl_temp2:
                  DELETE mtrl_temp2.
               END.
               FOR EACH mtrl_temp BREAK BY mtrl_temp.ENR: 
                  ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR). 
                  ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR).       
                  IF LAST-OF(mtrl_temp.ENR) THEN DO:
                     CREATE mtrl_temp2.
                     ASSIGN 
                     mtrl_temp2.ENR = mtrl_temp.ENR
                     mtrl_temp2.BENAMNING = mtrl_temp.BENAMNING 
                     mtrl_temp2.ENHET = mtrl_temp.ENHET 
                     mtrl_temp2.PRIS = mtrl_temp.PRIS
                     mtrl_temp2.TOTPRIS = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.TOTPRIS)                       
                     mtrl_temp2.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).                                                        
                  END.     
               END.    
               FOR EACH lin_upp:
                  FIND FIRST mtrl_temp2 WHERE mtrl_temp2.ENR = lin_upp.ENR NO-LOCK NO-ERROR.
                  IF AVAILABLE mtrl_temp2 THEN DO:                      
                     ASSIGN
                     mtrl_temp2.ANTAL = mtrl_temp2.ANTAL + lin_upp.TOTMETER
                     mtrl_temp2.TOTPRIS = mtrl_temp2.TOTPRIS + lin_upp.TOTPRIS.
                  END.
                  ELSE DO:                    
                     CREATE mtrl_temp2.
                     ASSIGN 
                     mtrl_temp2.ENR = lin_upp.ENR
                     mtrl_temp2.BENAMNING = lin_upp.BENAMNING 
                     mtrl_temp2.ENHET = lin_upp.ENHET 
                     mtrl_temp2.PRIS = lin_upp.PRIS
                     mtrl_temp2.TOTPRIS = lin_upp.TOTPRIS                       
                     mtrl_temp2.ANTAL = lin_upp.TOTMETER.
                  END.
               END.
               FOR EACH mtrl_temp2:
                  totmtrl = totmtrl + mtrl_temp2.TOTPRIS.
               END.
               KALKNATT.MTRLKOST = totmtrl.
            END.
         END.
      END.
      GET NEXT fastq NO-LOCK.
   END.
   CLOSE QUERY fastq.
END.

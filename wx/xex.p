FOR EACH inextrakopptemp:
         DELETE inextrakopptemp.
      END.
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBAONR"                   
      inextrakopptemp.KOPPLACHAR1 = aonrvar
      inextrakopptemp.KOPPLAINT1 =  delnrvar      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN finnsextra_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT musz).              
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
         CREATE inextrakopptemp.          
         ASSIGN
         inextrakopptemp.PROGRAM = "FBAONR"                   
         inextrakopptemp.KOPPLACHAR1 = uppaonrtemp.AONR       
         inextrakopptemp.KOPPLAINT1 =  uppaonrtemp.DELNR      
         inextrakopptemp.KOPPLACHAR2 = ?            
         inextrakopptemp.KOPPLAINT2 =  ?.
         RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp).        
         FOR EACH inextrakopptemp WHERE inextrakopptemp.PROGRAM = "FBAONR":
                  DELETE inextrakopptemp.
               END.
               CREATE inextrakopptemp.          
               ASSIGN
               inextrakopptemp.PROGRAM = "FBAONR"   
               inextrakopptemp.KOPPLACHAR1 = uppaonrtemp.AONR
               inextrakopptemp.KOPPLAINT1 =  uppaonrtemp.DELNR
               inextrakopptemp.KOPPLACHAR2 = gatill.AONR
               inextrakopptemp.KOPPLAINT2 =  gatill.DELNR.
               RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).
               FOR EACH inextrakopptemp:
      DELETE inextrakopptemp.
   END.
   FIND FIRST valdaao NO-ERROR.
   IF NOT AVAILABLE valdaao THEN DO:
      MESSAGE "Inget " LC(Guru.Konstanter:gaok) " är valt!"  VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FOR EACH valdaao:
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBAONR"                   
      inextrakopptemp.KOPPLACHAR1 = aonrvarorg
      inextrakopptemp.KOPPLAINT1 =  delnrvarorg      
      inextrakopptemp.KOPPLACHAR2 = valdaao.AONR            
      inextrakopptemp.KOPPLAINT2 =  valdaao.DELNR
      inextrakopptemp.SOKINT[1] = 100.
   END.
   RUN sparaextra_UI IN fbestapph (INPUT TABLE inextrakopptemp).

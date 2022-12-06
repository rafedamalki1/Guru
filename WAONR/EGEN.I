   /*EGEN_UI*/
   /*TIMMAR*/
   IF dagtemp.BELOPP > 0 THEN DO:
      IF inder = TRUE THEN DO:
         CREATE konttemp.
         ASSIGN 
         konttemp.DOMRADE = dagtemp.GEOMRADE
         konttemp.KOMRADE = dagtemp.OMRADE      
         konttemp.BELOPP = dagtemp.BELOPP * 1.15
         konttemp.DKONT = "9891" 
         konttemp.KKONT = "9991".    
      END.  
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE 
      konttemp.BELOPP = dagtemp.BELOPP
      konttemp.DKONT = "9010" 
      konttemp.KKONT = "9950".
      IF dagtemp.AKOD = 'T' OR dagtemp.AKOD = 'T1' OR dagtemp.AKOD = 'TB' THEN DO:
         ASSIGN   
         konttemp.DKONT = "9220" 
         konttemp.KKONT = "9952".
      END.      
   END.      
   /*ÖVERTIMMAR + LÖNETILL MED RES OVE*/    
   IF dagtemp.OBELOPP > 0 THEN DO: 
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE 
      konttemp.BELOPP = dagtemp.OBELOPP
      konttemp.DKONT = "9015" 
      konttemp.KKONT = "9956".
      IF dagtemp.AKOD = 'T' OR dagtemp.AKOD = 'T1' OR dagtemp.AKOD = 'TB' THEN DO:            
         ASSIGN   
         konttemp.DKONT = "9225"  
         konttemp.KKONT = "9957".
      END.            
   END.
   /*TRAKT + LÖNETILL TRA*/      
   IF dagtemp.TBELOPP > 0 THEN DO:
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE 
      konttemp.BELOPP = dagtemp.TBELOPP
      konttemp.DKONT = "9520" 
      konttemp.KKONT = "9955".       
   END.

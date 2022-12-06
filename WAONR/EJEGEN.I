   /*EJEGEN_UI*/
   /*TIMMAR*/   
   IF dagtemp.BELOPP > 0 THEN DO:  
      IF inder = TRUE THEN DO:
         CREATE konttemp.
         ASSIGN 
         konttemp.DOMRADE = dagtemp.GEOMRADE
         konttemp.KOMRADE = dagtemp.GEOMRADE     
         konttemp.BELOPP = (dagtemp.BELOPP * 1.15) * (dagtemp.PRIS / dagtemp.PRISI)
         konttemp.DKONT = "9892" 
         konttemp.KKONT = "9992".    
      END.      
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE
      konttemp.BELOPP = dagtemp.BELOPP 
      konttemp.DKONT = "9411" 
      konttemp.KKONT = "9471".   
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE   
      konttemp.BELOPP = dagtemp.BELOPP * (dagtemp.PRIS / dagtemp.PRISI)
      konttemp.DKONT = "9010" 
      konttemp.KKONT = "9950".
      IF dagtemp.AKOD = 'T' OR dagtemp.AKOD = 'T1' OR dagtemp.AKOD = 'TB' THEN DO:
         ASSIGN   
         konttemp.DKONT = "9220" 
         konttemp.KKONT = "9952".
      END.
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE 
      konttemp.BELOPP = dagtemp.BELOPP - dagtemp.BELOPP * (dagtemp.PRIS / dagtemp.PRISI)
      konttemp.DKONT = "9401" 
      konttemp.KKONT = "9461".
   END.   
   IF dagtemp.OBELOPP > 0 THEN DO:
      /*ÖVERTIMMAR + LÖNETILL MED RES OVE*/   
      IF dagtemp.PRISTYP = "RESTID..." THEN DO:
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
         CREATE konttemp.
         ASSIGN 
         konttemp.DOMRADE = dagtemp.GEOMRADE
         konttemp.KOMRADE = dagtemp.OMRADE 
         konttemp.BELOPP = dagtemp.OBELOPP
         konttemp.DKONT = "9412" 
         konttemp.KKONT = "9472".
      END.
      ELSE DO:
         CREATE konttemp.
         ASSIGN 
         konttemp.DOMRADE = dagtemp.GEOMRADE
         konttemp.KOMRADE = dagtemp.OMRADE 
         konttemp.BELOPP = dagtemp.OBELOPP * (dagtemp.PRIS / dagtemp.PRISI)
         konttemp.DKONT = "9015" 
         konttemp.KKONT = "9956".
         IF dagtemp.AKOD = 'T' OR dagtemp.AKOD = 'T1' OR dagtemp.AKOD = 'TB' THEN DO:            
            ASSIGN   
            konttemp.DKONT = "9225" 
            konttemp.KKONT = "9957".
         END.
         CREATE konttemp.
         ASSIGN 
         konttemp.DOMRADE = dagtemp.GEOMRADE
         konttemp.KOMRADE = dagtemp.OMRADE 
         konttemp.BELOPP = dagtemp.OBELOPP
         konttemp.DKONT = "9412" 
         konttemp.KKONT = "9472".
         CREATE konttemp.
         ASSIGN 
         konttemp.DOMRADE = dagtemp.GEOMRADE
         konttemp.KOMRADE = dagtemp.OMRADE 
         konttemp.BELOPP = dagtemp.OBELOPP - dagtemp.OBELOPP * (dagtemp.PRIS / dagtemp.PRISI)
         konttemp.DKONT = "9402" 
         konttemp.KKONT = "9462".
      END.         
   END.    
   /*TRAKT + LÖNETILL MED TRA*/ 
   IF dagtemp.TBELOPP > 0 THEN DO:    
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE 
      konttemp.BELOPP = dagtemp.TBELOPP
      konttemp.DKONT = "9520" 
      konttemp.KKONT = "9955".
      CREATE konttemp.
      ASSIGN 
      konttemp.DOMRADE = dagtemp.GEOMRADE
      konttemp.KOMRADE = dagtemp.OMRADE 
      konttemp.BELOPP = dagtemp.TBELOPP
      konttemp.DKONT = "9421" 
      konttemp.KKONT = "9481".       
   END.

 filnamnuppxml = fildir + "mtrl.xml".
   RUN UTXMLDYN.P PERSISTENT SET dynxml (INPUT okand).
   EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   IF Guru.Konstanter:appcon THEN DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.
      ELSE DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar.      
      END.
      RUN uttab_UI (INPUT 3).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
      RUN xmlslut_UI IN dynxml.
       FOR EACH tempnamn:
               IF tempnamn.NODTAB = "TEMPKABEL" THEN.
               ELSE RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).  
            END.  


PROCEDURE uttab_UI :
             IF vad = 3 THEN DO:
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmtrl:HANDLE.       
      tempnamn.TORGTH = TEMP-TABLE tempmtrl:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
   END.
   
END PROCEDURE.

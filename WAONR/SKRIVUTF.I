   /*SKRIVUTF.I*/
   IF SEL_UPP = Guru.Konstanter:gaol + "-tid-?vertid-traktamenten" THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).   
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-tid-detaljerat" THEN DO:
      RUN SKRIVVAL.W (INPUT FALSE).   
   END.
   ELSE IF SEL_UPP = "Tid-?vertid-traktamenten / " + LC(Guru.Konstanter:gomrk) THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).   
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-kostnadsreg.-kalkyl" THEN DO:            
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-personal" THEN DO:
      RUN SKRIVVAL.W (INPUT FALSE).         
   END.
   ELSE IF SEL_UPP = "Kontroll mot ekonomisystem" THEN DO:            
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Arbetstidens f?rdelning" THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Debiteringsgrad-" + LC(Guru.Konstanter:gomrk) THEN DO:       
      RUN SKRIVVAL.W (INPUT FALSE). 
   END.
   ELSE IF SEL_UPP = "Personal-l?netill?gg-personal" THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gomrk + "-interna int?kter/kostnader" THEN DO:       
     RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Det.lista Tid-?vertid-trakt./ " + Guru.Konstanter:gomrk THEN DO:         
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Projekt resultat lista (14600)" THEN DO:
      RUN SKRIVVAL.W (INPUT TRUE).   
   END.
   ELSE IF SEL_UPP = "Plan/Utfall" THEN DO:
      RUN SKRIVVAL.W (INPUT TRUE).
   END.
   ELSE IF SEL_UPP = "Underlag f?r l?pande r?kning / " + Guru.Konstanter:gomrk THEN DO:      
      RUN SKRIVVAL.W (INPUT TRUE).
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-kalkyl kontof?rdeln." THEN DO:
      RUN SKRIVVAL.W (INPUT TRUE).
   END.  
   ELSE IF SEL_UPP = "Procent godk?nda tidsedlar / " + LC(Guru.Konstanter:gomrk) THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).
   END.       
   ELSE IF SEL_UPP = "F?rv?ntade int?kter" THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).
   END.       
   ELSE IF SEL_UPP = "Direkt?rsrapport" THEN DO:        
      RUN SKRIVVAL.W (INPUT TRUE).    
   END. 
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-kostnadsreg.-kalkyl flerval" THEN DO:        
      RUN SKRIVVAL.W (INPUT TRUE).    
   END.
   ELSE IF SEL_UPP = "Kalkyler per " + LC(Guru.Konstanter:gaol) THEN DO:        
      RUN SKRIVVAL.W (INPUT TRUE).    
   END.
   ELSE IF SEL_UPP = "Aktivering av investering" THEN DO:        
      RUN SKRIVVAL.W (INPUT TRUE).
   END.
   ELSE IF SEL_UPP = "Tid p? egna " + LC(Guru.Konstanter:gaol) THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP BEGINS "Projektuppf?ljningslista" THEN DO:        
      RUN SKRIVVAL.W (INPUT TRUE).
   END.   
   ELSE DO:
      RUN SKRIVVAL.W (INPUT FALSE).
   END.

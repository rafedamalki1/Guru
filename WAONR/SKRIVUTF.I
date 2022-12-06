   /*SKRIVUTF.I*/
   IF SEL_UPP = Guru.Konstanter:gaol + "-tid-övertid-traktamenten" THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).   
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-tid-detaljerat" THEN DO:
      RUN SKRIVVAL.W (INPUT FALSE).   
   END.
   ELSE IF SEL_UPP = "Tid-övertid-traktamenten / " + LC(Guru.Konstanter:gomrk) THEN DO:        
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
   ELSE IF SEL_UPP = "Arbetstidens fördelning" THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Debiteringsgrad-" + LC(Guru.Konstanter:gomrk) THEN DO:       
      RUN SKRIVVAL.W (INPUT FALSE). 
   END.
   ELSE IF SEL_UPP = "Personal-lönetillägg-personal" THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gomrk + "-interna intäkter/kostnader" THEN DO:       
     RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Det.lista Tid-övertid-trakt./ " + Guru.Konstanter:gomrk THEN DO:         
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP = "Projekt resultat lista (14600)" THEN DO:
      RUN SKRIVVAL.W (INPUT TRUE).   
   END.
   ELSE IF SEL_UPP = "Plan/Utfall" THEN DO:
      RUN SKRIVVAL.W (INPUT TRUE).
   END.
   ELSE IF SEL_UPP = "Underlag för löpande räkning / " + Guru.Konstanter:gomrk THEN DO:      
      RUN SKRIVVAL.W (INPUT TRUE).
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaol + "-kalkyl kontofördeln." THEN DO:
      RUN SKRIVVAL.W (INPUT TRUE).
   END.  
   ELSE IF SEL_UPP = "Procent godkända tidsedlar / " + LC(Guru.Konstanter:gomrk) THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).
   END.       
   ELSE IF SEL_UPP = "Förväntade intäkter" THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).
   END.       
   ELSE IF SEL_UPP = "Direktörsrapport" THEN DO:        
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
   ELSE IF SEL_UPP = "Tid på egna " + LC(Guru.Konstanter:gaol) THEN DO:        
      RUN SKRIVVAL.W (INPUT FALSE).
   END.
   ELSE IF SEL_UPP BEGINS "Projektuppföljningslista" THEN DO:        
      RUN SKRIVVAL.W (INPUT TRUE).
   END.   
   ELSE DO:
      RUN SKRIVVAL.W (INPUT FALSE).
   END.

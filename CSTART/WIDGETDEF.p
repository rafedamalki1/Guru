/*WIDGETDEF.P*/
DEFINE INPUT PARAMETER vh AS HANDLE NO-UNDO.


DEFINE VARIABLE wslh AS HANDLE NO-UNDO.

IF vh = ? THEN RETURN.
IF vh:PRIVATE-DATA NE ? THEN DO:
END.                             
 
IF vh:PRIVATE-DATA = "1" THEN RETURN.
vh:PRIVATE-DATA = "1".

/*
IF LOOKUP(vh:TYPE,"FRAME") > 0 THEN DO:
   vh:FONT =  Guru.Konstanter:varforetypval[26].
END.
*/
IF LOOKUP(vh:TYPE,"IMAGE") > 0 THEN DO:
  IF Guru.Konstanter:globnystart = TRUE THEN DO:
      IF vh:IMAGE = "BILDER\sokpa.gif" OR vh:IMAGE = "BILDER/sokpa.gif" THEN DO:
         vh:CONVERT-3D-COLORS = TRUE.
         vh:LOAD-IMAGE("BILDER\xsokpa.gif") NO-ERROR.
      END.
   END.
END.

IF LOOKUP(vh:TYPE,"FILL-IN") > 0 THEN DO:
   
   IF Guru.Konstanter:globnystart = TRUE THEN DO:             
      /*
      IF vh:READ-ONLY = FALSE THEN DO:
         IF Guru.Konstanter:varforetypval[25] NE ? THEN vh:BGCOLOR = 15.              
         vh:BGCOLOR = 15.
      END.
      */
      
   END.
   IF vh:HEIGHT-CHARS LE 0.85 THEN DO:      
      vh:HEIGHT-CHARS = 0.85.
   END.
   ELSE IF vh:HEIGHT-CHARS > 0.85 AND vh:HEIGHT-CHARS < 1.1 THEN DO:
      vh:ROW = vh:ROW + (vh:HEIGHT-CHARS - 0.85) / 2.
      vh:HEIGHT-CHARS = 0.85.
   END.    
   /*
   vh:FONT =  Guru.Konstanter:varforetypval[26].
   */
   wslh = vh:SIDE-LABEL-HANDLE. 
   IF vh:LABEL = "BENÄMNING" THEN vh:FORMAT = "X(256)" NO-ERROR.
   ELSE IF vh:LABEL = "NAMN" THEN vh:FORMAT = "X(256)" NO-ERROR.
   ELSE IF vh:LABEL MATCHES ("*ADRESS*") THEN vh:FORMAT = "X(256)" NO-ERROR.
   ELSE IF vh:LABEL = "postnr" THEN vh:FORMAT = "xxx xx" NO-ERROR.
   ELSE IF vh:LABEL = "postnummer" THEN vh:FORMAT = "xxx xx" NO-ERROR.
   
END.               
IF LOOKUP(vh:TYPE,"TEXT") > 0 THEN DO:   
   IF vh:FONT = ? THEN vh:FONT =  Guru.Konstanter:varforetypval[26].
   
   IF vh:HEIGHT-CHARS LE 0.85 THEN DO:      
      vh:HEIGHT-CHARS = 0.85.
   END.
   ELSE IF vh:HEIGHT-CHARS > 0.85 AND vh:HEIGHT-CHARS < 1.1 THEN DO:
      vh:ROW = vh:ROW + (vh:HEIGHT-CHARS - 0.85) / 2.
      vh:HEIGHT-CHARS = 0.85.
   END.           
END.               
ELSE IF LOOKUP(vh:TYPE,"BROWSE") > 0 THEN DO:         
   DEFINE VARIABLE tableh AS HANDLE NO-UNDO.
   IF Guru.Konstanter:globnystart = TRUE THEN DO: 
      IF Guru.Konstanter:varforetypval[25] NE ? THEN vh:BGCOLOR = 15.
   END.
   /*
   vh:FONT =  Guru.Konstanter:varforetypval[26].
   */
   vh:ROW-HEIGHT = 0.63.
  
END.                
ELSE IF LOOKUP(vh:TYPE,"EDITOR") > 0 THEN DO:         
   IF Guru.Konstanter:globnystart = TRUE THEN DO: 
      IF Guru.Konstanter:varforetypval[25] NE ? THEN vh:BGCOLOR = 15.
   END.
   vh:FONT = 24.
  

END.                
ELSE IF LOOKUP(vh:TYPE,"RECTANGLE") > 0 THEN DO:
   vh:EDGE-PIXELS = 2.   
   IF vh:NAME = "RECT-57" THEN vh:ROW = vh:ROW + 0.04.
END.                     
ELSE IF LOOKUP(vh:TYPE,"COMBO-BOX") > 0 THEN DO:
   IF Guru.Konstanter:globnystart = TRUE THEN DO: 
      IF Guru.Konstanter:varforetypval[25] NE ? THEN vh:BGCOLOR = 15.
   END.
   IF vh:DATA-TYPE = "CHARACTER" THEN  vh:FORMAT = "x(256)" NO-ERROR.      
   /*
   vh:FONT =  Guru.Konstanter:varforetypval[26].
     */
END.                    
ELSE IF LOOKUP(vh:TYPE,"TOGGLE-BOX") > 0 THEN DO:         
   /*
   vh:FONT =  Guru.Konstanter:varforetypval[26].
     */
END.
ELSE IF LOOKUP(vh:TYPE,"SELECTION-LIST") > 0 THEN DO:         
   IF Guru.Konstanter:globnystart = TRUE THEN DO: 
      IF Guru.Konstanter:varforetypval[25] NE ? THEN vh:BGCOLOR = 15.
   END.
   IF vh:FONT =  24 THEN vh:FONT =  Guru.Konstanter:varforetypval[26].   
   IF vh:FONT = ? THEN vh:FONT =  Guru.Konstanter:varforetypval[26].
   
END.
ELSE IF LOOKUP(vh:TYPE,"BUTTON") > 0 THEN DO: 
   
   IF vh:IMAGE NE "" THEN.
   ELSE DO:
       IF vh:NAME MATCHES("FBTN_*") THEN.
       ELSE IF vh:NAME MATCHES("MBTN_*") THEN.
       ELSE DO:
          IF vh:HEIGHT-CHARS > 1.5 THEN DO:                     
             IF (vh:ROW + (vh:HEIGHT-CHARS - 1.5) / 2) > 1 THEN 
             vh:ROW = vh:ROW + (vh:HEIGHT-CHARS - 1.5) / 2.     
             vh:HEIGHT-CHARS = 1.5.                             
          END.                                                  
       END.
       
   END.
   IF vh:IMAGE-UP = "*export-d*" THEN DO:
      vh:LOAD-IMAGE ("BILDER\xbtn_uparrow.gif") NO-ERROR.
      RETURN.
   END.
   IF vh:IMAGE-UP MATCHES("*NER-u*") THEN DO:
      RUN nerbild_UI.      
      RETURN.
   END.
   IF vh:IMAGE-UP MATCHES("*UPP-u*") THEN DO:
      vh:LOAD-IMAGE ("BILDER\xbtn_uparrow.gif") NO-ERROR.
      RETURN.
   END.
    
   IF vh:IMAGE-UP MATCHES("*next-u*") THEN DO:
      vh:LOAD-IMAGE-UP ("BILDER\xbtn_over.gif") NO-ERROR.
      vh:LOAD-IMAGE-DOWN ("BILDER\xbtn_over.gif") NO-ERROR.
      vh:LOAD-IMAGE ("BILDER\xbtn_over.gif") NO-ERROR.
      vh:FLAT-BUTTON = TRUE NO-ERROR.                       
      RETURN.
   END.                    
   ELSE IF vh:IMAGE-UP MATCHES("*forwrd-u*") THEN DO:
      vh:LOAD-IMAGE-UP ("BILDER\xbtn_allover.gif") NO-ERROR.
      vh:LOAD-IMAGE-DOWN ("BILDER\xbtn_allover.gif") NO-ERROR.
      vh:LOAD-IMAGE ("BILDER\xbtn_allover.gif") NO-ERROR.
      vh:FLAT-BUTTON = TRUE NO-ERROR.                               
      RETURN.
   END.
   ELSE IF vh:IMAGE-UP MATCHES("*prev-u*") THEN DO:
      vh:LOAD-IMAGE-UP ("BILDER\xbtn_back.gif") NO-ERROR.
      vh:LOAD-IMAGE-DOWN ("BILDER\xbtn_back.gif") NO-ERROR.
      vh:LOAD-IMAGE ("BILDER\xbtn_back.gif") NO-ERROR.
      vh:FLAT-BUTTON = TRUE NO-ERROR.                      
      RETURN.
   END.
   ELSE IF vh:IMAGE-UP MATCHES("*rewind-u*") THEN DO:
      vh:LOAD-IMAGE-UP ("BILDER\xbtn_allback.gif") NO-ERROR.
      vh:LOAD-IMAGE-DOWN ("BILDER\xbtn_allback.gif") NO-ERROR.
      vh:LOAD-IMAGE ("BILDER\xbtn_allback.gif") NO-ERROR.
      vh:FLAT-BUTTON = TRUE NO-ERROR.                       
      RETURN.
   END.
   ELSE IF vh:IMAGE-UP MATCHES("*pilupp*") THEN DO:      
      vh:LOAD-IMAGE ("BILDER\xbtn_uparrow.gif") NO-ERROR.
   END.
   ELSE IF vh:IMAGE-UP MATCHES("*pilner*") THEN DO:
      RUN nerbild_UI.            
   END.
   IF vh:LABEL = "OK" THEN DO:
      vh:LOAD-IMAGE ("BILDER\xbtn_ok.gif") NO-ERROR.
      Guru.GlobalaVariabler:BtnOkh = vh. 
   END.   
   ELSE IF vh:LABEL = "Avsluta" THEN DO:
      vh:LOAD-IMAGE ("BILDER\xbtn_ok.gif") NO-ERROR.
      Guru.GlobalaVariabler:BtnOkh = vh.
   END.       
   ELSE IF vh:LABEL = "Avslutar" THEN DO: 
      vh:LOAD-IMAGE ("BILDER\xbtn_avs.gif") NO-ERROR.
      Guru.GlobalaVariabler:BtnAvbh = vh.
   END.               
   ELSE IF vh:LABEL = "Avbryt" THEN DO:
      vh:LOAD-IMAGE ("BILDER\xbtn_avb.gif") NO-ERROR.
      Guru.GlobalaVariabler:BtnAvbh = vh.
   END. 
   ELSE IF vh:LABEL = "Ny" THEN vh:LOAD-IMAGE ("BILDER\xbtn_ny.gif") NO-ERROR.      
   ELSE IF vh:LABEL = "Ändra" THEN vh:LOAD-IMAGE ("BILDER\xbtn_and.gif") NO-ERROR.      
   ELSE IF vh:LABEL = "Ta bort" THEN vh:LOAD-IMAGE ("BILDER\xbtn_bort.gif") NO-ERROR.      
   ELSE IF vh:LABEL = "Skriv ut" THEN vh:LOAD-IMAGE ("BILDER\xbtn_print.gif") NO-ERROR.         
   ELSE IF vh:LABEL = "Visa" THEN vh:LOAD-IMAGE ("BILDER\xbtn_visa.gif") NO-ERROR.         
   ELSE IF vh:LABEL = "Kopiera" THEN vh:LOAD-IMAGE ("BILDER\xbtn_kopi.gif") NO-ERROR.                  
   ELSE IF vh:LABEL = "Kör funktion" THEN vh:LOAD-IMAGE ("BILDER\xbtn_kor.gif") NO-ERROR.         
   ELSE IF vh:LABEL = "Skapa meddelande till Guruanvändare" THEN vh:LOAD-IMAGE ("BILDER\xbtn_medd.gif") NO-ERROR.  
          
   ELSE IF vh:LABEL = "Byt användare" THEN vh:LOAD-IMAGE ("BILDER\xbtn_byt.gif") NO-ERROR.   
   /*
   ELSE IF vh:LABEL = "Byt fönsterstorlek" THEN vh:LOAD-IMAGE ("BILDER\xbtn_bytw.gif") NO-ERROR.   
   */
   ELSE IF vh:LABEL = "Byt fönsterstorlek" THEN vh:LOAD-IMAGE ("BILDER\xbtn_profil.gif") NO-ERROR.   
   ELSE IF vh:LABEL = "Uppdatera program" THEN vh:LOAD-IMAGE ("BILDER\xbtn_uppdat.gif") NO-ERROR.   
   ELSE IF vh:LABEL MATCHES("*excel*") THEN vh:LOAD-IMAGE ("BILDER\xbtn_excel.gif") NO-ERROR.   
   ELSE IF vh:LABEL = ("visa i IE") THEN vh:LOAD-IMAGE ("BILDER\xbtn_ie.gif") NO-ERROR.   
   ELSE IF vh:LABEL = ("Visa i PDF") THEN vh:LOAD-IMAGE ("BILDER\xbtn_pdf.gif") NO-ERROR.   
   ELSE IF vh:LABEL = ("Från Outlook") THEN vh:LOAD-IMAGE ("BILDER\xbtn_outlook.gif") NO-ERROR.   
   ELSE IF vh:LABEL MATCHES("*hämta och*") AND vh:NAME NE "BTN_HAMTA" THEN vh:LOAD-IMAGE ("BILDER\xbtn_hamt.gif") NO-ERROR.   
   ELSE IF vh:LABEL = ("spara favorit") THEN vh:LOAD-IMAGE ("BILDER\xbtn_sparafav.gif") NO-ERROR.   
   ELSE IF vh:LABEL = ("hämta favorit") THEN vh:LOAD-IMAGE ("BILDER\xbtn_hamtafav.gif") NO-ERROR.   
   ELSE IF vh:LABEL = ("Lägg upp favoriter") THEN vh:LOAD-IMAGE ("BILDER\xbtn_uppfav.gif") NO-ERROR.      
   ELSE IF vh:LABEL = ("Tidplan") THEN vh:LOAD-IMAGE ("BILDER\xbtn_tidplan.gif") NO-ERROR.      
   ELSE IF vh:LABEL = ("Lås upp") THEN vh:LOAD-IMAGE ("BILDER\xbtn_lasupp.gif") NO-ERROR.      
   ELSE IF vh:LABEL = ("Åter") THEN vh:LOAD-IMAGE ("BILDER\xbtn_ater.gif") NO-ERROR.  
   ELSE IF vh:LABEL = ("Snabbspara") THEN vh:LOAD-IMAGE ("BILDER\xbtn_snabbspara.gif") NO-ERROR.      
   ELSE DO:
      IF vh:IMAGE NE "" THEN RUN storlek_UI. 
      RETURN.
   END.
   RUN storlek_UI.        
  
END.

PROCEDURE nerbild_UI :
   IF vh:IMAGE-UP MATCHES("*red*") THEN DO:
      vh:LOAD-IMAGE ("BILDER\xbtn_reddownarrow.gif") NO-ERROR.       
      IF vh:TOOLTIP = "" OR vh:TOOLTIP = ? THEN vh:TOOLTIP = "Ta bort post!".                 
   END.
   ELSE vh:LOAD-IMAGE ("BILDER\xbtn_downarrow.gif") NO-ERROR.
        
END PROCEDURE.
PROCEDURE storlek_UI :   
   IF vh:HEIGHT = 1.92 AND vh:WIDTH = 7.5 THEN DO:
      RETURN.
   END.
   IF vh:HEIGHT = 1.92 AND vh:WIDTH = 7.25 THEN DO:
      RETURN.
   END.
   IF vh:HEIGHT = 2.33 AND vh:WIDTH >= 7.25 THEN DO:
      RETURN.
   END.
   vh:HEIGHT-CHARS = vh:HEIGHT-CHARS + 0.05.
   IF vh:ROW - 0.04 > 1 THEN vh:ROW = vh:ROW - 0.04.
   vh:COLUMN = vh:COLUMN - 0.08.
   vh:WIDTH-CHARS = vh:WIDTH-CHARS + 0.25.       
   
END PROCEDURE.


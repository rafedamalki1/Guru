winlg
METHOD PUBLIC VOID SparaKatalog(INPUT sender AS System.Object, INPUT args AS System.EventArgs):     
IF aktuelltabvar = "P5" THEN DO:                
   THIS-OBJECT:ControlDataDb:p5spara().             
   THIS-OBJECT:ControlDataDb:P5andresP5P1Uppdat(ebrkatvar ).   
   THIS-OBJECT:LaddaOmGriddarP5().              
END.    
IF aktuelltabvar = "P4" THEN DO:                
   THIS-OBJECT:ControlDataDb:p4spara().      
   /*känn av om det är frekvenser och/eller resurser som är uppdaterade och kör*/       
   THIS-OBJECT:ControlDataDb:P4andresP4P1Uppdat(ebrkatvar ).                        
   THIS-OBJECT:ControlDataDb:P4andfrekP4P1Uppdat(ebrkatvar ).   
   THIS-OBJECT:LaddaOmGriddarP4().                     
END.    
IF aktuelltabvar = "P3" THEN DO:                
   THIS-OBJECT:ControlDataDb:p3spara().             
   /*känn av om det är frekvenser och/eller resurser som är uppdaterade och kör*/ 
   THIS-OBJECT:ControlDataDb:P3andresP3P1Uppdat(ebrkatvar ).                        
   THIS-OBJECT:ControlDataDb:P3andfrekP3P1Uppdat(ebrkatvar ).
   THIS-OBJECT:LaddaOmGriddarP3().                        
END.    
IF aktuelltabvar = "P2" THEN DO:                
   THIS-OBJECT:ControlDataDb:p2spara().             
   /*känn av om det är frekvenser och/eller resurser OCH /eller mtrl som är uppdaterade och kör*/ 
   THIS-OBJECT:ControlDataDb:P2andresP2P1Uppdat(ebrkatvar ).                        
   THIS-OBJECT:ControlDataDb:P2andfrekP2P1Uppdat(ebrkatvar ).
   THIS-OBJECT:ControlDataDb:P2andmtrlP2P1Uppdat(ebrkatvar ).
   THIS-OBJECT:LaddaOmGriddarP2().           
                
END.    
IF aktuelltabvar = "P1" THEN DO:                
   THIS-OBJECT:ControlDataDb:p1spara().             
   THIS-OBJECT:ControlDataDb:P1andfrekP1P1Uppdat(ebrkatvar ).
   THIS-OBJECT:LaddaOmGriddarP1().                        
END.    

METHOD PUBLIC VOID P4Uppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
   IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
      THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      IF THIS-OBJECT:ControlShell:beforekolnamn = kolnamn AND THIS-OBJECT:ControlShell:beforekolvarde = kolvarde THEN RETURN.
      IF kolnamn = "FREKVENS" THEN DO:  
         IF LOGICAL(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE) = TRUE THEN DO:            
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(89),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               queryvar =  "FOR EACH " + THIS-OBJECT:ekgp4resursTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
               qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4resursTTh,queryvar).
               qH:GET-FIRST().
               DO WHILE qH:QUERY-OFF-END = FALSE:
                  THIS-OBJECT:andTTh:BUFFER-CREATE().
                  THIS-OBJECT:andTTh:BUFFER-FIELD("PNIV"):BUFFER-VALUE = "4"
                  THIS-OBJECT:andTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE
                  THIS-OBJECT:andTTh:BUFFER-FIELD("RESURSBORT"):BUFFER-VALUE = TRUE.

                  THIS-OBJECT:ekgp4resursTTh:BUFFER-DELETE().
                  qH:GET-NEXT().         
               END.                   
            END.            
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(90),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               queryvar =  "FOR EACH " + THIS-OBJECT:ekgp4frekvensTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp4frekvensTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
               qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4frekvensTTh,queryvar).
               qH:GET-FIRST().
               DO WHILE qH:QUERY-OFF-END = FALSE:
                  THIS-OBJECT:andTTh:BUFFER-CREATE().
                  THIS-OBJECT:andTTh:BUFFER-FIELD("PNIV"):BUFFER-VALUE = "4"
                  THIS-OBJECT:andTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE
                  THIS-OBJECT:andTTh:BUFFER-FIELD("FREKBORT"):BUFFER-VALUE = TRUE.
                  THIS-OBJECT:ekgp4frekvensTTh:BUFFER-DELETE().
                  qH:GET-NEXT().         
               END.
               THIS-OBJECT:ControlShell:GridResursForP4:GuruFiltrera("").                    
            END.            
         END.
      END.
   END.                
END METHOD.

METHOD PUBLIC VOID buttonOverP5Resurs()(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
   IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
      THIS-OBJECT:ekgp5resursTTh:BUFFER-CREATE().
      THIS-OBJECT:ekgp5resursTTh:BUFFER-COPY(ekgresursForTTh).
      THIS-OBJECT:andTTh:BUFFER-CREATE().
      THIS-OBJECT:andTTh:BUFFER-FIELD("PNIV"):BUFFER-VALUE = "5"
      THIS-OBJECT:andTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE
      THIS-OBJECT:andTTh:BUFFER-FIELD("NY"):BUFFER-VALUE = TRUE.

END METHOD.
     
shell
  METHOD PRIVATE VOID gridp5resurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      /*THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde ).*/      
      RETURN.
   END METHOD.

    @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP5Resurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:gridResursForP5:Posn() = -1 THEN RETURN.
      THIS-OBJECT:ControlWinLg:buttonOverP5Resurs().
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP5ResursBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:gridP5Resurs:Posn() = -1 THEN RETURN.
      THIS-OBJECT:ControlWinLg:buttonOverP5ResursBort().
      RETURN.

   END METHOD.

  METHOD PRIVATE VOID gridp4_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF frekchvar = TRUE THEN frekchvar = FALSE.
      ELSE DO:
         THIS-OBJECT:ControlWinLg:P4Uppdate(INPUT kolnamn, INPUT kolvarde).      
         THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P4").
      END.   
      RETURN.
   END METHOD.

   @VisualDesigner.
   METHOD PRIVATE VOID gridp4_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      
      RETURN.
   END METHOD.
   
   
   @VisualDesigner.
   METHOD PRIVATE VOID gridP4_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:gridP4:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      frekchvar = FALSE.
      IF kolnamn = "FREKVENS" THEN DO:
         IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE". 
         THIS-OBJECT:ControlWinLg:P4Uppdate(INPUT kolnamn, INPUT kolvarde).      
         THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P4").
         frekchvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.
   



       

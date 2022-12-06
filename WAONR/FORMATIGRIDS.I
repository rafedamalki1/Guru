
/*------------------------------------------------------------------------
    FORMATIGRIDS.I
  ----------------------------------------------------------------------*/

IF temprubadd:Typvar = "DEC" OR temprubadd:Typvar = "INT" THEN DO:
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right.
   
END.
IF temprubadd:Typvar = "DEC" THEN DO:
   IF temprubadd:Readonly = TRUE  THEN DO:
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format = "###0.00".
   END.   
END.
IF temprubadd:Typvar = "DELNR" THEN DO:
   DEFINE VARIABLE GridformatCh AS CHARACTER NO-UNDO.
   
   IF Guru.Konstanter:varforetypchar[1] = ">99" THEN GridformatCh = "00".
   ELSE GridformatCh = REPLACE(Guru.Konstanter:varforetypchar[1],"9","0"). 
   GridformatCh = REPLACE(GridformatCh,">","0"). 
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format = GridformatCh.
   /*
   IF Guru.konstanter:varforetypchar[1] = "999" THEN THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format = "000".
   ELSE THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format = "00".
   */
    
END.
/*noll decimaler */
IF temprubadd:decFormat = "INT" THEN DO:
  
   IF temprubadd:Readonly = FALSE  THEN DO:
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="0#".
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "nnnnnnnn".
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:PromptChar = " ".
   END.   
END.         

               
IF temprubadd:decFormat = "KALKYL" THEN DO: 
   IF temprubadd:Readonly = TRUE THEN DO: /*tre decimaler*/
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="###0.000".
   END.
   ELSE DO:
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="###0.###".
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "-nnnnnnnn.nnn".
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:PromptChar = " ".
   END.      
END.

IF temprubadd:Typvar = "SCHAKTP" THEN DO:
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="0".   
END.
         
IF temprubadd:decFormat = "DEC0"  THEN DO: /*noll decimaler utom när man klickar  fältet*/
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="###0".
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "nnnn.nn".
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:PromptChar = " ".
END.
         
IF temprubadd:decFormat = "EKG" THEN DO: /*två decimaler*/
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="#,##0.00".
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "nnnn.nn".
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:PromptChar = " ".
END.
         
IF temprubadd:Typvar = "ARANDELOP" THEN DO: /*två siffror*/
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="0#".
END. 
                      
IF temprubadd:Typvar = "KALKLOP" THEN DO: /*två siffror*/
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="0#".
END.    
                 
IF temprubadd:Typvar = "EKGLOP" THEN DO: /*två siffror*/
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Format ="0#".
END.
    
         
IF temprubadd:Typvar = "DATETIME" OR temprubadd:DATATYP = "DATETIME" AND temprubadd:Typvar = "" THEN DO:
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "~{LOC~}yyyy/mm/dd hh:mm:ss tt". 
END.
    
IF temprubadd:Typvar = "TIME" THEN DO:
  
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "~{LOC~}hh:mm:ss".  
END.
         
IF temprubadd:Typvar = "DATE" THEN DO:
   THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:MaskInput = "~{LOC~}yyyy/mm/dd".  
END.

 
 /*------------------------------------------------------------------------
    File        : Combo
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Thu Dec 01 14:18:56 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinGrid.UltraCombo.



CLASS Controls.Combo INHERITS UltraCombo: 
   /*------------------------------------------------------------------------------
         Purpose: En combobox som använder temp-table, databinds på samma sätt som en Controls.Grid																  
         Notes:  - För att få värde anropa this-object:Value eller I gui-programmet combonamn:Value
                 - Använd en TT med en eller två fält med de rubriker du vill ha
                 - DisplayMember visas alltid, medans valuemember innehåller det bakomliggande värdet (this-object:Value)
                 - DisplayMember/ValueMember kan vara samma fält
                 - Har stöd för flera fält
                 -                  				  
   ------------------------------------------------------------------------------*/
   DEFINE PUBLIC VARIABLE grid AS Controls.Grid NO-UNDO.
   
   DEFINE PROPERTY GuruHideValueMem AS LOGICAL NO-UNDO INITIAL TRUE
      PUBLIC GET. 
      PUBLIC SET(frag AS LOGICAL):
      
      THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:ValueMember]:Hidden = frag.
      END.
   DEFINE PROPERTY ttSortBy AS CHARACTER NO-UNDO  
      PUBLIC GET. 
      PUBLIC SET.   
   DEFINE PROPERTY ttSortQuery AS CHARACTER NO-UNDO  
      PUBLIC GET. 
      PRIVATE SET.
   DEFINE PROPERTY Guruegenskap AS Controls.GridRubrikLista NO-UNDO
      PUBLIC GET.
      PRIVATE SET.
   DEFINE PUBLIC VARIABLE tqH AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE tBS AS Progress.Data.BindingSource NO-UNDO.
	CONSTRUCTOR PUBLIC Combo (  ):
		SUPER ().
      THIS-OBJECT:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      
     /* THIS-OBJECT:tBS:PositionChanged:Subscribe(Valuechanged).*/
	END CONSTRUCTOR.
   
   METHOD PUBLIC VOID GuruInitCombo(egenskaper AS Controls.GridRubrikLista, valMem AS CHARACTER, dismem AS CHARACTER):
      DEFINE VARIABLE numcol   AS INTEGER NO-UNDO.
      DEFINE VARIABLE rubfinns AS LOGICAL NO-UNDO.
      DEFINE VARIABLE numrub AS INTEGER NO-UNDO.
/*      THIS-OBJECT:*/
      THIS-OBJECT:Guruegenskap = egenskaper.
      THIS-OBJECT:Guruegenskap:Sortera().
/*      THIS-OBJECT:ttBufferHandle = tth.*/
      THIS-OBJECT:ValueMember = valMem.
      THIS-OBJECT:DisplayMember = dismem.               
      CREATE QUERY tqH.
      tqH:SET-BUFFERS(THIS-OBJECT:Guruegenskap:ttBufferHandle).
      GuruFiltrera().
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource(tqH, "*", "").            
      THIS-OBJECT:DataSource = tBS.      
      
      THIS-OBJECT:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False. /*Rubrik i dropdown grid*/       
      /*gömmer valuemember*/
      THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:ValueMember]:Hidden = FALSE .
      THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:DisplayMember]:HEADER:Caption = dismem.
      numrub = 0.      
      DO WHILE numrub < THIS-OBJECT:Guruegenskap:countRubrik():
         DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
         temprub = THIS-OBJECT:Guruegenskap:getRubrik(numrub).         
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:HEADER:VisiblePosition = temprub:Ordning.
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:HEADER:Caption = temprub:Rubrik.
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Hidden = temprub:Gomma.                  
        
         IF temprub:Bredd NE 20 THEN THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Width = THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Width + temprub:Bredd.
         numrub = numrub + 1.
      END.
      
      /* Gömma ej definierade columner, dvs kolumner som finns i temp-table, 
      men inte finns i GridRubrikLista-objektet.*/
      numrub = 0.
     
      numcol = THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count.
      
      DO WHILE numrub < numcol:
         rubfinns = egenskaper:finnsRubrik(THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numrub]:Header:Caption).
         IF rubfinns = FALSE THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numrub]:Hidden = TRUE.
         END.
         numrub = numrub + 1.
      END.
      /* end gömma*/
/*      GuruFiltrera().*/
   END METHOD.
   /*METHOD PRIVATE VOID ValueChanged():
   
   END METHOD.*/
   METHOD PUBLIC VOID GuruFiltrera():
      IF THIS-OBJECT:Guruegenskap:ttBufferHandle NE ? THEN DO:
         ttSortQuery = "FOR EACH " + THIS-OBJECT:Guruegenskap:ttBufferHandle:TABLE + " WHERE " + Guruegenskap:filter + ttSortBy.      
         tqH:QUERY-PREPARE(ttSortQuery).
         tqH:QUERY-OPEN ().
      END.
      /*
      ELSE MESSAGE "du har inte initierat combo" VIEW-AS ALERT-BOX.
      */
   END METHOD.
   
   METHOD PUBLIC VOID GuruFirstrow():
      THIS-OBJECT:Rows[1]:Activate() NO-ERROR. /* andra raden*/            
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraComboAction:FirstRow) NO-ERROR.      
   END METHOD.
   
	DESTRUCTOR PUBLIC Combo ( ):

	END DESTRUCTOR.

END CLASS.
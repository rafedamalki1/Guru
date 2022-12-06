 
 /*------------------------------------------------------------------------
    File        : ComboSimple
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Mon Dec 19 16:02:18 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.ComboSimple INHERITS Infragistics.Win.UltraWinEditors.UltraComboEditor: 
   /*------------------------------------------------------------------------------
         Purpose: Visa en enkel combobox med displaymember samt valuemember. 																	  
         Notes:  	Exempel:
                  comboSimple1:GuruInitCombo(TEMP-TABLE ttNamn:DEFAULT-BUFFER-HANDLE, "AONR", "BENAMNING").
                  comboSimple1:VALUE = 333.			
                  
                  Första raden binder TT till combo samt sätter ValueMember och DisplayMember
                  Andra Raden repositionerar comboboxen till AONR=333 (Men visar det som står i benämning)
                  Värde hämtas/sätts genom comboSimple1:Value
                  Sätt inte comboSimple1:Value till något som inte finns i TT :)													  
   ------------------------------------------------------------------------------*/
   
   DEFINE PUBLIC PROPERTY ttBufferHandle AS HANDLE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PROPERTY ttSortQuery AS CHARACTER NO-UNDO  
      PUBLIC GET. 
      PRIVATE SET.
   DEFINE PUBLIC VARIABLE tqH AS HANDLE                      NO-UNDO.
   DEFINE PUBLIC VARIABLE tBS AS Progress.Data.BindingSource NO-UNDO.
   
	CONSTRUCTOR PUBLIC ComboSimple (  ):
		SUPER ().
      THIS-OBJECT:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
	END CONSTRUCTOR.

   METHOD PUBLIC VOID GuruInitCombo(tth AS HANDLE, valMem AS CHARACTER, disMem AS CHARACTER):
      THIS-OBJECT:ttBufferHandle = tth.
      THIS-OBJECT:ValueMember = valMem.
      THIS-OBJECT:DisplayMember = disMem.
      CREATE QUERY tqH.
      tqH:SET-BUFFERS(THIS-OBJECT:ttBufferHandle).
      GuruFiltrera().
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource(tqH, "*", "").            
      THIS-OBJECT:DataSource = tBS.      
   END.
   
   METHOD PUBLIC VOID GuruFiltrera():
      IF THIS-OBJECT:ttBufferHandle NE ? THEN 
      DO:
         ttSortQuery = "FOR EACH " + THIS-OBJECT:ttBufferHandle:TABLE.      
         tqH:QUERY-PREPARE(ttSortQuery).
         tqH:QUERY-OPEN ().
      END.
      ELSE 
         MESSAGE "du har inte initierat combo" VIEW-AS ALERT-BOX.
   END METHOD.


   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.
      
	DESTRUCTOR PUBLIC ComboSimple ( ):

	END DESTRUCTOR.

END CLASS.
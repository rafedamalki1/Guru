 
 /*------------------------------------------------------------------------
    File        : jsontest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Wed Dec 09 13:22:46 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
/*USING Progress.Windows.Form.*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS jsontest : 
/*	DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridWrong AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridRight AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE EnrtillTTh AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE EnrfranTTh AS HANDLE NO-UNDO.
   DEFINE  PUBLIC VARIABLE Enrtillbuffh AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE Enrfranbuffh AS HANDLE NO-UNDO.
		
	CONSTRUCTOR PUBLIC jsontest (  ):
		
		
      SUPER().
      InitializeComponent().
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      StartGrid().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
  
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridRight = NEW Controls.Grid().
      THIS-OBJECT:GridWrong = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      CAST(THIS-OBJECT:GridRight, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridWrong, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:GridRight).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:GridWrong).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(782, 266).
      THIS-OBJECT:splitContainer1:SplitterDistance = 379.
      THIS-OBJECT:splitContainer1:TabIndex = 0.
      /*  */
      /* GridRight */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridRight:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridRight:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridRight:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridRight:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridRight:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridRight:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridRight:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridRight:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridRight:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridRight:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridRight:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridRight:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridRight:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridRight:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridRight:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridRight:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridRight:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridRight:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridRight:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridRight:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridRight:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridRight:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridRight:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridRight:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridRight:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridRight:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridRight:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridRight:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridRight:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridRight:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridRight:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridRight:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridRight:GuruContext = ?.
      THIS-OBJECT:GridRight:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridRight:Name = "GridRight".
      THIS-OBJECT:GridRight:Size = NEW System.Drawing.Size(379, 266).
      THIS-OBJECT:GridRight:TabIndex = 16.
      THIS-OBJECT:GridRight:Text = "Från".
      /*  */
      /* GridWrong */
      /*  */
      THIS-OBJECT:GridWrong:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance13:AlphaLevel = System.Convert:ToInt16(111).
      appearance13:BackColor = System.Drawing.SystemColors:Highlight.
      appearance13:BackColorAlpha = Infragistics.Win.Alpha:UseAlphaLevel.
      THIS-OBJECT:GridWrong:DisplayLayout:Override:ActiveCellAppearance = appearance13.
      appearance14:AlphaLevel = System.Convert:ToInt16(111).
      appearance14:BackColor = System.Drawing.SystemColors:Highlight.
      appearance14:BackColorAlpha = Infragistics.Win.Alpha:UseAlphaLevel.
      THIS-OBJECT:GridWrong:DisplayLayout:Override:ActiveRowAppearance = appearance14.
      THIS-OBJECT:GridWrong:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridWrong:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridWrong:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridWrong:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridWrong:GuruContext = ?.
      THIS-OBJECT:GridWrong:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridWrong:Name = "GridWrong".
      THIS-OBJECT:GridWrong:Size = NEW System.Drawing.Size(399, 266).
      THIS-OBJECT:GridWrong:TabIndex = 1.
      /*  */
      /* Gridtest */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(782, 266).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:Name = "Gridtest".
      THIS-OBJECT:Text = "Gridtest".
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridRight, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridWrong, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.*/
	
	METHOD PUBLIC STATIC JsonObject convertDatasetToJson ( INPUT phDataset AS HANDLE ):
        DEFINE VARIABLE hBuffer         AS HANDLE      NO-UNDO.

        DEFINE VARIABLE iBuffer  AS INTEGER     NO-UNDO.
        
        DEFINE VARIABLE oDataset AS JsonObject    NO-UNDO.
        DEFINE VARIABLE oTables  AS JsonArray     NO-UNDO.
        
        IF NOT VALID-HANDLE(phDataset) THEN RETURN ?.
        
        /* From top to bottom we have:
           - A JsonObject representing the Dataset object itself
           - A JsonArray of table names that are contained in the dataset
           - A JsonArray of records in each table
           - A JsonObject for each table record
        */
        
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS:
            hBuffer = phDataset:GET-BUFFER-HANDLE(iBuffer).
            
            // Create the Tables array
            IF iBuffer EQ 1 THEN
                oTables = NEW JsonArray().
                
            // Add the table to the Tables array
            oTables:Add(jsontest:convertTableToJson(hBuffer)).
        END.
        
        // Create the DATASET object 
        oDataset = NEW JsonObject().
        
        // Add the Tables array to the DATASET.  If the dataset was defined 
        // using SERIALIZE-HIDDEN, leave its name out of the generated Json
        oDataset:Add((IF phDataset:SERIALIZE-HIDDEN THEN "" ELSE phDataset:SERIALIZE-NAME),oTables).
        
        RETURN oDataset.
        
    END METHOD.
	
	
	METHOD PUBLIC STATIC JsonObject convertTableToJson ( INPUT phTempTable AS HANDLE ):
        DEFINE VARIABLE hBuffer       AS HANDLE     NO-UNDO.
        DEFINE VARIABLE hField        AS HANDLE     NO-UNDO.
        DEFINE VARIABLE hQuery        AS HANDLE     NO-UNDO.

        DEFINE VARIABLE iField        AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iExtent       AS INTEGER    NO-UNDO.

        DEFINE VARIABLE oTable        AS JsonObject NO-UNDO.
        DEFINE VARIABLE oTableRecords AS JsonArray  NO-UNDO.
        DEFINE VARIABLE oTableArray   AS JsonArray  NO-UNDO.
        DEFINE VARIABLE oRecord       AS JsonObject NO-UNDO.
        DEFINE VARIABLE oArrayField   AS JsonArray  NO-UNDO.

        DEFINE VARIABLE lBuffer       AS LOGICAL    NO-UNDO.
        
        IF NOT VALID-HANDLE(phTempTable) THEN 
            RETURN ?.
        
        IF phTempTable:TYPE EQ "BUFFER" AND 
           phTempTable:AVAILABLE THEN
            lBuffer = TRUE.
        ELSE IF phTempTable:TYPE EQ "BUFFER" THEN 
            phTempTable = phTempTable:TABLE-HANDLE.
                     
        IF (NOT lBuffer) AND 
           (NOT phTempTable:HAS-RECORDS) THEN 
            RETURN ?.

        hBuffer = (IF lBuffer THEN phTempTable ELSE phTempTable:DEFAULT-BUFFER-HANDLE).

        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hBuffer).

        IF lBuffer THEN
            hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1", hBuffer:NAME)).
        ELSE
            hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE ROWID('&1') EQ TO-ROWID('&2')", hBuffer:NAME, STRING(hBuffer:ROWID))).

        hQuery:QUERY-OPEN().

        /* This is the TABLE object itself.  Its label will reflect the table name */
        oTable = NEW JsonObject().
        oTableRecords = NEW JsonArray().

        DO WHILE hQuery:GET-NEXT():
            // Create the record object
            oRecord = NEW JsonObject().
            
            FLD-BLK:
            DO iField = 1 TO hBuffer:NUM-FIELDS:
                hField = hBuffer:BUFFER-FIELD(iField).
                
                // If the field was defined using the SERIALIZE-HIDDEN option, leave it out of the generated Json
                IF hField:SERIALIZE-HIDDEN THEN NEXT FLD-BLK.
                
                IF hField:EXTENT GT 0 THEN DO:
                    oArrayField = NEW JsonArray().
                    DO iExtent = 1 TO hField:EXTENT:
                        // Add the values from the array elements to the record
                        oArrayField:Add(hField:BUFFER-VALUE(iExtent)).
                    END.
                    // Add the array values to the record
                    oRecord:Add(hField:SERIALIZE-NAME,oArrayField).
                END.
                // Add the field to the record
                ELSE oRecord:Add(hField:SERIALIZE-NAME,hField:BUFFER-VALUE).
            END.
            // Add the record to the records array
            oTableRecords:Add(oRecord).
        END.
        
        // Add the records array to the Table object
        oTable:Add(hBuffer:SERIALIZE-NAME,oTableRecords).

        RETURN oTable.

        FINALLY:
            DELETE OBJECT hQuery NO-ERROR.                      
        END FINALLY.
    END METHOD.
	
	
/*   METHOD PUBLIC VOID StartGrid():
      CREATE TEMP-TABLE EnrfranTTh.
      EnrfranTTh:ADD-NEW-FIELD("LEVKOD","CHARACTER").
      EnrfranTTh:ADD-NEW-FIELD("ENR","CHARACTER").
      EnrfranTTh:ADD-NEW-FIELD("BENAMNING","CHARACTER"). 
      EnrfranTTh:ADD-NEW-FIELD("TTRECID","RECID").   
      EnrfranTTh:TEMP-TABLE-PREPARE("EnrfranTT").
      Enrfranbuffh = EnrfranTTh:DEFAULT-BUFFER-HANDLE.
      
      Enrfranbuffh:BUFFER-CREATE(). 
      Enrfranbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "A" .
      Enrfranbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "1234" .
      Enrfranbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "ARTIKEL N1" .
      Enrfranbuffh:BUFFER-CREATE(). 
      Enrfranbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "A" .
      Enrfranbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "1235" .
      Enrfranbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "ARTIKEL N2" .
      Enrfranbuffh:BUFFER-CREATE(). 
      Enrfranbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "A" .
      Enrfranbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "1236" .
      Enrfranbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "ARTIKEL N3" .
      
      
      CREATE TEMP-TABLE EnrtillTTh.
      EnrtillTTh:ADD-NEW-FIELD("LEVKOD","CHARACTER").
      EnrtillTTh:ADD-NEW-FIELD("ENR","CHARACTER").
      EnrtillTTh:ADD-NEW-FIELD("BENAMNING","CHARACTER").
      EnrtillTTh:ADD-NEW-FIELD("TTRECID","RECID").   
      EnrtillTTh:TEMP-TABLE-PREPARE("EnrtillTT").
      Enrtillbuffh = EnrtillTTh:DEFAULT-BUFFER-HANDLE.
      Enrtillbuffh:BUFFER-CREATE(). 
      Enrtillbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "A" .
      Enrtillbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "1234" .
      Enrtillbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "ARTIKEL N1" .
      Enrtillbuffh:BUFFER-CREATE(). 
      Enrtillbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "A" .
      Enrtillbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "1235" .
      Enrtillbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "ARTIKEL N2" .
      Enrtillbuffh:BUFFER-CREATE(). 
      Enrtillbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "A" .
      Enrtillbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "1236" .
      Enrtillbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "ARTIKEL N3" .
      
      
      
      
      CreateRight().
      CreateGridWrong().
   END METHOD.
    METHOD PUBLIC VOID CreateRight():
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      {GridRubrikListaVarExtent.i}
     
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LEVKOD","Lev-id",TRUE,55).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      
      THIS-OBJECT:GridRight:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE, INPUT antallev, INPUT THIS-OBJECT:Enrfranbuffh)).
      
     
      THIS-OBJECT:GridRight:GuruReOpen().
     
     
      
       
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridWrong():
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      {GridRubrikListaVarExtent.i}
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LEVKOD","Lev-id",TRUE,55).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      
     
      THIS-OBJECT:GridWrong:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE, INPUT antallev, INPUT THIS-OBJECT:Enrtillbuffh)). 
     
      THIS-OBJECT:GridWrong:GuruReOpen().
   END METHOD.*/
   
   
	DESTRUCTOR PUBLIC jsontest ( ):

	END DESTRUCTOR.

END CLASS.
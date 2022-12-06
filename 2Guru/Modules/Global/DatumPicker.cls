 /*------------------------------------------------------------------------
    File        : DatumPicker
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Therese.Kruse
    Created     : Wed Apr 13 12:17:09 CEST 2022
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
  
BLOCK-LEVEL ON ERROR UNDO, THROW.
CLASS Modules.Global.DatumPicker INHERITS Form : 
   

   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonCancel AS System.Windows.Forms.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE FranDatum AS Controls.FillinDate NO-UNDO.
    
   CONSTRUCTOR PUBLIC DatumPicker( ):
   
      InitializeComponent ( ).
      THIS-OBJECT:ComponentsCollection:Add(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.      

   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC DatumPicker( ):
      
      IF VALID-OBJECT(components) THEN DO:
         CAST(components, System.IDisposable):Dispose().
      END.

   END DESTRUCTOR.   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID FranDatum_Load( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      THIS-OBJECT:FranDatum:GuruText:TEXT = STRING(Guru.GlobalaVariabler:pickdatum). 
      RETURN.

   END METHOD.     

   METHOD PRIVATE VOID okButton_Click (sender AS System.Object, e AS System.EventArgs):
       
      Guru.GlobalaVariabler:pickdatum = DATE(THIS-OBJECT:FranDatum:GuruText:TEXT).      
      THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Ok.
      THIS-OBJECT:Close ( ). 
   END METHOD.

   METHOD PRIVATE VOID cancelButton_Click (sender AS System.Object, e AS System.EventArgs):
      //Guru.GlobalaVariabler:pickdatum = ?.
      THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Cancel.
      THIS-OBJECT:Close ( ).
   END METHOD.
    
   METHOD PRIVATE VOID InitializeComponent ( ):

      /* NOTE: The following method is automatically generated.
         
         We strongly suggest that the contents of this method only be modified using the 
         Visual Designer to avoid any incompatible modifications.
         
         Modifying the contents of this method using a code editor will invalidate any support for this file. */
      
      THIS-OBJECT:buttonOk = NEW System.Windows.Forms.Button().
      THIS-OBJECT:buttonCancel = NEW System.Windows.Forms.Button().
      THIS-OBJECT:FranDatum = NEW Controls.FillinDate().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* buttonOk */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
      THIS-OBJECT:buttonOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:buttonOk:Location = NEW System.Drawing.Point(73, 173).
      THIS-OBJECT:buttonOk:Name = "buttonOk".
      THIS-OBJECT:buttonOk:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonOk:TabIndex = 0.
      THIS-OBJECT:buttonOk:Text = "OK".
      THIS-OBJECT:buttonOk:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:buttonOk:Click:Subscribe(THIS-OBJECT:okButton_Click).
      /*  */
      /* buttonCancel */
      /*  */
      DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
      THIS-OBJECT:buttonCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:buttonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
      THIS-OBJECT:buttonCancel:Location = NEW System.Drawing.Point(157, 173).
      THIS-OBJECT:buttonCancel:Name = "buttonCancel".
      THIS-OBJECT:buttonCancel:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonCancel:TabIndex = 1.
      THIS-OBJECT:buttonCancel:Text = "Avbryt".
      THIS-OBJECT:buttonCancel:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:buttonCancel:Click:Subscribe(THIS-OBJECT:cancelButton_Click).
      /*  */
      /* FranDatum */
      /*  */
      THIS-OBJECT:FranDatum:Location = NEW System.Drawing.Point(14, 4).
      THIS-OBJECT:FranDatum:Margin = NEW System.Windows.Forms.Padding(5, 5, 5, 5).
      THIS-OBJECT:FranDatum:Name = "FranDatum".
      THIS-OBJECT:FranDatum:GuruLabel:TEXT = "Ange datum:".
      THIS-OBJECT:FranDatum:Size = NEW System.Drawing.Size(191, 28).
      THIS-OBJECT:FranDatum:TabIndex = 8.
      THIS-OBJECT:FranDatum:Load:Subscribe(THIS-OBJECT:FranDatum_Load).
      /*  */
      /* DatumPicker */
      /*  */
      THIS-OBJECT:AcceptButton = THIS-OBJECT:buttonOk.
      THIS-OBJECT:CancelButton = THIS-OBJECT:buttonCancel.
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(246, 210).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:FranDatum).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonCancel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonOk).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "DatumPicker".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "DatumVäljare".
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD. 
 
   METHOD PUBLIC VOID ShowModalDialog(  ):
      
      WAIT-FOR THIS-OBJECT:ShowDialog().

   END METHOD.
END CLASS.

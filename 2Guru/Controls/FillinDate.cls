 
/*------------------------------------------------------------------------
   File        : FillinChar
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : elpkl
   Created     : Mon Dec 05 09:12:56 CET 2011
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Controls.FillinDate INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer    NO-UNDO.
   DEFINE PUBLIC  VARIABLE GuruLabel       AS Infragistics.Win.Misc.UltraLabel    NO-UNDO.
   DEFINE PUBLIC  VARIABLE GuruText        AS Controls.TextDate                NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.

   
      
   CONSTRUCTOR PUBLIC FillinDate (  ):
      
      
      SUPER().
      InitializeComponent().
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
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GuruLabel = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:GuruText = NEW Controls.TextDate().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      CAST(THIS-OBJECT:GuruText, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:BackColor = System.Drawing.Color:Transparent.
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer1:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:GuruLabel).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:GuruText).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:splitContainer1:SplitterDistance = 80.
      THIS-OBJECT:splitContainer1:TabIndex = 0.
      THIS-OBJECT:splitContainer1:TabStop = FALSE.
      /*  */
      /* GuruLabel */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:GuruLabel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      appearance1:BackColor = System.Drawing.Color:Transparent.
      THIS-OBJECT:GuruLabel:Appearance = appearance1.
      THIS-OBJECT:GuruLabel:Location = NEW System.Drawing.Point(0, 4).
      THIS-OBJECT:GuruLabel:Name = "GuruLabel".
      THIS-OBJECT:GuruLabel:Size = NEW System.Drawing.Size(80, 23).
      THIS-OBJECT:GuruLabel:TabIndex = 0.
      THIS-OBJECT:GuruLabel:Text = "GuruLabel".
      /*  */
      /* GuruText */
      /*  */
      THIS-OBJECT:GuruText:Dock = System.Windows.Forms.DockStyle:Top.
      THIS-OBJECT:GuruText:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GuruText:Name = "GuruText".
      THIS-OBJECT:GuruText:Size = NEW System.Drawing.Size(156, 21).
      THIS-OBJECT:GuruText:TabIndex = 0.
      /*  */
      /* FillinChar */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:Name = "FillinChar".
      THIS-OBJECT:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:PerformLayout().
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GuruText, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

   DESTRUCTOR PUBLIC FillinDate ( ):

      IF VALID-OBJECT(components) THEN 
      DO:
         CAST(components, System.IDisposable):Dispose().
      END.

   END DESTRUCTOR.

END CLASS.
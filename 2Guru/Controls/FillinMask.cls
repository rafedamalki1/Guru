 
 /*------------------------------------------------------------------------
    File        : FillinMask
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Mar 25 15:12:23 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Controls.FillinMask INHERITS UserControl: 

   DEFINE PUBLIC VARIABLE GuruLabel AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE GuruText AS Controls.TextMasked NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
         
   CONSTRUCTOR PUBLIC FillinMask ():
      SUPER().
      InitializeComponent().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END CONSTRUCTOR.
   
   

   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:GuruText = NEW Controls.TextMasked().
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GuruLabel = NEW Infragistics.Win.Misc.UltraLabel().
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* GuruText */
      /*  */
      THIS-OBJECT:GuruText:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GuruText:EditAs = Infragistics.Win.UltraWinMaskedEdit.EditAsType:UseSpecifiedMask.
      THIS-OBJECT:GuruText:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GuruText:Name = "GuruText".
      THIS-OBJECT:GuruText:Size = NEW System.Drawing.Size(158, 20).
      THIS-OBJECT:GuruText:TabIndex = 0.
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
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
      THIS-OBJECT:splitContainer1:SplitterDistance = 78.
      THIS-OBJECT:splitContainer1:TabIndex = 2.
      THIS-OBJECT:splitContainer1:TabStop = FALSE.
      /*  */
      /* GuruLabel */
      /*  */
      THIS-OBJECT:GuruLabel:Location = NEW System.Drawing.Point(0, 2).
      THIS-OBJECT:GuruLabel:Name = "GuruLabel".
      THIS-OBJECT:GuruLabel:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:GuruLabel:TabIndex = 1.
      THIS-OBJECT:GuruLabel:Text = "GuruLabel".
      /*  */
      /* FillinMask */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:Name = "FillinMask".
      THIS-OBJECT:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

END CLASS.
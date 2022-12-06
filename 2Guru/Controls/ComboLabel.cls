 
 /*------------------------------------------------------------------------
    File        : ComboLabel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Tue Dec 20 13:29:19 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Controls.ComboLabel INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GuruLabel AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PUBLIC VARIABLE GuruCombo AS Controls.Combo NO-UNDO.

		
	CONSTRUCTOR PUBLIC ComboLabel (  ):
		
		
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
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GuruLabel = NEW System.Windows.Forms.Label().
      THIS-OBJECT:GuruCombo = NEW Controls.Combo().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      CAST(THIS-OBJECT:GuruCombo, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      THIS-OBJECT:splitContainer1:TabStop = FALSE.
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:GuruLabel).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:GuruCombo).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:splitContainer1:SplitterDistance = 80.
      THIS-OBJECT:splitContainer1:TabIndex = 0.
      /*  */
      /* GuruLabel */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:GuruLabel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:GuruLabel:Location = NEW System.Drawing.Point(4, 4).
      THIS-OBJECT:GuruLabel:Name = "GuruLabel".
      THIS-OBJECT:GuruLabel:Size = NEW System.Drawing.Size(77, 23).
      THIS-OBJECT:GuruLabel:TabIndex = 0.
      THIS-OBJECT:GuruLabel:Text = "GuruLabel".
      THIS-OBJECT:GuruLabel:UseCompatibleTextRendering = TRUE.
      /*  */
      /* GuruCombo */
      /*  */
      DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:GuruCombo:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:GuruCombo:CheckedListSettings:CheckStateMember = "".
      THIS-OBJECT:GuruCombo:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GuruCombo:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:GuruCombo:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:GuruCombo:Location = NEW System.Drawing.Point(3, 4).
      THIS-OBJECT:GuruCombo:Name = "GuruCombo".
      THIS-OBJECT:GuruCombo:Size = NEW System.Drawing.Size(150, 22).
      THIS-OBJECT:GuruCombo:TabIndex = 0.
      /*  */
      /* ComboLabel */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:Name = "ComboLabel".
      THIS-OBJECT:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:PerformLayout().
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GuruCombo, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC ComboLabel ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
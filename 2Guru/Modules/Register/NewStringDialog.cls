 /*------------------------------------------------------------------------
    File        : NewStringDialog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Thu May 03 11:20:32 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
  

CLASS Modules.Register.NewStringDialog INHERITS Form   : 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonCancel AS System.Windows.Forms.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE label2 AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PRIVATE VARIABLE InputSokChar AS System.Windows.Forms.TextBox NO-UNDO.
   DEFINE PRIVATE VARIABLE InputBen AS System.Windows.Forms.TextBox NO-UNDO.
    
   DEFINE PUBLIC VARIABLE Benamning AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE SokChar AS CHARACTER NO-UNDO.
    
	CONSTRUCTOR PUBLIC NewStringDialog( ):
	
		SUPER().
		InitializeComponent ( ).
		CATCH e AS Progress.Lang.Error:
			UNDO, THROW e.
		END CATCH.		

	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC NewStringDialog( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.	

    METHOD PRIVATE VOID okButton_Click (sender AS System.Object, e AS System.EventArgs):
      IF trim(THIS-OBJECT:InputBen:Text) NE "" THEN DO:
         THIS-OBJECT:Benamning = THIS-OBJECT:InputBen:Text.
         THIS-OBJECT:SokChar = THIS-OBJECT:InputSokChar:Text.
   		THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Ok.
   		THIS-OBJECT:Close ( ).
		END.
		ELSE DO:
		   System.Windows.Forms.MessageBox:Show("Benämning är obligatorisk!").
	   END. 
	END METHOD.

	METHOD PRIVATE VOID cancelButton_Click (sender AS System.Object, e AS System.EventArgs):
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
      THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
      THIS-OBJECT:label2 = NEW System.Windows.Forms.Label().
      THIS-OBJECT:InputBen = NEW System.Windows.Forms.TextBox().
      THIS-OBJECT:InputSokChar = NEW System.Windows.Forms.TextBox().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* buttonOk */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
      THIS-OBJECT:buttonOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:buttonOk:Location = NEW System.Drawing.Point(188, 69).
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
      THIS-OBJECT:buttonCancel:Location = NEW System.Drawing.Point(269, 69).
      THIS-OBJECT:buttonCancel:Name = "buttonCancel".
      THIS-OBJECT:buttonCancel:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonCancel:TabIndex = 1.
      THIS-OBJECT:buttonCancel:Text = "Cancel".
      THIS-OBJECT:buttonCancel:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:buttonCancel:Click:Subscribe(THIS-OBJECT:cancelButton_Click).
      /*  */
      /* label1 */
      /*  */
      THIS-OBJECT:label1:Location = NEW System.Drawing.Point(12, 9).
      THIS-OBJECT:label1:Name = "label1".
      THIS-OBJECT:label1:Size = NEW System.Drawing.Size(100, 21).
      THIS-OBJECT:label1:TabIndex = 2.
      THIS-OBJECT:label1:Text = "Benämning".
      THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
      /*  */
      /* label2 */
      /*  */
      THIS-OBJECT:label2:Location = NEW System.Drawing.Point(12, 40).
      THIS-OBJECT:label2:Name = "label2".
      THIS-OBJECT:label2:Size = NEW System.Drawing.Size(100, 21).
      THIS-OBJECT:label2:TabIndex = 3.
      THIS-OBJECT:label2:Text = "Sök-Char".
      THIS-OBJECT:label2:UseCompatibleTextRendering = TRUE.
      /*  */
      /* InputBen */
      /*  */
      THIS-OBJECT:InputBen:Location = NEW System.Drawing.Point(118, 6).
      THIS-OBJECT:InputBen:Name = "InputBen".
      THIS-OBJECT:InputBen:Size = NEW System.Drawing.Size(226, 20).
      THIS-OBJECT:InputBen:TabIndex = 4.
      /*  */
      /* InputSokChar */
      /*  */
      THIS-OBJECT:InputSokChar:Location = NEW System.Drawing.Point(118, 37).
      THIS-OBJECT:InputSokChar:Name = "InputSokChar".
      THIS-OBJECT:InputSokChar:Size = NEW System.Drawing.Size(226, 20).
      THIS-OBJECT:InputSokChar:TabIndex = 5.
      /*  */
      /* NewStringDialog */
      /*  */
      THIS-OBJECT:AcceptButton = THIS-OBJECT:buttonOk.
      THIS-OBJECT:CancelButton = THIS-OBJECT:buttonCancel.
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(356, 104).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:InputSokChar).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:InputBen).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:label2).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:label1).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonCancel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonOk).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "NewStringDialog".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "Ny sträng".
      THIS-OBJECT:ResumeLayout(FALSE).
      THIS-OBJECT:PerformLayout().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD. 
 
	METHOD PUBLIC VOID ShowModalDialog(  ):
		
		WAIT-FOR THIS-OBJECT:ShowDialog().

	END METHOD.


 
END CLASS.

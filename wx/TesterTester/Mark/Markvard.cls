 
 /*------------------------------------------------------------------------
    File        : Markvard
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Feb 14 09:42:59 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Mark.Markvard INHERITS UserControl: 
	
   DEFINE PRIVATE VARIABLE comboBox1 AS System.Windows.Forms.ComboBox NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE groupBox1 AS System.Windows.Forms.GroupBox NO-UNDO.
   DEFINE PUBLIC VARIABLE Root AS Guru.Root.
		
	CONSTRUCTOR PUBLIC Markvard ( INPUT r AS Guru.Root ):
      		
		
      SUPER().
      InitializeComponent().
      
      THIS-OBJECT:Root = r.
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:comboBox1 = NEW System.Windows.Forms.ComboBox().
      THIS-OBJECT:groupBox1 = NEW System.Windows.Forms.GroupBox().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* comboBox1 */
      /*  */
      THIS-OBJECT:comboBox1:FormattingEnabled = TRUE.
      THIS-OBJECT:comboBox1:Location = NEW System.Drawing.Point(59, 48).
      THIS-OBJECT:comboBox1:Name = "comboBox1".
      THIS-OBJECT:comboBox1:Size = NEW System.Drawing.Size(121, 21).
      THIS-OBJECT:comboBox1:TabIndex = 0.
      /*  */
      /* groupBox1 */
      /*  */
      THIS-OBJECT:groupBox1:Location = NEW System.Drawing.Point(59, 129).
      THIS-OBJECT:groupBox1:Name = "groupBox1".
      THIS-OBJECT:groupBox1:Size = NEW System.Drawing.Size(200, 100).
      THIS-OBJECT:groupBox1:TabIndex = 1.
      THIS-OBJECT:groupBox1:TabStop = FALSE.
      THIS-OBJECT:groupBox1:Text = "groupBox1".
      THIS-OBJECT:groupBox1:UseCompatibleTextRendering = TRUE.
      /*  */
      /* Markvard */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:groupBox1).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:comboBox1).
      THIS-OBJECT:Name = "Markvard".
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC Markvard ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
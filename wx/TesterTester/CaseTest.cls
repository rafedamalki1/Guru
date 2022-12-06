 
 /*------------------------------------------------------------------------
    File        : CaseTest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Thu Nov 13 08:28:09 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.
Using Progress.Data.BindingSource.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS TesterTester.CaseTest INHERITS Form: 

    DEFINE TEMP-TABLE MyTableTT
        FIELD ID AS INTEGER
        FIELD USERNAME AS CHARACTER
        FIELD PASSWORD AS CHARACTER.

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE MyGrid AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE MySource AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE MyQuery AS HANDLE NO-UNDO.
    DEFINE PRIVATE VARIABLE MyTable AS HANDLE NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

    CONSTRUCTOR PUBLIC CaseTest():
        SUPER().
        THIS-OBJECT:InitializeComponent().
        
        THIS-OBJECT:MyTable = TEMP-TABLE MyTableTT:DEFAULT-BUFFER-HANDLE.
        
        CREATE MyTableTT.
        
        CREATE QUERY THIS-OBJECT:MyQuery.
        THIS-OBJECT:MyQuery:SET-BUFFERS(MyTable).
        THIS-OBJECT:MyQuery:QUERY-PREPARE("FOR EACH MyTableTT").
        THIS-OBJECT:MyQuery:QUERY-OPEN().
        
        
        THIS-OBJECT:MySource = NEW Progress.Data.BindingSource(THIS-OBJECT:MyQuery, "*", "").
        THIS-OBJECT:MyGrid:DataSource = THIS-OBJECT:MySource.
        
    END CONSTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent ( ):
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:MyGrid = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        CAST(THIS-OBJECT:MyGrid, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* MyGrid */
        /*  */
        THIS-OBJECT:MyGrid:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:MyGrid:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:MyGrid:Name = "MyGrid".
        THIS-OBJECT:MyGrid:TabIndex = 0.
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlDark.
        THIS-OBJECT:panel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1".
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(410, 362).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* CaseTest */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(410, 362).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Name = "CaseTest".
        THIS-OBJECT:Text = "CaseTest".
        CAST(THIS-OBJECT:MyGrid, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.
END CLASS.
 
 /*------------------------------------------------------------------------
    File        : TextMasked
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Mar 25 15:01:22 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinMaskedEdit.UltraMaskedEdit.
USING System.Windows.Forms.*.

CLASS Controls.TextMasked INHERITS UltraMaskedEdit: 
      
  
   CONSTRUCTOR PUBLIC TextMasked():
      SUPER().
      
      THIS-OBJECT:ValueChanged:Subscribe(THIS-OBJECT:KollaLangd).
   END CONSTRUCTOR.
   
   
   /*Robin Sjöberg Elpool i Umeå AB  2 apr 2014 11:08:31 
   Subscriba till detta event för beteende vid fylld input, styrs av hur man skrivit inputmasken. Krävs att input är REQ dvs "A" och inte "a" osv.
   */
   DEFINE PUBLIC EVENT Fylld SIGNATURE VOID ().
   
   METHOD PUBLIC VOID ValjMask(mask AS CHARACTER):
      THIS-OBJECT:InputMask = mask.
   END METHOD. 

   METHOD PUBLIC VOID Fokus(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
     THIS-OBJECT:SelectAll().
   END METHOD. 
   
   METHOD PUBLIC VOID KollaLangd(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      IF THIS-OBJECT:IsFullyInput THEN DO:
         Fylld:Publish().     
      END.  
      /*  
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(200),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
      END.
      */   
   END METHOD.
   
   
   
   DESTRUCTOR PUBLIC TextMasked():
   END DESTRUCTOR.
   
END CLASS.
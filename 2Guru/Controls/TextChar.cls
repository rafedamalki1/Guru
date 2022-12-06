 
 /*------------------------------------------------------------------------
    File        : TextChar
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Wed Nov 30 11:02:16 CET 2011
    Notes       : 
                  
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraTextEditor.
/*USING System.Windows.Forms.KeyEventArgs.*/
USING System.Windows.Forms.*.
USING System.Text.*.
USING System.Runtime.InteropServices.ComTypes.*.


CLASS Controls.TextChar INHERITS UltraTextEditor: 
   /*------------------------------------------------------------------------------
         Purpose: Blocka ogiltiga tecken från en char text-box. 																	  
         Notes:  	Svår att felsöka  pga att input förstörs av debugging och messages
                  Om du ska lägga ut messages gör det på rader EFTER e:Handled = // blockedKey =																  
   ------------------------------------------------------------------------------*/     
   DEFINE PRIVATE VARIABLE const AS Controls.contextDummy NO-UNDO.
   DEFINE PROPERTY blockedKey AS LOGICAL NO-UNDO INITIAL FALSE
      GET.      SET.
	DEFINE PUBLIC PROPERTY GuruKontrollText AS LOGICAL NO-UNDO INITIAL FALSE
	GET.	SET.
   DEFINE PUBLIC PROPERTY GuruKontrollStart AS LOGICAL NO-UNDO INITIAL FALSE
   GET.   SET.
	DEFINE PRIVATE VARIABLE feltxtDefault AS CHARACTER NO-UNDO.
	DEFINE PRIVATE VARIABLE feltxt AS CHARACTER NO-UNDO.
	DEFINE PRIVATE VARIABLE feltxtStart AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE feltxtStartDefault AS CHARACTER NO-UNDO.
   
	CONSTRUCTOR PUBLIC TextChar (  ):
		SUPER ().
      ASSIGN
      const = NEW Controls.contextDummy()
      THIS-OBJECT:ContextMenu = const
      feltxt = '\/:*<>|"´' + "'" 
      feltxtStart = "-#" /* ej "-" vid E-nr*/
      feltxtStart = feltxtStart + feltxt
      feltxtStartDefault = feltxtStart
      feltxtdefault = feltxt.      
	END CONSTRUCTOR.
	/*
	CONSTRUCTOR PUBLIC TextChar (InHuvudClsh AS Modules.Beredning.FillEnter) :
      SUPER ().
      HuvudClsh = InHuvudClsh.
      ASSIGN
      const = NEW Controls.contextDummy()
      THIS-OBJECT:ContextMenu = const
      feltxt = '\/:*<>|"´' + "'" 
      feltxtStart = "-#" /* ej "-" vid E-nr*/
      feltxtStart = feltxtStart + feltxt
      feltxtStartDefault = feltxtStart
      feltxtdefault = feltxt.      
   END CONSTRUCTOR.
    */  
   METHOD PUBLIC VOID GuruTextRensa():
      feltxt = "".
   END METHOD.
   
   METHOD PUBLIC VOID GuruTextAdd(str AS CHARACTER):
      feltxt = feltxt + str. 
   END METHOD.
   
   METHOD PUBLIC VOID GuruTextDefault():
      feltxt = feltxtdefault.
   END METHOD.
   
   METHOD PUBLIC VOID GuruStartRensa():
      feltxtStart = "".
   END METHOD.
   
   METHOD PUBLIC VOID GuruStartAdd(str AS CHARACTER):
      feltxtStart = feltxtStart + str.       
   END METHOD.
   
   METHOD PUBLIC VOID GuruStartDefault():
      feltxtStart = feltxtStartDefault.
   END METHOD.
/*The keydown event is fired when a key is pressed. 
Unlike the keypress event, the keydown event is fired for all keys, 
regardless of whether they produce a character value. 
The keydown and keyup events provide a code indicating which key is pressed, 
while keypress indicates which character was entered.
*/
   METHOD PROTECTED OVERRIDE VOID OnKeyDown(e AS KeyEventArgs):
      DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE temptext AS CHARACTER NO-UNDO.
      blockedKey = FALSE.
      keycodestr = e:KEYCODE:ToString().
     
      IF keycodestr = "Delete" THEN DO:
         IF THIS-OBJECT:SelectionStart < 1 THEN DO:
            IF THIS-OBJECT:SelectionLength = 0 THEN
               temptext = SUBSTRING(THIS-OBJECT:Text, THIS-OBJECT:SelectionLength + 2,1).
            ELSE 
               temptext = SUBSTRING(THIS-OBJECT:Text, THIS-OBJECT:SelectionLength + 1,1).               
            
            IF GuruKontrollStart THEN
               e:Handled = startKoll(temptext).
         END.                
      END.
      
      IF keycodestr = "Back" THEN DO:            
         IF THIS-OBJECT:SelectionStart < 2 THEN DO:
            IF THIS-OBJECT:SelectionLength = 0 THEN
               temptext = SUBSTRING(THIS-OBJECT:Text, THIS-OBJECT:SelectionLength + 2,1).
            ELSE 
               temptext = SUBSTRING(THIS-OBJECT:Text, THIS-OBJECT:SelectionLength + 1,1).
            
            IF GuruKontrollStart THEN
               blockedKey = startKoll(temptext).
         END.            
      END.
      
      IF keycodestr = "v" AND e:Modifiers:ToString() = "Control" THEN DO:                        
         temptext = System.Windows.Forms.Clipboard:GetText().
         
         IF temptext NE "" THEN DO:
            IF THIS-OBJECT:SelectionStart < 1 THEN DO:
               temptext = SUBSTRING(temptext,1,1).                  
               /*kontrollera första tecknet av paste */
               IF GuruKontrollStart THEN DO:
                  blockedKey = startKoll(temptext).
               END.                  
            END.
            
            /*kontrollera övriga tecken */
            IF GuruKontrollText AND blockedKey = FALSE THEN DO:
               temptext = System.Windows.Forms.Clipboard:GetText().               
/*               blockedKey = textKoll(temptext).*/
               blockedKey = textKoll(temptext).
            END.      
         END .            
      END.
   END METHOD.
   
   METHOD PROTECTED OVERRIDE VOID OnKeyPress(e AS KeyPressEventArgs):  
      DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
      IF blockedKey OR e:Handled = TRUE THEN DO:
         e:Handled = TRUE.
         RETURN.
      END.
      
      keycodestr = e:KeyChar.     
      IF GuruKontrollStart THEN DO:
         IF THIS-OBJECT:SelectionStart < 1 THEN DO:        
            e:Handled = startKoll(keycodestr).
         END.   
      END.
      
      IF GuruKontrollText AND e:Handled = FALSE THEN DO:           
         e:Handled = textKoll(keycodestr).
      END.
   END METHOD.
  
   METHOD PRIVATE LOGICAL textKoll(str AS CHARACTER):
      DEFINE VARIABLE slutvar  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE rkollvar AS INTEGER   NO-UNDO.  
      rkollvar = 1.
      slutvar = LENGTH(feltxt).      
      REPEAT: 
         IF INDEX(str,SUBSTRING(feltxt,rkollvar,1)) NE 0 THEN 
         DO:            
            RETURN TRUE. 
         END.
         rkollvar = rkollvar + 1.
         IF rkollvar > slutvar THEN LEAVE.
      END.
      RETURN FALSE.  
   END METHOD.
   
  
   
   /* Kollar Start */
   METHOD PRIVATE LOGICAL Startkoll(str AS CHARACTER) :              
      DEFINE VARIABLE slutvar  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE rkollvar AS INTEGER   NO-UNDO.     
      rkollvar = 1. 
      slutvar = LENGTH(feltxtStart).
      REPEAT:     
         IF INDEX(str,SUBSTRING(feltxtStart,rkollvar,1)) NE 0 THEN DO:
            RETURN TRUE.
         END.
         rkollvar = rkollvar + 1.
         IF rkollvar > slutvar THEN LEAVE.
      END.
      RETURN FALSE.
   END METHOD.   
   
    /* Kollar Start */
  /* METHOD PRIVATE LOGICAL startKollRec(str AS CHARACTER, ii AS INTEGER) :
                                                      
      IF INDEX(str,SUBSTRING(feltxtStart,ii,1)) NE 0 THEN 
         RETURN TRUE.
      ELSE DO:
         ii = ii + 1.                  
         IF ii > LENGTH (feltxtStart) THEN 
            LEAVE.
         ELSE 
            startKollRec(str, ii).
      END.      
      RETURN FALSE.
   END METHOD.
   */
   
   /*METHOD PRIVATE LOGICAL textKollRec(str AS CHARACTER, ii AS INTEGER):
      IF INDEX(str,SUBSTRING(feltxt,ii,1)) NE 0 THEN                  
         RETURN TRUE.
      ELSE DO:
         ii = ii + 1.
         IF ii > LENGTH(feltxt) THEN
            LEAVE.
         ELSE
            textKollRec(str, ii).         
      END.
      RETURN FALSE.
   END METHOD.*/
	DESTRUCTOR PUBLIC TextChar ( ):

	END DESTRUCTOR.

END CLASS.
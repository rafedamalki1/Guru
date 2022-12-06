 
 /*------------------------------------------------------------------------
    File        : Button
    Purpose     : Sätta rätt bild beroende på knapp-text
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Thu Feb 10 13:47:12 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Windows.Forms.Button.



CLASS Controls.Button INHERITS Infragistics.Win.Misc.UltraButton:
    
    DEFINE VARIABLE ToolTipper AS System.Windows.Forms.ToolTip NO-UNDO.
    
    DEFINE PRIVATE VARIABLE InternalTooltip AS CHARACTER NO-UNDO.
    
    DEFINE PUBLIC PROPERTY GuruTooltip AS CHARACTER INITIAL "" NO-UNDO
    PUBLIC GET():
       RETURN THIS-OBJECT:InternalTooltip.
       END GET.
    PUBLIC SET(a AS CHARACTER):
       THIS-OBJECT:SetTooltip(a).
       THIS-OBJECT:InternalTooltip = a.
       END SET.
    
   CONSTRUCTOR PUBLIC Button (  ):
      SUPER().       
      BtnImage(). 
      InitializeTooltip().

   END CONSTRUCTOR.
   
   METHOD PRIVATE VOID InitializeTooltip():
      THIS-OBJECT:ToolTipper = NEW System.Windows.Forms.ToolTip().
      THIS-OBJECT:ToolTipper:AutoPopDelay = 0.
      THIS-OBJECT:ToolTipper:InitialDelay = 1.
      THIS-OBJECT:ToolTipper:ReshowDelay = 1.
      THIS-OBJECT:ToolTipper:ShowAlways = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID SetTooltip(t AS CHARACTER):
      THIS-OBJECT:ToolTipper:SetToolTip(THIS-OBJECT, t). 
   END METHOD.
   
   
/*   Denna metod bestämmer om en knapp ska visas, skicka med lämplig variabel från Start.Styrsek*/
   METHOD PUBLIC VOID guruSekUser(sekv AS LOGICAL):      
         THIS-OBJECT:VISIBLE = sekv.
         THIS-OBJECT:ENABLED = sekv.               
   END METHOD.
/*   Bestämmer text, skicka med lämplig variabel från Start.Konstanter*/   
   /*METHOD PUBLIC VOID guruSetText(txt AS CHARACTER):
      THIS-OBJECT:TEXT = txt.
   END METHOD.*/
   
   METHOD PUBLIC VOID guruSekBolag(sekv AS INTEGER):
   
   END METHOD.
   
   /* Sätter knapptexten */
   METHOD PRIVATE VOID BtnImage ():
      DEFINE VARIABLE img AS System.Drawing.Bitmap NO-UNDO.
      DEFINE VARIABLE pathx AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bildpath AS CHARACTER NO-UNDO.      
      bildpath = BtnNamn(THIS-OBJECT:TEXT).
      IF bildpath = "" THEN RETURN.
      img = NEW System.Drawing.Bitmap(Guru.Konstanter:gurubilder + bildpath) no-error.      
      THIS-OBJECT:Appearance:Image = img.     
       /*THIS-OBJECT:TextImageRelation = System.Windows.Forms.TextImageRelation:*/
      /*THIS-OBJECT:TextImageRelation = System.Windows.Forms.TextImageRelation:ImageAboveText.*/
   END METHOD.
   
   /* Returnerar namn beroende på text */
   METHOD PRIVATE CHARACTER BtnNamn(INPUT txt AS CHARACTER):
      IF txt = "Ok" THEN RETURN "/xbtn_ok.gif". 
      ELSE IF txt = "Avsluta" THEN RETURN "\xbtn_ok.gif".
      ELSE IF txt = "Avslutar" THEN RETURN "\xbtn_avs.gif". 
      ELSE IF txt = "Avbryt" THEN RETURN "\xbtn_avb.gif".
      ELSE IF txt = "Ny" THEN RETURN "\xbtn_ny.gif".
      ELSE IF txt = "Ändra" THEN RETURN "\xbtn_and.gif".
      ELSE IF txt = "Ta bort" THEN RETURN "\xbtn_bort.gif".
      ELSE IF txt = "Skriv ut" THEN RETURN "\xbtn_print.gif".
      ELSE IF txt = "Visa" THEN RETURN "\xbtn_visa.gif".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      ELSE IF txt = "" THEN RETURN "".
      
      ELSE RETURN "".
      
      RETURN "".
   END METHOD.
   
   /*
   METHOD PRIVATE VOID BtnSize ():
      THIS-OBJECT:SIZE = 
   
   END METHOD.*/
   
   /* Körs när button:TEXT sätts, t.ex. i InitializeComponent() */
   METHOD PROTECTED OVERRIDE VOID OnTextChanged(INPUT e AS System.EventArgs):      
      BtnImage().        
   END METHOD.

END CLASS.
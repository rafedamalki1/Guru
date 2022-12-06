 
 /*------------------------------------------------------------------------
    File        : EkgWinLg
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jul 01 12:05:11 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.EkgData.EkgWinLg: 
   
   /*start*/
   DEFINE PUBLIC VARIABLE Root                          AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ModuleHandle                  AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE P1Control                     AS Modules.EkgData.EkgP1Control                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE P2Control                     AS Modules.EkgData.EkgP2Control                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE P3Control                     AS Modules.EkgData.EkgP3Control                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE P4Control                     AS Modules.EkgData.EkgP4Control                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE P5Control                     AS Modules.EkgData.EkgP5Control                                   NO-UNDO.
   
   DEFINE VARIABLE dontrepo AS LOGICAL NO-UNDO. 
   DEFINE PRIVATE VARIABLE VisaExcel                    AS Controls.GuruExcelEmbed                                        NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
      /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   /*Drag Dropp*/
   DEFINE         VARIABLE DGP1F                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP2M                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP2F                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP2R                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP3F                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP3R                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP4F                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP4R                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGP5R                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGPS                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   
   /* Ribbongrupper */ 
   DEFINE PUBLIC  VARIABLE RibbonGroupSparaKatalog                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupKatInfo                  AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupTabInfo                  AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupVisa                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupFonster                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE         VARIABLE RibbonGroupSpara                AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
  
  /* Ribbongrupp SparaKatalog */
   DEFINE VARIABLE RibbonToolSparaKatalog          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   /*DEFINE VARIABLE RibbonToolBeraknaKatalog          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.*/
   
   /* Ribbongrupp KatInfo */
   DEFINE VARIABLE RibbonToolBenamning           AS Infragistics.Win.UltraWinToolbars.TextBoxTool                        NO-UNDO.
   DEFINE VARIABLE RibbonToolEbrKat             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                              NO-UNDO.
   DEFINE VARIABLE RibbonToolSubKatID            AS Infragistics.Win.UltraWinToolbars.TextBoxTool                               NO-UNDO.
   DEFINE VARIABLE RibbonToolArtal           AS Infragistics.Win.UltraWinToolbars.TextBoxTool                        NO-UNDO.

   
   /* Ribbongrupp TabInfo */
   DEFINE VARIABLE RibbonToolAktuellTab           AS Infragistics.Win.UltraWinToolbars.TextBoxTool                        NO-UNDO.
   DEFINE VARIABLE RibbonToolAktuellKod            AS Infragistics.Win.UltraWinToolbars.TextBoxTool                              NO-UNDO.
  
   /* Ribbongrupp Visa */
   DEFINE PUBLIC  VARIABLE RibbonToolVisaAllaKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaKostnader            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaEaMangder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   /* Ribbongrupp Fonster */
   DEFINE PUBLIC  VARIABLE RibbonToolFonsterKoder           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFonsterUnderkoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFonsterArbKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
      
   DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   
   /*Värden för aktuell katalog*/
   DEFINE PUBLIC PROPERTY ekgsubidvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY ebrkatvar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      
   DEFINE PUBLIC PROPERTY aktuelltabvar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY nivandvar AS CHARACTER INITIAL ""  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY raknavar AS CHARACTER INITIAL ""  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.    
   DEFINE PUBLIC PROPERTY lastkatvar AS LOGICAL  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.  
      
      
      
   CONSTRUCTOR PUBLIC EkgWinLg(INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlDataDb:ControlWinLg = THIS-OBJECT.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   
   METHOD PUBLIC LOGICAL Initialize(): 
      /*ända senare*/
      THIS-OBJECT:CreateControls().
      THIS-OBJECT:Root:WindowManager:Wnd:FormClosing:Subscribe(THIS-OBJECT:Avsluta).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:LaddaLabels().
      nivandvar = "".
      raknavar = "".
      THIS-OBJECT:ControlDataDb:LaddaKataloger().
      THIS-OBJECT:ControlDataDb:LaddaResurser().
      THIS-OBJECT:GomTabbar(). 
      THIS-OBJECT:CreateForm().
      THIS-OBJECT:DoljRibbonGrupper(FALSE).
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Kataloger"]:Selected = TRUE. 
      RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:ControlDataDb:EkgDef().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateFunctions().
      THIS-OBJECT:CreateRibbon().
      THIS-OBJECT:InitValuCH().
      THIS-OBJECT:ForetagAnpass().           
   END METHOD.
   
  METHOD PUBLIC VOID CreateControls():
      /*Skapar kontrollerna i Shell*/
      THIS-OBJECT:ControlShell:StartaPControl().
      /*Skapar sökvägar*/
      THIS-OBJECT:P1Control = THIS-OBJECT:ControlShell:P1Control.  
      THIS-OBJECT:P2Control = THIS-OBJECT:ControlShell:P2Control.
      THIS-OBJECT:P3Control = THIS-OBJECT:ControlShell:P3Control.
      THIS-OBJECT:P4Control = THIS-OBJECT:ControlShell:P4Control.
      THIS-OBJECT:P5Control = THIS-OBJECT:ControlShell:P5Control.        
   END METHOD.
   
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "Ekgshell",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT 0,INPUT "") .
   END METHOD.
      
   METHOD PUBLIC VOID Sparatab(INPUT valdtab AS CHARACTER):       
      IF valdtab = "Resurser" THEN DO:
         THIS-OBJECT:ControlDataDb:Resursspara().                   
      END.
      IF valdtab = "Resurspris" THEN DO:
         THIS-OBJECT:ControlDataDb:Katalogresursspara().               
         THIS-OBJECT:ControlDataDb:Subkatspara().
         IF raknavar NE "" THEN DO:
            raknavar = "".
            THIS-OBJECT:ControlDataDb:BeraknaP5alla().
            THIS-OBJECT:ControlDataDb:p5spara().
              
            THIS-OBJECT:ControlDataDb:BeraknaP4allaresurs().
            THIS-OBJECT:ControlDataDb:BeraknaP4allafrek().
            THIS-OBJECT:ControlDataDb:p4spara().
            
            THIS-OBJECT:ControlDataDb:BeraknaP3allaresurs(ebrkatvar).
            THIS-OBJECT:ControlDataDb:BeraknaP3allafrek().
            THIS-OBJECT:ControlDataDb:p3spara().
            
            THIS-OBJECT:ControlDataDb:BeraknaP2allaresurs(ebrkatvar).
            THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
            THIS-OBJECT:ControlDataDb:BeraknaP2allamtrl().
            THIS-OBJECT:ControlDataDb:p2spara().
            
            THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
            THIS-OBJECT:ControlDataDb:p1spara().
         END.   
         THIS-OBJECT:LaddaOmGriddarResurspris().
         THIS-OBJECT:LaddaOmGriddarMtrl().                   
      END.
      IF valdtab = "Materiel" THEN DO:
         THIS-OBJECT:ControlDataDb:Subkatspara().
         IF raknavar NE "" THEN DO:
            raknavar = "".
            THIS-OBJECT:ControlDataDb:BeraknaP2allamtrl().
            THIS-OBJECT:ControlDataDb:p2spara().
            THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
            THIS-OBJECT:ControlDataDb:p1spara().
         END.   
         THIS-OBJECT:LaddaOmGriddarMtrl().                           
      END.
      IF valdtab = "P5" THEN DO:                
         THIS-OBJECT:ControlDataDb:p5spara().  
         THIS-OBJECT:ControlDataDb:BeraknaP4allafrek().
         THIS-OBJECT:ControlDataDb:p4spara().
         THIS-OBJECT:ControlDataDb:BeraknaP3allafrek().
         THIS-OBJECT:ControlDataDb:p3spara().
         THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
         THIS-OBJECT:ControlDataDb:p2spara().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
         THIS-OBJECT:ControlDataDb:p1spara().
         THIS-OBJECT:LaddaOmGriddarP5().                  
      END.    
      IF valdtab = "P4" THEN DO:                
         THIS-OBJECT:ControlDataDb:p4spara().     
         THIS-OBJECT:ControlDataDb:BeraknaP3allafrek().
         THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().   
         THIS-OBJECT:LaddaOmGriddarP4(). 
      END.    
      IF valdtab = "P3" THEN DO:                
         THIS-OBJECT:ControlDataDb:p3spara().  
         THIS-OBJECT:ControlDataDb:ArbKoderSpara().         
         THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
         THIS-OBJECT:LaddaOmGriddarP3().          
                   
      END.    
      IF valdtab = "P2" THEN DO:                
         THIS-OBJECT:ControlDataDb:p2spara().      
         THIS-OBJECT:ControlDataDb:ArbKoderSpara().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().        
         THIS-OBJECT:LaddaOmGriddarP2().                          
      END.    
      IF valdtab = "P1" THEN DO:                
         THIS-OBJECT:ControlDataDb:p1spara().      
         THIS-OBJECT:ControlDataDb:ArbKoderSpara().            
      END.      
   END METHOD.
   
   METHOD PUBLIC VOID Laddaomtab(INPUT valdtab AS CHARACTER): 
      /*IF valdtab = "Resurser" THEN DO:
         THIS-OBJECT:ControlDataDb:Resursspara().                   
      END.*/
      IF valdtab = "Resurspris" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaKatalogResurser(INPUT ekgsubidvar, INPUT ebrkatvar). 
         THIS-OBJECT:ControlDataDb:LaddaKatalog(INPUT ekgsubidvar, INPUT ebrkatvar).         
         THIS-OBJECT:ControlShell:GridResurspris:GuruReopen().
         THIS-OBJECT:ControlShell:GridResursniva:GuruReopen().
         THIS-OBJECT:ControlShell:GridResursPaslag:GuruReopen().
         THIS-OBJECT:ControlShell:GridRegel:GuruReopen().      
         THIS-OBJECT:ControlShell:GridPaslag:GuruReopen().
         THIS-OBJECT:ControlShell:GridMateriel:GuruReopen().
      END.
      IF valdtab = "Materiel" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaKatalog(INPUT ekgsubidvar, INPUT ebrkatvar).         
         THIS-OBJECT:ControlShell:GridRegel:GuruReopen().      
         THIS-OBJECT:ControlShell:GridPaslag:GuruReopen().
         THIS-OBJECT:ControlShell:GridMateriel:GuruReopen().
      END.
      IF valdtab = "P5" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaP5(INPUT ekgsubidvar, INPUT ebrkatvar).         
         THIS-OBJECT:LaddaOmGriddarP5().
      END.    
      IF valdtab = "P4" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaP4(INPUT ekgsubidvar, INPUT ebrkatvar).
         THIS-OBJECT:LaddaOmGriddarP4().
      END.    
      IF valdtab = "P3" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT ekgsubidvar, INPUT ebrkatvar). 
         THIS-OBJECT:ControlDataDb:LaddaP3(INPUT ekgsubidvar, INPUT ebrkatvar).
         THIS-OBJECT:LaddaOmGriddarP3().         
      END.    
      IF valdtab = "P2" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT ekgsubidvar, INPUT ebrkatvar). 
         THIS-OBJECT:ControlDataDb:LaddaP2(INPUT ekgsubidvar, INPUT ebrkatvar).         
         THIS-OBJECT:LaddaOmGriddarP2().
      END.    
      IF valdtab = "P1" THEN DO:   
         THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT ekgsubidvar, INPUT ebrkatvar). 
         THIS-OBJECT:ControlDataDb:LaddaP1(INPUT ekgsubidvar, INPUT ebrkatvar).         
         THIS-OBJECT:LaddaOmGriddarP1().
      END.
   END METHOD.
      
   /*METHOD PUBLIC VOID BeraknaKatalog(INPUT sender AS System.Object, INPUT args AS System.EventArgs): 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
          
      nivandvar = "".
   END METHOD.*/    
    
   METHOD PUBLIC VOID SparaKatalog(INPUT sender AS System.Object, INPUT args AS System.EventArgs): 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Sparatab(INPUT aktuelltabvar).    
      nivandvar = "".
      raknavar = "".
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).          
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).      
   END METHOD.
   
    METHOD PUBLIC VOID KatalogUppdate(INPUT nyekgsubidvarch AS CHARACTER , INPUT kolnamn  AS CHARACTER, kolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:ekgsubkatalogerTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(nyekgsubidvarch)) NO-ERROR.
      IF THIS-OBJECT:ekgsubkatalogerTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
         THIS-OBJECT:ekgsubkatTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(nyekgsubidvarch)) NO-ERROR.
         IF THIS-OBJECT:ekgsubkatTTh:AVAILABLE THEN DO:        
             THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
         END.           
         eekgsubkatalogerTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
         THIS-OBJECT:eekgsubkatalogerTTh:BUFFER-CREATE().         
         THIS-OBJECT:eekgsubkatalogerTTh:BUFFER-COPY(ekgsubkatalogerTTh).
        
         THIS-OBJECT:ControlDataDb:AndKatalog(INPUT eekgsubkatalogerTTh).                
      END.
      
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID KatalogUppdate(INPUT nyekgsubidvarch AS CHARACTER , INPUT kolnamn  AS CHARACTER, kolvarde AS CHARACTER, INPUT beforekolnamn  AS CHARACTER, beforekolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:ekgsubkatalogerTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(nyekgsubidvarch)) NO-ERROR.
      IF THIS-OBJECT:ekgsubkatalogerTTh:AVAILABLE THEN DO:
         
         IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
         THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
         THIS-OBJECT:ekgsubkatTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(nyekgsubidvarch)) NO-ERROR.
         IF THIS-OBJECT:ekgsubkatTTh:AVAILABLE THEN DO:        
             THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
         END.           
         eekgsubkatalogerTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
         THIS-OBJECT:eekgsubkatalogerTTh:BUFFER-CREATE().         
         THIS-OBJECT:eekgsubkatalogerTTh:BUFFER-COPY(ekgsubkatalogerTTh).
        
         THIS-OBJECT:ControlDataDb:AndKatalog(INPUT eekgsubkatalogerTTh).           
         
      END.
      THIS-OBJECT:ControlShell:GridKataloger:GuruReopen().
      
      
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID InitValuCH():
      
     THIS-OBJECT:VisaAllaKoder(?,?).
     THIS-OBJECT:VisaKostnader(?,?).
     THIS-OBJECT:VisaEaMangder(?,?).
     THIS-OBJECT:Fonster(?,?).
   END METHOD.
   METHOD PUBLIC VOID ForetagAnpass():
 
   END METHOD.
   
   METHOD PUBLIC VOID CreateFunctions():
       /*Drag dropp*/
      THIS-OBJECT:DGP1F = NEW Controls.Subclasses.DragDropGroup("P1F").
      THIS-OBJECT:DGP1F:AddChild(THIS-OBJECT:P1Control:gridP1Frekvens).
      THIS-OBJECT:DGP1F:AddChild(THIS-OBJECT:P1Control:gridP2ForP1).  
      
      THIS-OBJECT:DGP2M = NEW Controls.Subclasses.DragDropGroup("P2M").
      THIS-OBJECT:DGP2M:AddChild(THIS-OBJECT:P2Control:gridP2Mtrl).
      THIS-OBJECT:DGP2M:AddChild(THIS-OBJECT:P2Control:gridMtrlForP2).
      
      THIS-OBJECT:DGP2F = NEW Controls.Subclasses.DragDropGroup("P2F").
      THIS-OBJECT:DGP2F:AddChild(THIS-OBJECT:P2Control:gridP2Frekvens).
      THIS-OBJECT:DGP2F:AddChild(THIS-OBJECT:P2Control:gridP3ForP2).
      
      THIS-OBJECT:DGP2R = NEW Controls.Subclasses.DragDropGroup("P2R").
      THIS-OBJECT:DGP2R:AddChild(THIS-OBJECT:P2Control:gridP2Resurs).
      THIS-OBJECT:DGP2R:AddChild(THIS-OBJECT:P2Control:gridResursForP2).
      
      THIS-OBJECT:DGP3F = NEW Controls.Subclasses.DragDropGroup("P3F").
      THIS-OBJECT:DGP3F:AddChild(THIS-OBJECT:P3Control:gridP3Frekvens).
      THIS-OBJECT:DGP3F:AddChild(THIS-OBJECT:P3Control:gridP4ForP3).
      
      THIS-OBJECT:DGP3R = NEW Controls.Subclasses.DragDropGroup("P3R").
      THIS-OBJECT:DGP3R:AddChild(THIS-OBJECT:P3Control:gridP3Resurs).
      THIS-OBJECT:DGP3R:AddChild(THIS-OBJECT:P3Control:gridResursForP3).
      
      THIS-OBJECT:DGP4F = NEW Controls.Subclasses.DragDropGroup("P4F").
      THIS-OBJECT:DGP4F:AddChild(THIS-OBJECT:P4Control:gridP4Frekvens).
      THIS-OBJECT:DGP4F:AddChild(THIS-OBJECT:P4Control:gridP5ForP4).
      
      THIS-OBJECT:DGP4R = NEW Controls.Subclasses.DragDropGroup("P4R").
      THIS-OBJECT:DGP4R:AddChild(THIS-OBJECT:P4Control:gridP4Resurs).
      THIS-OBJECT:DGP4R:AddChild(THIS-OBJECT:P4Control:gridResursForP4).
      
      THIS-OBJECT:DGP5R = NEW Controls.Subclasses.DragDropGroup("P5R").
      THIS-OBJECT:DGP5R:AddChild(THIS-OBJECT:P5Control:gridP5Resurs).
      THIS-OBJECT:DGP5R:AddChild(THIS-OBJECT:P5Control:gridResursForP5).
      
      THIS-OBJECT:DGPS = NEW Controls.Subclasses.DragDropGroup("PS").
      THIS-OBJECT:DGPS:AddChild(THIS-OBJECT:ControlShell:gridResursPaslag).
      THIS-OBJECT:DGPS:AddChild(THIS-OBJECT:ControlShell:gridPaslag).
      
         
       /* Button overs*/
                                                                                                                                      /*remove dubl   alla over*/ 
      THIS-OBJECT:P1Control:buttonOverP1FrekvensBort:GuruKoppla(THIS-OBJECT:P1Control:gridP1Frekvens,THIS-OBJECT:P1Control:gridP2ForP1,TRUE,FALSE,FALSE).
      THIS-OBJECT:P1Control:buttonOverP1frekvens:GuruKoppla(THIS-OBJECT:P1Control:gridP2ForP1,THIS-OBJECT:P1Control:gridP1Frekvens,FALSE,FALSE,FALSE).
      
      THIS-OBJECT:P2Control:buttonOverP2MtrlBort:GuruKoppla(THIS-OBJECT:P2Control:gridP2Mtrl,THIS-OBJECT:P2Control:gridMtrlForP2,TRUE,FALSE,FALSE).
      THIS-OBJECT:P2Control:buttonOverp2Mtrl:GuruKoppla(THIS-OBJECT:P2Control:gridMtrlForP2,THIS-OBJECT:P2Control:gridP2Mtrl,FALSE,FALSE,FALSE).
                                                                                                       
      THIS-OBJECT:P2Control:buttonOverP2FrekvensBort:GuruKoppla(THIS-OBJECT:P2Control:gridP2Frekvens,THIS-OBJECT:P2Control:gridP3ForP2,TRUE,FALSE,FALSE).
      THIS-OBJECT:P2Control:buttonOverP2Frekvens:GuruKoppla(THIS-OBJECT:P2Control:gridP3ForP2,THIS-OBJECT:P2Control:gridP2Frekvens,FALSE,FALSE,FALSE).
      
      THIS-OBJECT:P2Control:buttonOverP2resursBort:GuruKoppla(THIS-OBJECT:P2Control:gridP2Resurs,THIS-OBJECT:P2Control:gridResursForP2,TRUE,FALSE,FALSE).
      THIS-OBJECT:P2Control:buttonOverP2Resurs:GuruKoppla(THIS-OBJECT:P2Control:gridResursForP2,THIS-OBJECT:P2Control:gridP2Resurs,FALSE,FALSE,FALSE).
   
      THIS-OBJECT:P3Control:buttonOverP3FrekvensBort:GuruKoppla(THIS-OBJECT:P3Control:gridP3Frekvens,THIS-OBJECT:P3Control:gridP4ForP3,TRUE,FALSE,FALSE).
      THIS-OBJECT:P3Control:buttonOverP3Frekvens:GuruKoppla(THIS-OBJECT:P3Control:gridP4ForP3,THIS-OBJECT:P3Control:gridP3Frekvens,FALSE,FALSE,FALSE).
        
      THIS-OBJECT:P3Control:buttonOverP3ResursBort:GuruKoppla(THIS-OBJECT:P3Control:gridP3Resurs,THIS-OBJECT:P3Control:gridResursForP3,TRUE,FALSE,FALSE).
      THIS-OBJECT:P3Control:buttonOverP3Resurs:GuruKoppla(THIS-OBJECT:P3Control:gridResursForP3,THIS-OBJECT:P3Control:gridP3Resurs,FALSE,FALSE,FALSE).
      
      THIS-OBJECT:P4Control:buttonOverP4FrekvensBort:GuruKoppla(THIS-OBJECT:P4Control:gridP4Frekvens,THIS-OBJECT:P4Control:gridP5ForP4,TRUE,FALSE,FALSE).
      THIS-OBJECT:P4Control:buttonOverP4Frekvens:GuruKoppla(THIS-OBJECT:P4Control:gridP5ForP4,THIS-OBJECT:P4Control:gridP4Frekvens,FALSE,FALSE,FALSE).
         
      THIS-OBJECT:P4Control:buttonOverP4ResursBort:GuruKoppla(THIS-OBJECT:P4Control:gridP4Resurs,THIS-OBJECT:P4Control:gridResursForP4,TRUE,FALSE,FALSE).
      THIS-OBJECT:P4Control:buttonOverP4Resurs:GuruKoppla(THIS-OBJECT:P4Control:gridResursForP4,THIS-OBJECT:P4Control:gridP4Resurs,FALSE,FALSE,FALSE).
   
      THIS-OBJECT:P5Control:buttonOverP5ResursBort:GuruKoppla(THIS-OBJECT:P5Control:gridP5Resurs,THIS-OBJECT:P5Control:gridResursForP5,TRUE,FALSE,FALSE).
      THIS-OBJECT:P5Control:buttonOverP5Resurs:GuruKoppla(THIS-OBJECT:P5Control:gridResursForP5,THIS-OBJECT:P5Control:gridP5Resurs,FALSE,FALSE,FALSE).
        
      THIS-OBJECT:ControlShell:buttonOverResurspaslagBort:GuruKoppla(THIS-OBJECT:ControlShell:gridResursPaslag,THIS-OBJECT:ControlShell:gridPaslag,TRUE,FALSE,FALSE).
      THIS-OBJECT:ControlShell:buttonOverResurspaslag:GuruKoppla(THIS-OBJECT:ControlShell:gridPaslag,THIS-OBJECT:ControlShell:gridResursPaslag,FALSE,FALSE,FALSE).
      
      THIS-OBJECT:ControlShell:buttonOverResurspris:GuruKoppla(THIS-OBJECT:ControlShell:gridResurs,THIS-OBJECT:ControlShell:gridResursPris,FALSE,FALSE,FALSE). 
   END METHOD.
   
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboP3ArbKoder().
      THIS-OBJECT:CreateComboP2ArbKoder().
      THIS-OBJECT:CreateComboP1ArbKoder().
      THIS-OBJECT:CreateComboNivaer().
   END METHOD. 
   
   METHOD PUBLIC VOID CreateComboNivaer():
      DEFINE VARIABLE cmbNivaerlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE nivaerK      AS Controls.GridRubrik      NO-UNDO.
      cmbNivaerlist = NEW Controls.GridRubrikLista().
      cmbNivaerlist:ttBufferHandle = THIS-OBJECT:ekgnivaTTh.
      nivaerK = NEW Controls.GridRubrik().
      nivaerK:Falt = "NIVA".
      nivaerK:Rubrik = "Nivå".
      cmbNivaerlist:addRubrik(nivaerK).
      THIS-OBJECT:ControlShell:comboNiva:GuruCombo:GuruInitCombo(cmbNivaerlist:ttBufferHandle, "NIVA", "NIVA").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboP3ArbKoder():
      DEFINE VARIABLE cmbP3ArbKoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE p3arbkodK      AS Controls.GridRubrik      NO-UNDO.
      cmbP3ArbKoderlist = NEW Controls.GridRubrikLista().
      cmbP3ArbKoderlist:ttBufferHandle = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh.
      p3arbkodK = NEW Controls.GridRubrik().
      p3arbkodK:Falt = "P3ARBKOD".
      p3arbkodK:Rubrik = "P3ARBKOD".
      cmbP3ArbKoderlist:addRubrik(p3arbkodK).
      THIS-OBJECT:ControlShell:comboP3ArbKod:GuruCombo:GuruInitCombo(cmbP3ArbKoderlist:ttBufferHandle, "P3ARBKOD", "P3ARBKOD").
   END METHOD.

   METHOD PUBLIC VOID CreateComboP2ArbKoder():
      DEFINE VARIABLE cmbP2ArbKoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE p2arbkodK      AS Controls.GridRubrik      NO-UNDO.
      cmbP2ArbKoderlist = NEW Controls.GridRubrikLista().
      cmbP2ArbKoderlist:ttBufferHandle = THIS-OBJECT:ekgp2arbkodTTh.
      p2arbkodK = NEW Controls.GridRubrik().
      p2arbkodK:Falt = "P2ARBKOD".
      p2arbkodK:Rubrik = "P2ARBKOD".
      cmbP2ArbKoderlist:addRubrik(p2arbkodK).
      THIS-OBJECT:ControlShell:comboP2ArbKod:GuruCombo:GuruInitCombo(cmbP2ArbKoderlist:ttBufferHandle, "P2ARBKOD", "P2ARBKOD").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboP1ArbKoder():
      DEFINE VARIABLE cmbP1ArbKoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE p1arbkodK      AS Controls.GridRubrik      NO-UNDO.
      cmbP1ArbKoderlist = NEW Controls.GridRubrikLista().
      cmbP1ArbKoderlist:ttBufferHandle = THIS-OBJECT:ekgp1arbkodTTh.
      p1arbkodK = NEW Controls.GridRubrik().
      p1arbkodK:Falt = "P1ARBKOD".
      p1arbkodK:Rubrik = "P1ARBKOD".
      cmbP1ArbKoderlist:addRubrik(p1arbkodK).
      THIS-OBJECT:ControlShell:comboP1ArbKod:GuruCombo:GuruInitCombo(cmbP1ArbKoderlist:ttBufferHandle, "P1ARBKOD", "P1ARBKOD").
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridKataloger().
      THIS-OBJECT:CreateGridResurs().
      THIS-OBJECT:CreateGridRegel().
      THIS-OBJECT:CreateGridResurspris().
      THIS-OBJECT:CreateGridResursniva().
      THIS-OBJECT:CreateGridResursForResurspris().
      THIS-OBJECT:CreateGridPaslag().
      THIS-OBJECT:CreateGridResursPaslag().
      THIS-OBJECT:CreateGridMateriel().
      
      /*P5*/
      THIS-OBJECT:CreateGridP5().  
      THIS-OBJECT:CreateGridP5Resurs().
      THIS-OBJECT:CreateGridResursForP5().
      THIS-OBJECT:CreateGridImportP5().
      /*P4*/
      THIS-OBJECT:CreateGridP5ForP4().
      THIS-OBJECT:CreateGridP4().
      THIS-OBJECT:CreateGridP4Resurs().
      THIS-OBJECT:CreateGridResursForP4().
      THIS-OBJECT:CreateGridP4Frekvens().
      THIS-OBJECT:CreateGridImportP4().
      /*P3*/
      THIS-OBJECT:CreateGridP4ForP3().
      THIS-OBJECT:CreateGridP3().
      THIS-OBJECT:CreateGridP3Resurs().
      THIS-OBJECT:CreateGridP3Frekvens().
      THIS-OBJECT:CreateGridResursForP3().
      THIS-OBJECT:CreateGridP3arbkod().
      THIS-OBJECT:CreateGridImportP3().
      /*P2*/
      THIS-OBJECT:CreateGridP3ForP2().
      THIS-OBJECT:CreateGridP2().
      THIS-OBJECT:CreateGridP2Resurs().
      THIS-OBJECT:CreateGridP2Frekvens().
      THIS-OBJECT:CreateGridP2Mtrl().
      THIS-OBJECT:CreateGridMtrlForP2().
      THIS-OBJECT:CreateGridResursForP2().
      THIS-OBJECT:CreateGridP2arbkod().
      THIS-OBJECT:CreateGridImportP2().
      /*P1*/
      THIS-OBJECT:CreateGridP2ForP1().
      THIS-OBJECT:CreateGridP1().
      THIS-OBJECT:CreateGridP1Frekvens().
      THIS-OBJECT:CreateGridP1arbkod().
      THIS-OBJECT:CreateGridImportP1().
   END METHOD.
   
 METHOD PUBLIC VOID CreateGridKataloger(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARTAL","",FALSE,70).     
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250). 
      rubrikergrid[4] = NEW Controls.GridRubrik("STARTDATUM","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("SLUTDATUM","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANVANDSAVNAMN","",TRUE,200).
      IF Guru.Konstanter:kalk2sekvar[13] = TRUE THEN rubrikergrid[8] = NEW Controls.GridRubrik("LASTA","",FALSE,70, "LOG").
      ELSE rubrikergrid[8] = NEW Controls.GridRubrik("LASTA","",TRUE,70, "LOG").
      IF Guru.Konstanter:kalk2sekvar[14] = TRUE THEN rubrikergrid[9] = NEW Controls.GridRubrik("ANVANDS","",FALSE,70, "LOG").
      ELSE rubrikergrid[9] = NEW Controls.GridRubrik("ANVANDS","",TRUE,70, "LOG").
      THIS-OBJECT:ControlShell:GridKataloger:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 9, INPUT THIS-OBJECT:ekgsubkatalogerTTh)).
      THIS-OBJECT:ControlShell:GridKataloger:Text = "Kataloger".
      THIS-OBJECT:ControlShell:GridKataloger:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:ControlShell:GridKataloger:DisplayLayout:Bands[0]:Columns["STARTDATUM"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridKataloger:DisplayLayout:Bands[0]:Columns["SLUTDATUM"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridKataloger:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = FALSE.
      THIS-OBJECT:ControlShell:GridKataloger:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = FALSE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridResurs():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",FALSE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("AKTIV","",FALSE,70, "LOG").
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:ControlShell:GridResurs:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ekgresursTTh)).
      THIS-OBJECT:ControlShell:GridResurs:Text = "Resurser".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridRegel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("REGELID","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("TIMSUMKOD","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("EASORTKLG1","",TRUE,70).
      /*rubrikergrid[7] = NEW Controls.GridRubrik("EASORTKLG2","",TRUE,70).*/
      rubrikergrid[7] = NEW Controls.GridRubrik("UTOVERFREK","",TRUE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("EAFAKTOR","",TRUE,70, "LOG").
      
      
      THIS-OBJECT:ControlShell:GridRegel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:ekgregelTTh)).
      THIS-OBJECT:ControlShell:GridRegel:Text = "Regler".
      
      /*DÖLJ*/
      THIS-OBJECT:ControlShell:GridRegel:DisplayLayout:Bands[0]:Columns["EASORTKLG2"]:Hidden = FALSE.
      THIS-OBJECT:ControlShell:GridRegel:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridRegel:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
    METHOD PUBLIC VOID CreateGridResurspris():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("PRIS","",FALSE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("REGELID","",FALSE,50,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("REGELANMARKNING","",TRUE,130).
      rubrikergrid[10] = NEW Controls.GridRubrik("RESURSAKTIV","",TRUE,50, "LOG").
  
      THIS-OBJECT:ControlShell:GridResurspris:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:ekgresursprisTTh)).
      THIS-OBJECT:ControlShell:GridResurspris:Text = "Resurspriser".
      THIS-OBJECT:ControlShell:GridResurspris:tBS:PositionChanged:Subscribe(ValueChangeResursPris).
      /*DÖLJ*/
      /*THIS-OBJECT:ControlShell:GridResurspris:DisplayLayout:Bands[0]:Columns["REGELID"]:Hidden = TRUE.*/
      THIS-OBJECT:ControlShell:GridResurspris:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridResurspris:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridResursForResurspris():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,50).
      
      THIS-OBJECT:ControlShell:GridResursForResurspris:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:ekgresursTTh)).
      THIS-OBJECT:ControlShell:GridResursForResurspris:Text = "Resurser".
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridResursniva():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("NIVA","",TRUE,70).
      /*rubrikergrid[6] = NEW Controls.GridRubrik("FORVALDEJFREK","",FALSE,50, "LOG").
      rubrikergrid[7] = NEW Controls.GridRubrik("FORVALDFREK","",FALSE,50, "LOG").*/

      THIS-OBJECT:ControlShell:GridResursniva:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ekgresursnivaTTh)).
      THIS-OBJECT:ControlShell:GridResursniva:Text = "Resursnivåer".
           
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:ControlShell:GridResursniva:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridResursniva:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
    
  METHOD PUBLIC VOID CreateGridPaslag():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("PASLAGNR","",TRUE,40).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,200).
      rubrikergrid[5] = NEW Controls.GridRubrik("PASLAG","",FALSE,70,"DEC").
      
      THIS-OBJECT:ControlShell:GridPaslag:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ekgpaslagTTh)).
      THIS-OBJECT:ControlShell:GridPaslag:Text = "Påslag".
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:ControlShell:GridPaslag:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridPaslag:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   
  METHOD PUBLIC VOID CreateGridResursPaslag():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("PASLAGNR","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("PASLAGSBENAMNING","",TRUE,250).
        
      THIS-OBJECT:ControlShell:GridResursPaslag:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ekgresurspaslagTTh)).
      THIS-OBJECT:ControlShell:GridResursPaslag:Text = "Resurspåslag".
      
      /*DÖLJ*/
      THIS-OBJECT:ControlShell:GridResursPaslag:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridResursPaslag:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.  
   
  METHOD PUBLIC VOID CreateGridMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARTNR","",TRUE,100).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","",FALSE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("PRIS","",FALSE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("BASPRIS","",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("KALKYLPRIS","",FALSE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("UPPDATDATUM","",TRUE,150,"DATETIME").
      rubrikergrid[10] = NEW Controls.GridRubrik("LASTA","",FALSE,100, "LOG").
      rubrikergrid[11] = NEW Controls.GridRubrik("LEVKOD","",FALSE,50).
      rubrikergrid[12] = NEW Controls.GridRubrik("AKTIV","",FALSE,50, "LOG").
      
      THIS-OBJECT:ControlShell:GridMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 12, INPUT THIS-OBJECT:ekgmtrlTTh)).
      THIS-OBJECT:ControlShell:GridMateriel:Text = "Materiel".
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:ControlShell:GridMateriel:DisplayLayout:Bands[0]:Columns["LEVKOD"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridMateriel:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ControlShell:GridMateriel:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP5(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P5LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:P5Control:GridP5:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 16, INPUT THIS-OBJECT:ekgp5TTh)).
      THIS-OBJECT:P5Control:GridP5:Text = "P5-Koder".
      THIS-OBJECT:P5Control:GridP5:GuruUpdateTitle().
      THIS-OBJECT:P5Control:GridP5:tBS:PositionChanged:Subscribe(ValueChangeP5).
      
      /*DÖLJ*/
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["P5LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP5Resurs(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P5LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P5BENAMNING","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("P5ENHET","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,70).
      rubrikergrid[10] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("RESURSPRIS","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EAMONT","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").

      THIS-OBJECT:P5Control:GridP5Resurs:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp5resursTTh)).
      THIS-OBJECT:P5Control:GridP5Resurs:Text = "P5-Resurser".
      THIS-OBJECT:P5Control:GridP5Resurs:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["P5LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridResursForP5():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").   
      
      THIS-OBJECT:P5Control:GridResursForP5:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ekgresursForTTh)).
      THIS-OBJECT:P5Control:GridResursForP5:Text = "Resurser".
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridP5ForP4():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P5LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:P4Control:GridP5ForP4:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ekgp5TTh)).
      THIS-OBJECT:P4Control:GridP5ForP4:Text = "P5-Koder".
      THIS-OBJECT:P4Control:GridP5ForP4:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P4Control:GridP5ForP4:DisplayLayout:Bands[0]:Columns["P5LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP5ForP4:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP5ForP4:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.  
   
   METHOD PUBLIC VOID CreateGridImportP5(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSNR","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).
      
      THIS-OBJECT:P5Control:GridImportP5:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:importP5TTh)).
      THIS-OBJECT:P5Control:GridImportP5:Text = "P5 Kod Import".
      THIS-OBJECT:P5Control:GridImportP5:GuruUpdateTitle().
      THIS-OBJECT:FelMeddCheckImport(P5Control:GridImportP5).
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP4(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("RESURSNR","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).
      THIS-OBJECT:P4Control:GridImportP4:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:importP4TTh)).
      THIS-OBJECT:P4Control:GridImportP4:Text = "P4 Kod Import".
      THIS-OBJECT:P4Control:GridImportP4:GuruUpdateTitle().
      
      THIS-OBJECT:FelMeddCheckImport(P4Control:GridImportP4).   
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P5BENAMNING","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC"). 
      rubrikergrid[6] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).     
      THIS-OBJECT:P4Control:GridImportP4F:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:importP4P5TTh)).
      THIS-OBJECT:P4Control:GridImportP4F:Text = "P4 Frekvens Import".
      THIS-OBJECT:P4Control:GridImportP4F:GuruUpdateTitle().
      THIS-OBJECT:P4Control:GridImportP4F:VISIBLE = FALSE.
      
     THIS-OBJECT:FelMeddCheckImport(P4Control:GridImportP4F).
      
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP3(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTALMAN","",TRUE,70). 
      rubrikergrid[7] = NEW Controls.GridRubrik("TIDSUNDER","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSNR","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).
      THIS-OBJECT:P3Control:GridImportP3:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:importP3TTh)).
      THIS-OBJECT:P3Control:GridImportP3:Text = "P3 Kod Import".
      THIS-OBJECT:P3Control:GridImportP3:GuruUpdateTitle().
      
      THIS-OBJECT:FelMeddCheckImport(P3Control:GridImportP3).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).      
      THIS-OBJECT:P3Control:GridImportP3F:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:importP3P4TTh)).
      THIS-OBJECT:P3Control:GridImportP3F:Text = "P3 Frekvens Import".
      THIS-OBJECT:P3Control:GridImportP3F:GuruUpdateTitle().
      THIS-OBJECT:P3Control:GridImportP3F:VISIBLE = FALSE. 
      
      THIS-OBJECT:FelMeddCheckImport(P3Control:GridImportP3F).
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP2(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("RESURSNR","",TRUE,250).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).
      THIS-OBJECT:P2Control:GridImportP2:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:importP2TTh)).
      THIS-OBJECT:P2Control:GridImportP2:Text = "P2 Kod Import".
      THIS-OBJECT:P2Control:GridImportP2:GuruUpdateTitle().
      
      THIS-OBJECT:FelMeddCheckImport(P2Control:GridImportP2).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC"). 
      rubrikergrid[8] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).     
      THIS-OBJECT:P2Control:GridImportP2F:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:importP2P3TTh)).
      THIS-OBJECT:P2Control:GridImportP2F:Text = "P2 Frekvens Import".
      THIS-OBJECT:P2Control:GridImportP2F:GuruUpdateTitle().
      THIS-OBJECT:P2Control:GridImportP2F:VISIBLE = FALSE. 
      
      THIS-OBJECT:FelMeddCheckImport(P2Control:GridImportP2F).
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP1(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).
      THIS-OBJECT:P1Control:GridImportP1:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:importP1TTh)).
      THIS-OBJECT:P1Control:GridImportP1:Text = "P1 Kod Import".
      THIS-OBJECT:P1Control:GridImportP1:GuruUpdateTitle().
      
      THIS-OBJECT:FelMeddCheckImport(P1Control:GridImportP1).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").    
      rubrikergrid[8] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1000).  
      THIS-OBJECT:P1Control:GridImportP1F:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:importP1P2TTh)).
      THIS-OBJECT:P1Control:GridImportP1F:Text = "P1 Frekvens Import".
      THIS-OBJECT:P1Control:GridImportP1F:GuruUpdateTitle().
      THIS-OBJECT:P1Control:GridImportP1F:VISIBLE = FALSE.
      
      THIS-OBJECT:FelMeddCheckImport(P1Control:GridImportP1F).
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP4(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",FALSE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:P4Control:GridP4:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 17, INPUT THIS-OBJECT:ekgp4TTh)).
      THIS-OBJECT:P4Control:GridP4:Text = "P4-Koder".
      THIS-OBJECT:P4Control:GridP4:GuruUpdateTitle().
      THIS-OBJECT:P4Control:GridP4:tBS:PositionChanged:Subscribe(ValueChangeP4).
      
      /*DÖLJ*/
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP4Resurs(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P4ENHET","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,100,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("RESURSPRIS","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EAMONT","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
     
      THIS-OBJECT:P4Control:GridP4Resurs:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp4resursTTh)).
      THIS-OBJECT:P4Control:GridP4Resurs:Text = "P4-Resurser".
      THIS-OBJECT:P4Control:GridP4Resurs:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridResursForP4():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").   
      /*LENA*/
      rubrikergrid[6] = NEW Controls.GridRubrik("REGELID","",TRUE,100,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("UTOVERFREK","",TRUE,70,"LOG").
      
      THIS-OBJECT:P4Control:GridResursForP4:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:ekgresursForTTh)).
      THIS-OBJECT:P4Control:GridResursForP4:Text = "Resurser".
  END METHOD.  
   
  METHOD PUBLIC VOID CreateGridP4Frekvens(): 
      {GridRubrikListaVarExtent.i}
  
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("P5LOPNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("P5BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").  
      
      THIS-OBJECT:P4Control:GridP4Frekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp4frekvensTTh)).
      THIS-OBJECT:P4Control:GridP4Frekvens:Text = "P4-Frekvenser".
      THIS-OBJECT:P4Control:GridP4Frekvens:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["P5LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridP4ForP3():
      {GridRubrikListaVarExtent.i} 

      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:P3Control:GridP4ForP3:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ekgp4TTh)).
      THIS-OBJECT:P3Control:GridP4ForP3:Text = "P4-Koder".
      THIS-OBJECT:P3Control:GridP4ForP3:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P3Control:GridP4ForP3:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP4ForP3:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP4ForP3:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
  END METHOD.  
  
   METHOD PUBLIC VOID CreateGridP3():
      {GridRubrikListaVarExtent.i}  
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).     
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",FALSE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("BERTIM","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("UTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("ANTALMAN"," ",FALSE,70).
      rubrikergrid[22] = NEW Controls.GridRubrik("TIDSUNDER","",FALSE,70).
      rubrikergrid[23] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:P3Control:GridP3:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 23, INPUT THIS-OBJECT:ekgp3TTh)).
      THIS-OBJECT:P3Control:GridP3:Text = "P3-Koder".
      THIS-OBJECT:P3Control:GridP3:GuruUpdateTitle().
      THIS-OBJECT:P3Control:GridP3:tBS:PositionChanged:Subscribe(ValueChangeP3).
      
      /*DÖLJ*/
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
      /*IF ebrkatvar = "KLG2" THEN THIS-OBJECT:ControlShell:GridP3:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = TRUE.*/ 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP3Resurs(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P3ENHET","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,70).
      rubrikergrid[10] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("RESURSPRIS","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("UTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMONT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      
      THIS-OBJECT:P3Control:GridP3Resurs:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 20, INPUT THIS-OBJECT:ekgp3resursTTh)).
      THIS-OBJECT:P3Control:GridP3Resurs:Text = "P3-Resurser".
      THIS-OBJECT:P3Control:GridP3Resurs:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridP3Frekvens(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").
       
      THIS-OBJECT:P3Control:GridP3Frekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp3frekvensTTh)).
      THIS-OBJECT:P3Control:GridP3Frekvens:Text = "P3-Frekvenser".
      THIS-OBJECT:P3Control:GridP3Frekvens:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridResursForP3(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").   
      
      THIS-OBJECT:P3Control:GridResursForP3:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ekgresursForTTh)).
      THIS-OBJECT:P3Control:GridResursForP3:Text = "Resurser".
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridP3arbkod():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,150).
      
      THIS-OBJECT:P3Control:GridP3arbkod:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh)).
      THIS-OBJECT:P3Control:GridP3arbkod:Text = "P3-Arbetskoder".
      THIS-OBJECT:P3Control:GridP3arbkod:GuruUpdateTitle().
      THIS-OBJECT:P3Control:GridP3arbkod:tBS:PositionChanged:Subscribe(ValueChangeP3ArbKod).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P3Control:GridP3arbkod:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3arbkod:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP3ForP2(): 
      {GridRubrikListaVarExtent.i} 

      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:P2Control:GridP3ForP2:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ekgp3TTh)).
      THIS-OBJECT:P2Control:GridP3ForP2:Text = "P3-Koder".
      THIS-OBJECT:P2Control:GridP3ForP2:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P2Control:GridP3ForP2:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP3ForP2:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridP2():
      {GridRubrikListaVarExtent.i} 
  
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",FALSE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("BERTIM","",TRUE,70,"DEC").  
      rubrikergrid[9] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC"). 
      rubrikergrid[10] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MATERIELKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("UTRUSTKOST"," ",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:P2Control:GridP2:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 22, INPUT THIS-OBJECT:ControlDataDb:ekgp2TTh)).
      THIS-OBJECT:P2Control:GridP2:Text = "P2-Koder".
      THIS-OBJECT:P2Control:GridP2:GuruUpdateTitle().
      THIS-OBJECT:P2Control:GridP2:tBS:PositionChanged:Subscribe(ValueChangeP2).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2Resurs(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P2ENHET","",TRUE,100).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,70).
      rubrikergrid[10] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("RESURSPRIS","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("UTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMONT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
     
      THIS-OBJECT:P2Control:GridP2Resurs:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 20, INPUT THIS-OBJECT:ekgp2resursTTh)).
      THIS-OBJECT:P2Control:GridP2Resurs:Text = "P2-Resurser".
      THIS-OBJECT:P2Control:GridP2Resurs:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridP2Frekvens(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FBERTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FUTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FUTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("FEAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").
      
      THIS-OBJECT:P2Control:GridP2Frekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 22, INPUT THIS-OBJECT:ekgp2frekvensTTh)).
      THIS-OBJECT:P2Control:GridP2Frekvens:Text = "P2-Frekvenser".
      THIS-OBJECT:P2Control:GridP2Frekvens:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2Mtrl():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ARTNR","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("LEVKOD","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("KALKYLPRIS","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MTRLSUMMA","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("MTRLBENAMNING","",TRUE,250).
      
      THIS-OBJECT:P2Control:GridP2Mtrl:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 11, INPUT THIS-OBJECT:ekgp2mtrlTTh)).
      THIS-OBJECT:P2Control:GridP2Mtrl:Text = "P2-Materiel".
      THIS-OBJECT:P2Control:GridP2Mtrl:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["LEVKOD"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2ForP1():
      {GridRubrikListaVarExtent.i} 

      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:P1Control:GridP2ForP1:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ControlDataDb:ekgp2TTh)).
      THIS-OBJECT:P1Control:GridP2ForP1:Text = "P2-Koder".
      THIS-OBJECT:P1Control:GridP2ForP1:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P1Control:GridP2ForP1:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P1Control:GridP2ForP1:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridMtrlForP2():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("ARTNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",TRUE,230).
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("BASPRIS","",TRUE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("KALKYLPRIS","",TRUE,70,"DEC").

      THIS-OBJECT:P2Control:GridMtrlForP2:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:ekgmtrlTTh)).
      THIS-OBJECT:P2Control:GridMtrlForP2:Text = "Materiel".
      THIS-OBJECT:P2Control:GridMtrlForP2:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P2Control:GridMtrlForP2:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridMtrlForP2:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridResursForP2():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").   
      
      THIS-OBJECT:P2Control:GridResursForP2:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ekgresursForTTh)).
      THIS-OBJECT:P2Control:GridResursForP2:Text = "Resurser".
      THIS-OBJECT:P2Control:GridResursForP2:GuruUpdateTitle().
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2arbkod():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,150).
      
      THIS-OBJECT:P2Control:GridP2arbkod:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ekgp2arbkodTTh)).
      THIS-OBJECT:P2Control:GridP2arbkod:Text = "P2-Arbetskoder".
      THIS-OBJECT:P2Control:GridP2arbkod:GuruUpdateTitle().
      THIS-OBJECT:P2Control:GridP2arbkod:tBS:PositionChanged:Subscribe(ValueChangeP2ArbKod).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P2Control:GridP2arbkod:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2arbkod:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridP1():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("BERTIM","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MATERIELKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("UTRUSTKOST"," ",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:P1Control:GridP1:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 22, INPUT THIS-OBJECT:ekgp1TTh)).
      THIS-OBJECT:P1Control:GridP1:Text = "P1-Koder".
      THIS-OBJECT:P1Control:GridP1:GuruUpdateTitle().
      THIS-OBJECT:P1Control:GridP1:tBS:PositionChanged:Subscribe(ValueChangeP1).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = TRUE.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridP1Frekvens(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("P1BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FBERTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FUTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FMATERIELKOST","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FUTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("FEAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[23] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").
      
      THIS-OBJECT:P1Control:GridP1Frekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 23, INPUT THIS-OBJECT:ekgp1frekvensTTh)).
      THIS-OBJECT:P1Control:GridP1Frekvens:Text = "P1-Frekvenser".
      THIS-OBJECT:P1Control:GridP1Frekvens:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE.     
   END METHOD.
   
    METHOD PUBLIC VOID CreateGridP1arbkod():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,150).
      
      THIS-OBJECT:P1Control:GridP1arbkod:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ekgp1arbkodTTh)).
      THIS-OBJECT:P1Control:GridP1arbkod:Text = "P1-Arbetskoder".
      THIS-OBJECT:P1Control:GridP1arbkod:GuruUpdateTitle().
      THIS-OBJECT:P1Control:GridP1arbkod:tBS:PositionChanged:Subscribe(ValueChangeP1ArbKod).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:P1Control:GridP1arbkod:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:P1Control:GridP1arbkod:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
    
   METHOD PUBLIC VOID CreateRibbon():
        /*första ribbon tab*/
               
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule("EkgShell").
      
      THIS-OBJECT:RibbonGroupSparaKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Spara").
      THIS-OBJECT:RibbonGroupKatInfo = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Katalog").
      THIS-OBJECT:RibbonGroupTabInfo = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sök Arbetskod").
      THIS-OBJECT:RibbonGroupVisa = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visa").
      THIS-OBJECT:RibbonGroupFonster = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Fönster").
      THIS-OBJECT:RibbonGroupSpara = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Spara inställningar").
      /*SparaKatalog*/
      THIS-OBJECT:RibbonToolSparaKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Spara").
      THIS-OBJECT:RibbonToolSparaKatalog:SharedPropsInternal:Caption = "Spara".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaKatalog,INPUT "bilder/exitwnd.gif").
      THIS-OBJECT:RibbonToolSparaKatalog:SharedPropsInternal:Visible = TRUE.   
      /*/*BeraknaKatalog*/
      THIS-OBJECT:RibbonToolBeraknaKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Beräkna").
      THIS-OBJECT:RibbonToolBeraknaKatalog:SharedPropsInternal:Caption = "Beräkna".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolBeraknaKatalog,INPUT "bilder/kalkico.png").
      THIS-OBJECT:RibbonToolBeraknaKatalog:SharedPropsInternal:Visible = TRUE.*/ 
      /*KatInfo*/
      THIS-OBJECT:RibbonToolBenamning = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Benämning").
      THIS-OBJECT:RibbonToolBenamning:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolBenamning:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolEbrKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("EbrKat").
      THIS-OBJECT:RibbonToolEbrKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolEbrKat:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolArtal = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Årtal").
      THIS-OBJECT:RibbonToolArtal:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolArtal:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolSubKatID = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("SubKatInfo").
      THIS-OBJECT:RibbonToolSubKatID:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolSubKatID:SharedPropsInternal:Width = 192.
      
      /*TabInfo*/
      THIS-OBJECT:RibbonToolAktuellTab = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Tab").
      THIS-OBJECT:RibbonToolAktuellTab:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolAktuellTab:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolAktuellKod = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Kod").
      THIS-OBJECT:RibbonToolAktuellKod:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolAktuellKod:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
       /*Visa*/.
      THIS-OBJECT:RibbonToolVisaAllaKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaAllaKoder").
      THIS-OBJECT:RibbonToolVisaAllaKoder:SharedPropsInternal:Caption = "Alla koder".
      THIS-OBJECT:RibbonToolVisaAllaKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisaAllaKoder:ToolClick:Subscribe(THIS-OBJECT:VisaAllaKoder).
      
      THIS-OBJECT:RibbonToolVisaKostnader = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaKostnader").
      THIS-OBJECT:RibbonToolVisaKostnader:SharedPropsInternal:Caption = "Kostnader".
      THIS-OBJECT:RibbonToolVisaKostnader:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisaKostnader:ToolClick:Subscribe(THIS-OBJECT:VisaKostnader).
      THIS-OBJECT:RibbonToolVisaEaMangder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaEaMangder").
      THIS-OBJECT:RibbonToolVisaEaMangder:SharedPropsInternal:Caption = "Ea-Mängder".
      THIS-OBJECT:RibbonToolVisaEaMangder:ToolClick:Subscribe(THIS-OBJECT:VisaEaMangder).
      THIS-OBJECT:RibbonToolVisaEaMangder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /*Fonster*/
      THIS-OBJECT:RibbonToolFonsterKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FonsterKoder").
      THIS-OBJECT:RibbonToolFonsterKoder:SharedPropsInternal:Caption = "Koder".
      THIS-OBJECT:RibbonToolFonsterKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFonsterKoder:ToolClick:Subscribe(THIS-OBJECT:Fonster).
      THIS-OBJECT:RibbonToolFonsterUnderKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FonsterUnderKoder").
      THIS-OBJECT:RibbonToolFonsterUnderKoder:SharedPropsInternal:Caption = "Underliggande koder".
      THIS-OBJECT:RibbonToolFonsterUnderKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFonsterUnderKoder:ToolClick:Subscribe(THIS-OBJECT:Fonster).
      THIS-OBJECT:RibbonToolFonsterArbKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FonsterArbKoder").
      THIS-OBJECT:RibbonToolFonsterArbKoder:SharedPropsInternal:Caption = "Arbetskoder".
      THIS-OBJECT:RibbonToolFonsterArbKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFonsterArbKoder:ToolClick:Subscribe(THIS-OBJECT:Fonster).
      
      /*spara*/
      THIS-OBJECT:RibbonToolSparaDefs = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveDefs").
      THIS-OBJECT:RibbonToolSparaDefs = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveDefs").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Caption = "Spara inställningar".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = TRUE.
      
      /*prep*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaKatalog, THIS-OBJECT:RibbonGroupSparaKatalog).
      /*THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolBeraknaKatalog, THIS-OBJECT:RibbonGroupSparaKatalog).*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolBenamning,THIS-OBJECT:RibbonGroupKatinfo).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEbrKat,THIS-OBJECT:RibbonGroupKatinfo).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolArtal,THIS-OBJECT:RibbonGroupKatinfo).        
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSubKatID,THIS-OBJECT:RibbonGroupKatinfo). 
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAktuellTab,THIS-OBJECT:RibbonGroupTabinfo). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAktuellKod,THIS-OBJECT:RibbonGroupTabinfo). 
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaAllaKoder,THIS-OBJECT:RibbonGroupVisa). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaKostnader,THIS-OBJECT:RibbonGroupVisa). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaEaMangder,THIS-OBJECT:RibbonGroupVisa). 
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFonsterKoder,THIS-OBJECT:RibbonGroupFonster). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFonsterUnderKoder,THIS-OBJECT:RibbonGroupFonster). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFonsterArbKoder,THIS-OBJECT:RibbonGroupFonster). 
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs,RibbonGroupSpara). 
      
      ASSIGN 
      THIS-OBJECT:RibbonToolFonsterKoder:Checked = TRUE.
      THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = TRUE.
      THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = FALSE.
      THIS-OBJECT:RibbonToolVisaKostnader:Checked = TRUE.
      THIS-OBJECT:RibbonToolVisaEaMangder:Checked = TRUE.
      /*eventS triggers*/
    
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:SUBSCRIBE(SparaDefs).
      THIS-OBJECT:RibbonToolSparaKatalog:ToolClick:SUBSCRIBE(SparaKatalog).
      /*THIS-OBJECT:RibbonToolBeraknaKatalog:ToolClick:SUBSCRIBE(BeraknaKatalog).*/ 
      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"EkgShell",0,"",THIS-OBJECT:RibbonGroupVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"EkgShell",0,"",THIS-OBJECT:RibbonGroupFonster).
      
      /*Tillfällig kod*/
      THIS-OBJECT:UppdateraKatalogRibbon().    
   END METHOD.
   
  METHOD PUBLIC VOID DoljRibbonGrupper(INPUT gomas AS LOGICAL):
      THIS-OBJECT:RibbonGroupSparaKatalog:Visible = gomas.
      THIS-OBJECT:RibbonGroupKatInfo:VISIBLE = gomas.
      THIS-OBJECT:RibbonGroupTabInfo:VISIBLE = gomas.
  END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      THIS-OBJECT:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:tiduth:AVAILABLE THEN. 
      
   END METHOD.
   
   /*EVENTS*/
   
  METHOD PUBLIC VOID StripButtonRbort_Click(INPUT listaP AS CHARACTER):
      IF listaP = "P5" THEN DO:
         THIS-OBJECT:P5Control:GridImportP5:GuruBortrader(TRUE). 
         IF THIS-OBJECT:importP5TTh:AVAILABLE THEN.
         ELSE DO: 
            THIS-OBJECT:P5Control:StripButtonP5Rbort:VISIBLE = FALSE. 
            THIS-OBJECT:P5Control:StripButtonP5Rin:VISIBLE = FALSE.
         END.
      END.    
      IF listaP = "P4" THEN DO:
         IF THIS-OBJECT:P4Control:GridImportP4F:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P4Control:GridImportP4F:GuruBortrader(TRUE).
            IF THIS-OBJECT:importP4P5TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P4Control:StripButtonP4Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P4Control:StripButtonP4Rin:VISIBLE = FALSE.
            END.
         END.
         ELSE DO:
            THIS-OBJECT:P4Control:GridImportP4:GuruBortrader(TRUE). 
            IF THIS-OBJECT:importP4TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P4Control:StripButtonP4Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P4Control:StripButtonP4Rin:VISIBLE = FALSE.
            END.
         END.               
      END.
      IF listaP = "P3" THEN DO:
         IF THIS-OBJECT:P3Control:GridImportP3F:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P3Control:GridImportP3F:GuruBortrader(TRUE).
            IF THIS-OBJECT:importP3P4TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P3Control:StripButtonP3Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P3Control:StripButtonP3Rin:VISIBLE = FALSE.
            END.
         END.
         ELSE DO:
            THIS-OBJECT:P3Control:GridImportP3:GuruBortrader(TRUE). 
            IF THIS-OBJECT:importP3TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P3Control:StripButtonP3Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P3Control:StripButtonP3Rin:VISIBLE = FALSE.
            END.
         END.                
      END.
      IF listaP = "P2" THEN DO:
         IF THIS-OBJECT:P2Control:GridImportP2F:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P2Control:GridImportP2F:GuruBortrader(TRUE).
            IF THIS-OBJECT:importP2P3TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = FALSE.
            END.
         END.
         ELSE DO:
            THIS-OBJECT:P2Control:GridImportP2:GuruBortrader(TRUE). 
            IF THIS-OBJECT:importP2TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = FALSE.
            END.
         END.               
      END.
      IF listaP = "P1" THEN DO:
         IF THIS-OBJECT:P1Control:GridImportP1F:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P1Control:GridImportP1F:GuruBortrader(TRUE).
            IF THIS-OBJECT:importP1P2TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = FALSE.
            END.
         END.
         ELSE DO:
            THIS-OBJECT:P1Control:GridImportP1:GuruBortrader(TRUE). 
            IF THIS-OBJECT:importP1TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = FALSE.
            END.
         END.                        
      END.
   END METHOD.
   METHOD PUBLIC VOID StripButtonRin_Click(INPUT listaP AS CHARACTER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      /*IMPORTANDERS*/
      IF listaP = "P5" THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:importP5TTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP5TTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ekgp5TTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
               IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
               THIS-OBJECT:ekgp5resursTTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgp5resursTTh:AVAILABLE THEN DO:               
                  /*TAR BORT ALLA TIDIGARE RESURSER*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp5resursTTh:TABLE + " WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'".
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp5resursTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp5resursTTh:BUFFER-DELETE().
                     nivandvar = "1".                          
                     q2H:GET-NEXT().         
                  END. 
                  THIS-OBJECT:ControlDataDb:BeraknaP5en().                      
               END.
            END.
            ELSE DO:  
               /*skapar ny kod*/ 
               rid = THIS-OBJECT:ControlDataDb:NyP5LopNr(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE, ekgsubidvar, ebrkatvar).
               THIS-OBJECT:ekgp5TTh:FIND-BY-ROWID(rid).
               IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
               IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.             
            END.
            qH:GET-NEXT().         
         END. 
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            /*skapar ny P5resurs*/
            THIS-OBJECT:ekgp5TTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:  
               THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
               IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ekgp5resursTTh:BUFFER-CREATE().
                  THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                  THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                  THIS-OBJECT:ControlDataDb:P5ResursUpd().
                  THIS-OBJECT:ControlDataDb:BeraknaP5en().
               END.
            END.       
            qH:GET-NEXT().         
         END.   
         importP5TTh:EMPTY-TEMP-TABLE() NO-ERROR. 
         THIS-OBJECT:P5Control:StripButtonP5Rbort:VISIBLE = FALSE. 
         THIS-OBJECT:P5Control:StripButtonP5Rin:VISIBLE = FALSE.
         THIS-OBJECT:P5Control:GridP5:GuruReopen().
         THIS-OBJECT:P5Control:GridImportP5:GuruReopen().
      END.    
      IF listaP = "P4" THEN DO:
         IF THIS-OBJECT:P4Control:GridImportP4F:VISIBLE = TRUE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP4P5TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP4P5TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*TÖM FREKVENS*/
               IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP4P5TTh:BUFFER-DELETE().
               ELSE DO:
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp4frekvensTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp4frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.                     
                  /*TÖM RESURS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgP4resursTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP4resursTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgP4resursTTh:BUFFER-DELETE().
                     nivandvar = "1".
                     q2H:GET-NEXT().         
                  END.
               END.
                 
               qH:GET-NEXT().
            END.                              
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ekgP4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE.
                  THIS-OBJECT:ekgP5TTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
                  IF THIS-OBJECT:ekgP5TTh:AVAILABLE THEN DO:
                     nivandvar = "2".
                     THIS-OBJECT:ekgp4frekvensTTh:BUFFER-CREATE().
                     THIS-OBJECT:ekgp4frekvensTTh:BUFFER-COPY(importP4P5TTh).
                     THIS-OBJECT:ekgp4frekvensTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE = "".
                     THIS-OBJECT:ControlDataDb:P4FrekvensUpd().
                     THIS-OBJECT:ControlDataDb:BeraknaP4enfrek().
                  END.   
               END.   
               qH:GET-NEXT().
            END. 
            qH:GET-FIRST().
            importP4P5TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P4Control:GridImportP4F:GuruReopen().            
         END.
         ELSE DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP4TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP4TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ekgP4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                  THIS-OBJECT:ekgP4resursTTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
                  IF THIS-OBJECT:ekgP4resursTTh:AVAILABLE THEN DO:               
                     /*TAR BORT ALLA TIDIGARE RESURSER*/
                     query2var =  "FOR EACH " + THIS-OBJECT:ekgP4resursTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:ekgP4resursTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                     q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP4resursTTh,query2var).
                     q2H:GET-FIRST().
                     DO WHILE q2H:QUERY-OFF-END = FALSE:
                        THIS-OBJECT:ekgP4resursTTh:BUFFER-DELETE().
                        nivandvar = "1".                          
                        q2H:GET-NEXT().         
                     END. 
                     THIS-OBJECT:ControlDataDb:BeraknaP4enresurs().                      
                  END.
               END.
               ELSE DO:  
                  /*skapar ny kod*/ 
                  rid = THIS-OBJECT:ControlDataDb:NyP4LopNr(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE, ekgsubidvar, ebrkatvar).
                  THIS-OBJECT:ekgP4TTh:FIND-BY-ROWID(rid).
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.             
               END.
               THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.
               qH:GET-NEXT().         
            END. 
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*skapar ny P4resurs*/
               IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  THIS-OBJECT:ekgP4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
                  IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:  
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgP4resursTTh:BUFFER-CREATE().
                        THIS-OBJECT:ekgP4resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                        THIS-OBJECT:ekgP4resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                        THIS-OBJECT:ControlDataDb:P4ResursUpd().
                        THIS-OBJECT:ControlDataDb:BeraknaP4enresurs().
                     END.
                  END. 
                  /*TÖM FREKVENS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp4frekvensTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp4frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.     
               END.
               qH:GET-NEXT().         
            END.   
            importP4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P4Control:GridImportP4:GuruReopen().
         END.
         THIS-OBJECT:P4Control:StripButtonP4Rbort:VISIBLE = FALSE. 
         THIS-OBJECT:P4Control:StripButtonP4Rin:VISIBLE = FALSE.
      END.
      IF listaP = "P3" THEN DO:
         IF THIS-OBJECT:P3Control:GridImportP3F:VISIBLE = TRUE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP3P4TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP3P4TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*TÖM FREKVENS*/
               IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP3P4TTh:BUFFER-DELETE().
               ELSE DO:
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp3frekvensTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp3frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.                     
                  /*TÖM RESURS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgP3resursTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP3resursTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = FALSE  " ), NO-LOCK) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgp3resursTTh:BUFFER-DELETE().
                        nivandvar = "1".  
                     END.                           
                     q2H:GET-NEXT().         
                  END.                  
               END.                 
               qH:GET-NEXT().
            END.                              
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
               IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE.
                  THIS-OBJECT:ekgP4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
                     nivandvar = "2".
                     THIS-OBJECT:ekgp3frekvensTTh:BUFFER-CREATE().
                     THIS-OBJECT:ekgp3frekvensTTh:BUFFER-COPY(importP3P4TTh).
                     THIS-OBJECT:ekgp3frekvensTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE = "".
                     THIS-OBJECT:ControlDataDb:P3FrekvensUpd().
                     THIS-OBJECT:ControlDataDb:BeraknaP3enfrek().
                  END.   
               END.   
               qH:GET-NEXT().
            END. 
            qH:GET-FIRST().
            importP3P4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P3Control:GridImportP3F:GuruReopen().  
            importP3P4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P3Control:GridImportP3F:GuruReopen().
         END.
         ELSE DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP3TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP3TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN importP3TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     THIS-OBJECT:ekgP3resursTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                     IF THIS-OBJECT:ekgP3resursTTh:AVAILABLE THEN DO:               
                        /*TAR BORT ALLA TIDIGARE RESURSER*/
                        query2var =  "FOR EACH " + THIS-OBJECT:ekgP3resursTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                        q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP3resursTTh,query2var).
                        q2H:GET-FIRST().
                        DO WHILE q2H:QUERY-OFF-END = FALSE:
                           ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = FALSE  " ), NO-LOCK) NO-ERROR.
                           IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                              THIS-OBJECT:ekgp3resursTTh:BUFFER-DELETE().
                              nivandvar = "1".  
                           END.                           
                           q2H:GET-NEXT().         
                        END. 
                        THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(ebrkatvar).                      
                     END.
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP3LopNr(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE, ekgsubidvar, ebrkatvar).
                     THIS-OBJECT:ekgP3TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.             
                  END.
                  THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.
                  qH:GET-NEXT().
               END.            
            END. 
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*skapar ny P3resurs*/
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' +  STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:  
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgP3resursTTh:BUFFER-CREATE().
                        THIS-OBJECT:ekgP3resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                        THIS-OBJECT:ekgP3resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                        THIS-OBJECT:ControlDataDb:P3ResursUpd().
                        THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(ebrkatvar).
                     END.
                  END. 
                  /*TÖM FREKVENS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp3frekvensTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp3frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.     
               END.
               qH:GET-NEXT().         
            END.   
            importP3TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P3Control:GridImportP3:GuruReopen().
         END.
         THIS-OBJECT:P3Control:StripButtonP3Rbort:VISIBLE = FALSE. 
         THIS-OBJECT:P3Control:StripButtonP3Rin:VISIBLE = FALSE.      
      END.    
      IF listaP = "P2" THEN DO:
         IF THIS-OBJECT:P2Control:GridImportP2F:VISIBLE = TRUE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP2P3TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2P3TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*TÖM FREKVENS*/
               IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP2P3TTh:BUFFER-DELETE().
               ELSE DO:
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp2frekvensTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp2frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.                     
                  /*TÖM RESURS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgP2resursTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2resursTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = FALSE  " ), NO-LOCK) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgp2resursTTh:BUFFER-DELETE().
                        nivandvar = "1".  
                     END.                           
                     q2H:GET-NEXT().         
                  END.                  
               END.                 
               qH:GET-NEXT().
            END.                              
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
               IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE.
                  THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                     nivandvar = "2".
                     THIS-OBJECT:ekgp2frekvensTTh:BUFFER-CREATE().
                     THIS-OBJECT:ekgp2frekvensTTh:BUFFER-COPY(importP2P3TTh).
                     THIS-OBJECT:ekgp2frekvensTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE = "".
                     THIS-OBJECT:ControlDataDb:P2FrekvensUpd().
                     THIS-OBJECT:ControlDataDb:BeraknaP2enfrek().
                  END.   
               END.   
               qH:GET-NEXT().
            END. 
            qH:GET-FIRST().
            importP2P3TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P2Control:GridImportP2F:GuruReopen().
         END.
         ELSE DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP2TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN importP2TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     THIS-OBJECT:ekgP2resursTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                     IF THIS-OBJECT:ekgP2resursTTh:AVAILABLE THEN DO:               
                        /*TAR BORT ALLA TIDIGARE RESURSER*/
                        query2var =  "FOR EACH " + THIS-OBJECT:ekgP2resursTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                        q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2resursTTh,query2var).
                        q2H:GET-FIRST().
                        DO WHILE q2H:QUERY-OFF-END = FALSE:
                           ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = FALSE  " ), NO-LOCK) NO-ERROR.
                           IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                              THIS-OBJECT:ekgp2resursTTh:BUFFER-DELETE().
                              nivandvar = "1".  
                           END.                           
                           q2H:GET-NEXT().         
                        END. 
                        THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(ebrkatvar).                      
                     END.
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP2LopNr(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE, ekgsubidvar, ebrkatvar).
                     THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.             
                  END.
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.
                  qH:GET-NEXT().
               END.            
            END. 
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*skapar ny P2resurs*/
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' +  STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:  
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgP2resursTTh:BUFFER-CREATE().
                        THIS-OBJECT:ekgP2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                        THIS-OBJECT:ekgP2resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                        THIS-OBJECT:ControlDataDb:P2ResursUpd().
                        THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(ebrkatvar).
                     END.
                  END. 
                  /*TÖM FREKVENS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp2frekvensTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp2frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.     
               END.
               qH:GET-NEXT().         
            END.   
            importP2TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P2Control:GridImportP2:GuruReopen().
         END.
         THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = FALSE. 
         THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = FALSE. 
      END.
      IF listaP = "P1" THEN DO:
         IF THIS-OBJECT:P1Control:GridImportP1F:VISIBLE = TRUE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP1P2TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP1P2TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*TÖM FREKVENS*/
               IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP1P2TTh:BUFFER-DELETE().
               ELSE DO:
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgP1frekvensTTh:TABLE + " WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP1frekvensTTh,queryvar).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgP1frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.                     
               END.                 
               qH:GET-NEXT().
            END.                              
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
               IF THIS-OBJECT:ekgP1TTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE.
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
                     nivandvar = "2".
                     THIS-OBJECT:ekgP1frekvensTTh:BUFFER-CREATE().
                     THIS-OBJECT:ekgP1frekvensTTh:BUFFER-COPY(importP1P2TTh).
                     THIS-OBJECT:ekgP1frekvensTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = "".
                     THIS-OBJECT:ControlDataDb:P1FrekvensUpd().
                     THIS-OBJECT:ControlDataDb:BeraknaP1enfrek().
                  END.   
               END.   
               qH:GET-NEXT().
            END. 
            qH:GET-FIRST().
            importP1P2TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P1Control:GridImportP1F:GuruReopen().
         END.
         ELSE DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP1TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP1TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN importP1TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP1TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP1LopNr(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE, ekgsubidvar, ebrkatvar).
                     THIS-OBJECT:ekgP1TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.             
                  END.
                  THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.
                  qH:GET-NEXT().
               END.            
            END. 
            
            importP1TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P1Control:GridImportP1:GuruReopen().

         END.
         THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = FALSE. 
         THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = FALSE.                  
      END.
   END METHOD.
   METHOD PUBLIC VOID StripButtonImport_Click(INPUT listaP AS CHARACTER):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE globanv AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den Excelfil som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P5" THEN importP5TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P4" THEN importP4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P3" THEN importP3TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P2" THEN importP2TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P1" THEN importP1TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P4F" THEN importP4P5TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P3F" THEN importP3P4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P2F" THEN importP2P3TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "P1F" THEN importP1P2TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ExcelImportLista(fildir,listaP).

   END METHOD.   
   METHOD PRIVATE VOID ExcelImportLista(filnamn AS CHARACTER,listaP AS CHARACTER):
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(86),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN THIS-OBJECT:VisaExcel:iRad = 2.
      ELSE THIS-OBJECT:VisaExcel:iRad = 1.
      Guru.Konstanter:AmericanSet().
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:embedvar = FALSE.
      
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:ReadExcelStart(INPUT filnamn).   
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF invar NE "" THEN DO:
            IF listaP = "P5" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP5TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP5TTh:RECID.
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP5TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).
            END.      
            IF listaP = "P4" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP4TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP4TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP4TTh:RECID.
                  THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = LOGICAL(TRIM(invar),"Ja/Nej").
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).               
            END.    
            IF listaP = "P3" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP3TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP3TTh:RECID.
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = LOGICAL(TRIM(invar),"Ja/Nej").
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "G" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "H" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "I" THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).              
            END.
            IF listaP = "P2" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP2TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP2TTh:RECID.
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = LOGICAL(TRIM(invar),"Ja/Nej").
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "G" THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).
            END.
            IF listaP = "P1" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP1TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP1TTh:RECID.
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = LOGICAL(TRIM(invar),"Ja/Nej").                             
            END.   
            IF listaP = "P4F" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP4P5TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP4P5TTh:RECID.
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).               
            END.
            IF listaP = "P3F" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP3P4TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP3P4TTh:RECID.
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).               
            END. 
             IF listaP = "P2F" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP2P3TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP2P3TTh:RECID.
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "G" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).               
            END.
            IF listaP = "P1F" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP1P2TTh:BUFFER-CREATE().
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP1P2TTh:RECID.
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "G" THEN THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).               
            END.         
         END.
         THIS-OBJECT:VisaExcel:ColRight().
         IF listaP = "P5" THEN IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:VisaExcel:cColname = "A".                
         IF listaP = "P4" THEN IF THIS-OBJECT:VisaExcel:cColname = "G" THEN THIS-OBJECT:VisaExcel:cColname = "A".             
         IF listaP = "P3" THEN IF THIS-OBJECT:VisaExcel:cColname = "J" THEN THIS-OBJECT:VisaExcel:cColname = "A".                
         IF listaP = "P2" THEN IF THIS-OBJECT:VisaExcel:cColname = "H" THEN THIS-OBJECT:VisaExcel:cColname = "A".             
         IF listaP = "P1" THEN IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:VisaExcel:cColname = "A".           
         IF listaP = "P4F" THEN IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:VisaExcel:cColname = "A".
         IF listaP = "P3F" THEN IF THIS-OBJECT:VisaExcel:cColname = "G" THEN THIS-OBJECT:VisaExcel:cColname = "A".
         IF listaP = "P2F" THEN IF THIS-OBJECT:VisaExcel:cColname = "H" THEN THIS-OBJECT:VisaExcel:cColname = "A".
         IF listaP = "P1F" THEN IF THIS-OBJECT:VisaExcel:cColname = "H" THEN THIS-OBJECT:VisaExcel:cColname = "A".                
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().
      END.  
      THIS-OBJECT:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      DEBUGGER:SET-BREAK().
      IF listaP = "P5" THEN THIS-OBJECT:ImportListaP5().
      IF listaP = "P4" THEN THIS-OBJECT:ImportListaP4().
      IF listaP = "P3" THEN THIS-OBJECT:ImportListaP3().
      IF listaP = "P2" THEN THIS-OBJECT:ImportListaP2().
      IF listaP = "P1" THEN THIS-OBJECT:ImportListaP1().
      
      IF listaP = "P4F" THEN THIS-OBJECT:ImportListaP4F().
      IF listaP = "P3F" THEN THIS-OBJECT:ImportListaP3F().
      IF listaP = "P2F" THEN THIS-OBJECT:ImportListaP2F().
      IF listaP = "P1F" THEN THIS-OBJECT:ImportListaP1F().
   END METHOD.
   METHOD PRIVATE VOID ImportListaP5():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP5TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP5TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgp5TTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
            THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
            THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
            THIS-OBJECT:ekgp3resursTTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "' AND RESURSNR = "  + THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:               
               THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(101).
               THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 101.                      
            END.
         END.      
         ELSE DO:                    
            THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
            THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
         END.   
         THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
         ELSE DO:
            THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
            IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
         END.
         qH:GET-NEXT().         
      END.
      qH:GET-FIRST().
      IF THIS-OBJECT:importP5TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P5Control:StripButtonP5Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P5Control:StripButtonP5Rin:VISIBLE = TRUE.
      END.
      THIS-OBJECT:P5Control:GridImportP5:GuruReopen().
             
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP4():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP4TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP4TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgP4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE NE THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE THEN DO:            
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
            END.
            
            THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
            THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
            THIS-OBJECT:ekgp3resursTTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "' AND RESURSNR = "  + THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:               
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(101).
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 101.                      
            END.
         END.      
         ELSE DO:                    
            THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
            THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
         END.   
         IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
            THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
            ELSE DO:
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
               IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
            END.
            IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
                  THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.               
               END.     
            END.   
         END.   
         qH:GET-NEXT().         
      END.
      qH:GET-FIRST().
      IF THIS-OBJECT:importP4TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P4Control:StripButtonP4Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P4Control:StripButtonP4Rin:VISIBLE = TRUE.
      END.
      THIS-OBJECT:P4Control:GridImportP4:GuruReopen().
            
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP4F():  
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP4P5TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP4P5TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgp4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
               /*har resurs*/            
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
            END.            
            ELSE DO:
               /*kod finns*/
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(109).
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 109.
            END.   
         END.
         ELSE DO:
            /*kod finns ej*/
            THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
            THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
         END.
         IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN.
         ELSE DO:
            THIS-OBJECT:ekgp5TTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
            END.
            ELSE DO:
                /*kod finns ej*/
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END.    
            THIS-OBJECT:ekgp4frekvensTTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp4frekvensTTh:AVAILABLE THEN DO:
               /*kod har redan frekv*/
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
               IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
            END. 
            IF THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
               THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
               IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
            END.
         END.        
         qH:GET-NEXT().         
      END. 
      IF THIS-OBJECT:importP4P5TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P4Control:StripButtonP4Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P4Control:StripButtonP4Rin:VISIBLE = TRUE.
      END.
      THIS-OBJECT:P4Control:GridImportP4F:GuruReopen().
            
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP3():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP3TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP3TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:AVAILABLE THEN DO:
            /*arbetskod finns*/
             
         END.
         ELSE DO:
            IF ControlDataDb:FinnsArbkodImport("P3",THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) = TRUE THEN DO:
               /*ny arbetskod*/
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99. 
            END.   
            ELSE DO:
               /*kod kommer inte att läsasd in*/
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110. 
            END.   
         END.   
         IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN.
         ELSE DO:
            THIS-OBJECT:ekgp3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
               /*byte frekvens resurs*/
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE NE THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE THEN DO:            
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
               END.
               /*ersätter befintlig*/
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
               THIS-OBJECT:ekgp3resursTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE) + "' AND RESURSNR = "  + THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
               IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:
                  /*resursbort */           
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(101).
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 101.                      
               END.
            END.      
            ELSE DO:        
               /*ny kod*/            
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
            END.   
            IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
               THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
               IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
               ELSE DO:
                  /*ogiltig resurs*/
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                  IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
               END.
               IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
                     THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.               
                  END.     
               END. 
            END.
         END.       
         qH:GET-NEXT().         
      END.
      qH:GET-FIRST().
      IF THIS-OBJECT:importP3TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P3Control:StripButtonP3Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P3Control:StripButtonP3Rin:VISIBLE = TRUE.
      END.
      THIS-OBJECT:P3Control:GridImportP3:GuruReopen().
            
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP3F():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP3P4TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP3P4TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgp3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
         IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
               /*har resurs*/            
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
            END.            
            ELSE DO:
               /*kod finns*/
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(109).
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 109.
            END.
         END.
         ELSE DO:
            /*kod finns ej*/
            THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
            THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
         END.  
         IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN.
         ELSE DO:
            THIS-OBJECT:ekgp4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
            END.
            ELSE DO:
                /*kod finns ej*/
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END.   
            
            THIS-OBJECT:ekgp3frekvensTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp3frekvensTTh:AVAILABLE THEN DO:
               /*kod har redan frekv*/
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
               IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
            END. 
            IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
               THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
               IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
            END.     
         END.
         qH:GET-NEXT().         
      END. 

      IF THIS-OBJECT:importP3P4TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P3Control:StripButtonP3Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P3Control:StripButtonP3Rin:VISIBLE = TRUE.
      END. 
      THIS-OBJECT:P3Control:GridImportP3F:GuruReopen().
            
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP2():  
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP2TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgp2arbkodTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ekgp2arbkodTTh:AVAILABLE THEN DO:
            /*arbetskod finns*/
             
         END.
         ELSE DO:
            IF ControlDataDb:FinnsArbkodImport("P2",THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) = TRUE THEN DO:
               /*ny arbetskod*/
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99. 
            END.   
            ELSE DO:
               /*kod kommer inte att läsasd in*/
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110. 
            END.   
         END.   
         IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN.
         ELSE DO:
            THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
               /*byte frekvens resurs*/
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE NE THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE THEN DO:            
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
               END.
               /*ersätter befintlig*/
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
               THIS-OBJECT:ekgp2resursTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE) + "' AND RESURSNR = "  + THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
               IF THIS-OBJECT:ekgp2resursTTh:AVAILABLE THEN DO:
                  /*resursbort */           
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(101).
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 101.                      
               END.
            END.      
            ELSE DO:        
               /*ny kod*/            
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
            END.   
            IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
               THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
               IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
               ELSE DO:
                  /*ogiltig resurs*/
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                  IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
               END.
               IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
                     THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.               
                  END.     
               END. 
            END.              
         END.       
         qH:GET-NEXT().         
      END.
      qH:GET-FIRST().
      IF THIS-OBJECT:importP2TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = TRUE.
      END.
      THIS-OBJECT:P2Control:GridImportP2:GuruReopen().                  
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP2F():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP2P3TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2P3TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
               /*har resurs*/            
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
            END.            
            ELSE DO:
               /*kod finns*/
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(109).
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 109.
            END.
         END.
         ELSE DO:
            /*kod finns ej*/
            THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
            THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
         END.  
         IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN.
         ELSE DO:
            THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
            END.
            ELSE DO:
                /*kod finns ej*/
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END.   
            
            THIS-OBJECT:ekgP2frekvensTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgP2frekvensTTh:AVAILABLE THEN DO:
               /*kod har redan frekv*/
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
               IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
            END. 
            IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
               THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
               IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
            END.     
         END.
         qH:GET-NEXT().         
      END. 

      IF THIS-OBJECT:importP2P3TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = TRUE.
      END. 
      THIS-OBJECT:P2Control:GridImportP2F:GuruReopen().
            

            
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP1():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP1TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP1TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgP1arbkodTTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ekgP1arbkodTTh:AVAILABLE THEN DO:
            /*arbetskod finns*/
             
         END.
         ELSE DO:
            IF ControlDataDb:FinnsArbkodImport("P1",THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) = TRUE THEN DO:
               /*ny arbetskod*/
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99. 
            END.   
            ELSE DO:
               /*kod kommer inte att läsasd in*/
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110. 
            END.   
         END.   
         IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN.
         ELSE DO:
            THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:ekgP1TTh:AVAILABLE THEN DO:
               /*ersätter befintlig*/
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.               
            END.      
            ELSE DO:        
               /*ny kod*/            
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
               THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
            END.                            
         END.       
         qH:GET-NEXT().         
      END.
      qH:GET-FIRST().
      IF THIS-OBJECT:importP1TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = TRUE.
      END.
      THIS-OBJECT:P1Control:GridImportP1:GuruReopen().           
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP1F():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP1P2TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP1P2TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
         IF THIS-OBJECT:ekgP1TTh:AVAILABLE THEN DO:
            /*kod finns*/
            THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(109).
            THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 109.
         END.
         ELSE DO:
            /*kod finns ej*/
            THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
            THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
         END.  
         IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN.
         ELSE DO:
            THIS-OBJECT:ControlDataDb:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
            END.
            ELSE DO:
                /*kod finns ej*/
               THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
               THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END.   
            
            THIS-OBJECT:ekgP1frekvensTTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgP1frekvensTTh:AVAILABLE THEN DO:
               /*kod har redan frekv*/
               THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
               IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
            END. 
            IF THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
               THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
               IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
               ELSE THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
            END.     
         END.
         qH:GET-NEXT().         
      END. 

      IF THIS-OBJECT:importP1P2TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = TRUE.
      END. 
      THIS-OBJECT:P1Control:GridImportP1F:GuruReopen().  

            
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheckImport(INPUT imgrid AS Controls.Grid):
      
      imgrid:GuruGridFarg(TRUE).
      imgrid:GuruAddConditionalColor("INNR", 0, "98","MEDDELANDE", Guru.StaticMethods:DotNetColor(14)).  /*gul*/
      imgrid:GuruAddConditionalColor("INNR", 0, "99","MEDDELANDE", Guru.StaticMethods:DotNetColor(2)).  /*grön*/
      imgrid:GuruAddConditionalColor("INNR", 0, "100","MEDDELANDE", Guru.StaticMethods:DotNetColor(13)). /*rosa*/ 
      imgrid:GuruAddConditionalColor("INNR", 0, "101","MEDDELANDE", Guru.StaticMethods:DotNetColor(8)).  /*grå*/
      imgrid:GuruAddConditionalColor("INNR", 0, "106","MEDDELANDE", Guru.StaticMethods:DotNetColor(12)).  /*röd*/
      imgrid:GuruAddConditionalColor("INNR", 0, "107","MEDDELANDE", Guru.StaticMethods:DotNetColor(13)).  
      imgrid:GuruAddConditionalColor("INNR", 0, "108","MEDDELANDE", Guru.StaticMethods:DotNetColor(8)).  
      imgrid:GuruAddConditionalColor("INNR", 0, "109","MEDDELANDE", Guru.StaticMethods:DotNetColor(2)).
      imgrid:GuruAddConditionalColor("INNR", 0, "110","MEDDELANDE", Guru.StaticMethods:DotNetColor(12)).
      imgrid:GuruAddConditionalColor("INNR", 0, "111","MEDDELANDE", Guru.StaticMethods:DotNetColor(3)). /*turkos*/
      imgrid:GuruAddConditionalColor("INNR", 0, "112","MEDDELANDE", Guru.StaticMethods:DotNetColor(12)).  /*röd*/
      
       
      /*
      RESURSER
      
      IF sokidvar = 98 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod finns i Katalogen och kommer att ersätta befintlig kod.","").
      IF sokidvar = 99 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny kod! Läggs till i Katalogen.","").
      IF sokidvar = 100 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod har ogiltig resurs. Kommer att läsas in utan resurser.","").
      IF sokidvar = 101 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod finns i Katalogen och kommer att ersätta befintlig kod. Finns resurser på denna kod kommer de att ersättas med dessa resurser.","").
      IF sokidvar = 110 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Arbetskod finns inte! Posten kommer inte att läsas in.","").
      
      Frekvenser
      IF sokidvar = 106 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koderna för denna frekvens finns inte i katlogen. Frekvens kommer inte att läsas in!","").
      IF sokidvar = 107 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. Frekvens upplägget kommer att ersättas!","").
      IF sokidvar = 108 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har sedan tidigare ett resursupplägg. Detta kommer att ersättas med denna frekvens.","").     
      IF sokidvar = 109 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden kommer att få ett frekvens upplägg.","").
      
      IF sokidvar = 111 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Byte mellan resurs och frekvens kommer att ske.","").
      IF sokidvar = 112 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod har resurs. Inget kommer att läsas in.","").
      */
       
   END METHOD.
   METHOD PUBLIC LOGICAL ResursUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgresursTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "12".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC LOGICAL RegelUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgregelTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgregelTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgregelTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgregelTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "11".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC LOGICAL PaslagUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgpaslagTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         IF kolnamn = "PASLAG" THEN DO:
            IF kolvarde = STRING(0) THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(118),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "No" THEN RETURN FALSE.
            END.
                       
            IF THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD("PASLAGNR"):BUFFER-VALUE = "5" THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(125),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.               
                  THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.               
                  THIS-OBJECT:ControlDataDb:Beraknapaslag().
                  THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD("PASLAG"):BUFFER-VALUE = 0.
                  THIS-OBJECT:ControlShell:GridPaslag:GuruReopen(). 
                  nivandvar = "11".
                  raknavar = "11".
                  RETURN TRUE.
               END.
               ELSE RETURN FALSE.
            END.
            ELSE DO:     
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(117),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.               
                  THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.               
                  THIS-OBJECT:ControlDataDb:Beraknapaslag().
                  nivandvar = "11".
                  raknavar = "11".
                  RETURN TRUE.
               END.
               ELSE RETURN FALSE.
            END.                           
         END.
         ELSE DO:                  
            THIS-OBJECT:ekgpaslagTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            nivandvar = "11".
            RETURN TRUE.
         END.   
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   
   METHOD PUBLIC LOGICAL ResursNivaUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgresursnivaTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "10".         
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC LOGICAL ResursPaslagUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgresurspaslagTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgresurspaslagTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgresurspaslagTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgresurspaslagTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "10".
         raknavar = "10".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC LOGICAL ResursPrisUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgresursprisTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.         
         IF THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("REGELID"):BUFFER-VALUE = "5" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(97),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               dontrepo = TRUE.
               THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               THIS-OBJECT:ControlDataDb:BeraknaRP().
            END.        
            ELSE RETURN FALSE.    
         END.
         ELSE DO:
            dontrepo = TRUE.
            THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            THIS-OBJECT:ControlDataDb:BeraknaRP().
         END.
         THIS-OBJECT:ControlShell:GridResursPris:GuruReopen().
         nivandvar = "10".
         IF kolnamn = "PRIS" OR kolnamn = "REGELID" THEN DO:
            raknavar = "10".
         END.   
         dontrepo = FALSE.
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   
   METHOD PUBLIC LOGICAL UppdateraMtrlKost():
      /*bör inte behöva användas -allt uppdateras direkt*/   
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.      
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(113),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN DO:           
         THIS-OBJECT:ControlDataDb:Beraknamtrlkostalla().
      END.        
      ELSE RETURN FALSE.          
      THIS-OBJECT:ControlShell:GridMateriel:GuruReopen().
      nivandvar = "10".
      raknavar = "10".
      dontrepo = FALSE.
      RETURN TRUE.                    
   END METHOD.
   
   /*METHOD PUBLIC LOGICAL UppdateraMtrlKostlasta():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.      
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(114),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN DO:           
         THIS-OBJECT:ControlDataDb:Beraknamaterielkostlasta().
      END.        
      ELSE RETURN FALSE.          
      THIS-OBJECT:ControlShell:GridMateriel:GuruReopen().
      nivandvar = "10".
      dontrepo = FALSE.
      RETURN TRUE.                    
   END METHOD.*/
   METHOD PUBLIC LOGICAL MaterielUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE datvar AS DATE NO-UNDO.
      DEFINE VARIABLE tidvar AS INTEGER NO-UNDO.
      IF THIS-OBJECT:ekgmtrlTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.         
         IF kolnamn = "KALKYLPRIS" THEN DO:          
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(116),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               dontrepo = TRUE.               
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("LASTA"):BUFFER-VALUE = TRUE.               
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("UPPDATDATUM"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).                  
               THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().
            END.        
            ELSE RETURN FALSE.    
         END.
         ELSE IF kolnamn = "LASTA" THEN DO:            
            IF THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("LASTA"):BUFFER-VALUE = TRUE THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(121),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.               
                  THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.         
                  THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("UPPDATDATUM"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).
                  THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().
                  nivandvar = "11".
                  raknavar = "11".
               END.        
               ELSE RETURN FALSE.
            END.
            ELSE IF THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("LASTA"):BUFFER-VALUE = FALSE THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(122),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.               
                  THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.         
                  /*THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().*/
               END.        
               ELSE RETURN FALSE.
            END.   
         END.
         ELSE IF kolnamn = "PRIS" THEN DO:            
            THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().
            THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("UPPDATDATUM"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).
            nivandvar = "11".
            raknavar = "11".
            RETURN TRUE.
         END.   
         ELSE DO:
            /*BENÄMNING*/
            THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.            
            nivandvar = "11".       
            RETURN TRUE.
         END.   
            
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC LOGICAL P5Uppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp5TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgp5TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "4". 
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC VOID P5ResursUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp5resursTTh:AVAILABLE THEN DO:
         dontrepo = TRUE. 
         IF STRING(THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp5resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         DEBUGGER:SET-BREAK().
         THIS-OBJECT:ControlDataDb:LaddaP5VFresursSingel().
         THIS-OBJECT:ControlDataDb:BeraknaP5en().
         THIS-OBJECT:P5Control:GridP5:GuruReopen().
         dontrepo = FALSE.
         nivandvar = "1".         
      END.                     
   END METHOD.
   METHOD PUBLIC LOGICAL P4Uppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
         DEBUGGER:SET-BREAK().
         IF STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         IF kolnamn = "FREKVENS" THEN DO:  
            IF LOGICAL(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE) = FALSE THEN DO:            
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(89),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  /*lena*/
                  IF THIS-OBJECT:ekgp4resursTTh:AVAILABLE THEN DO:                     
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp4resursTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4resursTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:
                        THIS-OBJECT:ekgp4resursTTh:BUFFER-DELETE().
                        nivandvar = "1".                          
                        qH:GET-NEXT().         
                     END.                     
                  END.                                       
                  THIS-OBJECT:P4Control:GridP4Resurs:GuruReopen().
                  THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE").
                  THIS-OBJECT:ControlDataDb:BeraknaP4enfrek().                  
                  THIS-OBJECT:P4Control:GridP4:GuruReopen().
                  dontrepo = FALSE.                   
               END. 
               ELSE RETURN FALSE.           
            END.
            ELSE DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(90),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  IF THIS-OBJECT:ekgp4frekvensTTh:AVAILABLE THEN DO:
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp4frekvensTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp4frekvensTTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4frekvensTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:
                        THIS-OBJECT:ekgp4frekvensTTh:BUFFER-DELETE().
                        nivandvar = "2".
                        qH:GET-NEXT().         
                     END.                     
                  END.                     
                  THIS-OBJECT:P4Control:GridP4Frekvens:GuruReopen().
                  THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera("").
                  THIS-OBJECT:ControlDataDb:BeraknaP4enresurs().
                  THIS-OBJECT:P4Control:GridP4:GuruReopen().
                  dontrepo = FALSE.                    
               END.
               ELSE RETURN FALSE.            
            END.
         END.
         ELSE DO: 
            THIS-OBJECT:ekgp4TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            nivandvar = "4".
         END.   
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   METHOD PUBLIC VOID P4ResursUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp4resursTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp4resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.         
         THIS-OBJECT:ControlDataDb:LaddaP4VFresursSingel().
         THIS-OBJECT:ControlDataDb:BeraknaP4enresurs().
         THIS-OBJECT:P4Control:GridP4:GuruReopen().
         dontrepo = FALSE.        
         nivandvar = "1".         
      END.                     
   END METHOD.
   METHOD PUBLIC VOID P4FrekvensUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp4frekvensTTh:AVAILABLE THEN DO:
         DEBUGGER:SET-BREAK().
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp4frekvensTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp4frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp4frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         /*THIS-OBJECT:ControlDataDb:LaddaP4VFfrekSingel().*/
         THIS-OBJECT:ControlDataDb:LaddaP4VFfrekenarb().         
         THIS-OBJECT:ControlDataDb:BeraknaP4enfrek().
         THIS-OBJECT:P4Control:GridP4:GuruReopen().
         dontrepo = FALSE.
         nivandvar = "2".         
      END.                     
   END METHOD.
   
   METHOD PUBLIC LOGICAL P3ArbKodUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      IF THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "4".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   
   METHOD PUBLIC LOGICAL P3Uppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp3TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         DEBUGGER:SET-BREAK().
         IF kolnamn = "FREKVENS" THEN DO:  
            IF LOGICAL(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE) = FALSE THEN DO:            
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(89),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgp3TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp3resursTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)  + "' AND P3LOPNR = " + STRING(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3resursTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:                        
                        ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = FALSE  " ), NO-LOCK) NO-ERROR.
                        IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                           THIS-OBJECT:ekgp3resursTTh:BUFFER-DELETE().
                           nivandvar = "1".  
                        END.   
                        qH:GET-NEXT().         
                     END.
                  END.                      
                  THIS-OBJECT:P3Control:GridP3Resurs:GuruReopen().
                  THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE").
                  THIS-OBJECT:ControlDataDb:BeraknaP3enfrek().
                  /*MESSAGE "ANDERS SPARA UNDAN TTRECID OCH REPO WALLMAN FUNKAR INTE!!!!!!"
                  VIEW-AS ALERT-BOX.*/
                  THIS-OBJECT:P3Control:GridP3:GuruReopen().
                  
                  dontrepo = FALSE.
               END. 
               ELSE RETURN FALSE.           
            END.
            ELSE DO:

               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(93),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgp3TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  IF THIS-OBJECT:ekgp3frekvensTTh:AVAILABLE THEN DO:
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp3frekvensTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp3frekvensTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "' AND P3LOPNR = " + STRING(THIS-OBJECT:ekgp3frekvensTTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3frekvensTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:
                        THIS-OBJECT:ekgp3frekvensTTh:BUFFER-DELETE().
                        nivandvar = "2".
                        qH:GET-NEXT().         
                     END.
                  END.   
                  THIS-OBJECT:P3Control:GridP3Frekvens:GuruReopen().                  
                  IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:
                     /*Tillkommande beredning måste bort om det inte längre är frekvens . */
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp3resursTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)
                      + "' AND P3LOPNR = " + STRING(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE  + " AND RESURSNR = 101 ").
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3resursTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:                        
                           THIS-OBJECT:ekgp3resursTTh:BUFFER-DELETE().                           
                           
                        qH:GET-NEXT().         
                     END.
                  END.   
                  THIS-OBJECT:P3Control:GridP3Resurs:GuruReopen().                                    
                  THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera("").                  
                  THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(ebrkatvar).
                  THIS-OBJECT:P3Control:GridP3:GuruReopen().
                  
                  dontrepo = FALSE.                    
               END.  
               ELSE RETURN FALSE.          
            END.
         END.
         ELSE DO:
            THIS-OBJECT:ekgp3TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            nivandvar = "4".
         END.   
         RETURN TRUE.
      END. 
      ELSE RETURN FALSE.               
   END METHOD.
   METHOD PUBLIC VOID P3ResursUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp3resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:ControlDataDb:LaddaP3VFresursSingel(ebrkatvar).         
         IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(ebrkatvar).
         ELSE THIS-OBJECT:ControlDataDb:BeraknaP3enfrek().
         THIS-OBJECT:P3Control:GridP3:GuruReopen().
         dontrepo = FALSE.        
         nivandvar = "1".         
                  
      END.                     
   END METHOD.
   METHOD PUBLIC VOID P3FrekvensUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp3frekvensTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp3frekvensTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp3frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp3frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         /*THIS-OBJECT:ControlDataDb:LaddaP3VFfrekSingel().*/
         THIS-OBJECT:ControlDataDb:LaddaP3VFfrekenarb().
         THIS-OBJECT:ControlDataDb:BeraknaP3enfrek().
         THIS-OBJECT:P3Control:GridP3:GuruReopen().
         dontrepo = FALSE.
         nivandvar = "2".                 
      END.                     
   END METHOD.
   
   METHOD PUBLIC LOGICAL P2ArbKodUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      IF THIS-OBJECT:ekgp2arbkodTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp2arbkodTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp2arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgp2arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "4".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   
   METHOD PUBLIC LOGICAL P2Uppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
        
         IF kolnamn = "FREKVENS" THEN DO:  
            IF LOGICAL(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE) = FALSE THEN DO:            
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(89),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  IF THIS-OBJECT:ekgp2resursTTh:AVAILABLE THEN DO:
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp2resursTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)  + "' AND P2LOPNR = " + STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2resursTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:
                        ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = FALSE " ), NO-LOCK) NO-ERROR.
                        IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                           THIS-OBJECT:ekgp2resursTTh:BUFFER-DELETE().
                           nivandvar = "1".
                        END.   
                        qH:GET-NEXT().         
                     END.
                  END.                      
                  THIS-OBJECT:P2Control:GridP2Resurs:GuruReopen().
                  THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE").                  
                  THIS-OBJECT:ControlDataDb:BeraknaP2enfrek().                  
                  THIS-OBJECT:P2Control:GridP2:GuruReopen().
                  
                  dontrepo = FALSE. 
               END. 
               ELSE RETURN FALSE.           
            END.
            ELSE DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(93),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  IF THIS-OBJECT:ekgp2frekvensTTh:AVAILABLE THEN DO:
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp2frekvensTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp2frekvensTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "' AND P2LOPNR = " + STRING(THIS-OBJECT:ekgp2frekvensTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).                                                                                                                                                       
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2frekvensTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:
                        THIS-OBJECT:ekgp2frekvensTTh:BUFFER-DELETE().
                        nivandvar = "2".
                        qH:GET-NEXT().         
                     END.
                  END.   
                  THIS-OBJECT:P2Control:GridP2Frekvens:GuruReopen().                  
                  IF THIS-OBJECT:ekgp2resursTTh:AVAILABLE THEN DO:
                     /*Tillkommande beredning måste bort om det inte längre är frekvens . */
                     queryvar =  "FOR EACH " + THIS-OBJECT:ekgp2resursTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)
                      + "' AND P2LOPNR = " + STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE  + " AND RESURSNR = 101 ").
                     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2resursTTh,queryvar).
                     qH:GET-FIRST().
                     DO WHILE qH:QUERY-OFF-END = FALSE:                        
                           THIS-OBJECT:ekgp2resursTTh:BUFFER-DELETE().                                                      
                        qH:GET-NEXT().         
                     END.
                  END.   
                  THIS-OBJECT:P2Control:GridP2Resurs:GuruReopen().                                    
                  THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera("").
                                    
                  THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(ebrkatvar).
                  THIS-OBJECT:P2Control:GridP2:GuruReopen().
                    
               END.  
               ELSE RETURN FALSE.          
            END.
         END.
         ELSE DO:
            THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            nivandvar = "4".
         END.   
         RETURN TRUE.
      END. 
      ELSE RETURN FALSE.               
   END METHOD.
   METHOD PUBLIC VOID P2ResursUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp2resursTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:ControlDataDb:LaddaP2VFresursSingel(ebrkatvar).
         IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(ebrkatvar).
         ELSE THIS-OBJECT:ControlDataDb:BeraknaP2enfrek().
         THIS-OBJECT:P2Control:GridP2:GuruReopen().
         dontrepo = FALSE.
         nivandvar = "1".         
      END.                     
   END METHOD.
   METHOD PUBLIC VOID P2FrekvensUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp2frekvensTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
        
         IF STRING(THIS-OBJECT:ekgp2frekvensTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp2frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp2frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         /*THIS-OBJECT:ControlDataDb:LaddaP2VFfrekSingel().*/
         THIS-OBJECT:ControlDataDb:LaddaP2VFfrekenarb().
         THIS-OBJECT:ControlDataDb:BeraknaP2enfrek().
         THIS-OBJECT:P2Control:GridP2:GuruReopen().
         dontrepo = FALSE.
         nivandvar = "2".         
      END.                     
   END METHOD.
   METHOD PUBLIC VOID P2MtrlUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp2mtrlTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp2mtrlTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp2mtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp2mtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.         
         THIS-OBJECT:ControlDataDb:LaddaP2VFmtrlSingel().
         THIS-OBJECT:ControlDataDb:BeraknaP2mtrl().
         THIS-OBJECT:P2Control:GridP2:GuruReopen().
         dontrepo = FALSE.         
         nivandvar = "3".
                 
      END.                     
   END METHOD.
   
   METHOD PUBLIC LOGICAL P1ArbKodUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      IF THIS-OBJECT:ekgp1arbkodTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "4".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   
   METHOD PUBLIC LOGICAL P1Uppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp1TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgp1TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         nivandvar = "4".
         RETURN TRUE.
      END.  
      ELSE RETURN FALSE.              
   END METHOD.
   
   METHOD PUBLIC VOID P1FrekvensUppdate(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):      
      IF THIS-OBJECT:ekgp1frekvensTTh:AVAILABLE THEN DO:
         dontrepo = TRUE.
         IF STRING(THIS-OBJECT:ekgp1frekvensTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp1frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:ekgp1frekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         /*THIS-OBJECT:ControlDataDb:LaddaP1VFfrekSingel().*/
         THIS-OBJECT:ControlDataDb:LaddaP1VFfrekenarb().
         THIS-OBJECT:ControlDataDb:BeraknaP1enfrek().
         THIS-OBJECT:P1Control:GridP1:GuruReopen().
         dontrepo = FALSE.
         nivandvar = "2".         
      END.                     
   END METHOD.
   METHOD PUBLIC VOID FrekvensResursKoll(INPUT nivakoll AS CHARACTER):
     IF nivakoll = "P4" THEN DO: 
        IF THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO: 
           IF THIS-OBJECT:P4Control:TabManagerP4:Tabs["P4Resurs"]:Selected = TRUE THEN THIS-OBJECT:P4Control:TabManagerP4:Tabs["P4Frekvens"]:Selected = TRUE.
           THIS-OBJECT:P4Control:TabManagerP4:Tabs["P4Frekvens"]:Enabled = TRUE.
        END.
        ELSE DO:
           IF THIS-OBJECT:P4Control:TabManagerP4:Tabs["P4Frekvens"]:Selected = TRUE THEN THIS-OBJECT:P4Control:TabManagerP4:Tabs["P4Resurs"]:Selected = TRUE.
           THIS-OBJECT:P4Control:TabManagerP4:Tabs["P4Frekvens"]:Enabled = FALSE.
        END.   
     END.
     ELSE IF nivakoll = "P3" THEN DO:
        IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO: 
           IF THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Resurs"]:Selected = TRUE THEN THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:Selected = TRUE.
           THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:Enabled = TRUE.
        END.
        ELSE DO:
           IF THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:Selected = TRUE THEN THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Resurs"]:Selected = TRUE.
           THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:Enabled = FALSE.
        END. 
     END.
     ELSE IF nivakoll = "P2" THEN DO:
        IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO: 
           IF THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Resurs"]:Selected = TRUE THEN THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Selected = TRUE.
           THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Enabled = TRUE.
        END.
        ELSE DO:
           IF THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Selected = TRUE THEN THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Resurs"]:Selected = TRUE.
           THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Enabled = FALSE.
        END. 
     END.    
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeResursPris(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
      IF THIS-OBJECT:ekgresursprisTTh:AVAILABLE THEN DO:
         ekgresursnivaTTh:FIND-FIRST("WHERE ekgresursnivaTT.RESURSNR = '" + THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ekgresursnivaTTh:AVAILABLE THEN DO:
            THIS-OBJECT:ControlShell:GridResursNiva:GuruRadvaljare(THIS-OBJECT:ekgresursnivaTTh:ROWID, TRUE).
         END.
      END.     
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP5 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
        THIS-OBJECT:P5Control:GridP5Resurs:GuruFiltrera("ekgp5resursTT.P5ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE)+ "' AND ekgp5resursTT.P5LOPNR = " +  STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P5Control:GridP5Resurs:GuruFirstrow(). 
     END.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
     /*Behövs se över*/
        THIS-OBJECT:P5Control:GridP5Resurs:GuruFiltrera(" ").
        ekgp5resursTTh:FIND-FIRST("WHERE ekgp5resursTT.P5ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE)+ "' AND ekgp5resursTT.P5LOPNR = " + STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp5resursTTh:AVAILABLE THEN DO:
         THIS-OBJECT:P5Control:GridP5Resurs:GuruRadvaljare(THIS-OBJECT:ekgp5resursTTh:ROWID, TRUE).
        END.
         
     END.   
     /*Ribbon*/
     /*
     THIS-OBJECT:RibbonToolAktuellKod:Text = STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE).
     */
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP4 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:  
        THIS-OBJECT:P4Control:GridP4Resurs:GuruFiltrera("ekgp4resursTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4resursTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P4Control:GridP4Resurs:GuruFirstrow().
        THIS-OBJECT:P4Control:GridP4Frekvens:GuruFiltrera("ekgp4frekvensTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4frekvensTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P4Control:GridP4Frekvens:GuruFirstrow().    
        IF THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
           THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE"). 
        END.       
        ELSE THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera("").
                
     END.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
        /*Behövs se över*/
        THIS-OBJECT:P4Control:GridP4Resurs:GuruFiltrera(" ").
        ekgp4resursTTh:FIND-FIRST("WHERE ekgp4resursTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4resursTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp4resursTTh:AVAILABLE THEN DO:
         THIS-OBJECT:P4Control:GridP4Resurs:GuruRadvaljare(THIS-OBJECT:ekgp4resursTTh:ROWID, TRUE).
        END. 
        THIS-OBJECT:P4Control:GridP4Frekvens:GuruFiltrera(" ").
        ekgp4frekvensTTh:FIND-FIRST("WHERE ekgp4frekvensTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4frekvensTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp4frekvensTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P4Control:GridP4Frekvens:GuruRadvaljare(THIS-OBJECT:ekgp4frekvensTTh:ROWID, TRUE).
        END. 
     END.  
     /*Ribbon*/
     /*
     THIS-OBJECT:RibbonToolAktuellKod:Text = STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE).
     */
     THIS-OBJECT:FrekvensResursKoll("P4").
     
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP3ArbKod(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
        ekgp3TTh:FIND-FIRST("WHERE ekgp3TT.P3ARBKOD = '" + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
           THIS-OBJECT:P3Control:GridP3:GuruRadvaljare(THIS-OBJECT:ekgp3TTh:ROWID, TRUE).
        END.  
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP3 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
        IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:                 
           THIS-OBJECT:P3Control:GridP3Resurs:GuruFiltrera("ekgp3resursTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3resursTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
           THIS-OBJECT:P3Control:GridP3Resurs:GuruFirstrow().
           THIS-OBJECT:P3Control:GridP3Frekvens:GuruFiltrera("ekgp3frekvensTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3frekvensTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
           THIS-OBJECT:P3Control:GridP3Frekvens:GuruFirstrow().
           IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
             THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE"). 
           END.       
           ELSE THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera("ekgresursForTT.REGELID NE " + STRING(7)).   
        END.
      END.
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
              /*Behövs se över*/
              THIS-OBJECT:P3Control:GridP3Resurs:GuruFiltrera(" ").
              ekgp3resursTTh:FIND-FIRST("WHERE ekgp3resursTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3resursTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
              IF THIS-OBJECT:ekgp3resursTTh:AVAILABLE THEN DO:
                 THIS-OBJECT:P3Control:GridP3Resurs:GuruRadvaljare(THIS-OBJECT:ekgp3resursTTh:ROWID, TRUE).
              END. 
              THIS-OBJECT:P3Control:GridP3Frekvens:GuruFiltrera(" ").
              ekgp3frekvensTTh:FIND-FIRST("WHERE ekgp3frekvensTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3frekvensTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
              IF THIS-OBJECT:ekgp3frekvensTTh:AVAILABLE THEN DO:
                 THIS-OBJECT:P3Control:GridP3Frekvens:GuruRadvaljare(THIS-OBJECT:ekgp3frekvensTTh:ROWID, TRUE).
              END.      
     END.
     /*Ribbon*/
     /*
     THIS-OBJECT:RibbonToolAktuellKod:Text = STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE).
     */
     THIS-OBJECT:FrekvensResursKoll("P3").
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP2ArbKod(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
      ekgp2TTh:FIND-FIRST("WHERE ekgp2TT.P2ARBKOD = '" + THIS-OBJECT:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P2Control:GridP2:GuruRadvaljare(THIS-OBJECT:ControlDataDb:ekgp2TTh:ROWID, TRUE).
      END.  
   END METHOD.
    
   METHOD PRIVATE VOID ValueChangeP2 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
     IF dontrepo = TRUE THEN RETURN.  
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:                 
        THIS-OBJECT:P2Control:GridP2Resurs:GuruFiltrera("ekgp2resursTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2resursTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:GridP2Resurs:GuruFirstrow().
        THIS-OBJECT:P2Control:GridP2Frekvens:GuruFiltrera("ekgp2frekvensTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2frekvensTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:GridP2Frekvens:GuruFirstrow(). 
        
        THIS-OBJECT:P2Control:GridP2Mtrl:GuruFiltrera("ekgp2mtrlTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2mtrlTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:GridP2Mtrl:GuruFirstrow().
        IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
           THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE"). 
        END.       
        ELSE DO:
           THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera("ekgresursForTT.REGELID NE " + STRING(7)).
        END.      
     END.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
        THIS-OBJECT:P2Control:GridP2Resurs:GuruFiltrera(" ").
        ekgp2resursTTh:FIND-FIRST("WHERE ekgp2resursTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2resursTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp2resursTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P2Control:GridP2Resurs:GuruRadvaljare(THIS-OBJECT:ekgp2resursTTh:ROWID, TRUE).
        END. 
        
        THIS-OBJECT:P2Control:GridP2Frekvens:GuruFiltrera(" ").
        ekgp2frekvensTTh:FIND-FIRST("WHERE ekgp2frekvensTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2frekvensTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp2frekvensTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P2Control:GridP2Frekvens:GuruRadvaljare(THIS-OBJECT:ekgp2frekvensTTh:ROWID, TRUE).
        END. 
         
        THIS-OBJECT:P2Control:GridP2Mtrl:GuruFiltrera(" ").
        ekgp2mtrlTTh:FIND-FIRST("WHERE ekgp2mtrlTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2mtrlTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp2mtrlTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P2Control:GridP2Mtrl:GuruRadvaljare(THIS-OBJECT:ekgp2mtrlTTh:ROWID, TRUE).
        END. 
     END.  
     
     /*Ribbon*/
     /*
     THIS-OBJECT:RibbonToolAktuellKod:Text = STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
     */
     THIS-OBJECT:FrekvensResursKoll("P2").
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP1ArbKod(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
        ekgp1TTh:FIND-FIRST("WHERE ekgp1TT.P1ARBKOD = '" + THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN DO:
           THIS-OBJECT:P1Control:GridP1:GuruRadvaljare(THIS-OBJECT:ekgp1TTh:ROWID, TRUE).
        END.  
   END METHOD.
   
   METHOD PRIVATE VOID ValueChangeP1 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
     IF dontrepo = TRUE THEN RETURN.  
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
        THIS-OBJECT:P1Control:GridP1Frekvens:GuruFiltrera("ekgp1frekvensTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "' AND ekgp1frekvensTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P1Control:GridP1Frekvens:GuruFirstrow().
     END.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
        THIS-OBJECT:P1Control:GridP1Frekvens:GuruFiltrera(" ").
        ekgp1frekvensTTh:FIND-FIRST("WHERE ekgp1frekvensTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1frekvensTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp1frekvensTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P1Control:GridP1Frekvens:GuruRadvaljare(THIS-OBJECT:ekgp1frekvensTTh:ROWID, TRUE).
        END.  
     END. 
     
     /*Ribbon*/
     /*
     THIS-OBJECT:RibbonToolAktuellKod:Text = STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE).
     */
   END METHOD.
   
   METHOD PUBLIC VOID TabP1P2P3P4P5_SelectedTabChanging(INPUT nivtab AS CHARACTER, INPUT cTo AS CHARACTER,INPUT cFrom AS CHARACTER).
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      /*Uppdatera ribbon*/
      THIS-OBJECT:P5Control:splitContainerP5Import:Panel2Collapsed = TRUE.
      THIS-OBJECT:P4Control:splitContainerP4Import:Panel2Collapsed = TRUE.
      THIS-OBJECT:P3Control:splitContainerP3Import:Panel2Collapsed = TRUE.
      THIS-OBJECT:P2Control:splitContainerP2Import:Panel2Collapsed = TRUE.
      THIS-OBJECT:P1Control:splitContainerP1Import:Panel2Collapsed = TRUE.
      IF cTo = "Import" THEN DO:
         THIS-OBJECT:TabEkg_SelectedTabChanging(INPUT nivtab,INPUT nivtab). 
      END.   
     
   END METHOD.
   
   METHOD PUBLIC VOID TabEkg_SelectedTabChanging(INPUT cTo AS CHARACTER,INPUT cFrom AS CHARACTER).
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      /*Uppdatera ribbon*/
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:RibbonToolAktuellTab:Text = cTo.
      DEBUGGER:SET-BREAK().
      aktuelltabvar = cTo.
      IF nivandvar NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(92),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() = "Yes" THEN DO:  
            THIS-OBJECT:Sparatab(INPUT cFrom).                             
         END.         
         IF rrr:ToString() = "No" THEN DO:              
            THIS-OBJECT:Laddaomtab(INPUT cFrom).            
            /*THIS-OBJECT:ValjKatalog(ekgsubidvar, ebrkatvar).*/
         END.                 
      END.   
      nivandvar = "".
      IF cTo = "Kataloger" THEN DO:
         
      END.
      
      IF cTo = "Export" THEN DO:
         
         MESSAGE "Här"
         VIEW-AS ALERT-BOX.
         THIS-OBJECT:ControlShell:StartaExport().
         MESSAGE "Där"
         VIEW-AS ALERT-BOX.
         {GridRubrikListaVarExtent.i}
         rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
         rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
         rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",FALSE,50).
         rubrikergrid[4] = NEW Controls.GridRubrik("AKTIV","",FALSE,70, "LOG").
         rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
        THIS-OBJECT:ControlShell:EkgExport:gridExport:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ekgresursTTh)).
        THIS-OBJECT:ControlShell:EkgExport:gridExport:Text = "Resurser".
      END.
      
      IF cTo = "Resurspris" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaResurserPriserVF().
         THIS-OBJECT:ControlShell:GridResursPaslag:GuruReopen().
         THIS-OBJECT:ControlShell:gridResurspris:GuruReopen().
         THIS-OBJECT:ControlShell:GridResursPaslag:GuruReopen().
         THIS-OBJECT:ControlShell:GridResursniva:GuruReopen().
         THIS-OBJECT:ControlShell:comboNiva:GuruCombo:GuruFiltrera().
         
      END.
      IF cTo = "P5" THEN DO:
         THIS-OBJECT:P5Control:GridP5:GuruCurrentRowid(ekgp5tth:ROWID).
         THIS-OBJECT:ControlDataDb:HamtaResursFor(5).
         THIS-OBJECT:P5Control:GridResursForP5:GuruFiltrera().
         THIS-OBJECT:ControlDataDb:LaddaP5VFresurs().        
         THIS-OBJECT:P5Control:GridP5:GuruGetCurrentRowid().
      END.
      IF cTo = "P4" THEN DO:
         THIS-OBJECT:P4Control:GridP4:GuruCurrentRowid(ekgp4tth:ROWID).
         THIS-OBJECT:ControlDataDb:HamtaResursFor(4).
         THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera().         
         THIS-OBJECT:ControlDataDb:LaddaP4VFresurs().         
         THIS-OBJECT:ControlDataDb:LaddaP4VFfrek().
         THIS-OBJECT:P4Control:GridP5forP4:GuruFiltrera().        
         THIS-OBJECT:P4Control:GridP4:GuruGetCurrentRowid().
      END.
      IF cTo = "P3" THEN DO:
        THIS-OBJECT:P3Control:GridP3:GuruCurrentRowid(ekgp3tth:ROWID).
        THIS-OBJECT:ControlDataDb:HamtaResursFor(3).
        THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera().        
        THIS-OBJECT:ControlDataDb:LaddaP3VFresurs(ebrkatvar).        
        THIS-OBJECT:ControlDataDb:LaddaP3VFfrek().        
        IF ebrkatvar = "KLG1" THEN DO:
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = FALSE.
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.                     
        END.   
        IF ebrkatvar = "KLG2" THEN DO:
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = TRUE.
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
        END. 
        THIS-OBJECT:P3Control:GridP4forP3:GuruFiltrera().
        THIS-OBJECT:P3Control:GridP3:GuruGetCurrentRowid().
        /*Ev Flytta senare*/
        THIS-OBJECT:ControlShell:comboP3ArbKod:GuruCombo:GuruFiltrera().
      END.
      IF cTo = "P2" THEN DO:
        THIS-OBJECT:P2Control:GridP2:GuruCurrentRowid(ekgp2tth:ROWID). 
        THIS-OBJECT:ControlDataDb:HamtaResursFor(2).
        THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera().        
        THIS-OBJECT:ControlDataDb:LaddaP2VFresurs( ebrkatvar).
        THIS-OBJECT:ControlDataDb:LaddaP2VFmtrl().         
        THIS-OBJECT:ControlDataDb:LaddaP2VFfrek().        
        IF ebrkatvar = "KLG1" THEN DO:
           THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
           THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = TRUE.         
        END.   
        IF ebrkatvar = "KLG2" THEN DO:
           THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
           THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = FALSE.   
        END.
        THIS-OBJECT:P2Control:GridP3forP2:GuruFiltrera().
        THIS-OBJECT:P2Control:GridP2:GuruGetCurrentRowid().
        THIS-OBJECT:P2Control:GridMtrlForP2:GuruFiltrera("ekgmtrlTT.AKTIV = TRUE").
        /*Ev Flytta senare*/
        THIS-OBJECT:ControlShell:comboP2ArbKod:GuruCombo:GuruFiltrera().
      END.
      IF cTo = "P1" THEN DO:     
         THIS-OBJECT:P1Control:GridP1:GuruCurrentRowid(ekgp1tth:ROWID).
         THIS-OBJECT:ControlDataDb:LaddaP1VFfrek().         
         IF ebrkatvar = "KLG1" THEN DO:
            THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
            THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = TRUE.            
         END.   
         ELSE IF ebrkatvar = "KLG2" THEN DO:
            THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
             THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = FALSE. 
         END.
         THIS-OBJECT:P1Control:GridP2forP1:GuruFiltrera().
         THIS-OBJECT:P1Control:GridP1:GuruGetCurrentRowid().
        /*Ev Flytta senare*/
        THIS-OBJECT:ControlShell:comboP1ArbKod:GuruCombo:GuruFiltrera().
      END.
      THIS-OBJECT:VisaAllaKoder(?,?).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
   
   METHOD PUBLIC VOID ValjKatalog(INPUT nyekgsubidvar AS INTEGER, INPUT nyebrkatvar AS CHARACTER):
      THIS-OBJECT:AnvandsAvOff(STRING(ekgsubidvar)).
      
      /*Uppdatera värden*/
      ekgsubidvar = nyekgsubidvar.
      ebrkatvar = nyebrkatvar.
      lastkatvar = FALSE.
     
      /*Är katalogen låst?*/
      IF THIS-OBJECT:ControlDataDb:LastKat(nyekgsubidvar) = TRUE THEN lastkatvar = TRUE.
      
      /*Används katalogen av någon?*/
      IF THIS-OBJECT:ControlDataDb:AnvandsKat(nyekgsubidvar) = TRUE AND Guru.Konstanter:globanv NE STRING(THIS-OBJECT:ControlDataDb:AnvandsAv(nyekgsubidvar)) 
      THEN lastkatvar = TRUE.
      ELSE THIS-OBJECT:AnvandsAvOn(STRING(THIS-OBJECT:ControlShell:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
      
      THIS-OBJECT:ControlDataDb:LastUppGammalKat(nyekgsubidvar).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      
      /*Kör metoder*/
      THIS-OBJECT:DoljRibbonGrupper(TRUE).
      THIS-OBJECT:LastLage().
      THIS-OBJECT:LaddaDataSet().
      THIS-OBJECT:LaddaOmGriddar().
      THIS-OBJECT:VisaTabbar().
      THIS-OBJECT:UppdateraKatalogRibbon().
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      /*Ta bort senare*/
      IF lastkatvar = TRUE THEN MESSAGE "Katalogen är öppnad i låst läge"
                                VIEW-AS ALERT-BOX.
   END METHOD.
   
   METHOD PUBLIC VOID Avsluta(INPUT sender AS System.Object , INPUT e AS System.Windows.Forms.FormClosingEventArgs):
      /*
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(65), THIS-OBJECT:Root:LanguageManager:GetString(72), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).            
      IF rrr:ToString() EQ "Yes" THEN DO:
      */
         THIS-OBJECT:AnvandsAvOff(STRING(ekgsubidvar)).
      /*END.
      IF rrr:ToString() EQ "No" THEN DO:
         e:Cancel = TRUE.
      END. */
   END METHOD.
   
   METHOD PUBLIC VOID KopieraKatalog(INPUT nyekgsubidvar AS INTEGER, INPUT nyebrkatvar AS CHARACTER):
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(87),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN .
      ELSE RETURN. 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ControlDataDb:KopKatalog(INPUT-OUTPUT nyekgsubidvar, INPUT nyebrkatvar).
      THIS-OBJECT:ControlDataDb:LaddaKataloger().
      THIS-OBJECT:ControlShell:GridKataloger:GuruReopen().      
      THIS-OBJECT:ekgsubkatalogerTTh:FIND-FIRST("WHERE EKGSUBID = " +  STRING(nyekgsubidvar) ) NO-ERROR.
      THIS-OBJECT:ControlShell:GridKataloger:GuruRepositionto(THIS-OBJECT:ekgsubkatalogerTTh:ROWID).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      
   END METHOD.
   
   METHOD PUBLIC VOID TaBortKatalog(INPUT nyekgsubidvar AS INTEGER, INPUT nyebrkatvar AS CHARACTER):
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(88),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN .
      ELSE RETURN. 
      THIS-OBJECT:ControlShell:GridKataloger:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ControlDataDb:BortKatalog(INPUT nyekgsubidvar, INPUT nyebrkatvar).
      THIS-OBJECT:ControlShell:GridKataloger:GuruBortrader(FALSE).  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).         
   END METHOD.
   
   METHOD PUBLIC VOID LaddaDataSet():  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ControlDataDb:LaddaKatalog(INPUT ekgsubidvar, INPUT ebrkatvar).
      THIS-OBJECT:ControlDataDb:LaddaKatalogResurser(INPUT ekgsubidvar, INPUT ebrkatvar).  
      THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT ekgsubidvar, INPUT ebrkatvar). 
      THIS-OBJECT:ControlDataDb:LaddaP3(INPUT ekgsubidvar, INPUT ebrkatvar).
      THIS-OBJECT:ControlDataDb:LaddaP2(INPUT ekgsubidvar, INPUT ebrkatvar).
      THIS-OBJECT:ControlDataDb:LaddaP1(INPUT ekgsubidvar, INPUT ebrkatvar).    
      
      IF ebrkatvar = "KLG1" THEN DO:  
         THIS-OBJECT:ControlDataDb:LaddaP5(INPUT ekgsubidvar, INPUT ebrkatvar).
         THIS-OBJECT:ControlDataDb:LaddaP4(INPUT ekgsubidvar, INPUT ebrkatvar).
      END.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
   
   /*Sätter labels för blandannat fillinfält.*/
   METHOD PUBLIC VOID LaddaLabels():
      /*NyResurs*/
      THIS-OBJECT:ControlShell:fillinResursnr:GuruLabel:Text = "Resurs:".
      THIS-OBJECT:ControlShell:fillinResursBenamning:GuruLabel:Text = "Benämning:".
      /*NyMtrl*/
      THIS-OBJECT:ControlShell:fillinArtNr:GuruLabel:Text = "Artikelnummer:".
      /*NyPaslag*/
      THIS-OBJECT:ControlShell:fillinPaslagBenamning:GuruLabel:Text = "Benämning:".
      /*NyP5*/
      THIS-OBJECT:ControlShell:fillinP5arbkod:GuruLabel:Text = "Arbetskod:".
      /*NyP4*/
      THIS-OBJECT:ControlShell:fillinP4arbkod:GuruLabel:Text = "Arbetskod:".
      /*NyP3*/
      THIS-OBJECT:ControlShell:comboP3ArbKod:GuruLabel:Text = "Arbetskod:".
      THIS-OBJECT:ControlShell:fillinP3LopNr:GuruLabel:Text = "Löpnummer:".
      /*NyP3arbkod*/
      THIS-OBJECT:ControlShell:fillinP3ArbKod:GuruLabel:Text = "Arbetskod:".
      /*NyP2*/
      THIS-OBJECT:ControlShell:comboP2ArbKod:GuruLabel:Text = "Arbetskod:".
      THIS-OBJECT:ControlShell:fillinP2LopNr:GuruLabel:Text = "Löpnummer:".
      /*NyP2arbkod*/
      THIS-OBJECT:ControlShell:fillinP2ArbKod:GuruLabel:Text = "Arbetskod:".
      /*NyP1*/
      THIS-OBJECT:ControlShell:comboP1ArbKod:GuruLabel:Text = "Arbetskod:".
      THIS-OBJECT:ControlShell:fillinP1LopNr:GuruLabel:Text = "Löpnummer:".
      /*NyP1arbkod*/
      THIS-OBJECT:ControlShell:fillinP1ArbKod:GuruLabel:Text = "Arbetskod:".
      
      THIS-OBJECT:ControlShell:comboNiva:GuruLabel:Text = "Nivå:".
   END METHOD.
   
   METHOD PUBLIC VOID LaddaOmGriddar():   
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).       
      THIS-OBJECT:LaddaOmGriddarResurspris().
      THIS-OBJECT:LaddaOmGriddarMtrl().      
      THIS-OBJECT:LaddaOmGriddarP1().
      THIS-OBJECT:LaddaOmGriddarP2().
      THIS-OBJECT:LaddaOmGriddarP3().      
      IF ebrkatvar = "KLG1" THEN DO:
         THIS-OBJECT:LaddaOmGriddarP5().
         THIS-OBJECT:LaddaOmGriddarP4().         
      END.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).     
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarResurspris():
      THIS-OBJECT:ControlShell:GridRegel:GuruReopen().
      THIS-OBJECT:ControlShell:GridResurspris:GuruReopen().
      THIS-OBJECT:ControlShell:GridResursniva:GuruReopen().
      THIS-OBJECT:ControlShell:GridResursForResurspris:GuruReopen().
      THIS-OBJECT:ControlShell:GridPaslag:GuruReopen().
      THIS-OBJECT:ControlShell:GridResursPaslag:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarMtrl():
      THIS-OBJECT:ControlShell:GridMateriel:GuruReopen().
   END METHOD.   
   METHOD PUBLIC VOID LaddaOmGriddarp5():
      /*P5*/
      THIS-OBJECT:P5Control:GridP5:GuruReopen().  
      THIS-OBJECT:P5Control:GridP5Resurs:GuruReopen().
      THIS-OBJECT:P5Control:GridResursForP5:GuruReopen().
   END METHOD.   
   METHOD PUBLIC VOID LaddaOmGriddarp4():
      /*P4*/
      THIS-OBJECT:P4Control:GridP5ForP4:GuruReopen(). 
      THIS-OBJECT:P4Control:GridP4:GuruReopen().
      THIS-OBJECT:P4Control:GridP4Resurs:GuruReopen().
      THIS-OBJECT:P4Control:GridResursForP4:GuruReopen().
      THIS-OBJECT:P4Control:GridP4Frekvens:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarp3():
      /*P3*/
      THIS-OBJECT:P3Control:GridP4ForP3:GuruReopen().
      THIS-OBJECT:P3Control:GridP3:GuruReopen().
      THIS-OBJECT:P3Control:GridP3Resurs:GuruReopen().
      THIS-OBJECT:P3Control:GridResursForP3:GuruReopen().
      THIS-OBJECT:P3Control:GridP3Frekvens:GuruReopen().
      THIS-OBJECT:P3Control:GridP3arbkod:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarp2():
      /*P2*/
      THIS-OBJECT:P2Control:GridP3ForP2:GuruReopen().
      THIS-OBJECT:P2Control:GridP2:GuruReopen().
      THIS-OBJECT:P2Control:GridP2Resurs:GuruReopen().
      THIS-OBJECT:P2Control:GridResursForP2:GuruReopen().
      THIS-OBJECT:P2Control:GridP2Frekvens:GuruReopen().
      THIS-OBJECT:P2Control:GridP2Mtrl:GuruReopen().
      THIS-OBJECT:P2Control:GridMtrlForP2:GuruReopen().
      THIS-OBJECT:P2Control:GridP2arbkod:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarp1():
      /*P1*/
      THIS-OBJECT:P1Control:GridP2ForP1:GuruReopen().
      THIS-OBJECT:P1Control:GridP1:GuruReopen().
      THIS-OBJECT:P1Control:GridP1Frekvens:GuruReopen().
      THIS-OBJECT:P1Control:GridP1arbkod:GuruReopen().
   END METHOD.   
   
   METHOD PUBLIC VOID AnvandsAvOn(ekgsubid AS CHARACTER):
      DEFINE VARIABLE anvandarNamn AS CHARACTER.
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
         ASSIGN
         anvandarNamn = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.
      END.
      THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDS", INPUT "TRUE").
      THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAV", INPUT Guru.Konstanter:globanv).
      THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAVNAMN", INPUT anvandarNamn).
      THIS-OBJECT:ControlShell:gridKataloger:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID AnvandsAvOff(ekgsubid AS CHARACTER):
      IF INTEGER(ekgsubid) > 0 THEN DO:
         THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDS", INPUT "FALSE").
         THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAV", INPUT "").
         THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAVNAMN", INPUT "").
         THIS-OBJECT:ControlShell:gridKataloger:GuruReopen().
         END.
      RETURN.
   END METHOD.

    
  METHOD PUBLIC VOID GomTabbar():
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Resurspris"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Materiel"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Export"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P5"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P4"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P3"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P2"]:VISIBLE = FALSE.
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P1"]:VISIBLE = FALSE.
   END METHOD. 
   
   METHOD PUBLIC VOID VisaTabbar():
      IF ebrkatvar = "KLG1" THEN DO:
         /*INSTÄLLNINGAR FÖR KLG1, bryt ut om det blir mycket*/
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Resurspris"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Materiel"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Export"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P5"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P4"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P3"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P2"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P1"]:VISIBLE = TRUE.
         THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:VISIBLE = TRUE.
         
      END.  
      
      IF ebrkatvar = "KLG2" THEN DO:
         /*INSTÄLLNINGAR FÖR KLG2, bryt ut om det blir mycket*/
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Resurspris"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Materiel"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["Export"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P5"]:VISIBLE = FALSE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P4"]:VISIBLE = FALSE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P3"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P2"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P1"]:VISIBLE = TRUE.
         THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:VISIBLE = FALSE.
      END.  
   END METHOD.
   
   METHOD PUBLIC VOID VisaAllaKoder(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
       
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P5" THEN THIS-OBJECT:ValueChangeP5(?,?).  
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P4" THEN THIS-OBJECT:ValueChangeP4(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P3" THEN THIS-OBJECT:ValueChangeP3(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P2" THEN THIS-OBJECT:ValueChangeP2(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P1" THEN THIS-OBJECT:ValueChangeP1(?,?).
   END METHOD.
   
   METHOD PUBLIC VOID Fonster(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
       THIS-OBJECT:VisaAllaPFonster(TRUE).
       THIS-OBJECT:DoljOver(FALSE).
   
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = FALSE THEN DO:
           THIS-OBJECT:DoljKoderFonster(FALSE).
           THIS-OBJECT:DoljArbKoderFonster(TRUE).
           THIS-OBJECT:DoljUnderKoderFonster(TRUE).
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = FALSE THEN DO:
           THIS-OBJECT:DoljKoderFonster(FALSE).
           THIS-OBJECT:DoljArbKoderFonster(FALSE).
           THIS-OBJECT:DoljUnderKoderFonster(TRUE).
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = TRUE THEN DO:
           THIS-OBJECT:DoljKoderFonster(FALSE).
           THIS-OBJECT:DoljArbKoderFonster(TRUE).
           THIS-OBJECT:DoljUnderKoderFonster(FALSE).
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = FALSE THEN DO:
           THIS-OBJECT:DoljKoderFonster(TRUE). 
           THIS-OBJECT:DoljArbKoderFonster(FALSE).
           THIS-OBJECT:DoljUnderKoderFonster(TRUE).
           THIS-OBJECT:P4Control:p4MainSC:Visible = FALSE. 
           THIS-OBJECT:P5Control:p5MainSC:Visible = FALSE. 
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = TRUE THEN DO:
           THIS-OBJECT:DoljKoderFonster(TRUE).
           THIS-OBJECT:DoljArbKoderFonster(FALSE).
           THIS-OBJECT:DoljUnderKoderFonster(FALSE).
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = TRUE THEN DO:
           THIS-OBJECT:DoljKoderFonster(TRUE).
           THIS-OBJECT:DoljArbKoderFonster(TRUE).
           THIS-OBJECT:DoljUnderKoderFonster(FALSE).
           THIS-OBJECT:DoljOver(TRUE).
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = FALSE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = FALSE THEN DO:
           THIS-OBJECT:DoljKoderFonster(TRUE).
           THIS-OBJECT:DoljArbKoderFonster(TRUE).
           THIS-OBJECT:DoljUnderKoderFonster(TRUE).
           THIS-OBJECT:VisaAllaPFonster(FALSE).
       END.
       IF THIS-OBJECT:RibbonToolFonsterKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterArbKoder:Checked = TRUE AND THIS-OBJECT:RibbonToolFonsterUnderKoder:Checked = TRUE THEN DO:
           THIS-OBJECT:DoljKoderFonster(FALSE).
           THIS-OBJECT:DoljArbKoderFonster(FALSE).
           THIS-OBJECT:DoljUnderKoderFonster(FALSE).
       END.
  END METHOD.

  METHOD PUBLIC VOID DoljKoderFonster(gomas AS LOGICAL):
     THIS-OBJECT:P5Control:p5MainSC:Panel1Collapsed = gomas.
     THIS-OBJECT:P4Control:p4MainSC:Panel1Collapsed = gomas.
     THIS-OBJECT:P3Control:p3KodSC:Panel2Collapsed = gomas.
     THIS-OBJECT:P2Control:p2KodSC:Panel2Collapsed = gomas.
     THIS-OBJECT:P1Control:p1KodSC:Panel2Collapsed = gomas.
  END METHOD.

  METHOD PUBLIC VOID DoljArbKoderFonster(gomas AS LOGICAL):
     THIS-OBJECT:P1Control:p1KodSC:Panel1Collapsed = gomas.
     THIS-OBJECT:P2Control:p2KodSC:Panel1Collapsed = gomas.
     THIS-OBJECT:P3Control:p3KodSC:Panel1Collapsed = gomas.
  END METHOD.

 METHOD PUBLIC VOID DoljUnderKoderFonster(gomas AS LOGICAL):
    THIS-OBJECT:P5Control:p5MainSC:Panel2Collapsed = gomas.
    THIS-OBJECT:P4Control:p4MainSC:Panel2Collapsed = gomas.
    THIS-OBJECT:P3Control:p3MainSC:Panel2Collapsed = gomas.
    THIS-OBJECT:P2Control:p2MainSC:Panel2Collapsed = gomas.
    THIS-OBJECT:P1Control:p1MainSC:Panel2Collapsed = gomas.
 END METHOD.

 METHOD PUBLIC VOID VisaAllaPFonster(gomas AS LOGICAL): 
    THIS-OBJECT:P1Control:p1MainSC:Visible = gomas.
    THIS-OBJECT:P2Control:p2MainSC:Visible = gomas. 
    THIS-OBJECT:P3Control:p3MainSC:Visible = gomas. 
    THIS-OBJECT:P4Control:p4MainSC:Visible = gomas. 
    THIS-OBJECT:P5Control:p5MainSC:Visible = gomas. 
 END METHOD.

 METHOD PUBLIC VOID VisaAllFonster(gomas AS LOGICAL): 
    THIS-OBJECT:P1Control:p1KodSC:Visible = gomas.
    THIS-OBJECT:P2Control:p2KodSC:Visible = gomas. 
    THIS-OBJECT:P3Control:p3KodSC:Visible = gomas.
 END METHOD.

 METHOD PUBLIC VOID DoljOver(gomas AS LOGICAL): 
    THIS-OBJECT:P1Control:p1MainSC:Panel1Collapsed = gomas.
    THIS-OBJECT:P2Control:p2MainSC:Panel1Collapsed = gomas.
    THIS-OBJECT:P3Control:p3MainSC:Panel1Collapsed = gomas.
    THIS-OBJECT:P5Control:p5MainSC:Panel1Collapsed = gomas.
    THIS-OBJECT:P4Control:p4MainSC:Panel1Collapsed = gomas.
    THIS-OBJECT:P3Control:p3MainSC:Panel1Collapsed = gomas.
 END METHOD.
 
   METHOD PUBLIC VOID VisaKostnader(INPUT sender AS System.Object, INPUT args AS System.EventArgs):  
      DEFINE VARIABLE gomas AS LOGICAL NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      DEBUGGER:SET-BREAK().
      IF THIS-OBJECT:RibbonToolVisaKostnader:Checked = TRUE THEN gomas = FALSE.
      ELSE gomas = TRUE.
      /*P5*/
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridResursForP5:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.
             
      /*P4*/
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FMASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridResursForP4:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.
         
      /*P3*/
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE. 
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FMASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridResursForP3:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.
         
      /*P2*/
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["MATERIELKOST"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = TRUE. 
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE. 
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FMASKINKOST"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["KALKYLPRIS"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["MTRLSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridMtrlForP2:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.    
      THIS-OBJECT:P2Control:GridResursForP2:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.
         
      /*P1*/
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["MATERIELKOST"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FMATERIELKOST"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FMASKINKOST"]:Hidden = gomas.
      
      
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = TRUE.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
  END METHOD.
 
  
   METHOD PUBLIC VOID RibbonSokUpdate(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      DEFINE VARIABLE sokarbkod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE soklopnr AS CHARACTER NO-UNDO.
     
      IF e:KeyChar  = CHR(13) THEN DO:
         IF aktuelltabvar = "P1" THEN DO:
            IF ebrkatvar = "KLG1" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,1,4)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,5,2)).
            END.
            ELSE IF ebrkatvar = "KLG2" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,1,3)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,4,2)).
            END.
            IF soklopnr = "" THEN soklopnr = "1".
            ekgp1TTh:FIND-FIRST("WHERE ekgp1TT.P1ARBKOD = '" + sokarbkod + "'" + " AND ekgp1TT.P1LOPNR = " + soklopnr, NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN.
            ELSE ekgp1TTh:FIND-FIRST("WHERE ekgp1TT.P1ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P1Control:GridP1:GuruRadvaljare(THIS-OBJECT:ekgp1TTh:ROWID, TRUE).
            END.   
         END.
         ELSE IF aktuelltabvar = "P2" THEN DO:
            IF ebrkatvar = "KLG1" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,1,3)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,4,2)).
            END.
            ELSE IF ebrkatvar = "KLG2" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,1,2)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,3,2)).
            END.
            IF soklopnr = "" THEN soklopnr = "1".
            ekgp2TTh:FIND-FIRST("WHERE ekgp2TT.P2ARBKOD = '" + sokarbkod + "'" + " AND ekgp2TT.P2LOPNR = " + soklopnr, NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN.
            ELSE ekgp2TTh:FIND-FIRST("WHERE ekgp2TT.P2ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ControlDataDb:ekgp2TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P2Control:GridP2:GuruRadvaljare(THIS-OBJECT:ControlDataDb:ekgp2TTh:ROWID, TRUE).
            END.
         END.
         ELSE IF aktuelltabvar = "P3" THEN DO:
            sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,1,3)).
            soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolAktuellKod:Text,4,2)).
            IF soklopnr = "" THEN soklopnr = "1".
            ekgp3TTh:FIND-FIRST("WHERE ekgp3TT.P3ARBKOD = '" + sokarbkod + "'" + " AND ekgp3TT.P3LOPNR = " + soklopnr, NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN.
            ELSE ekgp3TTh:FIND-FIRST("WHERE ekgp3TT.P3ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P3Control:GridP3:GuruRadvaljare(THIS-OBJECT:ekgp3TTh:ROWID, TRUE).
            END.   
         END.
         ELSE IF aktuelltabvar = "P4" THEN DO:
            sokarbkod = THIS-OBJECT:RibbonToolAktuellKod:Text.
            ekgp4TTh:FIND-FIRST("WHERE ekgp4TT.P4ARBKOD BEGINS '" + sokarbkod + "'" , NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P4Control:GridP4:GuruRadvaljare(THIS-OBJECT:ekgp4TTh:ROWID, TRUE).
            END. 
         END.
         ELSE IF aktuelltabvar = "P5" THEN DO:
            sokarbkod = THIS-OBJECT:RibbonToolAktuellKod:Text.
            ekgp5TTh:FIND-FIRST("WHERE ekgp5TT.P5ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
               
               THIS-OBJECT:P5Control:GridP5:GuruRadvaljare(THIS-OBJECT:ekgp5TTh:ROWID, TRUE).
            END. 
         END.             
      END.
   END METHOD.
  METHOD PUBLIC VOID VisaEaMangder(INPUT sender AS System.Object, INPUT args AS System.EventArgs):  
      DEFINE VARIABLE gomas AS LOGICAL NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      IF THIS-OBJECT:RibbonToolVisaEaMangder:Checked = TRUE THEN gomas = FALSE.
      ELSE gomas = TRUE.      
      /*P5*/
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["EAMONT"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P5Control:GridResursForP5:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      
      /*P4*/
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EAMONT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      THIS-OBJECT:P4Control:GridResursForP4:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      
      /*P3*/
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE. 
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAMONT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridResursForP3:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      
      /*P2*/
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAMONT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = TRUE. 
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridResursForP2:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      
      /*P1*/
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE. 
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      IF ebrkatvar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = TRUE.         
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
    
  METHOD PUBLIC VOID LastLage():
      DEFINE VARIABLE boolvar1 AS Infragistics.Win.DefaultableBoolean.
      DEFINE VARIABLE boolvar2 AS LOGICAL.
      DEFINE VARIABLE boolvar3 AS LOGICAL.
      
      IF lastkatvar = TRUE THEN DO:
         boolvar1 = Infragistics.Win.DefaultableBoolean:False.
         boolvar2 = TRUE.
         boolvar3 = FALSE.
      END.
         IF lastkatvar = FALSE THEN DO:
         boolvar1 = Infragistics.Win.DefaultableBoolean:True.
         boolvar2 = FALSE.
         boolvar3 = TRUE.
      END.
          THIS-OBJECT:RibbonGroupSparaKatalog:Visible = boolvar3.
          
          THIS-OBJECT:ControlShell:GridResurs:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridRegel:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridResurspris:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridResursniva:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridResursForResurspris:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridPaslag:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridResursPaslag:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:GridMateriel:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:ControlShell:splitContainerPaslagOver:Panel2Collapsed = boolvar2.
          THIS-OBJECT:ControlShell:paslagMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:ControlShell:resursPaslagMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:ControlShell:regelMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:ControlShell:splitContainerNiva:Panel1Collapsed = boolvar2.
          THIS-OBJECT:ControlShell:splitContainerBasresurser:Panel1Collapsed = boolvar2.
          THIS-OBJECT:ControlShell:mtrlMenuSC:Panel1Collapsed = boolvar2.
          /*P5*/
          THIS-OBJECT:P5Control:GridP5:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P5Control:GridResursForP5:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P5Control:TabManagerP5:Tabs["Import"]:VISIBLE = boolvar3.
          THIS-OBJECT:P5Control:p5KoderMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P5Control:p5ResursSC:Panel1Collapsed = boolvar2.
          /*P4*/
          THIS-OBJECT:P4Control:GridP5ForP4:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P4Control:GridP4:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P4Control:GridResursForP4:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P4Control:TabManagerP4:Tabs["Import"]:VISIBLE = boolvar3.
          THIS-OBJECT:P4Control:p4KoderMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P4Control:p4ResursSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P4Control:p4FrekvensSC:Panel1Collapsed = boolvar2.
          /*P3*/
          THIS-OBJECT:P3Control:GridP4ForP3:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P3Control:GridP3:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P3Control:GridP3Arbkod:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P3Control:GridResursForP3:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P3Control:TabManagerP3:Tabs["Import"]:VISIBLE = boolvar3.
          THIS-OBJECT:P3Control:p3KoderMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P3Control:p3ArbKodSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P3Control:p3ResursSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P3Control:p3FrekvensSC:Panel1Collapsed = boolvar2.
          /*P2*/
          THIS-OBJECT:P2Control:GridP3ForP2:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridP2:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridP2Arbkod:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridResursForP2:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:GridMtrlForP2:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2Control:TabManagerP2:Tabs["Import"]:VISIBLE = boolvar3.
          THIS-OBJECT:P2Control:p2KoderMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P2Control:p2ArbKodSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P2Control:p2MaterielSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P2Control:p2ResursSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P2Control:p2FrekvensSC:Panel1Collapsed = boolvar2.
          /*P1*/
          THIS-OBJECT:P1Control:GridP2ForP1:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P1Control:GridP1:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P1Control:GridP1Arbkod:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P1Control:TabManagerP1:Tabs["Import"]:VISIBLE = boolvar3.
          THIS-OBJECT:P1Control:p1KoderMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P1Control:p1ArbKodSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P1Control:p1FrekvensSC:Panel1Collapsed = boolvar2.   
  END.
  
   METHOD PUBLIC VOID NyMtrl():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyMtrl(THIS-OBJECT:ControlShell:fillinArtNr:GuruText:Text, ekgsubidvar, ebrkatvar).
      THIS-OBJECT:ControlShell:GridMateriel:GuruFiltrera().
      THIS-OBJECT:ControlShell:GridMateriel:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:ControlShell:GridMateriel:GuruCellValjare("BENAMNING").
      nivandvar = "11".
   END METHOD.

   METHOD PUBLIC VOID NyPaslag():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyPaslag(THIS-OBJECT:ControlShell:fillinPaslagBenamning:GuruText:Text, ekgsubidvar, ebrkatvar).
      THIS-OBJECT:ControlShell:GridPaslag:GuruFiltrera().
      THIS-OBJECT:ControlShell:GridPaslag:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:ControlShell:GridPaslag:GuruCellValjare("PASLAG").
      nivandvar = "11".    
      raknavar = "11".  
   END METHOD.

   METHOD PUBLIC VOID NyResurs():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      
      THIS-OBJECT:ekgresursTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:ControlShell:fillinResursNr:GuruText:Text ) NO-ERROR.
      IF THIS-OBJECT:ekgresursTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ControlShell:GridResurs:GuruRadvaljare(THIS-OBJECT:ekgresursTTh:ROWID, TRUE).
      END.   
      ELSE DO:
         rid = THIS-OBJECT:ControlDataDb:NyResurs(THIS-OBJECT:ControlShell:fillinResursBenamning:GuruText:Text,INTEGER(THIS-OBJECT:ControlShell:fillinResursNr:GuruText:Text)).
         THIS-OBJECT:ControlShell:GridResurs:GuruFiltrera().
         THIS-OBJECT:ControlShell:GridResurs:GuruRadvaljare(rid, TRUE).
         THIS-OBJECT:ControlShell:GridResurs:GuruCellValjare("ENHET").
         nivandvar = "12".
      END.         
   END METHOD.
   METHOD PUBLIC VOID NyNiva():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      THIS-OBJECT:ekgresursnivaTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE  + " AND NIVA = " + STRING(THIS-OBJECT:ControlShell:comboNiva:GuruCombo:VALUE)) NO-ERROR.      
      IF THIS-OBJECT:ekgresursnivaTTh:AVAILABLE THEN.  
      ELSE DO:
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-CREATE().         
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-COPY(ekgresursprisTTh).
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD("NIVA"):BUFFER-VALUE = STRING(THIS-OBJECT:ControlShell:comboNiva:GuruCombo:VALUE).
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD("FORVALDEJFREK"):BUFFER-VALUE = FALSE.
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD("FORVALDFREK"):BUFFER-VALUE = FALSE.      
         THIS-OBJECT:ekgresursnivaTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = ekgresursnivaTTh:RECID. 
         rid = THIS-OBJECT:ekgresursnivaTTh:ROWID.
         THIS-OBJECT:ControlShell:GridResursniva:GuruReopen().        
         THIS-OBJECT:ControlShell:GridResursniva:GuruRadvaljare(rid, TRUE).         
         nivandvar = "10".
      END.       
      
  END METHOD.
   
   METHOD PUBLIC VOID NyP5():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP5LopNr(THIS-OBJECT:ControlShell:fillinP5arbkod:GuruText:Text, ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P5Control:GridP5:GuruFiltrera().
      THIS-OBJECT:P5Control:GridP5:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P5Control:GridP5:GuruCellValjare("BENAMNING").
      nivandvar = "6".       
   END METHOD.
   
  METHOD PUBLIC VOID NyP4():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP4LopNr(THIS-OBJECT:ControlShell:fillinP4arbkod:GuruText:Text, ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P4Control:GridP4:GuruFiltrera().
      THIS-OBJECT:P4Control:GridP4:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P4Control:GridP4:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
   
  METHOD PUBLIC VOID NyP3ArbKod():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP3ArbKod(STRING(THIS-OBJECT:ControlShell:fillinP3arbkod:GuruText:Text), ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P3Control:GridP3ArbKod:GuruFiltrera().
      THIS-OBJECT:P3Control:GridP3ArbKod:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P3Control:GridP3ArbKod:GuruCellValjare("BENAMNING").
      THIS-OBJECT:ControlShell:comboP3ArbKod:GuruCombo:GuruFiltrera().
      nivandvar = "8".       
  END METHOD.
  
   METHOD PUBLIC VOID NyP3():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP3LopNr(STRING(THIS-OBJECT:ControlShell:comboP3arbkod:GuruCombo:VALUE), INTEGER(THIS-OBJECT:ControlShell:fillinP3lopnr:GuruText:Text), ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P3Control:GridP3:GuruFiltrera().
      THIS-OBJECT:P3Control:GridP3:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P3Control:GridP3:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
   
  METHOD PUBLIC VOID NyP2ArbKod():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP2ArbKod(STRING(THIS-OBJECT:ControlShell:fillinP2arbkod:GuruText:Text), ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P2Control:GridP2ArbKod:GuruFiltrera().
      THIS-OBJECT:P2Control:GridP2ArbKod:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P2Control:GridP2ArbKod:GuruCellValjare("BENAMNING"). 
      THIS-OBJECT:ControlShell:comboP2ArbKod:GuruCombo:GuruFiltrera().
      nivandvar = "8".      
  END METHOD.
  
  METHOD PUBLIC VOID NyP2():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP2LopNr(STRING(THIS-OBJECT:ControlShell:comboP2arbkod:GuruCombo:VALUE), INTEGER(THIS-OBJECT:ControlShell:fillinP2lopnr:GuruText:Text), ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P2Control:GridP2:GuruFiltrera().
      THIS-OBJECT:P2Control:GridP2:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P2Control:GridP2:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
  
  METHOD PUBLIC VOID NyP1ArbKod():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP1ArbKod(STRING(THIS-OBJECT:ControlShell:fillinP1arbkod:GuruText:Text), ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P1Control:GridP1ArbKod:GuruFiltrera().
      THIS-OBJECT:P1Control:GridP1ArbKod:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P1Control:GridP1ArbKod:GuruCellValjare("BENAMNING"). 
      THIS-OBJECT:ControlShell:comboP1ArbKod:GuruCombo:GuruFiltrera().
      nivandvar = "8".      
  END METHOD.
   
  METHOD PUBLIC VOID NyP1():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP1LopNr(STRING(THIS-OBJECT:ControlShell:comboP1arbkod:GuruCombo:VALUE), INTEGER(THIS-OBJECT:ControlShell:fillinP1lopnr:GuruText:Text), ekgsubidvar, ebrkatvar).
      THIS-OBJECT:P1Control:GridP1:GuruFiltrera().
      THIS-OBJECT:P1Control:GridP1:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P1Control:GridP1:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
   
  METHOD PUBLIC VOID TaBortMtrl():
     DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
     rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(123),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
     IF rrr:ToString() = "Yes" THEN DO:
        IF THIS-OBJECT:ControlDataDb:TaBortMtrl(STRING(THIS-OBJECT:ControlShell:GridMateriel:ActiveRow:Cells["ARTNR"]:Text)) = TRUE THEN DO:
           THIS-OBJECT:ControlShell:GridMateriel:GuruBortRader(FALSE).
           THIS-OBJECT:ControlShell:GridMateriel:GuruReopen().
           nivandvar = "11".           
        END.   
     END.         
  END METHOD.
  
  METHOD PUBLIC VOID TaBortPaslag():
      THIS-OBJECT:ControlDataDb:TaBortPaslag(INTEGER(THIS-OBJECT:ControlShell:GridPaslag:ActiveRow:Cells["PASLAGNR"]:Text)).
      THIS-OBJECT:ControlShell:GridPaslag:GuruBortRader(FALSE).
      nivandvar = "11".
      raknavar = "11".      
  END METHOD.
  
  METHOD PUBLIC VOID TaBortResursniva(INPUT resnr AS INTEGER, nivnr AS INTEGER):
    DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
    rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(115),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
    IF rrr:ToString() = "Yes" THEN DO:          
        THIS-OBJECT:ControlShell:GridResursniva:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.        
        THIS-OBJECT:ekgresursnivaTTh:FIND-FIRST("WHERE RESURSNR = "  + STRING(resnr)  + " AND NIVA = " + STRING(resnr)) NO-ERROR.
        THIS-OBJECT:ControlShell:GridResursniva:GuruBortRader(FALSE).
        nivandvar = "12".               
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortResurspris(INPUT resnr AS INTEGER):
    DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
    rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(102),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
    IF rrr:ToString() = "Yes" THEN DO:    
        THIS-OBJECT:ControlShell:GridResurspris:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.  
        IF THIS-OBJECT:ControlDataDb:TaBortResurspris(resnr) = TRUE THEN DO:                   
           THIS-OBJECT:ControlShell:GridResurspris:GuruBortRader(FALSE).
           THIS-OBJECT:ControlShell:GridResursNiva:GuruReopen(). 
           nivandvar = "12".           
        END.   
     END.   
  END METHOD.
   
  METHOD PUBLIC VOID TaBortP5():
     IF THIS-OBJECT:buttonOverBortFragalopnr() = TRUE THEN DO:
        THIS-OBJECT:P5Control:GridP5:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        IF THIS-OBJECT:ControlDataDb:TaBortP5(STRING(THIS-OBJECT:P5Control:GridP5:ActiveRow:Cells["P5ARBKOD"]:Text), INTEGER(THIS-OBJECT:P5Control:GridP5:ActiveRow:Cells["P5LOPNR"]:Text)) = TRUE THEN DO:           
           THIS-OBJECT:P5Control:GridP5:GuruBortRader(FALSE).
           nivandvar = "5".           
        END.   
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP4():
     IF THIS-OBJECT:buttonOverBortFragalopnr() = TRUE THEN DO:
        THIS-OBJECT:P4Control:GridP4:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        IF THIS-OBJECT:ControlDataDb:TaBortP4(STRING(THIS-OBJECT:P4Control:GridP4:ActiveRow:Cells["P4ARBKOD"]:Text), INTEGER(THIS-OBJECT:P4Control:GridP4:ActiveRow:Cells["P4LOPNR"]:Text)) = TRUE THEN DO:
           THIS-OBJECT:P4Control:GridP4:GuruBortRader(FALSE).
           nivandvar = "5".           
        END.   
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP3ArbKod():
     IF THIS-OBJECT:buttonOverBortFragaarbkod() = TRUE THEN DO:
        THIS-OBJECT:P3Control:GridP3ArbKod:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.   
        IF THIS-OBJECT:ControlDataDb:TaBortP3ArbKod(STRING(THIS-OBJECT:P3Control:GridP3ArbKod:ActiveRow:Cells["P3ARBKOD"]:Text)) = TRUE THEN DO:
           THIS-OBJECT:P3Control:GridP3ArbKod:GuruBortRader(FALSE).
           THIS-OBJECT:P3Control:GridP3:GuruReopen().  
           nivandvar = "7".
        END.   
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP3():     
     IF THIS-OBJECT:buttonOverBortFragalopnr() = TRUE THEN DO:
        THIS-OBJECT:P3Control:GridP3:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        IF THIS-OBJECT:ControlDataDb:TaBortP3(STRING(THIS-OBJECT:P3Control:GridP3:ActiveRow:Cells["P3ARBKOD"]:Text), INTEGER(THIS-OBJECT:P3Control:GridP3:ActiveRow:Cells["P3LOPNR"]:Text)) = TRUE THEN DO:
           THIS-OBJECT:P3Control:GridP3:GuruBortRader(FALSE).
           nivandvar = "5".
        END.   
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP2ArbKod():
     IF THIS-OBJECT:buttonOverBortFragaarbkod() = TRUE THEN DO:
        THIS-OBJECT:P2Control:GridP2ArbKod:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        IF THIS-OBJECT:ControlDataDb:TaBortP2ArbKod(STRING(THIS-OBJECT:P2Control:GridP2ArbKod:ActiveRow:Cells["P2ARBKOD"]:Text)) THEN DO:
           THIS-OBJECT:P2Control:GridP2ArbKod:GuruBortRader(FALSE).
           THIS-OBJECT:P2Control:GridP2:GuruReopen().
           nivandvar = "7".
        END.   
      END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP2():
     IF THIS-OBJECT:buttonOverBortFragalopnr() = TRUE THEN DO:
        THIS-OBJECT:P2Control:GridP2:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        IF THIS-OBJECT:ControlDataDb:TaBortP2(STRING(THIS-OBJECT:P2Control:GridP2:ActiveRow:Cells["P2ARBKOD"]:Text), INTEGER(THIS-OBJECT:P2Control:GridP2:ActiveRow:Cells["P2LOPNR"]:Text)) = TRUE THEN DO:
           THIS-OBJECT:P2Control:GridP2:GuruBortRader(FALSE).
           nivandvar = "5".
        END.              
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP1ArbKod():
     IF THIS-OBJECT:buttonOverBortFragaarbkod() = TRUE THEN DO:
        THIS-OBJECT:P1Control:GridP1ArbKod:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        THIS-OBJECT:ControlDataDb:TaBortP1ArbKod(STRING(THIS-OBJECT:P1Control:GridP1ArbKod:ActiveRow:Cells["P1ARBKOD"]:Text)).
        THIS-OBJECT:P1Control:GridP1ArbKod:GuruBortRader(FALSE).
        THIS-OBJECT:P1Control:GridP1:GuruReopen().
        nivandvar = "7".        
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortP1():
     IF THIS-OBJECT:buttonOverBortFragalopnr() = TRUE THEN DO:
        THIS-OBJECT:P1Control:GridP1:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
        THIS-OBJECT:ControlDataDb:TaBortP1(STRING(THIS-OBJECT:P1Control:GridP1:ActiveRow:Cells["P1ARBKOD"]:Text), INTEGER(THIS-OBJECT:P1Control:GridP1:ActiveRow:Cells["P1LOPNR"]:Text)).
        THIS-OBJECT:P1Control:GridP1:GuruBortRader(FALSE).
        nivandvar = "5".     
     END.   
  END METHOD.
   
   METHOD PUBLIC VOID UppdateraKatalogRibbon(): 
      THIS-OBJECT:ekgsubkatTTh:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:ekgsubkatTTh:AVAILABLE THEN DO:         
         THIS-OBJECT:RibbonToolSubKatID:Text = THIS-OBJECT:ekgsubkatTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE.
         THIS-OBJECT:RibbonToolEbrKat:Text = THIS-OBJECT:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.
         THIS-OBJECT:RibbonToolBenamning:Text = THIS-OBJECT:ekgsubkatTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         THIS-OBJECT:RibbonToolArtal:Text = THIS-OBJECT:ekgsubkatTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE.     
      END.
   END METHOD.
   
   METHOD PUBLIC LOGICAL buttonOverBortFraga(): 
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.   
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN FALSE.
      ELSE RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL buttonOverBortFragalopnr(): 
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.   
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(95), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN FALSE.
      ELSE RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL buttonOverBortFragaarbkod(): 
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.   
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(96), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN FALSE.
      ELSE RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID NyResurspris(): 
      IF THIS-OBJECT:ControlShell:gridResurs:Posn() = -1 THEN RETURN.
      ekgresursprisTTh:FIND-FIRST("WHERE RESURSNR = " + STRING(THIS-OBJECT:ekgresursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ekgresursprisTTh:AVAILABLE THEN.
      ELSE DO:
         THIS-OBJECT:ControlShell:buttonOverResurspris:GuruOver().
         THIS-OBJECT:ControlDataDb:ResursprisUpd().
         THIS-OBJECT:ControlShell:buttonOverResurspris:GuruOverefteruppdat().
         IF THIS-OBJECT:ControlShell:gridResurs:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:ControlShell:buttonOverResurspris:GuruTogrid:GuruCellvaljare("PRIS").
         nivandvar = "1".
      END.   
   END METHOD. 
   
      
   METHOD PUBLIC VOID buttonOverP5ResursBort(): 
      IF THIS-OBJECT:P5Control:gridP5Resurs:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P5Control:buttonOverP5ResursBort:GuruOver().         
         THIS-OBJECT:ControlDataDb:BeraknaP5en().
         THIS-OBJECT:P5Control:GridP5:GuruReopen().           
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP5Resurs(): 
      IF THIS-OBJECT:P5Control:gridResursForP5:Posn() = -1 THEN RETURN.
      THIS-OBJECT:P5Control:buttonOverP5Resurs:GuruOver().
      THIS-OBJECT:ControlDataDb:P5ResursUpd().
      THIS-OBJECT:P5Control:buttonOverP5Resurs:GuruOverefteruppdat().
      IF THIS-OBJECT:P5Control:gridResursForP5:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P5Control:buttonOverP5Resurs:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "1".
   END METHOD. 
   
   METHOD PUBLIC VOID buttonOverP4ResursBort(): 
      IF THIS-OBJECT:P4Control:gridP4Resurs:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P4Control:buttonOverP4ResursBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP4enresurs().
         THIS-OBJECT:P4Control:GridP4:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP4Resurs(): 
      IF THIS-OBJECT:P4Control:gridResursForP4:Posn() = -1 THEN RETURN.
      THIS-OBJECT:P4Control:buttonOverP4Resurs:GuruOver().
      THIS-OBJECT:ControlDataDb:P4ResursUpd().
      THIS-OBJECT:P4Control:buttonOverP4Resurs:GuruOverefteruppdat().
      IF THIS-OBJECT:P4Control:gridResursForP4:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P4Control:buttonOverP4Resurs:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP3ResursBort(): 
      IF THIS-OBJECT:P3Control:gridP3Resurs:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P3Control:buttonOverP3ResursBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(ebrkatvar).
         THIS-OBJECT:P3Control:GridP3:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP3Resurs(): 
      IF THIS-OBJECT:P3Control:gridResursForP3:Posn() = -1 THEN RETURN.
      THIS-OBJECT:P3Control:buttonOverP3Resurs:GuruOver().  
      THIS-OBJECT:ControlDataDb:P3ResursUpd().
      THIS-OBJECT:P3Control:buttonOverP3Resurs:GuruOverefteruppdat().
      IF THIS-OBJECT:P3Control:gridResursForP3:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P3Control:buttonOverP3Resurs:GuruTogrid:GuruCellvaljare("ANTAL").    
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP2ResursBort(): 
      IF THIS-OBJECT:P2Control:gridP2Resurs:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P2Control:buttonOverP2ResursBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(ebrkatvar).
         THIS-OBJECT:P2Control:GridP2:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP2Resurs(): 
      IF THIS-OBJECT:P2Control:gridResursForP2:Posn() = -1 THEN RETURN.
      THIS-OBJECT:P2Control:buttonOverP2Resurs:GuruOver().
      THIS-OBJECT:ControlDataDb:P2ResursUpd().
      THIS-OBJECT:P2Control:buttonOverP2Resurs:GuruOverefteruppdat().
      IF THIS-OBJECT:P2Control:gridResursForP2:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P2Control:buttonOverP2Resurs:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "1".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP4FrekvensBort(): 
      IF THIS-OBJECT:P4Control:gridP4Frekvens:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P4Control:buttonOverP4FrekvensBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP4enfrek().
         THIS-OBJECT:P4Control:GridP4:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP4Frekvens(): 
      IF THIS-OBJECT:P4Control:gridP5ForP4:Posn() = -1 THEN RETURN.
      THIS-OBJECT:VisaAllaKoderKoll("P4F").
      THIS-OBJECT:P4Control:buttonOverP4Frekvens:GuruOver().
      THIS-OBJECT:ControlDataDb:P4FrekvensUpd().
      THIS-OBJECT:VisaAllaKoderAter("P4F").
      IF THIS-OBJECT:P4Control:gridP5ForP4:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P4Control:buttonOverP4Frekvens:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "2".      
   END METHOD.
   METHOD PUBLIC VOID buttonOverP3FrekvensBort(): 
      IF THIS-OBJECT:P3Control:gridP3Frekvens:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P3Control:buttonOverP3FrekvensBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP3enfrek().
         THIS-OBJECT:P3Control:GridP3:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP3Frekvens(): 
      IF THIS-OBJECT:P3Control:gridP4ForP3:Posn() = -1 THEN RETURN.
      THIS-OBJECT:VisaAllaKoderKoll("P3F").
      THIS-OBJECT:P3Control:buttonOverP3Frekvens:GuruOver().      
      THIS-OBJECT:ControlDataDb:P3FrekvensUpd().
      THIS-OBJECT:VisaAllaKoderAter("P3F").
      IF THIS-OBJECT:P3Control:gridP4ForP3:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P3Control:buttonOverP3Frekvens:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP2FrekvensBort(): 
      IF THIS-OBJECT:P2Control:gridP2Frekvens:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P2Control:buttonOverP2FrekvensBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP2enfrek().
         THIS-OBJECT:P2Control:GridP2:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP2Frekvens(): 
      IF THIS-OBJECT:P2Control:gridP3ForP2:Posn() = -1 THEN RETURN.
      THIS-OBJECT:VisaAllaKoderKoll("P2F").
      THIS-OBJECT:P2Control:buttonOverP2Frekvens:GuruOver().
      THIS-OBJECT:ControlDataDb:P2FrekvensUpd().
      THIS-OBJECT:VisaAllaKoderAter("P2F").
      IF THIS-OBJECT:P2Control:gridP3ForP2:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P2Control:buttonOverP2Frekvens:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP2MtrlBort(): 
      IF THIS-OBJECT:P2Control:gridP2Mtrl:Posn() = -1 THEN RETURN.
      DEBUGGER:SET-BREAK().
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:    
         dontrepo = TRUE.     
         THIS-OBJECT:P2Control:buttonOverP2MtrlBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP2mtrl().
         THIS-OBJECT:P2Control:GridP2:GuruReopen().          
         dontrepo = FALSE.               
      END.   
      ELSE RETURN.
      
      nivandvar = "3".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP2Mtrl(): 
      IF THIS-OBJECT:P2Control:gridMtrlForP2:Posn() = -1 THEN RETURN.
      THIS-OBJECT:VisaAllaKoderKoll("P2M").
      THIS-OBJECT:P2Control:buttonOverP2Mtrl:GuruOver().
      THIS-OBJECT:ControlDataDb:P2MtrlUpd(). 
      THIS-OBJECT:VisaAllaKoderAter("P2M").
      IF THIS-OBJECT:P2Control:gridMtrlForP2:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P2Control:buttonOverP2Mtrl:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "3".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP1FrekvensBort(): 
      IF THIS-OBJECT:P1Control:gridP1Frekvens:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:P1Control:buttonOverP1FrekvensBort:GuruOver().
         THIS-OBJECT:ControlDataDb:BeraknaP1enfrek().
         THIS-OBJECT:P1Control:GridP1:GuruReopen().
         dontrepo = FALSE.
      END.   
      ELSE RETURN.
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverP1Frekvens(): 
      IF THIS-OBJECT:P1Control:gridP2ForP1:Posn() = -1 THEN RETURN.
      THIS-OBJECT:VisaAllaKoderKoll("P1F").
      THIS-OBJECT:P1Control:buttonOverP1Frekvens:GuruOver().
      THIS-OBJECT:ControlDataDb:P1FrekvensUpd().
      THIS-OBJECT:VisaAllaKoderAter("P1F").
      IF THIS-OBJECT:P1Control:gridP2ForP1:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:P1Control:buttonOverP1Frekvens:GuruTogrid:GuruCellvaljare("ANTAL").
      nivandvar = "2".
   END METHOD.
   METHOD PUBLIC VOID buttonOverResurspaslagBort(): 
      IF THIS-OBJECT:ControlShell:gridResursPaslag:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:ControlShell:buttonOverResurspaslagBort:GuruOver().
         dontrepo = TRUE.
      END.   
      ELSE RETURN.
      nivandvar = "10".
      raknavar = "10".
   END METHOD.
   
   METHOD PUBLIC VOID buttonOverResurspaslag(): 
      IF THIS-OBJECT:ControlShell:gridPaslag:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:ControlShell:gridResursPris:Posn() = -1 THEN RETURN.
      THIS-OBJECT:ControlShell:gridResursPaslag:Guruegenskap:filter = "".
      THIS-OBJECT:ControlShell:gridResursPaslag:GuruFiltrera("ekgresurspaslagTT.RESURSNR = " + STRING(THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE)).
      THIS-OBJECT:ControlShell:buttonOverResurspaslag:GuruOver().
      THIS-OBJECT:ControlShell:gridResursPaslag:Guruegenskap:filter = "".
      THIS-OBJECT:ControlDataDb:ResurspaslagsUpd().
      THIS-OBJECT:ControlShell:buttonOverResurspaslag:GuruOverefteruppdat().
      nivandvar = "10".
      raknavar = "10".
   END METHOD.
   
   METHOD PUBLIC VOID VisaAllaKoderKoll(INPUT nivfrek AS CHARACTER): 
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = FALSE THEN RETURN.
      IF nivfrek = "P4F" THEN THIS-OBJECT:P4Control:GridP4Frekvens:GuruFiltrera("ekgp4frekvensTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4frekvensTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).      
      IF nivfrek = "P3F" THEN THIS-OBJECT:P3Control:GridP3Frekvens:GuruFiltrera("ekgp3frekvensTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3frekvensTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
      IF nivfrek = "P2F" THEN THIS-OBJECT:P2Control:GridP2Frekvens:GuruFiltrera("ekgp2frekvensTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2frekvensTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
      IF nivfrek = "P1F" THEN THIS-OBJECT:P1Control:GridP1Frekvens:GuruFiltrera("ekgp1frekvensTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1frekvensTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
      IF nivfrek = "P2M" THEN THIS-OBJECT:P2Control:GridP2Mtrl:GuruFiltrera("ekgp2mtrlTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2mtrlTT.P2LOPNR = " + STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)). 
   END METHOD.
   
   METHOD PUBLIC VOID VisaAllaKoderAter(INPUT nivfrek AS CHARACTER): 
      IF nivfrek = "P4F" THEN DO:
         IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = TRUE  THEN THIS-OBJECT:P4Control:GridP4Frekvens:Guruegenskap:filter = "". 
         THIS-OBJECT:P4Control:buttonOverP4Frekvens:GuruOverefteruppdat().           
      END. 
      IF nivfrek = "P3F" THEN DO:
         IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = TRUE  THEN THIS-OBJECT:P3Control:GridP3Frekvens:Guruegenskap:filter = "". 
         THIS-OBJECT:P3Control:buttonOverP3Frekvens:GuruOverefteruppdat().           
      END.  
      IF nivfrek = "P2F" THEN DO:
         IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = TRUE  THEN THIS-OBJECT:P2Control:GridP2Frekvens:Guruegenskap:filter = "". 
         THIS-OBJECT:P2Control:buttonOverP2Frekvens:GuruOverefteruppdat().           
      END.  
      IF nivfrek = "P1F" THEN DO:
         IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = TRUE  THEN THIS-OBJECT:P1Control:GridP1Frekvens:Guruegenskap:filter = "". 
         THIS-OBJECT:P1Control:buttonOverP1Frekvens:GuruOverefteruppdat().           
      END. 
      IF nivfrek = "P2M" THEN DO:
         IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = TRUE  THEN THIS-OBJECT:P2Control:GridP2Mtrl:Guruegenskap:filter = "". 
         THIS-OBJECT:P2Control:buttonOverP2Mtrl:GuruOverefteruppdat().           
      END.             
   END METHOD.
   DESTRUCTOR EkgWinLg():
   END DESTRUCTOR.   
       
END CLASS.
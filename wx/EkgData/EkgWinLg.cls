 
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
   {EKGTH.I}
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
   DEFINE PUBLIC VARIABLE MaterielControl               AS Modules.EkgData.EkgMaterielControl                             NO-UNDO.
   DEFINE PUBLIC VARIABLE RPControl                     AS Modules.EkgData.EkgRPControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KatalogerControl              AS Modules.EkgData.EkgKatalogerControl                            NO-UNDO.
   DEFINE PUBLIC VARIABLE ExportControl                 AS Modules.EkgData.EkgExportControl                               NO-UNDO.
   DEFINE PUBLIC VARIABLE ResursControl                 AS Modules.EkgData.EkgResursControl                               NO-UNDO.
   DEFINE PUBLIC VARIABLE JamforControl                 AS Modules.EkgData.EkgJamforControl                               NO-UNDO.
   DEFINE PUBLIC VARIABLE P2KomControl                 AS Modules.EkgData.EkgP2Kom                                       NO-UNDO.
   
   DEFINE VARIABLE dontrepo AS LOGICAL NO-UNDO. 
   DEFINE PUBLIC  VARIABLE VisaExcel                    AS Controls.GuruExcelEmbed                                        NO-UNDO.
   
   DEFINE PUBLIC VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
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
   DEFINE PUBLIC  VARIABLE RibbonGroupKatMeny                AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupSparaKatalog                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupKatInfo                  AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupTabInfo                  AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupVisa                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupFonster                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonGroupSpara                AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
  /* Ribbongrupp RibbonGroupKatMeny */
  DEFINE PUBLIC VARIABLE RibbonToolHemKnapp            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
  DEFINE PUBLIC VARIABLE RibbonToolHemKnappBildval            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
  DEFINE PUBLIC VARIABLE RibbonToolKatalogKnapp          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
  DEFINE PUBLIC VARIABLE RibbonToolKatalogKnappBildval          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   
  /* Ribbongrupp SparaKatalog */
   DEFINE VARIABLE RibbonToolSparaKatalog          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE VARIABLE RibbonToolStangKatalog          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   /*DEFINE VARIABLE RibbonToolBeraknaKatalog          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.*/
   
   /* Ribbongrupp KatInfo */
   DEFINE VARIABLE RibbonToolBenamning           AS Infragistics.Win.UltraWinToolbars.TextBoxTool                        NO-UNDO.
   DEFINE VARIABLE RibbonToolEbrKat             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                              NO-UNDO.
   /*DEFINE VARIABLE RibbonToolSubKatID            AS Infragistics.Win.UltraWinToolbars.TextBoxTool                               NO-UNDO.*/
   DEFINE VARIABLE RibbonToolArtal           AS Infragistics.Win.UltraWinToolbars.TextBoxTool                        NO-UNDO.

   
   /* Ribbongrupp TabInfo */
   DEFINE VARIABLE RibbonToolAktuellTab           AS Infragistics.Win.UltraWinToolbars.TextBoxTool                        NO-UNDO.
   DEFINE VARIABLE RibbonToolSokKod            AS Infragistics.Win.UltraWinToolbars.TextBoxTool                              NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonToolAktivKod            AS Infragistics.Win.UltraWinToolbars.TextBoxTool                              NO-UNDO.
   
   /* Ribbongrupp Visa */
   DEFINE PUBLIC  VARIABLE RibbonToolVisaAllaKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaKostnader            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaEaMangder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   /* Ribbongrupp Fonster */
   DEFINE PUBLIC  VARIABLE RibbonToolFonsterKoder           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFonsterUnderkoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFonsterArbKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
      
   DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolManual             AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   /*Värden för aktuell katalog*/
   DEFINE PUBLIC PROPERTY EkgSubidVar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY EbrKatVar AS CHARACTER  NO-UNDO
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
   DEFINE PUBLIC PROPERTY tablage AS CHARACTER INITIAL "Start" NO-UNDO
      PUBLIC GET.
      PUBLIC SET.   
           
      
      
   CONSTRUCTOR PUBLIC EkgWinLg(INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlDataDb:ControlWinLg = THIS-OBJECT.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      
      {EKGDBH.i ControlDataDb}
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   METHOD PUBLIC INTEGER KlgTryckAvKost(INPUT avrundning AS DECIMAL, INPUT antaldec AS INTEGER ):
      /*
      IF ABSOLUTE(avrundning) > 99999 THEN DO:
         RETURN  INTEGER (ROUND(avrundning / 1000,antaldec) * 1000).
      END.
      ELSE IF ABSOLUTE(avrundning) > 9999 THEN DO:
         RETURN  INTEGER (ROUND(avrundning / 100,antaldec) * 100).
      END. 
      ELSE IF ABSOLUTE(avrundning) > 999 THEN DO:
         RETURN  INTEGER (ROUND(avrundning / 10,antaldec) * 10).
      END.   
      ELSE RETURN  INTEGER (ROUND(avrundning,antaldec)).
      */
      RETURN  INTEGER (ROUND(avrundning,0)).
   END METHOD.
   METHOD PUBLIC CHARACTER  KlgTryckAvTid(INPUT avrundning AS DECIMAL, INPUT antaldec AS INTEGER ):
      DEFINE VARIABLE avrundningCh AS CHARACTER NO-UNDO.
      IF ABSOLUTE(avrundning) >= 1 THEN DO:
         IF antaldec = 0 THEN avrundningCh = TRIM(STRING(ROUND(avrundning,antaldec),"->>>>>>>>>9")). 
         IF antaldec = 1 THEN avrundningCh = TRIM(STRING(ROUND(avrundning,antaldec),"->>>>>>>>>9.9")).
         IF antaldec = 2 THEN avrundningCh = TRIM(STRING(ROUND(avrundning,antaldec),"->>>>>>>>>9.99")).
         IF antaldec = 3 THEN avrundningCh = TRIM(STRING(ROUND(avrundning,antaldec),"->>>>>>>>>9.999")).
         RETURN avrundningCh.
      END.
      ELSE DO:
         avrundningCh = TRIM(STRING(ROUND(avrundning,2),"->>>>>>>>9.99")).
         RETURN avrundningCh.
      END. 
      
   END METHOD.
   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:ControlDataDb:LaddaKataloger().
      THIS-OBJECT:ControlDataDb:LaddaResurser().
      THIS-OBJECT:CreateControls().
      THIS-OBJECT:Root:WindowManager:Wnd:FormClosing:Subscribe(THIS-OBJECT:Avsluta).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:LaddaLabels().
      nivandvar = "".
      raknavar = "".
     
      THIS-OBJECT:CreateForm().
      THIS-OBJECT:VisaStart(?,?).
      THIS-OBJECT:RibbonToolKatalogKnapp:SharedPropsInternal:Visible = FALSE.
      THIS-OBJECT:RPControl:SplitContainerBas1:Panel1Collapsed = TRUE.
      THIS-OBJECT:ResursAdmin().
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
      THIS-OBJECT:MaterielControl = THIS-OBJECT:ControlShell:MaterielControl.
      THIS-OBJECT:RPControl = THIS-OBJECT:ControlShell:RPControl. 
      THIS-OBJECT:KatalogerControl = THIS-OBJECT:ControlShell:KatalogerControl.
      THIS-OBJECT:ExportControl = THIS-OBJECT:ControlShell:ExportControl.
      THIS-OBJECT:ResursControl = THIS-OBJECT:ControlShell:ResursControl.
      THIS-OBJECT:JamforControl = THIS-OBJECT:ControlShell:JamforControl.
      THIS-OBJECT:P2KomControl = THIS-OBJECT:ControlShell:P2KomControl.
               
   END METHOD.
   
   METHOD PUBLIC VOID Manual(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      DEFINE VARIABLE path AS CHARACTER.
      path = SEARCH("Modules\EkgData\EkgManual.pdf").
      System.Diagnostics.Process:Start(path).
   END METHOD.
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "Ekgshell",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT 0,INPUT "") .
   END METHOD.
      
   METHOD PUBLIC VOID Sparatab(INPUT valdtab AS CHARACTER):       
      IF valdtab = "Resurspris" THEN DO:
         THIS-OBJECT:ControlDataDb:Katalogresursspara().               
         THIS-OBJECT:ControlDataDb:Subkatspara().
         /*
         IF raknavar NE "" THEN DO:*/
         /*uppdatera alltid hela vägen vid spar Lena 20140320*/
            raknavar = "".
            THIS-OBJECT:ControlDataDb:BeraknaP5alla().
            THIS-OBJECT:ControlDataDb:p5spara().
              
            THIS-OBJECT:ControlDataDb:BeraknaP4allaresurs().
            THIS-OBJECT:ControlDataDb:BeraknaP4allafrek().
            THIS-OBJECT:ControlDataDb:p4spara().
            
            THIS-OBJECT:ControlDataDb:BeraknaP3allaresurs(THIS-OBJECT:EbrKatVar).
            THIS-OBJECT:ControlDataDb:BeraknaP3allafrek().
            THIS-OBJECT:ControlDataDb:p3spara().
            
            THIS-OBJECT:ControlDataDb:BeraknaP2allaresurs(THIS-OBJECT:EbrKatVar).
            THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
            THIS-OBJECT:ControlDataDb:BeraknaP2allamtrl().
            THIS-OBJECT:ControlDataDb:p2spara().
            
            THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
            THIS-OBJECT:ControlDataDb:p1spara().
         /*END.*/   
         THIS-OBJECT:LaddaOmGriddarResurspris().
         THIS-OBJECT:LaddaOmGriddarMtrl().                   
      END.
      IF valdtab = "Materiel" THEN DO:
         THIS-OBJECT:ControlDataDb:Subkatspara().
         IF raknavar NE "" THEN DO:
         /*uppdatera alltid hela vägen vid spar Lena 20160208*/
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
         THIS-OBJECT:ControlDataDb:p3spara().
         THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
         THIS-OBJECT:ControlDataDb:p2spara().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
         THIS-OBJECT:ControlDataDb:p1spara().
           
         THIS-OBJECT:LaddaOmGriddarP4(). 
      END.    
      IF valdtab = "P3" THEN DO:                
         THIS-OBJECT:ControlDataDb:p3spara().  
         THIS-OBJECT:ControlDataDb:ArbKoderSpara().         
         THIS-OBJECT:ControlDataDb:BeraknaP2allafrek().
         THIS-OBJECT:ControlDataDb:p2spara().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
         THIS-OBJECT:ControlDataDb:p1spara().
         THIS-OBJECT:LaddaOmGriddarP3().          
                   
      END.    
      IF valdtab = "P2" THEN DO:                
         THIS-OBJECT:ControlDataDb:p2spara().      
         THIS-OBJECT:ControlDataDb:ArbKoderSpara().
         THIS-OBJECT:ControlDataDb:BeraknaP1allafrek().
         THIS-OBJECT:ControlDataDb:p1spara().        
         THIS-OBJECT:LaddaOmGriddarP2().                          
      END.    
      IF valdtab = "P1" THEN DO:     
         THIS-OBJECT:ControlDataDb:p1spara().      
         THIS-OBJECT:ControlDataDb:ArbKoderSpara(). 
         THIS-OBJECT:LaddaOmGriddarP1().           
      END. 
      IF valdtab = "P2Kom" THEN DO:     
                   
         THIS-OBJECT:ControlDataDb:p2KomSpara().
         THIS-OBJECT:ControlDataDb:P2KomKontroll().
         THIS-OBJECT:P2KomControl:splitContainerAllt:Panel1Collapsed = TRUE.
         THIS-OBJECT:P2KomControl:splitContainerAllt:Panel2Collapsed = FALSE.
         THIS-OBJECT:P2KomControl:splitContainerVisaKontroll:Panel1Collapsed = TRUE.
         THIS-OBJECT:P2KomControl:splitContainerVisaKontroll:Panel2Collapsed = FALSE.
         THIS-OBJECT:P2KomControl:GridSvepKomKoll:GuruReopen().
         THIS-OBJECT:P2KomControl:GridSvepKomKoll:GuruUpdateTitle().        
      END. 
         
   END METHOD.
   
   METHOD PUBLIC VOID Laddaomtab(INPUT valdtab AS CHARACTER): 
      IF valdtab = "Resurspris" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaKatalogResurser(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar). 
         THIS-OBJECT:ControlDataDb:LaddaKatalog(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).         
         THIS-OBJECT:RPControl:GridResurspris:GuruReopen().
         THIS-OBJECT:RPControl:GridResursniva:GuruReopen().
         THIS-OBJECT:RPControl:GridResursPaslag:GuruReopen().
         THIS-OBJECT:RPControl:GridRegel:GuruReopen().      
         THIS-OBJECT:RPControl:GridPaslag:GuruReopen().
         THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().
      END.
      IF valdtab = "Materiel" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaKatalog(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).         
         THIS-OBJECT:RPControl:GridRegel:GuruReopen().      
         THIS-OBJECT:RPControl:GridPaslag:GuruReopen().
         THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().
         listekgmtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
         THIS-OBJECT:MaterielControl:GridListaEjAnvMtrl:GuruReopen().
      END.
      IF valdtab = "P5" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaP5(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).         
         THIS-OBJECT:LaddaOmGriddarP5().
      END.    
      IF valdtab = "P4" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaP4(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:LaddaOmGriddarP4().
      END.    
      IF valdtab = "P3" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar). 
         THIS-OBJECT:ControlDataDb:LaddaP3(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:LaddaOmGriddarP3().         
      END.    
      IF valdtab = "P2" THEN DO:                
         THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar). 
         THIS-OBJECT:ControlDataDb:LaddaP2(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).         
         THIS-OBJECT:LaddaOmGriddarP2().
      END.    
      IF valdtab = "P1" THEN DO:   
         THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar). 
         THIS-OBJECT:ControlDataDb:LaddaP1(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).         
         THIS-OBJECT:LaddaOmGriddarP1().
      END.
      IF valdtab = "P2kom" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaP2Kom(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:LaddaOmGriddarp2k().
         THIS-OBJECT:P2KomControl:GridArbLop:GuruFiltrera(THIS-OBJECT:P2KomControl:ArbLopFilter()).
                    
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
   
   METHOD PUBLIC VOID SparaResurs():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.  
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(139),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question). 
      IF rrr:ToString() = "Yes" THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
         THIS-OBJECT:ControlDataDb:ResursSpara().   
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).          
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      END.
      ELSE RETURN.  
      IF THIS-OBJECT:EbrKatVar NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(145),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
         THIS-OBJECT:ValjKatalog(THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      END.
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
      THIS-OBJECT:KatalogerControl:GridKataloger:GuruReopen().
      
      
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
      THIS-OBJECT:DGPS:AddChild(THIS-OBJECT:RPControl:gridResursPaslag).
      THIS-OBJECT:DGPS:AddChild(THIS-OBJECT:RPControl:gridPaslag).
      
         
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
        
      THIS-OBJECT:RPControl:buttonOverResurspaslagBort:GuruKoppla(THIS-OBJECT:RPControl:gridResursPaslag,THIS-OBJECT:RPControl:gridPaslag,TRUE,FALSE,FALSE).
      THIS-OBJECT:RPControl:buttonOverResurspaslag:GuruKoppla(THIS-OBJECT:RPControl:gridPaslag,THIS-OBJECT:RPControl:gridResursPaslag,FALSE,FALSE,FALSE).
      
      THIS-OBJECT:RPControl:buttonOverResurspris:GuruKoppla(THIS-OBJECT:RPControl:gridResursForResurspris,THIS-OBJECT:RPControl:gridResursPris,FALSE,FALSE,FALSE).
                                                         /*från grid                               till grid                 ta bort dubb  alla */
      THIS-OBJECT:P2KomControl:buttonOverKom:GuruKoppla(THIS-OBJECT:P2KomControl:GridKom,THIS-OBJECT:P2KomControl:GridKomVal,FALSE,FALSE ,FALSE).
      THIS-OBJECT:P2KomControl:buttonOverKomBort:GuruKoppla(THIS-OBJECT:P2KomControl:GridKomVal,THIS-OBJECT:P2KomControl:GridKom,TRUE,FALSE,FALSE).
      
      THIS-OBJECT:P2KomControl:buttonOverArbKom:GuruKoppla(THIS-OBJECT:P2KomControl:GridArbLop,THIS-OBJECT:P2KomControl:GridKoppladeKom,FALSE,TRUE,FALSE).
      THIS-OBJECT:P2KomControl:buttonOverArbKomBort:GuruKoppla(THIS-OBJECT:P2KomControl:GridKoppladeKom,THIS-OBJECT:P2KomControl:GridArbLop,TRUE,FALSE,FALSE).
      
       THIS-OBJECT:P2KomControl:buttonOverArbKomDirekt:GuruKoppla(THIS-OBJECT:P2KomControl:GridKomVal,THIS-OBJECT:P2KomControl:GridKoppladeKom,FALSE,TRUE ,TRUE).
      
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboP3ArbKoder().
      THIS-OBJECT:CreateComboP2ArbKoder().
      THIS-OBJECT:CreateComboP1ArbKoder().
      THIS-OBJECT:CreateComboNivaer().
      THIS-OBJECT:CreateComboTryckExp().
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
   
      
   METHOD PUBLIC VOID CreateComboTryckExp():
      DEFINE VARIABLE cmbTryckExplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE TryckExpK      AS Controls.GridRubrik      NO-UNDO.
      THIS-OBJECT:ControlDataDb:SkapaExportVal().
      cmbTryckExplist = NEW Controls.GridRubrikLista().
      cmbTryckExplist:ttBufferHandle = THIS-OBJECT:ekgexptryckvalTTh.
      TryckExpK = NEW Controls.GridRubrik().
      TryckExpK:Falt = "VAL".
      TryckExpK:Rubrik = "VAL".
      cmbTryckExplist:addRubrik(TryckExpK).
      THIS-OBJECT:ExportControl:comboTryckExp:GuruCombo:GuruInitCombo(cmbTryckExplist:ttBufferHandle, "VAL", "VAL").
      THIS-OBJECT:ExportControl:comboTryckExp:GuruLabel:Text = "Nivåer till tryck:".
      THIS-OBJECT:ekgexptryckvalTTh:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:ExportControl:comboTryckExp:GuruCombo:VALUE = THIS-OBJECT:ekgexptryckvalTTh:BUFFER-FIELD("VAL"):BUFFER-VALUE.
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboP3ArbKoder():
      DEFINE VARIABLE cmbP3ArbKoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE p3arbkodK      AS Controls.GridRubrik      NO-UNDO.
      cmbP3ArbKoderlist = NEW Controls.GridRubrikLista().
      cmbP3ArbKoderlist:ttBufferHandle = THIS-OBJECT:ekgp3arbkodTTh.
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
      /*
      THIS-OBJECT:CreateGridKataloger().
  
      THIS-OBJECT:CreateGridResurs().
      THIS-OBJECT:CreateGridRegel().
      THIS-OBJECT:CreateGridResurspris().
      THIS-OBJECT:CreateGridResursniva().
      THIS-OBJECT:CreateGridResursForResurspris().
      THIS-OBJECT:CreateGridPaslag().
      THIS-OBJECT:CreateGridResursPaslag().
  
      THIS-OBJECT:CreateGridMateriel().
      */
      /*P5
      THIS-OBJECT:CreateGridP5().  
      THIS-OBJECT:CreateGridP5Resurs().
      THIS-OBJECT:CreateGridResursForP5().
      THIS-OBJECT:CreateGridImportP5().
      */
      /*P4
      THIS-OBJECT:CreateGridP5ForP4().
      THIS-OBJECT:CreateGridP4().
      THIS-OBJECT:CreateGridP4Resurs().
      THIS-OBJECT:CreateGridResursForP4().
      THIS-OBJECT:CreateGridP4Frekvens().
      THIS-OBJECT:CreateGridImportP4().
      */
      /*P3
      THIS-OBJECT:CreateGridP4ForP3().
      THIS-OBJECT:CreateGridP3().
      THIS-OBJECT:CreateGridP3Resurs().
      THIS-OBJECT:CreateGridP3Frekvens().
      THIS-OBJECT:CreateGridResursForP3().
      THIS-OBJECT:CreateGridP3arbkod().
      THIS-OBJECT:CreateGridImportP3().
      */
      /*P2
      THIS-OBJECT:CreateGridP3ForP2().
      THIS-OBJECT:CreateGridP2().
      THIS-OBJECT:CreateGridP2Resurs().
      THIS-OBJECT:CreateGridP2Frekvens().
      THIS-OBJECT:CreateGridP2Mtrl().
      THIS-OBJECT:CreateGridMtrlForP2().
      THIS-OBJECT:CreateGridResursForP2().
      THIS-OBJECT:CreateGridP2arbkod().
      THIS-OBJECT:CreateGridImportP2().
      */
      /*P1
      THIS-OBJECT:CreateGridP2ForP1().
      THIS-OBJECT:CreateGridP1().
      THIS-OBJECT:CreateGridP1Frekvens().
      THIS-OBJECT:CreateGridP1arbkod().
      THIS-OBJECT:CreateGridImportP1().
      */
   END METHOD.
   
   METHOD PUBLIC VOID CreateRibbon():
        /*första ribbon tab*/
               
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule("EkgShell").
      
      THIS-OBJECT:RibbonGroupKatMeny = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("KatMeny").
      THIS-OBJECT:RibbonGroupSparaKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Hantera katalog").
      THIS-OBJECT:RibbonGroupKatInfo = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Katalog").
      THIS-OBJECT:RibbonGroupTabInfo = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sök arbetskod").
      THIS-OBJECT:RibbonGroupVisa = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visa").
      THIS-OBJECT:RibbonGroupFonster = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Fönster").
      THIS-OBJECT:RibbonGroupSpara = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Spara inställningar").
      
      /*KatalogMeny*/
      THIS-OBJECT:RibbonGroupKatMeny:PreferredToolSize = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      THIS-OBJECT:RibbonGroupKatMeny:Caption = "".
      THIS-OBJECT:RibbonToolHemKnapp = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Startmeny").
      THIS-OBJECT:RibbonToolHemKnappBildval = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Startmeny").
      THIS-OBJECT:RibbonToolHemKnapp:SharedPropsInternal:Caption = "Hem".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 6,INPUT THIS-OBJECT:RibbonToolHemKnappBildval,INPUT "").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolHemKnapp,INPUT "bilder/ekghem.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolHemKnapp,INPUT "bilder/ekghem.gif").
      /*THIS-OBJECT:Root:ModuleManager:RibbonImageBackground(INPUT THIS-OBJECT:RibbonToolHemKnapp,INPUT "bilder/ebr.jpg").*/
      
      THIS-OBJECT:RibbonToolHemKnapp:SharedPropsInternal:Visible = TRUE.   
      THIS-OBJECT:RibbonToolHemKnapp:ToolClick:Subscribe(THIS-OBJECT:VisaStart).
      THIS-OBJECT:RibbonToolKatalogKnapp = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Katalog").
      THIS-OBJECT:RibbonToolKatalogKnappBildval = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Katalog").
      THIS-OBJECT:RibbonToolKatalogKnapp:SharedPropsInternal:Caption = "Katalog".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 6,INPUT THIS-OBJECT:RibbonToolKatalogKnappBildval,INPUT "").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolKatalogKnapp,INPUT "bilder/ekgkat.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolKatalogKnapp,INPUT "bilder/ekgkat.gif").
      THIS-OBJECT:RibbonToolKatalogKnapp:SharedPropsInternal:Visible = TRUE.
      THIS-OBJECT:RibbonToolKatalogKnapp:ToolClick:Subscribe(THIS-OBJECT:VisaKatalog).
      
      /*SparaKatalog*/
      THIS-OBJECT:RibbonToolSparaKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Spara").
      THIS-OBJECT:RibbonToolSparaKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Spara").
      THIS-OBJECT:RibbonToolSparaKatalog:SharedPropsInternal:Caption = "Spara förändringar".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaKatalog,INPUT "bilder/sparakatmall.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSparaKatalog,INPUT "bilder/sparakatmall.gif").
      THIS-OBJECT:RibbonToolSparaKatalog:SharedPropsInternal:Visible = TRUE.
      /*StangKatalog*/
      THIS-OBJECT:RibbonToolStangKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Stang").
      THIS-OBJECT:RibbonToolStangKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Stang").
      THIS-OBJECT:RibbonToolStangKatalog:SharedPropsInternal:Caption = "Stäng katalog".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolStangKatalog,INPUT "bilder/stangkat3.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolStangKatalog,INPUT "bilder/stangkat3.gif").
      THIS-OBJECT:RibbonToolStangKatalog:SharedPropsInternal:Visible = TRUE.     
      /*/*BeraknaKatalog*/
      THIS-OBJECT:RibbonToolBeraknaKatalog = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Beräkna").
      THIS-OBJECT:RibbonToolBeraknaKatalog:SharedPropsInternal:Caption = "Beräkna".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolBeraknaKatalog,INPUT "bilder/kalkico.png").
      THIS-OBJECT:RibbonToolBeraknaKatalog:SharedPropsInternal:Visible = TRUE.*/ 
      
      /*KatInfo*/
      THIS-OBJECT:RibbonToolBenamning = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Benämning").
      THIS-OBJECT:RibbonToolBenamning:SharedPropsInternal:ToolTipText = "Katalogens benämning".
      THIS-OBJECT:RibbonToolBenamning:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolBenamning:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolEbrKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("EbrKat").
      THIS-OBJECT:RibbonToolEbrKat:SharedPropsInternal:ToolTipText = "Katalogens id och typ".
      THIS-OBJECT:RibbonToolEbrKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolEbrKat:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolArtal = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Årtal").
      THIS-OBJECT:RibbonToolArtal:SharedPropsInternal:ToolTipText = "Katalogens årtal".
      THIS-OBJECT:RibbonToolArtal:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolArtal:SharedPropsInternal:Width = 192.
      /*THIS-OBJECT:RibbonToolSubKatID = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("SubKatInfo").
      THIS-OBJECT:RibbonToolSubKatID:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolSubKatID:SharedPropsInternal:Width = 192.*/
      
      /*TabInfo*/
      THIS-OBJECT:RibbonGroupTabInfo:Visible = FALSE.
      THIS-OBJECT:RibbonToolAktuellTab = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Tab").
      THIS-OBJECT:RibbonToolAktuellTab:SharedPropsInternal:ToolTipText = "Aktuell flik".
      THIS-OBJECT:RibbonToolAktuellTab:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolAktuellTab:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolSokKod = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Sök").
      THIS-OBJECT:RibbonToolSokKod:SharedPropsInternal:ToolTipText = "Sök".
      THIS-OBJECT:RibbonToolSokKod:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolSokKod:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolSokKod:BeforeToolEnterEditMode:Subscribe(THIS-OBJECT:RibbonSokClick).
      /*THIS-OBJECT:RibbonToolSokKod:EditAppearance:BackColor = System.Drawing.Color:Azure.*/
      THIS-OBJECT:RibbonToolSokKod:EditAppearance:BorderColor = System.Drawing.Color:White.
      THIS-OBJECT:RibbonToolAktivKod = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Kod").
      THIS-OBJECT:RibbonToolAktivKod:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolAktivKod:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolAktivKod:SharedPropsInternal:ToolTipText = "Markerad kod".
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
      
      THIS-OBJECT:RibbonToolManual = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Manual").
      THIS-OBJECT:RibbonToolManual :SharedPropsInternal:Caption = "Manual".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolManual ,INPUT "bilder/kalkyl-manual2.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolManual ,INPUT "bilder/kalkyl-manual2.gif").
      THIS-OBJECT:RibbonToolManual:SharedPropsInternal:Visible = TRUE.
      
      /*prep*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolHemKnapp, THIS-OBJECT:RibbonGroupKatMeny).     
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKatalogKnapp, THIS-OBJECT:RibbonGroupKatMeny).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaKatalog, THIS-OBJECT:RibbonGroupSparaKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolStangKatalog, THIS-OBJECT:RibbonGroupSparaKatalog).
      /*THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolBeraknaKatalog, THIS-OBJECT:RibbonGroupSparaKatalog).*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolBenamning,THIS-OBJECT:RibbonGroupKatinfo).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEbrKat,THIS-OBJECT:RibbonGroupKatinfo).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolArtal,THIS-OBJECT:RibbonGroupKatinfo).        
      /*THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSubKatID,THIS-OBJECT:RibbonGroupKatinfo). */
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAktuellTab,THIS-OBJECT:RibbonGroupTabinfo). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAktivKod,THIS-OBJECT:RibbonGroupTabinfo). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSokKod,THIS-OBJECT:RibbonGroupTabinfo).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaAllaKoder,THIS-OBJECT:RibbonGroupVisa). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaKostnader,THIS-OBJECT:RibbonGroupVisa). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaEaMangder,THIS-OBJECT:RibbonGroupVisa). 
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFonsterKoder,THIS-OBJECT:RibbonGroupFonster). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFonsterUnderKoder,THIS-OBJECT:RibbonGroupFonster). 
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFonsterArbKoder,THIS-OBJECT:RibbonGroupFonster). 
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolManual,THIS-OBJECT:RibbonGroupKatMeny).
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
      THIS-OBJECT:RibbonToolManual:ToolClick:Subscribe(Manual).
      THIS-OBJECT:RibbonToolSparaKatalog:ToolClick:SUBSCRIBE(SparaKatalog).
      THIS-OBJECT:RibbonToolStangKatalog:ToolClick:SUBSCRIBE(StangKatalogClick).
      /*THIS-OBJECT:RibbonToolBeraknaKatalog:ToolClick:SUBSCRIBE(BeraknaKatalog).*/ 
      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"EkgShell",0,"",THIS-OBJECT:RibbonGroupVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"EkgShell",0,"",THIS-OBJECT:RibbonGroupFonster).
          
   END METHOD.
   
  METHOD PUBLIC VOID VisaRibbonGrupper(INPUT gomas AS LOGICAL):
      THIS-OBJECT:RibbonGroupSparaKatalog:VISIBLE = gomas.
      THIS-OBJECT:RibbonGroupKatInfo:VISIBLE = gomas.
      THIS-OBJECT:RibbonGroupTabInfo:VISIBLE = gomas.
      THIS-OBJECT:RibbonGroupVisa:VISIBLE = gomas.
      THIS-OBJECT:RibbonGroupFonster:VISIBLE = gomas.
      THIS-OBJECT:RibbonGroupSpara:VISIBLE = gomas.
  END METHOD.
  
  METHOD PUBLIC VOID VisaStart(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     THIS-OBJECT:VisaRibbonGrupper(FALSE).
     IF tablage ="katalog" THEN DO:
      THIS-OBJECT:TabEkg_SelectedTabChanging(THIS-OBJECT:ControlShell:TabManagerMain:SelectedTab:Key,THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:Key).
     END.
     IF tablage = "start" THEN DO: 
         THIS-OBJECT:ControlShell:TabManagerMain:Tabs["Kataloger"]:Selected = TRUE.   
     END.   
     THIS-OBJECT:ControlShell:shellMainPanel:Controls:Remove(THIS-OBJECT:ControlShell:katalogSC). 
     THIS-OBJECT:ControlShell:shellMainPanel:Controls:Add(THIS-OBJECT:ControlShell:startSC).
     tablage = "start".
  END METHOD.
  
  METHOD PUBLIC VOID VisaKatalog(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     THIS-OBJECT:VisaRibbonGrupper(TRUE).
     IF tablage ="start" THEN DO:
      THIS-OBJECT:TabEkg_SelectedTabChanging(THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:Key,THIS-OBJECT:ControlShell:TabManagerMain:SelectedTab:Key).
     END.
     THIS-OBJECT:ControlShell:shellMainPanel:Controls:Remove(THIS-OBJECT:ControlShell:startSC). 
     THIS-OBJECT:ControlShell:shellMainPanel:Controls:Add(THIS-OBJECT:ControlShell:katalogSC).
     tablage = "katalog".
     THIS-OBJECT:UppdateraKatalogRibbon().
  END METHOD.
  
  METHOD PUBLIC VOID FelMeddCheck():
     THIS-OBJECT:tiduth:FIND-FIRST() NO-ERROR.
     IF THIS-OBJECT:tiduth:AVAILABLE THEN.      
      
  END METHOD.
   
   /*EVENTS*/
  METHOD PUBLIC VOID ExportKolFix(INPUT cRad AS CHARACTER):
     THIS-OBJECT:VisaExcel:KolumnFormat("A1:" + "A" + cRad,"@").
     THIS-OBJECT:VisaExcel:KolumnFormat("B1:" + "B" + cRad,"@").        
  END METHOD.
    
    
  
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
         IF THIS-OBJECT:P2Control:GridImportP2:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P2Control:GridImportP2:GuruBortrader(TRUE). 
            IF THIS-OBJECT:importP2TTh:AVAILABLE THEN.
            ELSE DO: 
               THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = FALSE. 
               THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = FALSE.
            END.
         END. 
         IF THIS-OBJECT:P2Control:GridImportP2M:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P2Control:GridImportP2M:GuruBortrader(TRUE). 
            IF THIS-OBJECT:importP2MTTh:AVAILABLE THEN.
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
      IF listaP = "Mtrl" THEN DO:
         THIS-OBJECT:MaterielControl:GridImportMtrl:GuruBortrader(TRUE).                      
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
            IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP5TTh:BUFFER-DELETE().
            ELSE DO:
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
                        /*nivandvar = "1".*/                          
                        q2H:GET-NEXT().         
                     END. 
                     THIS-OBJECT:ControlDataDb:BeraknaP5en().
                     nivandvar = "1".                      
                  END.
               END.
               ELSE DO:  
                  /*skapar ny kod*/                   
                  rid = THIS-OBJECT:ControlDataDb:NyP5LopNr(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                  THIS-OBJECT:ekgp5TTh:FIND-BY-ROWID(rid).
                  IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                  nivandvar = "1".             
               END.
               
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
                  nivandvar = "1".
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
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4frekvensTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp4frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.                     
                  /*TÖM RESURS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgP4resursTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP4resursTTh,query2var).
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
               IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP4TTh:BUFFER-DELETE().
               ELSE DO:
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
                           /*nivandvar = "1".*/                          
                           q2H:GET-NEXT().         
                        END. 
                        THIS-OBJECT:ControlDataDb:BeraknaP4enresurs().
                        nivandvar = "1".                      
                     END.
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP4LopNr(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                     THIS-OBJECT:ekgP4TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     nivandvar = "1".             
                  END.
                  THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.
               END.   
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
                        nivandvar = "1".
                     END.
                  END. 
                  /*TÖM FREKVENS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp4frekvensTTh:TABLE + " WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'".
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp4frekvensTTh,query2var).
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
         THIS-OBJECT:P4Control:GridP4:GuruReopen().
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
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3frekvensTTh,query2var).
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
            queryvar =  "FOR EACH " + THIS-OBJECT:importP3TTh:TABLE + " WHERE P3LOPNR = 0".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP3TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP3TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ekgp3arbkodTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
                  IF THIS-OBJECT:ekgp3arbkodTTh:AVAILABLE THEN.
                  ELSE DO:
                     rid = THIS-OBJECT:ControlDataDb:NyP3ArbKod(TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar). 
                     THIS-OBJECT:ekgp3arbkodTTh:FIND-BY-ROWID(rid).
                     nivandvar = "8".                     
                  END.
                  IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.  
                  importP3TTh:BUFFER-DELETE().
               END.   
               qH:GET-NEXT().         
            END. 
            queryvar =  "FOR EACH " + THIS-OBJECT:importP3TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP3TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN importP3TTh:BUFFER-DELETE().
               ELSE IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP3TTh:BUFFER-DELETE().            
               ELSE DO:
                  THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE.
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
                              /*nivandvar = "1".*/  
                           END.                           
                           q2H:GET-NEXT().         
                        END. 
                        THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(THIS-OBJECT:EbrKatVar).
                        nivandvar = "1".                      
                     END.
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP3LopNr(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                     THIS-OBJECT:ekgP3TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     nivandvar = "1".             
                  END.
                  THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.
               END.
               qH:GET-NEXT().            
            END. 
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*skapar ny P3resurs*/
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' +  STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:  
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        /*ej 101-tillkommande beredning*/
                        IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 101 THEN.
                        ELSE DO:
                           THIS-OBJECT:ekgP3resursTTh:BUFFER-CREATE().
                           THIS-OBJECT:ekgP3resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                           THIS-OBJECT:ekgP3resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                           THIS-OBJECT:ControlDataDb:P3ResursUpd().
                           THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(THIS-OBJECT:EbrKatVar).
                           nivandvar = "1".
                        END.   
                     END.
                  END. 
                  /*TÖM FREKVENS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp3frekvensTTh:TABLE + " WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + " AND P3LOPNR = " + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp3frekvensTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp3frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.     
               END.
               ELSE DO:
                  /* om frekvens tillåt bara resurser som är utöver frekvens*/
                  THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' +  STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = TRUE  " ), NO-LOCK) NO-ERROR.                                           
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgP3resursTTh:BUFFER-CREATE().
                        THIS-OBJECT:ekgP3resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                        THIS-OBJECT:ekgP3resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                        THIS-OBJECT:ControlDataDb:P3ResursUpd().
                        THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(THIS-OBJECT:EbrKatVar).
                        nivandvar = "1".
                     END.
                  END.                     
               END.   
               qH:GET-NEXT().         
            END.   
            importP3TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P3Control:GridImportP3:GuruReopen().
         END.
         THIS-OBJECT:P3Control:GridP3:GuruReopen().
         THIS-OBJECT:P3Control:GridP3arbkod:GuruReopen().
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
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2frekvensTTh,query2var).
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
               THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
               IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE.
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
         IF THIS-OBJECT:P2Control:GridImportP2:VISIBLE = TRUE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP2TTh:TABLE + " WHERE P2LOPNR = 0".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP2TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ekgP2arbkodTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
                  IF THIS-OBJECT:ekgP2arbkodTTh:AVAILABLE THEN.
                  ELSE DO:
                     rid = THIS-OBJECT:ControlDataDb:NyP2ArbKod(TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                     nivandvar = "8". 
                     THIS-OBJECT:ekgP2arbkodTTh:FIND-BY-ROWID(rid).
                  END.
                  IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.  
                  importP2TTh:BUFFER-DELETE().
               END.   
               qH:GET-NEXT().         
            END. 
            queryvar =  "FOR EACH " + THIS-OBJECT:importP2TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN importP2TTh:BUFFER-DELETE().
               ELSE IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP2TTh:BUFFER-DELETE().            
               ELSE DO:
                  THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     THIS-OBJECT:ekgP2resursTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                     IF THIS-OBJECT:ekgP2resursTTh:AVAILABLE THEN DO:               
                        /*TAR BORT ALLA TIDIGARE RESURSER*/
                        query2var =  "FOR EACH " + THIS-OBJECT:ekgP2resursTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                        q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2resursTTh,query2var).
                        q2H:GET-FIRST().
                        DO WHILE q2H:QUERY-OFF-END = FALSE:
                           ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:ekgp2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE ), NO-LOCK) NO-ERROR.
                           IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                              THIS-OBJECT:ekgp2resursTTh:BUFFER-DELETE().
                              nivandvar = "1".  
                           END.                           
                           q2H:GET-NEXT().         
                        END. 
                        THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(THIS-OBJECT:EbrKatVar).
                        nivandvar = "1".                      
                     END.
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP2LopNr(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                     THIS-OBJECT:ekgP2TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     nivandvar = "1".             
                  END.
                  THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.                  
               END.
               qH:GET-NEXT().            
            END. 
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*skapar ny P2resurs*/
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' +  STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:  
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 101 THEN.
                        ELSE DO:
                           THIS-OBJECT:ekgP2resursTTh:BUFFER-CREATE().
                           THIS-OBJECT:ekgP2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                           THIS-OBJECT:ekgP2resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                           THIS-OBJECT:ControlDataDb:P2ResursUpd().
                           THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(THIS-OBJECT:EbrKatVar).
                           nivandvar = "1".
                        END.   
                     END.
                  END. 
                  /*TÖM FREKVENS*/
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp2frekvensTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2frekvensTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp2frekvensTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.     
               END.
               ELSE DO:
                  /* om frekvens tillåt bara resurser som är utöver frekvens*/
                  THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' +  STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = TRUE  " ), NO-LOCK) NO-ERROR.                       
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        THIS-OBJECT:ekgP2resursTTh:BUFFER-CREATE().
                        THIS-OBJECT:ekgP2resursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE.
                        THIS-OBJECT:ekgP2resursTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                        THIS-OBJECT:ControlDataDb:P2ResursUpd().
                        THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(THIS-OBJECT:EbrKatVar).
                        nivandvar = "1".
                     END.
                  END.   
               END.   
               qH:GET-NEXT().         
            END.   
            importP2TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P2Control:GridImportP2:GuruReopen().
         END.
         IF THIS-OBJECT:P2Control:GridImportP2M:VISIBLE = TRUE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:importP2MTTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2MTTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*TÖM MATERIEL*/
               IF THIS-OBJECT:importP2MTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN importP2MTTh:BUFFER-DELETE().
               ELSE DO:
                  query2var =  "FOR EACH " + THIS-OBJECT:ekgp2mtrlTTh:TABLE + " WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgp2mtrlTTh,query2var).
                  q2H:GET-FIRST().
                  DO WHILE q2H:QUERY-OFF-END = FALSE:
                     THIS-OBJECT:ekgp2mtrlTTh:BUFFER-DELETE().
                     nivandvar = "2".
                     q2H:GET-NEXT().         
                  END.                                      
               END.                 
               qH:GET-NEXT().
            END.                              
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
               IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
                  /*skapar ny p2mtrl*/
                  THIS-OBJECT:ekgp2mtrlTTh:BUFFER-CREATE().
                  THIS-OBJECT:ekgp2mtrlTTh:BUFFER-COPY(importP2MTTh).
                  THIS-OBJECT:ekgp2mtrlTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = ekgp2mtrlTTh:RECID.
                  THIS-OBJECT:ekgp2mtrlTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE = THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE.
                  THIS-OBJECT:ekgp2mtrlTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.
                  THIS-OBJECT:ControlDataDb:LaddaP2VFmtrlsingel().
                  THIS-OBJECT:ControlDataDb:BeraknaP2mtrl().

                  /*
                  IF THIS-OBJECT:importP2MTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                  */
                  nivandvar = "2".
               END.   
               qH:GET-NEXT().
            END. 
            qH:GET-FIRST().
            importP2MTTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P2Control:GridImportP2M:GuruReopen().
            THIS-OBJECT:P2Control:GridP2mtrl:GuruReopen().
         END.

         THIS-OBJECT:P2Control:GridP2:GuruReopen().
         THIS-OBJECT:P2Control:GridP2arbkod:GuruReopen().
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
                  q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP1frekvensTTh,query2var).
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
                  THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
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
            queryvar =  "FOR EACH " + THIS-OBJECT:importP1TTh:TABLE + " WHERE P1LOPNR = 0".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP1TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP1TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ekgP1arbkodTTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
                  IF THIS-OBJECT:ekgP1arbkodTTh:AVAILABLE THEN.
                  ELSE DO:
                     rid = THIS-OBJECT:ControlDataDb:NyP1ArbKod(TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar). 
                     THIS-OBJECT:ekgP1arbkodTTh:FIND-BY-ROWID(rid).
                     nivandvar = "8".
                  END.
                  IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.  
                  importP1TTh:BUFFER-DELETE().
               END.   
               qH:GET-NEXT().         
            END. 
            queryvar =  "FOR EACH " + THIS-OBJECT:importP1TTh:TABLE.
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP1TTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN importP1TTh:BUFFER-DELETE().
               ELSE IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112 THEN importP1TTh:BUFFER-DELETE().
               ELSE DO:
                  THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgP1TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     nivandvar = "1".
                  END.
                  ELSE DO:  
                     /*skapar ny kod*/ 
                     rid = THIS-OBJECT:ControlDataDb:NyP1LopNr(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                     THIS-OBJECT:ekgP1TTh:FIND-BY-ROWID(rid).
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                     nivandvar = "1".             
                  END.
                  THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = THIS-OBJECT:importP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE.                  
               END.
               qH:GET-NEXT().            
            END. 
            
            importP1TTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:P1Control:GridImportP1:GuruReopen().

         END.
         THIS-OBJECT:P1Control:GridP1:GuruReopen().
         THIS-OBJECT:P1Control:GridP1arbkod:GuruReopen().
         THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = FALSE. 
         THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = FALSE.                  
      END.
      IF listaP = "Mtrl" THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:importMtrlTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importMtrlTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN importMtrlTTh:BUFFER-DELETE().
            ELSE DO:
               THIS-OBJECT:ekgMtrlTTh:FIND-FIRST("WHERE ARTNR = '"  + TRIM(THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgMtrlTTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                  IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE.
                  THIS-OBJECT:MaterielUppdate("IMPORTPRIS", STRING(THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).              
               END.
               ELSE DO:  
                  /*skapar ny kod*/ 
                  rid = THIS-OBJECT:ControlDataDb:NyMtrl(THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
                  THIS-OBJECT:ekgMtrlTTh:FIND-BY-ROWID(rid).
                  IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
                  IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE.
                  THIS-OBJECT:MaterielUppdate("IMPORTPRIS", STRING(THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
                                  
               END.
            END.
            qH:GET-NEXT().            
         END. 
         
         importMtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:MaterielControl:GridImportMtrl:GuruReopen().
         THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().            
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
      IF listaP = "P2M" THEN importP2MTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      IF listaP = "Mtrl" THEN importMtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ExcelImportLista(fildir,listaP).

   END METHOD.   
    
   METHOD PRIVATE VOID ExcelImportLista(filnamn AS CHARACTER,listaP AS CHARACTER):
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(86),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN THIS-OBJECT:VisaExcel:iRad = 3.
      ELSE THIS-OBJECT:VisaExcel:iRad = 2.
      Guru.Konstanter:AmericanSet().
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      
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
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
                  THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                  IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:                     
                     THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSBENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgresursForTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  END.
               END.      
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
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
                  THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
                  THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                  IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:                     
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSBENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgresursForTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  END.
               END.   
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
               IF THIS-OBJECT:VisaExcel:cColname = "H" THEN DO:
                  THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
                  THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                  IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:                     
                     THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSBENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgresursForTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  END.
               END.   
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
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
                  THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = TRIM(invar).
                  THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                  IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:                     
                     THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSBENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgresursForTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                  END.
               END.   
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
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = TRIM(invar).                  .   
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
            IF listaP = "P2M" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importP2MTTh:BUFFER-CREATE().
                  THIS-OBJECT:importP2MTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importP2MTTh:RECID.
                  THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importP2MTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE = TRIM(invar).                                   .   
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importP2MTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = TRIM(invar).               
            END.
            IF listaP = "Mtrl" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:importMtrlTTh:BUFFER-CREATE().
                  THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = importMtrlTTh:RECID.
                  THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE = TRIM(invar).
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = TRIM(invar).               
            END.         
         END.
         ELSE DO:
            /*matcha in benämningar om de utelämnats*/
            IF listaP = "P4F" THEN DO:               
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp4TTh:FIND-FIRST("WHERE P4ARBKOD = "  + THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.   
               END.                  
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp5TTh:FIND-FIRST("WHERE P5ARBKOD = "  + THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P5BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.                                
            END.
            
            IF listaP = "P3F" THEN DO:               
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.                     
                     IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.                  
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'" + ' AND P4LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)) NO-ERROR.                     
                     IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P4BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.                                
            END. 
             IF listaP = "P2F" THEN DO:               
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.                     
                     IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.                  
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = "" THEN DO:                     
                     THIS-OBJECT:ekgp3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.                     
                     IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.                                
            END.
            IF listaP = "P1F" THEN DO:               
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.                     
                     IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = TRIM(invar).
                  IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = "" THEN DO:
                     THIS-OBJECT:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.                     
                     IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN DO:                     
                        THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
                     END.
                  END.
               END.               
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
         IF listaP = "P2M" THEN IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:VisaExcel:cColname = "A".
         IF listaP = "Mtrl" THEN IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:VisaExcel:cColname = "A".                 
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().
      END.  
      THIS-OBJECT:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      
      IF listaP = "P5" THEN THIS-OBJECT:ImportListaP5().
      IF listaP = "P4" THEN THIS-OBJECT:ImportListaP4().
      IF listaP = "P3" THEN THIS-OBJECT:ImportListaP3().
      IF listaP = "P2" THEN THIS-OBJECT:ImportListaP2().
      IF listaP = "P1" THEN THIS-OBJECT:ImportListaP1().
      
      IF listaP = "P4F" THEN THIS-OBJECT:ImportListaP4F().
      IF listaP = "P3F" THEN THIS-OBJECT:ImportListaP3F().
      IF listaP = "P2F" THEN THIS-OBJECT:ImportListaP2F().
      IF listaP = "P1F" THEN THIS-OBJECT:ImportListaP1F().
      IF listaP = "P2M" THEN THIS-OBJECT:ImportListaP2M().
      IF listaP = "Mtrl" THEN THIS-OBJECT:ImportListaMtrl().
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP5():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP5TTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP5TTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP5TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP5TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP5TTh:BUFFER-DELETE().
         ELSE DO:
            THIS-OBJECT:ekgp5TTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
               THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
               THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
               THIS-OBJECT:ekgp5resursTTh:FIND-FIRST("WHERE P5ARBKOD = '"  + TRIM(THIS-OBJECT:importP5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "' AND RESURSNR = "  + THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
               IF THIS-OBJECT:ekgp5resursTTh:AVAILABLE THEN DO:               
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(101).
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 101.                      
               END.
            END.      
            ELSE DO:    
               IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = ""  THEN DO:
                   THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(153).
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
               END.
               ELSE IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = ""  THEN DO:
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(154).
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
               END.
               ELSE DO:                                
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
                  THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
               END.   
            END.   
            THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP5TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
            ELSE DO:
               THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
               IF THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
            END.
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
         IF TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP4TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP4TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP4TTh:BUFFER-DELETE().
         ELSE DO:            
            IF ControlDataDb:FrekkollImport("P4", THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE,0,THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE) = TRUE THEN DO:
               /*olika poster för samma kod har angett frekvens = Ja och frekvens = nej  */
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(155).
               THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
            END.   
            ELSE DO:                                
               THIS-OBJECT:ekgP4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE NE THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE THEN DO:            
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
                  END.
                  
                  THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
                  THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
                  THIS-OBJECT:ekgp4resursTTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "' AND RESURSNR = "  + THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                  IF THIS-OBJECT:ekgp4resursTTh:AVAILABLE THEN DO:               
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(101).
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 101.                      
                  END.
               END.      
               ELSE DO:
                  IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = ""  THEN DO:
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(153).
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                  END.
                  ELSE IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = ""  THEN DO:
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(154).
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                  END.
                  ELSE DO:                    
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
                  END.   
               END.   
               IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                  IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
                  ELSE DO:
                     THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                     IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                  END.
                  IF THIS-OBJECT:ekgP4TTh:AVAILABLE THEN DO:
                     IF THIS-OBJECT:ekgP4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
                        THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(146).
                        IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.               
                     END.     
                  END.   
               END.
               ELSE DO:
                   ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = TRUE  " ), NO-LOCK) NO-ERROR.             
                   IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.                   
                   ELSE IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 0 THEN.
                   ELSE DO:
                      THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                      IF THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                   END.         
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
         IF TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP4P5TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP4P5TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP4P5TTh:BUFFER-DELETE().
         ELSE DO:
            THIS-OBJECT:ekgp4TTh:FIND-FIRST("WHERE P4ARBKOD = '"  + TRIM(THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  /*har resurs*/            
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
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
               IF THIS-OBJECT:ekgp4TTh:AVAILABLE AND THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
                  THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
                  IF THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP4P5TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
               END.
            END.
         END.           
         qH:GET-NEXT().         
      END. 
      qH:GET-FIRST().
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
         IF TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP3TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP3TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP3TTh:BUFFER-DELETE().
         ELSE DO:
            IF ControlDataDb:FrekkollImport("P3", THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE,THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE) = TRUE THEN DO:
               /*olika poster för samma kod har angett frekvens = Ja och frekvens = nej  */
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(155).
               THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
            END.   
            ELSE DO:
               THIS-OBJECT:ekgp3arbkodTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgp3arbkodTTh:AVAILABLE THEN DO:
                  /*arbetskod finns*/             
               END.
               ELSE DO:
                  IF ControlDataDb:FinnsArbkodImport("P3",THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) = TRUE THEN DO:
                     /*ny arbetskod*/
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).                    
                     ELSE THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(147).
                     THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99. 
                  END.   
                  ELSE DO:
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE = 0 AND THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "" THEN DO:
                        /*kod kommer inte att läsasd in*/
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(153).
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110.
                     END.
                     ELSE DO:    
                        /*kod kommer inte att läsasd in*/
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110.
                     END.    
                  END.   
               END.   
               IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE = 0 THEN.
               ELSE IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN.
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
                     IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = ""  THEN DO:
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(153).
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                     END.
                     ELSE IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = ""  THEN DO:
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(154).
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                     END.
                     ELSE DO:        
                        /*ny kod*/            
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(147).
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
                     END.   
                  END.   
                  IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 101 THEN DO:
                           THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(156).
                           IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                        END.   
                     END.   
                     ELSE DO:
                        /*ogiltig resurs*/
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                        IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                     END.
                     
                     IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
                        IF THIS-OBJECT:ekgP3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
                           THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(146).
                           IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.               
                        END.     
                     END. 
                  END.
                  ELSE DO:
                     ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = TRUE  " ), NO-LOCK) NO-ERROR.             
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
                     ELSE IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 0 THEN.                     
                     ELSE DO:
                        THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                        IF THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                     END.         
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
         IF TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP3P4TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP3P4TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP3P4TTh:BUFFER-DELETE().
         ELSE DO:            
            THIS-OBJECT:ekgp3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  /*har resurs*/            
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
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
               THIS-OBJECT:ekgp3frekvensTTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.               
               IF THIS-OBJECT:ekgp3frekvensTTh:AVAILABLE THEN DO:
                  /*kod har redan frekv*/
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
                  IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
               END. 
               IF THIS-OBJECT:ekgp3TTh:AVAILABLE AND THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
                  THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
                  IF THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP3P4TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
               END.     
            END.
         END.   
         qH:GET-NEXT().         
      END. 
      qH:GET-FIRST().
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
         IF TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP2TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP2TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP2TTh:BUFFER-DELETE().
         ELSE DO:
            IF ControlDataDb:FrekkollImport("P2", THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE,THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE) = TRUE THEN DO:
               /*olika poster för samma kod har angett frekvens = Ja och frekvens = nej  */
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(155).
               THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
            END.   
            ELSE DO:
               THIS-OBJECT:ekgp2arbkodTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
               IF THIS-OBJECT:ekgp2arbkodTTh:AVAILABLE THEN DO:
                  /*arbetskod finns*/
                   
               END.
               ELSE DO:
                  IF ControlDataDb:FinnsArbkodImport("P2",THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) = TRUE THEN DO:
                     /*ny arbetskod*/
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
                     ELSE THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(147).
                     THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99. 
                  END.   
                  ELSE DO:
                     /*kod kommer inte att läsasd in*/
                     THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
                     THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110. 
                  END.   
               END.  
               IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = 0 THEN.
               ELSE IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN.
               ELSE DO:
                  THIS-OBJECT:ekgp2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                  IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN DO:
                     /*byte frekvens resurs*/
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE NE THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE THEN DO:            
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
                     IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = ""  THEN DO:
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(153).
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                     END.
                     ELSE IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = ""  THEN DO:
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(154).
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                     END.
                     ELSE DO:        
                        /*ny kod*/            
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(147).
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
                     END.  
                  END.   
                  IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                     THIS-OBJECT:ekgresursForTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE) NO-ERROR.
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN DO:
                        IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 101 THEN DO:
                           THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(156).
                           IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                        END.
                     END.   
                     ELSE DO:
                        /*ogiltig resurs*/
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                        IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                     END.
                     IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
                        IF THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
                           THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(146).
                           IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.               
                        END.     
                     END. 
                  END.
                  ELSE DO:                     
                     ekgresursForTTh:FIND-FIRST("WHERE ekgresursForTT.RESURSNR = " + STRING(THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + " AND ekgresursForTT.UTOVERFREK = TRUE  " ), NO-LOCK) NO-ERROR.             
                     IF THIS-OBJECT:ekgresursForTTh:AVAILABLE THEN.
                     ELSE IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE = 0 THEN.
                     ELSE DO:
                        THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(100).
                        IF THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 0 OR THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99 THEN THIS-OBJECT:importP2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 100.
                     END.         
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
         IF TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP2P3TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP2P3TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP2P3TTh:BUFFER-DELETE().
         ELSE DO:
            THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN DO:
                  /*har resurs*/            
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(111).
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 111.
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
               THIS-OBJECT:ekgP3TTh:FIND-FIRST("WHERE P3ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "'" + ' AND P3LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)) NO-ERROR.               
               IF THIS-OBJECT:ekgP3TTh:AVAILABLE THEN DO:
               END.
               ELSE DO:
                   /*kod finns ej*/
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
               END.   
               THIS-OBJECT:ekgP2frekvensTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.               
               IF THIS-OBJECT:ekgP2frekvensTTh:AVAILABLE THEN DO:
                  /*kod har redan frekv*/
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
                  IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
               END. 
               IF THIS-OBJECT:ekgp2TTh:AVAILABLE AND THIS-OBJECT:ekgP2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
                  THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
                  IF THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP2P3TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
               END.     
            END.
         END.   
         qH:GET-NEXT().         
      END. 
      qH:GET-FIRST().
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
         IF TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP1TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP1TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP1TTh:BUFFER-DELETE().
         ELSE DO:
            THIS-OBJECT:ekgP1arbkodTTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgP1arbkodTTh:AVAILABLE THEN DO:
               /*arbetskod finns*/
                
            END.
            ELSE DO:
               IF ControlDataDb:FinnsArbkodImport("P1",THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) = TRUE THEN DO:
                  /*ny arbetskod*/
                  IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE = 0 THEN THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(99).
                  ELSE THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(147).
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99. 
               END.   
               ELSE DO:
                  /*kod kommer inte att läsasd in*/
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(110).
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110. 
               END.   
            END.  
            IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE = 0 THEN.
            ELSE IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 110 THEN.
            ELSE DO:
               THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
               IF THIS-OBJECT:ekgP1TTh:AVAILABLE THEN DO:
                  /*ersätter befintlig*/
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(98).
                  THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.               
               END.      
               ELSE DO:
                  IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = ""  THEN DO:
                     THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(153).
                     THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                  END.
                  ELSE IF THIS-OBJECT:importP1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = ""  THEN DO:
                     THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(154).
                     THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 112.
                  END.
                  ELSE DO:        
                     /*ny kod*/            
                     THIS-OBJECT:importP1TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(147).
                     THIS-OBJECT:importP1TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
                  END.   
               END.                            
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
         IF TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP1P2TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP1P2TTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP1P2TTh:BUFFER-DELETE().
         ELSE DO:
            THIS-OBJECT:ekgP1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.
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
               THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.               
               IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
               END.
               ELSE DO:
                   /*kod finns ej*/
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(106).
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
               END.   
               
               THIS-OBJECT:ekgP1frekvensTTh:FIND-FIRST("WHERE P1ARBKOD = '"  + TRIM(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "'" + ' AND P1LOPNR = ' + STRING(THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)) NO-ERROR.               
               IF THIS-OBJECT:ekgP1frekvensTTh:AVAILABLE THEN DO:
                  /*kod har redan frekv*/
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(107).
                  IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 107.
               END. 
               /*IF THIS-OBJECT:ekgp1TTh:AVAILABLE AND THIS-OBJECT:ekgP1TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE  = FALSE THEN DO:
                  THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(108).
                  IF THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
                  ELSE THIS-OBJECT:importP1P2TTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 108.
               END.*/     
            END.
         END.   
         qH:GET-NEXT().         
      END. 
      qH:GET-FIRST().
      IF THIS-OBJECT:importP1P2TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P1Control:StripButtonP1Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P1Control:StripButtonP1Rin:VISIBLE = TRUE.
      END. 
      THIS-OBJECT:P1Control:GridImportP1F:GuruReopen().  

            
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaP2M():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importP2MTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importP2MTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         /*IF TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Gula" THEN THIS-OBJECT:importP2MTTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Resurs" THEN THIS-OBJECT:importP2MTTh:BUFFER-DELETE().
         ELSE IF TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) BEGINS "Gröna" THEN THIS-OBJECT:importP2MTTh:BUFFER-DELETE().
         ELSE DO:*/
            THIS-OBJECT:ekgP2TTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:ekgP2TTh:AVAILABLE THEN DO:
              /*kod finns*/
            END.
            ELSE DO:
               /*kod finns ej*/
               THIS-OBJECT:importP2MTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(158).
               THIS-OBJECT:importP2MTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END. 
            THIS-OBJECT:ekgMtrlTTh:FIND-FIRST("WHERE ARTNR = '"  + TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE) + "'") NO-ERROR.
            IF THIS-OBJECT:ekgMtrlTTh:AVAILABLE THEN DO:
            END.
            ELSE DO:
               /*Materiel finns ej*/
               THIS-OBJECT:importP2MTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(159).
               THIS-OBJECT:importP2MTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END.
            IF THIS-OBJECT:importP2MTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106 THEN.
            ELSE DO:
               THIS-OBJECT:ekgP2MtrlTTh:FIND-FIRST("WHERE P2ARBKOD = '"  + TRIM(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "'" + ' AND P2LOPNR = ' + STRING(THIS-OBJECT:importP2MTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)) NO-ERROR.
                 IF THIS-OBJECT:ekgP2MtrlTTh:AVAILABLE THEN DO:
                 /*Kod har materiel sedan tidigare*/    
                 THIS-OBJECT:importP2MTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(160).
                 THIS-OBJECT:importP2MTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 98.
                 END.            
                 ELSE DO:
                 /*Kod har inte matriel sedan tidigare*/
                 THIS-OBJECT:importP2MTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(161).
                 THIS-OBJECT:importP2MTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
                 END. 
            END.
         qH:GET-NEXT().         
      END. 
      qH:GET-FIRST().
      IF THIS-OBJECT:importP2MTTh:AVAILABLE THEN DO:
         THIS-OBJECT:P2Control:StripButtonP2Rbort:VISIBLE = TRUE. 
         THIS-OBJECT:P2Control:StripButtonP2Rin:VISIBLE = TRUE.
      END. 
      THIS-OBJECT:P2Control:GridImportP2M:GuruReopen().
                     
   END METHOD.
   
   METHOD PRIVATE VOID ImportListaMtrl():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:importMtrlTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:importMtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgMtrlTTh:FIND-FIRST("WHERE ARTNR = '"  + TRIM(THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE) + "'") NO-ERROR.
         IF THIS-OBJECT:ekgMtrlTTh:AVAILABLE THEN DO:
            /*materiel finns*/
            /*materiel är låst för uppdatering*/
            IF THIS-OBJECT:ekgMtrlTTh:BUFFER-FIELD("LASTA"):BUFFER-VALUE = TRUE THEN DO:
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(133).
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
            END.
            ELSE DO:
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(129).
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
            END.
         END.
         ELSE DO:
            /*materiel finns ej*/
            /*saknar benämning*/
            IF THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "" THEN DO:
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(131).
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 106.
            END.
            ELSE DO:
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("MEDDELANDE"):BUFFER-VALUE  = THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(130).
               THIS-OBJECT:importMtrlTTh:BUFFER-FIELD("INNR"):BUFFER-VALUE = 99.
            END.
         END.
         qH:GET-NEXT().         
      END. 

      THIS-OBJECT:MaterielControl:GridImportMtrl:GuruReopen().           
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
         IF kolnamn = "BENAMNING" THEN DO:            
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(135),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               /*nivandvar = "12".*/
               RETURN TRUE.
            END.
            ELSE RETURN FALSE.
         END.
         IF kolnamn = "ENHET" THEN DO:            
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(136),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               /*nivandvar = "12".*/
               RETURN TRUE.
            END.
            ELSE RETURN FALSE.
         END.
         IF kolnamn = "AKTIV" THEN DO:    
            IF kolvarde = "TRUE" THEN DO:        
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(138),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  /*nivandvar = "12".*/
                  RETURN TRUE.
               END.
               ELSE RETURN FALSE.
            END.
            ELSE IF kolvarde = "FALSE" THEN DO:        
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(137),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  /*nivandvar = "12".*/
                  RETURN TRUE.
               END.
               ELSE RETURN FALSE.
            END.     
         END.
         IF kolnamn = "ANMARKNING" THEN DO:
            THIS-OBJECT:ekgresursTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
            /*nivandvar = "12".*/
            RETURN TRUE.
         END.         
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
                  THIS-OBJECT:RPControl:GridPaslag:GuruReopen(). 
                  nivandvar = "11".
                  raknavar = "11".

                  rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(126) + " " + kolvarde + ". Om det gamla värdet skall återställas- räkna ner med faktor " + STRING((( 1 / ( 1 + DECIMAL(kolvarde)) - 1))), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).         
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
         /*Ny kod*/
         IF kolnamn = "REGELID" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(105),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               THIS-OBJECT:ekgregelTTh:FIND-FIRST("WHERE REGELID = '"  + kolvarde + "'") NO-ERROR.
               IF THIS-OBJECT:ekgregelTTh:AVAILABLE THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  THIS-OBJECT:ControlDataDb:BeraknaRP().
               END.  
               ELSE DO:
                  rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(104),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Error).
                  RETURN FALSE.   
               END.
             END.  
             ELSE RETURN FALSE.
         END.
         /**/
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
         ELSE IF kolnamn = "PRIS" THEN DO:            
            IF THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("REGELID"):BUFFER-VALUE = "1" THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(165),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  THIS-OBJECT:ControlDataDb:BeraknaRP().
                  
               END.  
               ELSE RETURN FALSE.
            END.   
            ELSE DO:                        
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(127),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
                  THIS-OBJECT:ControlDataDb:BeraknaRP().
                  
               END.  
               ELSE RETURN FALSE.
            END.    
         END.
         THIS-OBJECT:RPControl:GridResursPris:GuruReopen().
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
      THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().
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
         IF kolnamn = "IMPORTPRIS" THEN DO:                     
            THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = kolvarde.
            THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().
            THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("UPPDATDATUM"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).
            nivandvar = "11".
            raknavar = "11".
            RETURN TRUE.                   
         END.   
         IF STRING(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.         
         IF kolnamn = "KALKYLPRIS" THEN DO:          
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(116),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:
               dontrepo = TRUE.               
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("LASTA"):BUFFER-VALUE = TRUE.               
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("UPPDATDATUM"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).                  
               THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().
               nivandvar = "11".
               raknavar = "11".
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
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(128),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN DO:            
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               THIS-OBJECT:ControlDataDb:BeraknaMtrlkosten().
               THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("UPPDATDATUM"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).
               THIS-OBJECT:MaterielControl:GridMateriel:valueSynch("UPPDATDATUM").
               THIS-OBJECT:MaterielControl:GridMateriel:valueSynch("KALKYLPRIS").
               THIS-OBJECT:MaterielControl:GridMateriel:valueSynch("BASPRIS").
               nivandvar = "11".
               raknavar = "11".
               RETURN TRUE.
            END.        
            ELSE RETURN FALSE.                  
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
      IF THIS-OBJECT:ekgp3arbkodTTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp3arbkodTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp3arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
         THIS-OBJECT:ekgp3arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
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
         
         IF THIS-OBJECT:EbrKatVar = "KLG1" AND kolnamn = "FREKVENS" THEN DO:  
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
                  THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(THIS-OBJECT:EbrKatVar).
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
         THIS-OBJECT:ControlDataDb:LaddaP3VFresursSingel(THIS-OBJECT:EbrKatVar).         
         IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(THIS-OBJECT:EbrKatVar).
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
      IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN DO:
         IF STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN FALSE.
        
         IF kolnamn = "FREKVENS" THEN DO:  
            IF LOGICAL(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE) = FALSE THEN DO:            
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(89),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
               IF rrr:ToString() = "Yes" THEN DO:
                  dontrepo = TRUE.
                  THIS-OBJECT:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
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
                  THIS-OBJECT:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
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
                                    
                  THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(THIS-OBJECT:EbrKatVar).
                  THIS-OBJECT:P2Control:GridP2:GuruReopen().
                    
               END.  
               ELSE RETURN FALSE.          
            END.
         END.
         ELSE DO:
            THIS-OBJECT:ekgp2TTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
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
         THIS-OBJECT:ControlDataDb:LaddaP2VFresursSingel(THIS-OBJECT:EbrKatVar).
         IF THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = FALSE THEN THIS-OBJECT:ControlDataDb:BeraknaP2enresurs(THIS-OBJECT:EbrKatVar).
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
         IF kolnamn = "ANMARKNING" THEN DO:
            THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = TRIM(kolvarde).
            THIS-OBJECT:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = TRIM(kolvarde).
            END.  
            ELSE DO:
               THIS-OBJECT:ekgp1TTh:FIND-FIRST("WHERE P1ARBKOD = '"  + THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
               IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-CREATE().
                  THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-COPY(ekgp1TTh).
                  THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-COPY(ekgp1arbkodTTh). 
                  THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE =  THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE.
                  THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE =  THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE.
                  THIS-OBJECT:ekgarbkodextraanmTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = ekgarbkodextraanmTTh:RECID.                
               END.
            END.    
         END. 
         ELSE DO:
            THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         END.
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
        IF THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO: 
           IF THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Resurs"]:Selected = TRUE THEN THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Selected = TRUE.
           THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Enabled = TRUE.
        END.
        ELSE DO:
           IF THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Selected = TRUE THEN THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Resurs"]:Selected = TRUE.
           THIS-OBJECT:P2Control:TabManagerP2:Tabs["P2Frekvens"]:Enabled = FALSE.
        END. 
     END.    
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeResursPris(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
      IF THIS-OBJECT:ekgresursprisTTh:AVAILABLE THEN DO:
         ekgresursnivaTTh:FIND-FIRST("WHERE ekgresursnivaTT.RESURSNR = '" + THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ekgresursnivaTTh:AVAILABLE THEN DO:
            THIS-OBJECT:RPControl:GridResursNiva:GuruRadvaljare(THIS-OBJECT:ekgresursnivaTTh:ROWID, TRUE).
         END.
      END.     
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeMateriel(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
      THIS-OBJECT:RibbonToolAktivKod:Text = STRING(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE).       
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP5 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
      DEFINE VARIABLE komvar AS CHARACTER NO-UNDO.
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
       
        komvar = STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "' AND ekgp5resursTT.P5LOPNR = " +  STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5LOPNR"):BUFFER-VALUE).
        THIS-OBJECT:P5Control:GridP5Resurs:GuruFiltrera("ekgp5resursTT.P5ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE) + "' AND ekgp5resursTT.P5LOPNR = " +  STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5LOPNR"):BUFFER-VALUE)).
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
     THIS-OBJECT:RibbonToolAktivKod:Text = STRING(THIS-OBJECT:ekgp5TTh:BUFFER-FIELD("P5ARBKOD"):BUFFER-VALUE).
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP4 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
        THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE"). 
     END.      
     ELSE THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera(""). 
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:  
        ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR.
        THIS-OBJECT:ControlDataDb:HamtP1detaljlistaenstaka(THIS-OBJECT:EbrKatVar, "4").
        THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
        THIS-OBJECT:P4Control:GridP4Resurs:GuruFiltrera("ekgp4resursTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4resursTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P4Control:GridP4Resurs:GuruFirstrow().
        THIS-OBJECT:P4Control:GridP4Frekvens:GuruFiltrera("ekgp4frekvensTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4frekvensTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P4Control:GridP4Frekvens:GuruFirstrow().          
        THIS-OBJECT:P4Control:gridP4Lista:GuruFiltrera("ekgp1detaljTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE) + "' AND ekgp1detaljTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).        
        THIS-OBJECT:P4Control:gridP4Lista:GuruFirstrow().
       
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
        IF THIS-OBJECT:P4Control:gridP4Lista:VISIBLE = TRUE THEN DO:           
           THIS-OBJECT:P4Control:gridP4Lista:GuruFiltrera(" ").
           ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.        
           IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:             
              THIS-OBJECT:P4Control:gridP4Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
           END.
           ELSE DO:
              THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
              ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
              THIS-OBJECT:ControlDataDb:HamtP1detaljlista(THIS-OBJECT:EbrKatVar, "4").
              THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
              THIS-OBJECT:P4Control:gridP4Lista:GuruFiltrera(" ").
              ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.        
              IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:                 
                 THIS-OBJECT:P4Control:gridP4Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
              END.   
              THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
           END.
        END.      
     END.  
     THIS-OBJECT:RibbonToolAktivKod:Text = STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE).
     THIS-OBJECT:FrekvensResursKoll("P4").
     
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP3ArbKod(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
        ekgp3TTh:FIND-FIRST("WHERE ekgp3TT.P3ARBKOD = '" + THIS-OBJECT:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
           THIS-OBJECT:P3Control:GridP3:GuruRadvaljare(THIS-OBJECT:ekgp3TTh:ROWID, TRUE).
        END.  
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP3 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
          THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE"). 
     END.       
     ELSE THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera("ekgresursForTT.REGELID NE " + STRING(7)).
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
        IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO: 
           ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
           THIS-OBJECT:ControlDataDb:HamtP1detaljlistaenstaka(THIS-OBJECT:EbrKatVar, "3").
           THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).                
           THIS-OBJECT:P3Control:GridP3Resurs:GuruFiltrera("ekgp3resursTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3resursTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
           THIS-OBJECT:P3Control:GridP3Resurs:GuruFirstrow().
           THIS-OBJECT:P3Control:GridP3Frekvens:GuruFiltrera("ekgp3frekvensTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3frekvensTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
           THIS-OBJECT:P3Control:GridP3Frekvens:GuruFirstrow().
           THIS-OBJECT:P3Control:gridP3Lista:GuruFiltrera("ekgp1detaljTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + "' AND ekgp1detaljTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
           THIS-OBJECT:P3Control:gridP3Lista:GuruFirstrow().   
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
         IF THIS-OBJECT:P3Control:gridP3Lista:VISIBLE = TRUE THEN DO:
            THIS-OBJECT:P3Control:gridP3Lista:GuruFiltrera(" ").         
            ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
               THIS-OBJECT:P3Control:gridP3Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
            END.
            ELSE DO:
               THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
               ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
               THIS-OBJECT:ControlDataDb:HamtP1detaljlista(THIS-OBJECT:EbrKatVar, "3").
               THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
               THIS-OBJECT:P3Control:gridP3Lista:GuruFiltrera(" ").         
               ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
               IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:P3Control:gridP3Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
               END.   
               THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
            END.
         END.            
     END.
     THIS-OBJECT:RibbonToolAktivKod:Text = STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE).   
     THIS-OBJECT:FrekvensResursKoll("P3").
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP2ArbKod(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
      ekgp2TTh:FIND-FIRST("WHERE ekgp2TT.P2ARBKOD = '" + THIS-OBJECT:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P2Control:GridP2:GuruRadvaljare(THIS-OBJECT:ekgp2TTh:ROWID, TRUE).
      END.  
   END METHOD.
    
   METHOD PUBLIC VOID ValueChangeP2 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
     IF dontrepo = TRUE THEN RETURN.
     IF THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("FREKVENS"):BUFFER-VALUE = TRUE THEN DO:
           THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera("ekgresursForTT.UTOVERFREK = TRUE"). 
        END.       
        ELSE DO:
           THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera("ekgresursForTT.REGELID NE " + STRING(7)).
     END.   
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:     
        ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
        THIS-OBJECT:ControlDataDb:HamtP1detaljlistaenstaka(THIS-OBJECT:EbrKatVar, "2").
        THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).            
        THIS-OBJECT:P2Control:GridP2Resurs:GuruFiltrera("ekgp2resursTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2resursTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:GridP2Resurs:GuruFirstrow().
        THIS-OBJECT:P2Control:GridP2Frekvens:GuruFiltrera("ekgp2frekvensTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2frekvensTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:GridP2Frekvens:GuruFirstrow(). 
        
        THIS-OBJECT:P2Control:GridP2Mtrl:GuruFiltrera("ekgp2mtrlTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2mtrlTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:GridP2Mtrl:GuruFirstrow().   
        THIS-OBJECT:P2Control:gridP2Lista:GuruFiltrera("ekgp1detaljTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + "' AND ekgp1detaljTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
        THIS-OBJECT:P2Control:gridP2Lista:GuruFirstrow().
     END.
     IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
        THIS-OBJECT:P2Control:GridP2Resurs:GuruFiltrera(" ").
        ekgp2resursTTh:FIND-FIRST("WHERE ekgp2resursTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2resursTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp2resursTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P2Control:GridP2Resurs:GuruRadvaljare(THIS-OBJECT:ekgp2resursTTh:ROWID, TRUE).
        END. 
        
        THIS-OBJECT:P2Control:GridP2Frekvens:GuruFiltrera(" ").
        ekgp2frekvensTTh:FIND-FIRST("WHERE ekgp2frekvensTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2frekvensTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp2frekvensTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P2Control:GridP2Frekvens:GuruRadvaljare(THIS-OBJECT:ekgp2frekvensTTh:ROWID, TRUE).
        END. 
         
        THIS-OBJECT:P2Control:GridP2Mtrl:GuruFiltrera(" ").
        ekgp2mtrlTTh:FIND-FIRST("WHERE ekgp2mtrlTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2mtrlTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
        IF THIS-OBJECT:ekgp2mtrlTTh:AVAILABLE THEN DO:
           THIS-OBJECT:P2Control:GridP2Mtrl:GuruRadvaljare(THIS-OBJECT:ekgp2mtrlTTh:ROWID, TRUE).
        END.
        IF THIS-OBJECT:P2Control:gridP2Lista:VISIBLE = TRUE THEN DO: 
           THIS-OBJECT:P2Control:gridP2Lista:GuruFiltrera(" ").
           ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
           IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
              THIS-OBJECT:P2Control:gridP2Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
           END.
           ELSE DO:
              THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
              ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
              THIS-OBJECT:ControlDataDb:HamtP1detaljlista(THIS-OBJECT:EbrKatVar, "2").
              THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
              THIS-OBJECT:P2Control:gridP2Lista:GuruFiltrera(" ").
              ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
              IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
                 THIS-OBJECT:P2Control:gridP2Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
              END.
              THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
           END.
        END.      
     END.  
     
     THIS-OBJECT:RibbonToolAktivKod:Text = STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE).
     THIS-OBJECT:FrekvensResursKoll("P2").
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP1ArbKod(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):    
      ekgp1TTh:FIND-FIRST("WHERE ekgp1TT.P1ARBKOD = '" + THIS-OBJECT:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'" , NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ekgp1TTh:AVAILABLE THEN DO:
         THIS-OBJECT:P1Control:GridP1:GuruRadvaljare(THIS-OBJECT:ekgp1TTh:ROWID, TRUE).
      END.  
   END METHOD.
   
   METHOD PUBLIC VOID ValueChangeP1 (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      IF dontrepo = TRUE THEN RETURN.  
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
         ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
         THIS-OBJECT:ControlDataDb:HamtP1detaljlistaenstaka(THIS-OBJECT:EbrKatVar, "1").
         THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:P1Control:GridP1Frekvens:GuruFiltrera("ekgp1frekvensTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "' AND ekgp1frekvensTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:P1Control:GridP1Frekvens:GuruFirstrow().
         THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruFiltrera("ekgp1mtrlTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "' AND ekgp1mtrlTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruFirstrow().
         THIS-OBJECT:P1Control:gridP1Lista:GuruFiltrera("ekgp1detaljTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "' AND ekgp1detaljTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:P1Control:gridP1Lista:GuruFirstrow().
         
      END.
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:
         THIS-OBJECT:P1Control:GridP1Frekvens:GuruFiltrera(" ").
         ekgp1frekvensTTh:FIND-FIRST("WHERE ekgp1frekvensTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1frekvensTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ekgp1frekvensTTh:AVAILABLE THEN DO:
            THIS-OBJECT:P1Control:GridP1Frekvens:GuruRadvaljare(THIS-OBJECT:ekgp1frekvensTTh:ROWID, TRUE).
         END.
         THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruFiltrera(" ").
         ekgp1mtrlTTh:FIND-FIRST("WHERE ekgp1mtrlTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1mtrlTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ekgp1mtrlTTh:AVAILABLE THEN DO:
            THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruRadvaljare(THIS-OBJECT:ekgp1mtrlTTh:ROWID, TRUE).
         END.
          /* */
         /*THIS-OBJECT:P1Control:gridP1Lista:GuruFiltrera(" ").
         ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
         
         IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
            THIS-OBJECT:P1Control:gridP1Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
         END.
         
         ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
         THIS-OBJECT:ControlDataDb:HamtP1detaljlista(THIS-OBJECT:EbrKatVar, "1").
         THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).*/
         
         /* hämta bara för en arbetskod på p1nivå*/
         /* ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
         THIS-OBJECT:ControlDataDb:HamtP1detaljlistaenstaka(THIS-OBJECT:EbrKatVar, "1").
         THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:P1Control:gridP1Lista:GuruFiltrera("ekgp1detaljTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + "' AND ekgp1detaljTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:P1Control:gridP1Lista:GuruFirstrow().*/
         /* hämta bara för en arbetskod på p1nivå*/  
         
         /*test hämta alla koder även för P1 detaljerad annar 110 000 poster ohanterbart funkar ej till excel*/
         THIS-OBJECT:P1Control:gridP1Lista:GuruFiltrera(" ").
           ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
           IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
              THIS-OBJECT:P1Control:gridP1Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
           END.
           ELSE DO:
              THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
              ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
              THIS-OBJECT:ControlDataDb:HamtP1detaljlista(THIS-OBJECT:EbrKatVar, "1").
              THIS-OBJECT:ControlDataDb:Raknaresurslista(THIS-OBJECT:EbrKatVar).
              THIS-OBJECT:P1Control:gridP1Lista:GuruFiltrera(" ").
              ekgp1detaljTTh:FIND-FIRST("WHERE ekgp1detaljTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1detaljTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
              IF THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO:
                 THIS-OBJECT:P1Control:gridP1Lista:GuruRadvaljare(THIS-OBJECT:ekgp1detaljTTh:ROWID, TRUE).
              END.
              THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
           END.
           /*test hämta alla koder även för P1 detaljerad annar 110 000 poster ohanterbart funkar ej till excel*/
      END. 
      THIS-OBJECT:RibbonToolAktivKod:Text = STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE).
   END METHOD.
   
   METHOD PUBLIC VOID ValueListaEjAnvMtrlCH (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):   
      /*Anders Olsson Elpool i Umeå AB  31 okt 2014 13:26:28 
      Avmarkerar poster i vänstar griden 
      */    
      THIS-OBJECT:MaterielControl:GridMateriel:GuruAvmarkera(). 
      THIS-OBJECT:ekgmtrlTTh:FIND-FIRST("WHERE ekgmtrlTT.ARTNR = '" + TRIM(THIS-OBJECT:listekgmtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE) + "'") NO-ERROR.
      IF THIS-OBJECT:ekgmtrlTTh:AVAILABLE THEN DO:
         THIS-OBJECT:MaterielControl:GridMateriel:GuruRadvaljare(THIS-OBJECT:ekgmtrlTTh:ROWID, TRUE).
      END.  
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
   METHOD PUBLIC VOID ResetRadval():
      THIS-OBJECT:P5Control:GridP5:GuruResetCurrentRowid().
      THIS-OBJECT:P4Control:GridP4:GuruResetCurrentRowid().
      THIS-OBJECT:P3Control:GridP3:GuruResetCurrentRowid().
      THIS-OBJECT:P2Control:GridP2:GuruResetCurrentRowid().
      THIS-OBJECT:P1Control:GridP1:GuruResetCurrentRowid().
   END METHOD.
   METHOD PUBLIC VOID TabEkg_SelectedTabChanging(INPUT cTo AS CHARACTER,INPUT cFrom AS CHARACTER).
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      /*Uppdatera ribbon*/
      THIS-OBJECT:RibbonGroupVisa:Visible = TRUE.
      THIS-OBJECT:RibbonGroupFonster:Visible = TRUE.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:dontrepo = FALSE.
      THIS-OBJECT:RibbonToolAktuellTab:Text = cTo.    
      aktuelltabvar = cTo.
      IF aktuelltabvar = "Materiel" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Caption= "Sök Artnr".
         THIS-OBJECT:RibbonToolAktivKod:SharedPropsInternal:ToolTipText = "Markerat materiel".
      END.
      ELSE DO:
         THIS-OBJECT:RibbonGroupTabInfo:Caption = "Sök Arbetskod".
         THIS-OBJECT:RibbonToolAktivKod:SharedPropsInternal:ToolTipText = "Markerad kod".   
      END.
      IF cFrom = "P1" THEN DO:
         THIS-OBJECT:P1Control:GridP1:GuruCurrentRowid(ekgp1tth:ROWID).   
      END.
      IF cFrom = "P2" THEN DO:
         THIS-OBJECT:P2Control:GridP2:GuruCurrentRowid(ekgp2tth:ROWID).   
      END.
      IF cFrom = "P3" THEN DO:
         THIS-OBJECT:P3Control:GridP3:GuruCurrentRowid(ekgp3tth:ROWID).   
      END.
      IF cFrom = "P4" THEN DO:
         THIS-OBJECT:P4Control:GridP4:GuruCurrentRowid(ekgp4tth:ROWID).   
      END.
      IF cFrom = "P5" THEN DO:
         THIS-OBJECT:P5Control:GridP5:GuruCurrentRowid(ekgp5tth:ROWID).   
      END.
      IF cFrom = "P2Kom" THEN DO:
         THIS-OBJECT:P2KomControl:GridKoppladeKom:GuruCurrentRowid(ekgKoppArbSvepTTh:ROWID).
            
      END.
      IF nivandvar NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(92),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() = "Yes" THEN DO:  
            THIS-OBJECT:Sparatab(INPUT cFrom).                             
         END.         
         IF rrr:ToString() = "No" THEN DO:              
            THIS-OBJECT:Laddaomtab(INPUT cFrom).            
            /*THIS-OBJECT:ValjKatalog(THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).*/
         END.                 
      END.
        
      nivandvar = "".
      
      
      IF cFrom = "Resurser" THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaResurser().
         THIS-OBJECT:LaddaomGriddarResurs().   
      END. 
      
      IF cTo = "Jämför" THEN DO:
         IF THIS-OBJECT:EkgSubidVar > 0 THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(141), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
            THIS-OBJECT:RibbonGroupTabInfo:Visible = FALSE.  
               IF THIS-OBJECT:ControlDataDb:AnvandsKat(THIS-OBJECT:EkgSubidVar) = TRUE AND Guru.Konstanter:globanv = STRING(THIS-OBJECT:ControlDataDb:AnvandsAv(THIS-OBJECT:EkgSubidVar)) THEN DO:
               THIS-OBJECT:AnvandsAvOff(STRING(THIS-OBJECT:EkgSubidVar)).
            END.
            THIS-OBJECT:StangKatalog(THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
         END.
      END.  
      
      IF cTo = "Kataloger" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Visible = FALSE.
         THIS-OBJECT:ControlDataDb:LaddaKataloger().
         THIS-OBJECT:KatalogerControl:GridKataloger:GuruReopen().    
      END. 
          
      IF cTo = "Export" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Visible = FALSE.    
      END.   
      IF cTo = "Resurser" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Visible = FALSE.
      END.     
      IF cTo = "Resurspris" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Visible = FALSE.
         THIS-OBJECT:ControlDataDb:LaddaResurserPriserVF().
         THIS-OBJECT:RPControl:GridResursPaslag:GuruReopen().
         THIS-OBJECT:RPControl:gridResurspris:GuruReopen().
         THIS-OBJECT:RPControl:GridResursPaslag:GuruReopen().
         THIS-OBJECT:RPControl:GridResursniva:GuruReopen().
         THIS-OBJECT:ControlShell:comboNiva:GuruCombo:GuruFiltrera().
         
      END.
      IF cTo = "Materiel" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.
         THIS-OBJECT:RibbonToolSokKod:Text = "Sök".
         THIS-OBJECT:RibbonToolAktivKod:Text = "".
         /*lena 20131111 tillaggt*/  
                
         THIS-OBJECT:RPControl:GridRegel:GuruReopen().      
         THIS-OBJECT:RPControl:GridPaslag:GuruReopen().
         THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().
         listekgmtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
         THIS-OBJECT:MaterielControl:GridListaEjAnvMtrl:GuruReopen().
         /*Robin Sjöberg Elpool i Umeå AB  20 nov 2013 11:16:46 GuruFirstRow tillagt */
         /*THIS-OBJECT:MaterielControl:GridMateriel:GuruFirstRow().*/
      END.
      IF cTo = "P5" THEN DO:
         THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.
         THIS-OBJECT:RibbonToolSokKod:Text = "Sök".
         THIS-OBJECT:RibbonToolAktivKod:Text = "".
         THIS-OBJECT:ControlDataDb:HamtaResursFor(5).
         THIS-OBJECT:P5Control:GridResursForP5:GuruFiltrera().
         THIS-OBJECT:ControlDataDb:LaddaP5VFresurs().        
         THIS-OBJECT:P5Control:GridP5:GuruGetCurrentRowid().
      END.
      IF cTo = "P4" THEN DO:
         ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR.          
         THIS-OBJECT:RibbonToolSokKod:Text = "Sök".
         THIS-OBJECT:RibbonToolAktivKod:Text = "".
         THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.
         THIS-OBJECT:ControlDataDb:HamtaResursFor(4).
         THIS-OBJECT:P4Control:GridResursForP4:GuruFiltrera().        
         THIS-OBJECT:ControlDataDb:LaddaP4VFresurs().         
         THIS-OBJECT:ControlDataDb:LaddaP4VFfrek().
         THIS-OBJECT:P4Control:GridP5forP4:GuruFiltrera().        
         THIS-OBJECT:P4Control:GridP4:GuruGetCurrentRowid().
         THIS-OBJECT:P4Control:GridP4Lista:GuruFiltrera("").
         THIS-OBJECT:P4Control:GridP4Lista:GuruReopen().
      END.
      IF cTo = "P3" THEN DO:
        ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
        THIS-OBJECT:RibbonToolSokKod:Text = "Sök".
        THIS-OBJECT:RibbonToolAktivKod:Text = "".
        THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.
        THIS-OBJECT:ControlDataDb:HamtaResursFor(3).
        THIS-OBJECT:P3Control:GridResursForP3:GuruFiltrera().      
        THIS-OBJECT:ControlDataDb:LaddaP3VFresurs(THIS-OBJECT:EbrKatVar).        
        THIS-OBJECT:ControlDataDb:LaddaP3VFfrek().      
        THIS-OBJECT:P3Control:GridP3Lista:GuruFiltrera("").
        THIS-OBJECT:P3Control:GridP3Lista:GuruReopen().  
        IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = FALSE.
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.     
           THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
           THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = FALSE.
           THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = FALSE.                
        END.   
        IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = TRUE.
           THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
           THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
           THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = TRUE.
           THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = TRUE.
        END. 
        THIS-OBJECT:P3Control:GridP4forP3:GuruFiltrera().
        THIS-OBJECT:P3Control:GridP3:GuruGetCurrentRowid().
        /*Ev Flytta senare*/
        THIS-OBJECT:ControlShell:comboP3ArbKod:GuruCombo:GuruFiltrera().
      END.
      IF cTo = "P2" THEN DO:
        ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR.
        THIS-OBJECT:RibbonToolSokKod:Text = "Sök".
        THIS-OBJECT:RibbonToolAktivKod:Text = "".
        THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.
        THIS-OBJECT:ControlDataDb:HamtaResursFor(2).
        THIS-OBJECT:P2Control:GridResursForP2:GuruFiltrera().        
        THIS-OBJECT:ControlDataDb:LaddaP2VFresurs( THIS-OBJECT:EbrKatVar).
        THIS-OBJECT:ControlDataDb:LaddaP2VFmtrl().         
        THIS-OBJECT:ControlDataDb:LaddaP2VFfrek().
        THIS-OBJECT:P2Control:GridP2Lista:GuruFiltrera("").
        THIS-OBJECT:P2Control:GridP2Lista:GuruReopen().        
        IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
           THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
           THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = TRUE.
           THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
           THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = FALSE.
           THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = FALSE.         
        END.   
        IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
           THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
           THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = FALSE.
           THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
           THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = TRUE.
           THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = TRUE.   
        END.
        THIS-OBJECT:P2Control:GridP3forP2:GuruFiltrera().
        THIS-OBJECT:P2Control:GridP2:GuruGetCurrentRowid().
        THIS-OBJECT:P2Control:GridMtrlForP2:GuruFiltrera("ekgmtrlTT.AKTIV = TRUE").
        /*Ev Flytta senare*/
        THIS-OBJECT:ControlShell:comboP2ArbKod:GuruCombo:GuruFiltrera().
      END.
      IF cTo = "P1" THEN DO:
         ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:RibbonToolSokKod:Text = "Sök".
         THIS-OBJECT:RibbonToolAktivKod:Text = "".
         THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.   
         THIS-OBJECT:ControlDataDb:LaddaP1VFfrek().
         THIS-OBJECT:P1Control:GridP1Lista:GuruFiltrera("").
         THIS-OBJECT:P1Control:GridP1Lista:GuruReopen().         
         IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
            THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
            THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = TRUE.
            THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = TRUE.
            THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = FALSE.
            THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = FALSE.            
         END.   
         ELSE IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
            THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.
            THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTTIM"]:Hidden = FALSE.
            THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["UTRUSTTIM"]:Hidden = FALSE.            
            THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = TRUE.
            THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = TRUE. 
         END.
         THIS-OBJECT:P1Control:GridP2forP1:GuruFiltrera().
        
         THIS-OBJECT:P1Control:GridP1:GuruGetCurrentRowid().
         
        /*Ev Flytta senare*/
        THIS-OBJECT:ControlShell:comboP1ArbKod:GuruCombo:GuruFiltrera().
      END.
      
      IF cTo = "P2kom" THEN DO:
         
         THIS-OBJECT:RibbonGroupVisa:Visible = FALSE.
         THIS-OBJECT:RibbonGroupFonster:Visible = FALSE.
         THIS-OBJECT:RibbonToolSokKod:Text = "Sök Arbetskod".
         THIS-OBJECT:RibbonToolAktivKod:Text = "".
         THIS-OBJECT:RibbonGroupTabInfo:Visible = TRUE.  
         THIS-OBJECT:P2KomControl:GridP2KomResult:GuruReopen().
         THIS-OBJECT:P2KomControl:GridKoppladeKom:GuruReopen().
         THIS-OBJECT:P2KomControl:GridKom:GuruReopen().
         THIS-OBJECT:P2KomControl:GridArbLop:GuruFiltrera(THIS-OBJECT:P2KomControl:ArbLopFilter()).
           
      END.   
      THIS-OBJECT:VisaAllaKoder(?,?).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.

   METHOD PUBLIC VOID ValjKatalog(INPUT nyekgsubidvar AS INTEGER, INPUT nyebrkatvar AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = FALSE.
      /*Robin Sjöberg Elpool i Umeå AB  23 apr 2015 09:48:32 
      Stänger katalog om öppen (tömmer DS tts osv)
      */
      IF THIS-OBJECT:EkgSubidVar NE 0 AND THIS-OBJECT:EbrKatVar NE "" THEN THIS-OBJECT:StangKatalog(THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar). 

      /*Uppdatera värden*/
      THIS-OBJECT:EkgSubidVar = nyekgsubidvar.
      THIS-OBJECT:EbrKatVar = nyebrkatvar.
      nivandvar = "".
      raknavar = "".
      lastkatvar = FALSE.
      
      /*Är katalogen låst?*/
      IF THIS-OBJECT:ControlDataDb:LastKat(nyekgsubidvar) = TRUE THEN lastkatvar = TRUE.
      
      /*Används katalogen av någon?*/
      IF THIS-OBJECT:ControlDataDb:AnvandsKat(nyekgsubidvar) = TRUE AND Guru.Konstanter:globanv NE STRING(THIS-OBJECT:ControlDataDb:AnvandsAv(nyekgsubidvar)) 
      THEN lastkatvar = TRUE.
      ELSE THIS-OBJECT:AnvandsAvOn(STRING(THIS-OBJECT:KatalogerControl:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
      
      THIS-OBJECT:ControlDataDb:LastUppGammalKat(nyekgsubidvar).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      
      /*Kör metoder*/
      THIS-OBJECT:RibbonToolKatalogKnapp:SharedPropsInternal:Visible = TRUE.
      THIS-OBJECT:VisaRibbonGrupper(TRUE).
      THIS-OBJECT:VisaKostnader(?,?).
      THIS-OBJECT:VisaEaMangder(?,?).
      THIS-OBJECT:LaddaDataSet().
      THIS-OBJECT:LaddaOmGriddar().
      THIS-OBJECT:VisaTabbar().
      THIS-OBJECT:UppdateraKatalogRibbon().
      THIS-OBJECT:VisaKatalog(?,?).
      THIS-OBJECT:LastLage().
      
      /*Robin Sjöberg Elpool i Umeå AB  22 apr 2015 10:06:15 
      P2K Visa läge 
      */
      THIS-OBJECT:P2KomControl:splitContainerAllt:Panel1Collapsed = FALSE.
      THIS-OBJECT:P2KomControl:splitContainerAllt:Panel2Collapsed = TRUE.
      
      THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P1"]:Selected = TRUE.
      THIS-OBJECT:TabEkg_SelectedTabChanging(INPUT "P1",INPUT "").
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).

      IF lastkatvar = TRUE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(134),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID StangKatalogClick(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      IF nivandvar NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(152),"", System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() = "Yes" THEN DO:  
            THIS-OBJECT:Sparatab(INPUT aktuelltabvar).                             
         END.         
         IF rrr:ToString() = "Cancel" THEN RETURN.                             
      END. 
      THIS-OBJECT:StangKatalog(THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).        
   END METHOD.
   
   METHOD PUBLIC VOID StangKatalog(INPUT inEkgSubidVar AS INTEGER, INPUT inebrkat AS CHARACTER):    
      THIS-OBJECT:ControlDataDb:TomKatalog().
      THIS-OBJECT:AnvandsAvOff(STRING(inEkgSubidVar)).
      /*Uppdatera värden*/
      THIS-OBJECT:EkgSubidVar = 0.
      THIS-OBJECT:EbrKatVar = "".
      THIS-OBJECT:nivandvar = "".
      THIS-OBJECT:raknavar = "".
      THIS-OBJECT:lastkatvar = FALSE.
      THIS-OBJECT:VisaStart(?,?).
      THIS-OBJECT:RibbonToolKatalogKnapp:SharedPropsInternal:Visible = FALSE.
      /*Robin Sjöberg Elpool i Umeå AB  23 apr 2015 10:10:53 
      flyttat från THIS-OBJECT:ResetRadval(). från ValjKat() 
      */
      THIS-OBJECT:ResetRadval().
      
   END METHOD.
   
   METHOD PUBLIC VOID Avsluta(INPUT sender AS System.Object , INPUT e AS System.Windows.Forms.FormClosingEventArgs):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      IF nivandvar NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(132),"", System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() = "Yes" THEN DO:  
            THIS-OBJECT:Sparatab(INPUT aktuelltabvar).                             
         END.         
         IF rrr:ToString() = "Cancel" THEN DO:              
            e:Cancel = TRUE.
         END.                  
      END.
      THIS-OBJECT:AnvandsAvOff(STRING(THIS-OBJECT:EkgSubidVar)).
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
      THIS-OBJECT:KatalogerControl:GridKataloger:GuruReopen().      
      THIS-OBJECT:ekgsubkatalogerTTh:FIND-FIRST("WHERE EKGSUBID = " +  STRING(nyekgsubidvar) ) NO-ERROR.
      THIS-OBJECT:KatalogerControl:GridKataloger:GuruRepositionto(THIS-OBJECT:ekgsubkatalogerTTh:ROWID).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      
   END METHOD.
   
   METHOD PUBLIC VOID TaBortKatalog(INPUT nyekgsubidvar AS INTEGER, INPUT nyebrkatvar AS CHARACTER):
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(88),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN .
      ELSE RETURN. 
      THIS-OBJECT:KatalogerControl:GridKataloger:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ControlDataDb:BortKatalog(INPUT nyekgsubidvar, INPUT nyebrkatvar).
      THIS-OBJECT:KatalogerControl:GridKataloger:GuruBortrader(FALSE).  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).         
   END METHOD.
   
   METHOD PUBLIC VOID LaddaDataSet():  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ControlDataDb:LaddaKatalog(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:ControlDataDb:LaddaKatalogResurser(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).  
      THIS-OBJECT:ControlDataDb:LaddaArbKoder(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar). 
      THIS-OBJECT:ControlDataDb:LaddaP3(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:ControlDataDb:LaddaP2(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:ControlDataDb:LaddaP1(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:ControlDataDb:LaddaP2Kom(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).    
      
      IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:  
         THIS-OBJECT:ControlDataDb:LaddaP5(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:ControlDataDb:LaddaP4(INPUT THIS-OBJECT:EkgSubidVar, INPUT THIS-OBJECT:EbrKatVar).
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
      THIS-OBJECT:LaddaOmGriddarP2k().
      THIS-OBJECT:LaddaOmGriddarP3().      
      IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
         THIS-OBJECT:LaddaOmGriddarP5().
         THIS-OBJECT:LaddaOmGriddarP4().         
      END.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).     
   END METHOD.
  
   METHOD PUBLIC VOID LaddaOmGriddarResurs():
      THIS-OBJECT:RPControl:GridResursForResurspris:GuruReopen().
      THIS-OBJECT:ResursControl:gridResurs:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID LaddaOmGriddarResurspris():
      THIS-OBJECT:RPControl:GridRegel:GuruReopen().
      THIS-OBJECT:RPControl:GridResurspris:GuruReopen().
      THIS-OBJECT:RPControl:GridResursniva:GuruReopen().
      
      THIS-OBJECT:RPControl:GridResursForResurspris:GuruFiltrera("ekgresursTT.AKTIV = TRUE").
      THIS-OBJECT:RPControl:GridResursForResurspris:GuruReopen().    
      THIS-OBJECT:RPControl:GridPaslag:GuruReopen().
      THIS-OBJECT:RPControl:GridResursPaslag:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarMtrl():
      THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().
      THIS-OBJECT:MaterielControl:GridImportMtrl:GuruReopen().
      THIS-OBJECT:MaterielControl:GridListaEjAnvMtrl:GuruReopen().  
   END METHOD.   
   METHOD PUBLIC VOID LaddaOmGriddarp5():
      /*P5*/
      THIS-OBJECT:P5Control:GridP5:GuruReopen().
      THIS-OBJECT:P5Control:GridP5Resurs:GuruReopen().
      THIS-OBJECT:P5Control:GridResursForP5:GuruReopen().
      /*THIS-OBJECT:P5Control:GridP5Lista:GuruReopen(). ej i bruk*/
      THIS-OBJECT:P5Control:GridImportP5:GuruReopen().
   END METHOD.   
   METHOD PUBLIC VOID LaddaOmGriddarp4():
      /*P4*/
      THIS-OBJECT:P4Control:GridP5ForP4:GuruReopen(). 
      THIS-OBJECT:P4Control:GridP4:GuruReopen().
      THIS-OBJECT:P4Control:GridP4Resurs:GuruReopen().
      THIS-OBJECT:P4Control:GridResursForP4:GuruReopen().
      THIS-OBJECT:P4Control:GridP4Frekvens:GuruReopen().
      THIS-OBJECT:P4Control:GridP4Lista:GuruReopen().
      THIS-OBJECT:P4Control:GridImportP4F:GuruReopen().
      THIS-OBJECT:P4Control:GridImportP4:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarp3():
      /*P3*/
      THIS-OBJECT:P3Control:GridP4ForP3:GuruReopen().
      THIS-OBJECT:P3Control:GridP3:GuruReopen().
      THIS-OBJECT:P3Control:GridP3Resurs:GuruReopen().
      THIS-OBJECT:P3Control:GridResursForP3:GuruReopen().
      THIS-OBJECT:P3Control:GridP3Frekvens:GuruReopen().
      THIS-OBJECT:P3Control:GridP3arbkod:GuruReopen().
      THIS-OBJECT:P3Control:GridP3Lista:GuruReopen().
      THIS-OBJECT:P3Control:GridImportP3F:GuruReopen().
      THIS-OBJECT:P3Control:GridImportP3:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID LaddaOmGriddarp2k():
      /*P2K*/
      THIS-OBJECT:P2KomControl:GridP2KomResult:GuruReopen().
      THIS-OBJECT:P2KomControl:GridKoppladeKom:GuruReopen().
      THIS-OBJECT:P2KomControl:GridKom:GuruReopen().
      THIS-OBJECT:P2KomControl:GridKomVal:GuruReopen().
      THIS-OBJECT:P2KomControl:GridP2KomResult:GuruReopen().
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
      THIS-OBJECT:P2Control:GridP2Lista:GuruReopen().
      THIS-OBJECT:P2Control:GridImportP2M:GuruReopen().
      THIS-OBJECT:P2Control:GridImportP2F:GuruReopen().
      THIS-OBJECT:P2Control:GridImportP2:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID LaddaOmGriddarp1():
      /*P1*/
      THIS-OBJECT:P1Control:GridP2ForP1:GuruReopen().
      THIS-OBJECT:P1Control:GridP1:GuruReopen().
      THIS-OBJECT:P1Control:GridP1Frekvens:GuruReopen().
      THIS-OBJECT:P1Control:GridP1arbkod:GuruReopen().
      THIS-OBJECT:P1Control:GridP1Lista:GuruReopen().
      THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruReopen().
      THIS-OBJECT:P1Control:GridImportP1F:GuruReopen().
      THIS-OBJECT:P1Control:GridImportP1:GuruReopen().
      
      
   END METHOD.   
   
   METHOD PUBLIC VOID AnvandsAvOn(ekgsubid AS CHARACTER):
      DEFINE VARIABLE anvandarNamn AS CHARACTER.
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
         ASSIGN
         anvandarNamn = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.
      END.
      THIS-OBJECT:ControlDataDb:LaddaKataloger().
      THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDS", INPUT "TRUE").
      THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAV", INPUT Guru.Konstanter:globanv).
      THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAVNAMN", INPUT anvandarNamn).
      
      THIS-OBJECT:KatalogerControl:gridKataloger:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID AnvandsAvOff(ekgsubid AS CHARACTER):
      IF INTEGER(ekgsubid) > 0 THEN DO:
         THIS-OBJECT:ControlDataDb:LaddaKataloger().
         THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDS", INPUT "FALSE").
         THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAV", INPUT "").
         THIS-OBJECT:KatalogUppdate(INPUT ekgsubid, INPUT "ANVANDSAVNAMN", INPUT "").
         THIS-OBJECT:KatalogerControl:gridKataloger:GuruReopen().
      END.
      RETURN.
   END METHOD.

   
    METHOD PUBLIC VOID VisaTabbar():
      IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P5"]:VISIBLE = TRUE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P4"]:VISIBLE = TRUE.
         THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:VISIBLE = TRUE.  
      END.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P5"]:VISIBLE = FALSE.
         THIS-OBJECT:ControlShell:TabManagerEkg:Tabs["P4"]:VISIBLE = FALSE.
         THIS-OBJECT:P3Control:TabManagerP3:Tabs["P3Frekvens"]:VISIBLE = FALSE.
      END.  
   END METHOD.
   
   METHOD PUBLIC VOID VisaAllaKoder(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*ccc lena*/   
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).   
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ TRUE THEN DO:         
         ekgp1detaljTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:P1Control:GridP1Lista:GuruReopen(). 
         THIS-OBJECT:P2Control:GridP2Lista:GuruReopen(). 
         THIS-OBJECT:P3Control:GridP3Lista:GuruReopen(). 
         THIS-OBJECT:P4Control:GridP4Lista:GuruReopen(). 
      END.    
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P5" THEN THIS-OBJECT:ValueChangeP5(?,?).  
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P4" THEN THIS-OBJECT:ValueChangeP4(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P3" THEN THIS-OBJECT:ValueChangeP3(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P2" THEN THIS-OBJECT:ValueChangeP2(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P1" THEN THIS-OBJECT:ValueChangeP1(?,?).
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "Materiel" THEN THIS-OBJECT:ValueListaEjAnvMtrlCH(?,?).
      THIS-OBJECT:GomValdKodFalt().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
   
   METHOD PUBLIC VOID GomValdKodFalt():
      DEFINE VARIABLE gomvar AS LOGICAL NO-UNDO.
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = TRUE THEN gomvar = FALSE.
      ELSE gomvar = TRUE.
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P5" THEN DO:
         THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["P5ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["P5BENAMNING"]:Hidden = gomvar.
         THIS-OBJECT:P5Control:GridP5Resurs:DisplayLayout:Bands[0]:Columns["P5ENHET"]:Hidden = gomvar.           
      END.
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P4" THEN DO:
         THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["P4BENAMNING"]:Hidden = gomvar.
         THIS-OBJECT:P4Control:GridP4Resurs:DisplayLayout:Bands[0]:Columns["P4ENHET"]:Hidden = gomvar.
         THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["P4ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P4Control:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["P4BENAMNING"]:Hidden = gomvar.            
      END.
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P3" THEN DO:
         THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["P3ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["P3LOPNR"]:Hidden = gomvar.
         THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["P3BENAMNING"]:Hidden = gomvar.
         THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["P3ENHET"]:Hidden = gomvar.
         THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["P3ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["P3LOPNR"]:Hidden = gomvar.
         THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["P3BENAMNING"]:Hidden = gomvar.            
      END.
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P2" THEN DO:
         THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["P2ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["P2LOPNR"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["P2BENAMNING"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["P2ENHET"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["P2ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["P2LOPNR"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["P2BENAMNING"]:Hidden = gomvar. 
         THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["P2ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["P2LOPNR"]:Hidden = gomvar.
         THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["P2BENAMNING"]:Hidden = gomvar.                   
      END.
      IF THIS-OBJECT:RibbonToolAktuellTab:Text = "P1" THEN  DO:
         THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["P1ARBKOD"]:Hidden = gomvar.
         THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["P1LOPNR"]:Hidden = gomvar.
         THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["P1BENAMNING"]:Hidden = gomvar.                            
      END.
   END METHOD.
   
   METHOD PUBLIC VOID HamtP1detaljlista(INPUT pniv AS CHARACTER):
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE). 
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked EQ FALSE THEN DO:
         THIS-OBJECT:ControlDataDb:HamtP1detaljlistaenochen(THIS-OBJECT:EbrKatVar, pniv).
      END.
      ELSE DO:     
         THIS-OBJECT:ControlDataDb:HamtP1detaljlista(THIS-OBJECT:EbrKatVar, pniv).
      END.   
      IF pniv = "1" THEN DO:
         THIS-OBJECT:P1Control:GridP1Lista:GuruReopen().
         THIS-OBJECT:P1Control:GridP1Lista:GuruFirstrow().
      END.
      IF pniv = "2" THEN DO:
         THIS-OBJECT:P2Control:GridP2Lista:GuruReopen().
         THIS-OBJECT:P2Control:GridP2Lista:GuruFirstrow().
      END.
      IF pniv = "3" THEN DO:
         THIS-OBJECT:P3Control:GridP3Lista:GuruReopen().
         THIS-OBJECT:P3Control:GridP3Lista:GuruFirstrow().
      END.
      IF pniv = "4" THEN DO:
         THIS-OBJECT:P4Control:GridP4Lista:GuruReopen().
         THIS-OBJECT:P4Control:GridP4Lista:GuruFirstrow().
      END.          
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).  
   END METHOD.
   
   /*METHOD PUBLIC VOID HamtP1detaljlistaen(INPUT pniv AS CHARACTER):
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE). 
      THIS-OBJECT:ControlDataDb:HamtP1detaljlistaen(THIS-OBJECT:EbrKatVar, pniv).
      IF pniv = "1" THEN DO:
         THIS-OBJECT:P1Control:GridP1Lista:GuruReopen().
         THIS-OBJECT:P1Control:GridP1Lista:GuruFirstrow().
      END.
      IF pniv = "2" THEN DO:
         THIS-OBJECT:P2Control:GridP2Lista:GuruReopen().
         THIS-OBJECT:P2Control:GridP2Lista:GuruFirstrow().
      END.
      IF pniv = "3" THEN DO:
         THIS-OBJECT:P3Control:GridP3Lista:GuruReopen().
         THIS-OBJECT:P3Control:GridP3Lista:GuruFirstrow().
      END.
      IF pniv = "4" THEN DO:
         THIS-OBJECT:P4Control:GridP4Lista:GuruReopen().
         THIS-OBJECT:P4Control:GridP4Lista:GuruFirstrow().
      END.          
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).  
   END METHOD.*/
   
   METHOD PUBLIC VOID HamtP1detaljmtrl():
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE). 
      THIS-OBJECT:ControlDataDb:HamtP1detaljmtrl().
     
      THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruReopen().
      THIS-OBJECT:P1Control:GridP1ArtikelFrekvens:GuruFirstrow().
            
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).  
   END METHOD.

   
   METHOD PUBLIC VOID HamtMtrllista():
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).     
      listekgmtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
      queryvar =  "FOR EACH " + THIS-OBJECT:ekgmtrlTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgmtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgP2mtrlTTh:FIND-FIRST("WHERE ARTNR = '"  + TRIM(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE) + "'") NO-ERROR.      
         IF NOT THIS-OBJECT:ekgP2mtrlTTh:AVAILABLE THEN DO:                    
            THIS-OBJECT:listekgmtrlTTh:BUFFER-CREATE().         
            THIS-OBJECT:listekgmtrlTTh:BUFFER-COPY(ekgmtrlTTh).
         END.
         qH:GET-NEXT().         
      END.
     
      THIS-OBJECT:MaterielControl:GridListaEjAnvMtrl:GuruReopen().
      THIS-OBJECT:MaterielControl:GridListaEjAnvMtrl:GuruFirstrow().    
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
   END METHOD.
   
   METHOD PUBLIC VOID EjMtrllista():
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).     
      EjekgP2mtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
      queryvar =  "FOR EACH " + THIS-OBJECT:ekgP2mtrlTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ekgmtrlTTh:FIND-FIRST("WHERE ARTNR = '"  + TRIM(THIS-OBJECT:ekgP2mtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE) + "'") NO-ERROR.      
         IF NOT THIS-OBJECT:ekgmtrlTTh:AVAILABLE THEN DO:                    
            THIS-OBJECT:EjekgP2mtrlTTh:BUFFER-CREATE().         
            THIS-OBJECT:EjekgP2mtrlTTh:BUFFER-COPY(ekgP2mtrlTTh).
         END.
         qH:GET-NEXT().         
      END.
      THIS-OBJECT:MaterielControl:GridEjTraff:GuruReopen().
      THIS-OBJECT:MaterielControl:GridEjTraff:GuruFirstrow().    
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
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
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE. 
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FMASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridResursForP3:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
         
      /*P2*/
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["MATERIELKOST"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE. 
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["SUMMA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["ARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["MASKINKOST"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE. 
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["OVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FARBETEKOST"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FMASKINKOST"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["KALKYLPRIS"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["MTRLSUMMA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridMtrlForP2:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.    
      THIS-OBJECT:P2Control:GridResursForP2:DisplayLayout:Bands[0]:Columns["PRIS"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
         
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
      
      
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FUTRUSTKOST"]:Hidden = TRUE.
      
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["UTRUSTKOST"]:Hidden = TRUE.
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
  END METHOD.
 
  METHOD PUBLIC VOID RibbonSokClick(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.BeforeToolEnterEditModeEventArgs):
   THIS-OBJECT:RibbonToolSokKod:Text = "".   
  END METHOD.
  
  METHOD PUBLIC VOID RibbonSokUpdate(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      DEFINE VARIABLE sokarbkod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE soklopnr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE filter AS CHARACTER NO-UNDO.
      IF e:KeyChar  = CHR(13) THEN DO:
         IF aktuelltabvar = "P1" THEN DO:
            IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,4)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,5,2)).
            END.
            ELSE IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,3)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,4,2)).
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
            IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,3)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,4,2)).
            END.
            ELSE IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,2)).
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,3,2)).
            END.
            IF soklopnr = "" THEN soklopnr = "1".
            ekgp2TTh:FIND-FIRST("WHERE ekgp2TT.P2ARBKOD = '" + sokarbkod + "'" + " AND ekgp2TT.P2LOPNR = " + soklopnr, NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN.
            ELSE ekgp2TTh:FIND-FIRST("WHERE ekgp2TT.P2ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp2TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P2Control:GridP2:GuruRadvaljare(THIS-OBJECT:ekgp2TTh:ROWID, TRUE).
            END.
         END.
         ELSE IF aktuelltabvar = "P3" THEN DO:
            sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,3)).
            soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,4,2)).
            IF soklopnr = "" THEN soklopnr = "1".
            ekgp3TTh:FIND-FIRST("WHERE ekgp3TT.P3ARBKOD = '" + sokarbkod + "'" + " AND ekgp3TT.P3LOPNR = " + soklopnr, NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN.
            ELSE ekgp3TTh:FIND-FIRST("WHERE ekgp3TT.P3ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp3TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P3Control:GridP3:GuruRadvaljare(THIS-OBJECT:ekgp3TTh:ROWID, TRUE).
            END.   
         END.
         ELSE IF aktuelltabvar = "P4" THEN DO:
            sokarbkod = THIS-OBJECT:RibbonToolSokKod:Text.
            ekgp4TTh:FIND-FIRST("WHERE ekgp4TT.P4ARBKOD BEGINS '" + sokarbkod + "'" , NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp4TTh:AVAILABLE THEN DO:
               THIS-OBJECT:P4Control:GridP4:GuruRadvaljare(THIS-OBJECT:ekgp4TTh:ROWID, TRUE).
            END. 
         END.
         ELSE IF aktuelltabvar = "P5" THEN DO:
            sokarbkod = THIS-OBJECT:RibbonToolSokKod:Text.
            ekgp5TTh:FIND-FIRST("WHERE ekgp5TT.P5ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgp5TTh:AVAILABLE THEN DO:
               
               THIS-OBJECT:P5Control:GridP5:GuruRadvaljare(THIS-OBJECT:ekgp5TTh:ROWID, TRUE).
            END. 
         END.
         ELSE IF aktuelltabvar = "Materiel" THEN DO:
            sokarbkod = THIS-OBJECT:RibbonToolSokKod:Text.
            ekgmtrlTTh:FIND-FIRST("WHERE ekgmtrlTT.ARTNR BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ekgmtrlTTh:AVAILABLE THEN DO:
               THIS-OBJECT:MaterielControl:GridMateriel:GuruRadvaljare(THIS-OBJECT:ekgmtrlTTh:ROWID, TRUE).
            END. 
         END.  
         ELSE IF aktuelltabvar = "P2Kom" THEN DO:
            filter =P2KomControl:GridArbLop:GuruFiltreraGet().
            IF THIS-OBJECT:EbrKatVar = "KLG1" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,3)).
               /*
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,4,2)).
               */
            END.
            ELSE IF THIS-OBJECT:EbrKatVar = "KLG2" THEN DO:
               sokarbkod = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,1,2)).
               /*
               soklopnr = TRIM(SUBSTRING(THIS-OBJECT:RibbonToolSokKod:Text,3,2)).
               */
            END.
            
            IF THIS-OBJECT:P2KomControl:splitContainerAllt:Panel1Collapsed = FALSE THEN DO:
               ekgKoppArbSvepTTh:FIND-FIRST("WHERE P2ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
               IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:P2KomControl:GridKoppladeKom:GuruRadvaljare(THIS-OBJECT:ekgKoppArbSvepTTh:ROWID, TRUE).
               END.
               IF filter = "" THEN THIS-OBJECT:ekgArbLopKodTTh:FIND-FIRST("WHERE P2ARBKOD BEGINS '" + sokarbkod + "'") NO-ERROR.
               ELSE DO:
                  THIS-OBJECT:ekgArbLopKodTTh:FIND-FIRST("WHERE " + filter + " AND P2ARBKOD BEGINS '" + sokarbkod + "'") NO-ERROR.
               END.   
               IF THIS-OBJECT:ekgArbLopKodTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:P2KomControl:GridArbLop:GuruRadvaljare(THIS-OBJECT:ekgArbLopKodTTh:ROWID, TRUE).
               END.
            
            END.
            ELSE DO:
               THIS-OBJECT:ekgP2KomResultTTh:FIND-FIRST("WHERE P2ARBKOD BEGINS '" + sokarbkod + "'", NO-LOCK) NO-ERROR.
               IF THIS-OBJECT:ekgP2KomResultTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:P2KomControl:GridP2KomResult:GuruRadvaljare(THIS-OBJECT:ekgP2KomResultTTh:ROWID, TRUE).
               END.
               
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
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE. 
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAMONT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      THIS-OBJECT:P3Control:GridP3Resurs:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridP3Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      THIS-OBJECT:P3Control:GridResursForP3:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P3Control:GridP3Lista:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      
      /*P2*/
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAMONT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      THIS-OBJECT:P2Control:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = TRUE. 
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      THIS-OBJECT:P2Control:GridResursForP2:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P2Control:GridP2Lista:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.
      
      /*P1*/
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAMASK"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EA"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE. 
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAMASK"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAOVRIGT"]:Hidden = gomas.
      THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEA"]:Hidden = gomas.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["FEAUTRUST"]:Hidden = TRUE.
      IF THIS-OBJECT:EbrKatVar = "KLG2" THEN THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = gomas.
      ELSE THIS-OBJECT:P1Control:GridP1Lista:DisplayLayout:Bands[0]:Columns["EAUTRUST"]:Hidden = TRUE.         
      
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
          /*Övriga*/
          THIS-OBJECT:RibbonToolSparaKatalog:SharedPropsInternal:Visible = boolvar3.
          THIS-OBJECT:RPControl:GridRegel:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:RPControl:GridResurspris:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:RPControl:GridResursniva:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:RPControl:GridResursForResurspris:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:RPControl:GridPaslag:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:RPControl:GridResursPaslag:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:MaterielControl:GridMateriel:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:MaterielControl:laggTillMtrlButton:VISIBLE = boolvar3.
          THIS-OBJECT:MaterielControl:taBortMtrlButton:VISIBLE = boolvar3. 
          THIS-OBJECT:MaterielControl:mtrlImportButton:VISIBLE = boolvar3.
          THIS-OBJECT:RPControl:splitContainerPaslagOver:Panel2Collapsed = boolvar2.
          THIS-OBJECT:RPControl:paslagMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:RPControl:resursPaslagMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:RPControl:regelMenuSC:Panel1Collapsed = boolvar2.
          THIS-OBJECT:RPControl:splitContainerNiva:Panel1Collapsed = boolvar2.
          THIS-OBJECT:RPControl:splitContainerResursPrisLista:Panel1Collapsed = boolvar2.
          THIS-OBJECT:RPControl:splitContainerBasresurser:Panel1Collapsed = boolvar2.
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
          
          /*P2Kom*/
          THIS-OBJECT:P2KomControl:GridKoppladeKom:DisplayLayout:Override:AllowUpdate = boolvar1.
          THIS-OBJECT:P2KomControl:GridKom:DisplayLayout:Override:AllowUpdate = boolvar1.
          
          THIS-OBJECT:P2KomControl:splitContainerValdaKom:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P2KomControl:splitContainerKoppladeSvep:Panel1Collapsed = boolvar2.
          THIS-OBJECT:P2KomControl:splitContainerKomTillBort:Panel1Collapsed = boolvar2.
          
          
  END.
  
  METHOD PUBLIC VOID ResursAdmin():
    IF Guru.Konstanter:kalk2sekvar[15] = TRUE THEN DO:
        THIS-OBJECT:ResursControl:ResursertoolStrip:VISIBLE = TRUE.
        THIS-OBJECT:ResursControl:gridResurs:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:TRUE.
    END.
    ELSE DO:
        THIS-OBJECT:ResursControl:ResursertoolStrip:VISIBLE = FALSE.
        THIS-OBJECT:ResursControl:gridResurs:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:FALSE.
    END.
END METHOD.

   METHOD PUBLIC VOID NyMtrl():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyMtrl(THIS-OBJECT:ControlShell:fillinArtNr:GuruText:Text, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:MaterielControl:GridMateriel:GuruFiltrera().
      THIS-OBJECT:MaterielControl:GridMateriel:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:MaterielControl:GridMateriel:GuruCellValjare("BENAMNING").
      nivandvar = "11".
   END METHOD.

   METHOD PUBLIC VOID NyPaslag():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyPaslag(THIS-OBJECT:ControlShell:fillinPaslagBenamning:GuruText:Text, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:RPControl:GridPaslag:GuruFiltrera().
      THIS-OBJECT:RPControl:GridPaslag:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:RPControl:GridPaslag:GuruCellValjare("PASLAG").
      nivandvar = "11".    
      raknavar = "11".  
   END METHOD.

   METHOD PUBLIC VOID NyResurs():
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      
      THIS-OBJECT:ekgresursTTh:FIND-FIRST("WHERE RESURSNR = "  + THIS-OBJECT:ControlShell:fillinResursNr:GuruText:Text ) NO-ERROR.
      IF THIS-OBJECT:ekgresursTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ResursControl:GridResurs:GuruRadvaljare(THIS-OBJECT:ekgresursTTh:ROWID, TRUE).
      END.   
      ELSE DO:
         rid = THIS-OBJECT:ControlDataDb:NyResurs(THIS-OBJECT:ControlShell:fillinResursBenamning:GuruText:Text,INTEGER(THIS-OBJECT:ControlShell:fillinResursNr:GuruText:Text)).
         THIS-OBJECT:ResursControl:GridResurs:GuruFiltrera().
         THIS-OBJECT:ResursControl:GridResurs:GuruRadvaljare(rid, TRUE).
         THIS-OBJECT:ResursControl:GridResurs:GuruCellValjare("ENHET").
         /*nivandvar = "12".*/
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
         THIS-OBJECT:RPControl:GridResursniva:GuruReopen().        
         THIS-OBJECT:RPControl:GridResursniva:GuruRadvaljare(rid, TRUE).         
         nivandvar = "10".
      END.       
      
  END METHOD.
   
   METHOD PUBLIC VOID NyP5():
      IF THIS-OBJECT:ControlShell:fillinP5arbkod:GuruText:Text = "" THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP5LopNr(THIS-OBJECT:ControlShell:fillinP5arbkod:GuruText:Text, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P5Control:GridP5:GuruFiltrera().
      THIS-OBJECT:P5Control:GridP5:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P5Control:GridP5:GuruCellValjare("BENAMNING").
      nivandvar = "6".       
   END METHOD.
   
  METHOD PUBLIC VOID NyP4():
      IF THIS-OBJECT:ControlShell:fillinP4arbkod:GuruText:Text = "" THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP4LopNr(THIS-OBJECT:ControlShell:fillinP4arbkod:GuruText:Text, THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P4Control:GridP4:GuruFiltrera().
      THIS-OBJECT:P4Control:GridP4:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P4Control:GridP4:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
   
  METHOD PUBLIC VOID NyP3ArbKod():
      IF THIS-OBJECT:ControlShell:fillinP3arbkod:GuruText:Text = "" THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP3ArbKod(STRING(THIS-OBJECT:ControlShell:fillinP3arbkod:GuruText:Text), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P3Control:GridP3ArbKod:GuruFiltrera().
      THIS-OBJECT:P3Control:GridP3ArbKod:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P3Control:GridP3ArbKod:GuruCellValjare("BENAMNING").
      THIS-OBJECT:ControlShell:comboP3ArbKod:GuruCombo:GuruFiltrera().
      nivandvar = "8".       
  END METHOD.
  
   METHOD PUBLIC VOID NyP3():
      IF THIS-OBJECT:ControlShell:fillinP3lopnr:GuruText:Text = "" OR STRING(THIS-OBJECT:ControlShell:comboP3arbkod:GuruCombo:VALUE) = "" OR STRING(THIS-OBJECT:ControlShell:comboP3arbkod:GuruCombo:VALUE) = ? THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP3LopNr(STRING(THIS-OBJECT:ControlShell:comboP3arbkod:GuruCombo:VALUE), INTEGER(THIS-OBJECT:ControlShell:fillinP3lopnr:GuruText:Text), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P3Control:GridP3:GuruFiltrera().
      THIS-OBJECT:P3Control:GridP3:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P3Control:GridP3:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
   
  METHOD PUBLIC VOID NyP2ArbKod():
      IF THIS-OBJECT:ControlShell:fillinP2arbkod:GuruText:Text = "" THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP2ArbKod(STRING(THIS-OBJECT:ControlShell:fillinP2arbkod:GuruText:Text), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P2Control:GridP2ArbKod:GuruFiltrera().
      THIS-OBJECT:P2Control:GridP2ArbKod:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P2Control:GridP2ArbKod:GuruCellValjare("BENAMNING"). 
      THIS-OBJECT:ControlShell:comboP2ArbKod:GuruCombo:GuruFiltrera().
      nivandvar = "8".      
  END METHOD.
  
  METHOD PUBLIC VOID NyP2():
      IF THIS-OBJECT:ControlShell:fillinP2lopnr:GuruText:Text = "" OR STRING(THIS-OBJECT:ControlShell:comboP2arbkod:GuruCombo:VALUE) = "" OR STRING(THIS-OBJECT:ControlShell:comboP2arbkod:GuruCombo:VALUE) = ? THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP2LopNr(STRING(THIS-OBJECT:ControlShell:comboP2arbkod:GuruCombo:VALUE), INTEGER(THIS-OBJECT:ControlShell:fillinP2lopnr:GuruText:Text), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P2Control:GridP2:GuruFiltrera().
      THIS-OBJECT:P2Control:GridP2:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P2Control:GridP2:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
  
  METHOD PUBLIC VOID NyP1ArbKod():
      IF THIS-OBJECT:ControlShell:fillinP1arbkod:GuruText:Text = "" THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP1ArbKod(STRING(THIS-OBJECT:ControlShell:fillinP1arbkod:GuruText:Text), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P1Control:GridP1ArbKod:GuruFiltrera().
      THIS-OBJECT:P1Control:GridP1ArbKod:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P1Control:GridP1ArbKod:GuruCellValjare("BENAMNING"). 
      THIS-OBJECT:ControlShell:comboP1ArbKod:GuruCombo:GuruFiltrera().
      nivandvar = "8".      
  END METHOD.
   
  METHOD PUBLIC VOID NyP1():
      IF THIS-OBJECT:ControlShell:fillinP1lopnr:GuruText:Text = "" OR STRING(THIS-OBJECT:ControlShell:comboP1arbkod:GuruCombo:VALUE) = "" OR STRING(THIS-OBJECT:ControlShell:comboP1arbkod:GuruCombo:VALUE) = ? THEN RETURN.
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      rid = THIS-OBJECT:ControlDataDb:NyP1LopNr(STRING(THIS-OBJECT:ControlShell:comboP1arbkod:GuruCombo:VALUE), INTEGER(THIS-OBJECT:ControlShell:fillinP1lopnr:GuruText:Text), THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
      THIS-OBJECT:P1Control:GridP1:GuruFiltrera().
      THIS-OBJECT:P1Control:GridP1:GuruRadvaljare(rid, TRUE).
      THIS-OBJECT:P1Control:GridP1:GuruCellValjare("BENAMNING").
      nivandvar = "6".      
   END METHOD.
   
  METHOD PUBLIC VOID TaBortMtrl():
     DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
     rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(123),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
     IF rrr:ToString() = "Yes" THEN DO:
        IF THIS-OBJECT:ControlDataDb:TaBortMtrl(STRING(THIS-OBJECT:MaterielControl:GridMateriel:ActiveRow:Cells["ARTNR"]:Text)) = TRUE THEN DO:
           THIS-OBJECT:MaterielControl:GridMateriel:GuruBortRader(FALSE).
           THIS-OBJECT:MaterielControl:GridMateriel:GuruReopen().
           nivandvar = "11".           
        END.   
     END.         
  END METHOD.
  
  METHOD PUBLIC VOID TaBortPaslag():
      THIS-OBJECT:ControlDataDb:TaBortPaslag(INTEGER(THIS-OBJECT:RPControl:GridPaslag:ActiveRow:Cells["PASLAGNR"]:Text)).
      THIS-OBJECT:RPControl:GridPaslag:GuruBortRader(FALSE).
      nivandvar = "11".
      raknavar = "11".      
  END METHOD.
  
  METHOD PUBLIC VOID TaBortResursniva(INPUT resnr AS INTEGER, nivnr AS INTEGER):
    DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
    rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(115),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
    IF rrr:ToString() = "Yes" THEN DO:          
        THIS-OBJECT:RPControl:GridResursniva:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.        
        THIS-OBJECT:ekgresursnivaTTh:FIND-FIRST("WHERE RESURSNR = "  + STRING(resnr)  + " AND NIVA = " + STRING(resnr)) NO-ERROR.
        THIS-OBJECT:RPControl:GridResursniva:GuruBortRader(FALSE).
        nivandvar = "12".               
     END.   
  END METHOD.
  
  METHOD PUBLIC VOID TaBortResurspris(INPUT resnr AS INTEGER):
    DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
    rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(102),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
    IF rrr:ToString() = "Yes" THEN DO:    
        THIS-OBJECT:RPControl:GridResurspris:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.  
        IF THIS-OBJECT:ControlDataDb:TaBortResurspris(resnr) = TRUE THEN DO:                   
           THIS-OBJECT:RPControl:GridResurspris:GuruBortRader(FALSE).
           THIS-OBJECT:RPControl:GridResursNiva:GuruReopen(). 
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
  METHOD PUBLIC VOID AndringP2kom():
     nivandvar = "P2Kom".
  END METHOD. 
  METHOD PUBLIC VOID UppdateraKatalogRibbon(): 
      THIS-OBJECT:ekgsubkatalogerTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(THIS-OBJECT:EkgSubidVar)) NO-ERROR.
      IF THIS-OBJECT:ekgsubkatalogerTTh:AVAILABLE THEN DO:         
         /*THIS-OBJECT:RibbonToolSubKatID:Text = THIS-OBJECT:ekgsubkatTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE.*/
         THIS-OBJECT:RibbonToolEbrKat:Text = "ID: " + STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + " Typ: " + STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE).
         THIS-OBJECT:RibbonToolBenamning:Text = THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         THIS-OBJECT:RibbonToolArtal:Text = "Årtal: " + STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE).     
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
      IF THIS-OBJECT:ResursControl:gridResurs:Posn() = -1 THEN RETURN.
      ekgresursprisTTh:FIND-FIRST("WHERE RESURSNR = " + STRING(THIS-OBJECT:ekgresursTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ekgresursprisTTh:AVAILABLE THEN.
      ELSE DO:
         THIS-OBJECT:RPControl:buttonOverResurspris:GuruOver().
         THIS-OBJECT:ControlDataDb:ResursprisUpd(THIS-OBJECT:EkgSubidVar, THIS-OBJECT:EbrKatVar).
         THIS-OBJECT:RPControl:buttonOverResurspris:GuruOverefteruppdat().
         IF THIS-OBJECT:ResursControl:gridResurs:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:RPControl:buttonOverResurspris:GuruTogrid:GuruCellvaljare("PRIS").
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
         THIS-OBJECT:ControlDataDb:BeraknaP3enresurs(THIS-OBJECT:EbrKatVar).
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
      IF THIS-OBJECT:RPControl:gridResursPaslag:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:buttonOverBortFraga() = TRUE THEN DO:
         dontrepo = TRUE.
         THIS-OBJECT:RPControl:buttonOverResurspaslagBort:GuruOver().
         dontrepo = TRUE.
      END.   
      ELSE RETURN.
      nivandvar = "10".
      raknavar = "10".
   END METHOD.
   
   METHOD PUBLIC VOID buttonOverResurspaslag(): 
      IF THIS-OBJECT:RPControl:gridPaslag:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:RPControl:gridResursPris:Posn() = -1 THEN RETURN.
      THIS-OBJECT:RPControl:gridResursPaslag:Guruegenskap:filter = "".
      THIS-OBJECT:RPControl:gridResursPaslag:GuruFiltrera("ekgresurspaslagTT.RESURSNR = " + STRING(THIS-OBJECT:ekgresursprisTTh:BUFFER-FIELD("RESURSNR"):BUFFER-VALUE)).
      THIS-OBJECT:RPControl:buttonOverResurspaslag:GuruOver().
      THIS-OBJECT:RPControl:gridResursPaslag:Guruegenskap:filter = "".
      THIS-OBJECT:ControlDataDb:ResurspaslagsUpd().
      THIS-OBJECT:RPControl:buttonOverResurspaslag:GuruOverefteruppdat().
      nivandvar = "10".
      raknavar = "10".
   END METHOD.
   
   METHOD PUBLIC VOID VisaAllaKoderKoll(INPUT nivfrek AS CHARACTER): 
      IF THIS-OBJECT:RibbonToolVisaAllaKoder:Checked = FALSE THEN RETURN.
      IF nivfrek = "P4F" THEN THIS-OBJECT:P4Control:GridP4Frekvens:GuruFiltrera("ekgp4frekvensTT.P4ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4ARBKOD"):BUFFER-VALUE)+ "' AND ekgp4frekvensTT.P4LOPNR = " + STRING(THIS-OBJECT:ekgp4TTh:BUFFER-FIELD("P4LOPNR"):BUFFER-VALUE)).      
      IF nivfrek = "P3F" THEN THIS-OBJECT:P3Control:GridP3Frekvens:GuruFiltrera("ekgp3frekvensTT.P3ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)+ "' AND ekgp3frekvensTT.P3LOPNR = " + STRING(THIS-OBJECT:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
      IF nivfrek = "P2F" THEN THIS-OBJECT:P2Control:GridP2Frekvens:GuruFiltrera("ekgp2frekvensTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2frekvensTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
      IF nivfrek = "P1F" THEN THIS-OBJECT:P1Control:GridP1Frekvens:GuruFiltrera("ekgp1frekvensTT.P1ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)+ "' AND ekgp1frekvensTT.P1LOPNR = " + STRING(THIS-OBJECT:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
      IF nivfrek = "P2M" THEN THIS-OBJECT:P2Control:GridP2Mtrl:GuruFiltrera("ekgp2mtrlTT.P2ARBKOD = " + "'" + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)+ "' AND ekgp2mtrlTT.P2LOPNR = " + STRING(THIS-OBJECT:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)). 
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
   
   METHOD PUBLIC VOID ExcelTillWord(): 
      THIS-OBJECT:VisaExcel:Kopiera("A1:N45").
   END METHOD.
   
   DESTRUCTOR EkgWinLg():
      
      DELETE OBJECT  THIS-OBJECT:VisaExcel NO-ERROR.
   END DESTRUCTOR.   
       
END CLASS.

 /*------------------------------------------------------------------------
    File        : KalkylAdm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 13:32:37 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.

 

CLASS Modules.KalkylAdm.KalkylAdmShell INHERITS UserControl: 
   DEFINE VARIABLE cFrom AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkAdmdbControl  AS Modules.KalkylAdm.KalkylAdmDb NO-UNDO.
   {KALKYLEXTRATAB.I}
   DEFINE PUBLIC VARIABLE KalkylAdmKatControl                 AS Modules.KalkylAdm.KalkylAdmKatalog                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylAdmMallControl                 AS Modules.KalkylAdm.KalkylAdmMall                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylAdmSekControl                 AS Modules.KalkylAdm.KalkylAdmSek                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylAdmPrisControl                 AS Modules.KalkylAdm.KalkylAdmPris                                  NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylAdmKodControl                 AS Modules.KalkylAdm.KalkylAdmKod                                  NO-UNDO.
   DEFINE PUBLIC VARIABLE CellChvar AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.
   DEFINE VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   
   /*start*/
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   
   
	/*fönster*/   
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMall AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabPriser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabSek AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKoder AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylAdm AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   
   /* Ribbongrupper */
   
   DEFINE         VARIABLE RibbonGroupAdm                  AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupKatalog              AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupDelKatalog           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupPrisKatalog          AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupVisaPristyp          AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupArbetskoder          AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
   DEFINE         VARIABLE RibbonGroupSpara                AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
   /*ribbon ellement*/
   DEFINE         VARIABLE RibbonDummy                  AS Infragistics.Win.UltraWinToolbars.LabelTool                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmDelKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmPriser            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmSek            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmFrek              AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
	DEFINE PUBLIC  VARIABLE RibbonToolArtal            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSek            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolStyrande            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkapad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAvslutad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*BEHÖVS EJ
   DEFINE PUBLIC  VARIABLE RibbonToolDelKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKopplaDelKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   */
   DEFINE PUBLIC  VARIABLE RibbonToolDelSkapad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolDelAvslutad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE RibbonToolPrisKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO. 
   DEFINE PUBLIC  VARIABLE RibbonToolSokBen            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolTaEgnaKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolTaEgnaPriser            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE RibbonToolVisningRegel            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.   
	
	DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolTypKalk            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolLopnr              AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*Drag Dropp*/
   DEFINE         VARIABLE DGAnvand                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGHuvkat                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
	DEFINE         VARIABLE DGLopMallar                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
	
	DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.	
	DEFINE PUBLIC  VARIABLE RibbonToolManual             AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
	
   DEFINE VARIABLE katfrekinH AS HANDLE NO-UNDO.
  CONSTRUCTOR PUBLIC KalkylAdmShell ( INPUT r AS Guru.Root ):
	   SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:InitializeTTs().
      
      THIS-OBJECT:KalkAdmdbControl = THIS-OBJECT:Root:DatabaseManager:KalkylAdm.
      
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
       
	END CONSTRUCTOR.

		
	METHOD PUBLIC VOID AfterCopyKat( ):
      THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:GuruReopen().
      THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:GuruFirstrow().
      THIS-OBJECT:ValueGridSubKatKoderCh(?,?).
       
      RETURN.
   END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID MallarUppdate(INPUT mallvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      
      THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(mallvar)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:AVAILABLE THEN DO:
         THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
      THIS-OBJECT:KalkylAdmMallControl:GridMallar:GuruReopen().
      
      RETURN.
   END METHOD.
	
	METHOD PUBLIC VOID MallarValdaUppdate(INPUT arbvar AS CHARACTER, INPUT lopvar AS CHARACTER,INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      THIS-OBJECT:KalkAdmdbControl:KalkmallKodertth:FIND-FIRST("WHERE ARBKOD = '" + STRING(arbvar) + "' AND LOPNR = " + STRING(lopvar)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:KalkmallKodertth:AVAILABLE THEN DO:
         THIS-OBJECT:KalkAdmdbControl:KalkmallKodertth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
      THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:GuruReopen().
      
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID KatalogUppdate(INPUT klogidvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(klogidvar)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:KatalogTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.
      queryvar = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:IngKatalogTTh:TABLE + " WHERE KLOGID = " + STRING(klogidvar). 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkAdmdbControl:IngKatalogTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkAdmdbControl:IngKatalogTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(klogidvar)) NO-ERROR.
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruReopen().
      THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruReopen().
      THIS-OBJECT:KalkylAdmSekControl:GridKatSek:GuruReopen().  
      /*    
      THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(klogidvar)) NO-ERROR.
        */    
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID KatalogAfterBort():
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID KatalogSubUppdate(INPUT klogsubidvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      queryvar = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:TABLE + " WHERE KLOGSUBID =" + klogsubidvar. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.      
         qH:GET-NEXT().
      END.
      
      queryvar = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:TABLE + " WHERE KLOGSUBID =" + klogsubidvar. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkAdmdbControl:Katalogsubtth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruReopen().
      THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:GuruReopen().  
      THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruReopen().  
      THIS-OBJECT:KalkylAdmMallControl:GridSubMall:GuruReopen().
      /*
      THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(klogsubidvar)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:AVAILABLE THEN THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(klogsubidvar)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:AVAILABLE THEN THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().*/
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID KatalogPrisUppdate(INPUT inqueryvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      queryvar = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:PriserTTh:TABLE + " WHERE " + inqueryvar. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkAdmdbControl:PriserTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkAdmdbControl:PriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      IF kolnamn = "BENAMNING" OR kolnamn = "PRIS" THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:LopsubTTh:TABLE + " WHERE " + inqueryvar. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkAdmdbControl:LopsubTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.      
            qH:GET-NEXT().
         END.
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      END.
     
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ControlShell = THIS-OBJECT. 
      THIS-OBJECT:StartaControl().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKatalog(?).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarHmt(0).
      THIS-OBJECT:CreateForm().
      
      
      /*
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylAdmDef().
      */  
      
      RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID StartaControl():
    
      KalkylAdmKatControl = NEW Modules.KalkylAdm.KalkylAdmKatalog(THIS-OBJECT:Root).
      THIS-OBJECT:TabKataloger:Controls:Add(THIS-OBJECT:KalkylAdmKatControl).
      KalkylAdmKatControl:Dock = System.Windows.Forms.DockStyle:FILL. 
      
      KalkylAdmMallControl = NEW Modules.KalkylAdm.KalkylAdmMall(THIS-OBJECT:Root).
      THIS-OBJECT:TabMall:Controls:Add(THIS-OBJECT:KalkylAdmMallControl).
      KalkylAdmMallControl:Dock = System.Windows.Forms.DockStyle:FILL. 
      
      KalkylAdmSekControl = NEW Modules.KalkylAdm.KalkylAdmSek(THIS-OBJECT:Root).
      THIS-OBJECT:TabSek:Controls:Add(THIS-OBJECT:KalkylAdmSekControl).
      KalkylAdmSekControl:Dock = System.Windows.Forms.DockStyle:FILL. 
      
      KalkylAdmPrisControl = NEW Modules.KalkylAdm.KalkylAdmPris(THIS-OBJECT:Root).
      THIS-OBJECT:TabPriser:Controls:Add(THIS-OBJECT:KalkylAdmPrisControl).
      KalkylAdmPrisControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkylAdmKodControl = NEW Modules.KalkylAdm.KalkylAdmKod(THIS-OBJECT:Root).
      THIS-OBJECT:TabKoder:Controls:Add(THIS-OBJECT:KalkylAdmKodControl).
      KalkylAdmKodControl:Dock = System.Windows.Forms.DockStyle:FILL.
         
   END METHOD. 
   
   METHOD PUBLIC VOID ExcelImportLista(vad AS INTEGER, filnamn AS CHARACTER):
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:ReadExcelStart(INPUT filnamn).   
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF vad = 1 THEN DO:
            IF invar NE "" THEN DO:            
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-CREATE().           
                  klangd = LENGTH(invar).
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 3))).
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 2),3)) NO-ERROR.               
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = invar.
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).    
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("BANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MOANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MAANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "H" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "I" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MATERIEL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "J" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "K" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("OVRKOST"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "L" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("TOTSUM"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "M" THEN THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("KOMM"):BUFFER-VALUE = invar.
            END.   
            THIS-OBJECT:VisaExcel:ColRight().
            IF THIS-OBJECT:VisaExcel:cColname = "N" THEN DO:
               THIS-OBJECT:VisaExcel:cColname = "A". 
               THIS-OBJECT:VisaExcel:Rowdown().
            END.            
         END.   
         ELSE IF vad = 2 THEN DO:
            IF invar NE "" THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-CREATE().                             
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                                 
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
               END.   
               IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = invar.
               IF THIS-OBJECT:VisaExcel:cColname = "D" THEN THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = TRIM(invar).    
               IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("BANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MOANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "G" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MAANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "I" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "J" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MATERIEL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "K" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "L" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("OVRKOST"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "M" THEN DO:
                  invar = REPLACE(invar,",",".").
                 THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("TOTSUM"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
               IF THIS-OBJECT:VisaExcel:cColname = "N" THEN THIS-OBJECT:KalkAdmdbControl:KaladmimportTTh:BUFFER-FIELD("KOMM"):BUFFER-VALUE = invar.
            END.   
               
            THIS-OBJECT:VisaExcel:ColRight().
            IF THIS-OBJECT:VisaExcel:cColname = "O" THEN DO:
               THIS-OBJECT:VisaExcel:cColname = "A". 
               THIS-OBJECT:VisaExcel:Rowdown().
            END.                                               
         END.                     
      END.  
      THIS-OBJECT:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      /*THIS-OBJECT:ImportLista().*/
   END METHOD.
     

   
   METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab(TRUE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.KalkylAdm.KalkylAdmShell").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabPriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabKoder = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabMall = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabSek = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabKalkylAdm = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      CAST(THIS-OBJECT:TabKalkylAdm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkylAdm:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      /*  */
      /* TabKataloger */
      /*  */
      THIS-OBJECT:TabKataloger:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKataloger:Name = "TabKataloger".
      THIS-OBJECT:TabKataloger:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* TabPriser */
      /*  */
      THIS-OBJECT:TabPriser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabPriser:Name = "TabPriser".
      THIS-OBJECT:TabPriser:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* TabKoder */
      /*  */
      THIS-OBJECT:TabKoder:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKoder:Name = "TabKoder".
      THIS-OBJECT:TabKoder:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* TabMall */
      /*  */
      THIS-OBJECT:TabMall:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMall:Name = "TabMall".
      THIS-OBJECT:TabMall:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* TabSek */
      /*  */
      THIS-OBJECT:TabSek:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabSek:Name = "TabSek".
      THIS-OBJECT:TabSek:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* TabKalkylAdm */
      /*  */
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabPriser).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabKataloger).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabKoder).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabSek).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabMall).
      THIS-OBJECT:TabKalkylAdm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabKalkylAdm:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabKalkylAdm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabKalkylAdm:Name = "TabKalkylAdm".
      THIS-OBJECT:TabKalkylAdm:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabKalkylAdm:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:TabKalkylAdm:TabIndex = 0.
      THIS-OBJECT:TabKalkylAdm:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
      ultraTab2:Appearance = appearance1.
      ultraTab2:Key = "Kataloger".
      ultraTab2:TabPage = THIS-OBJECT:TabKataloger.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Kataloger".
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      ultraTab1:Appearance = appearance2.
      ultraTab1:Key = "Priser".
      ultraTab1:TabPage = THIS-OBJECT:TabPriser.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Priser".
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      ultraTab3:Appearance = appearance3.
      ultraTab3:Key = "Koder".
      ultraTab3:TabPage = THIS-OBJECT:TabKoder.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Arbetskoder".
      ultraTab4:Key = "Visa".
      ultraTab4:TabPage = THIS-OBJECT:TabVisa.
      ultraTab4:Text = "Visa".
      ultraTab4:ToolTipText = "Visa kataloger".
      appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
      ultraTab6:Appearance = appearance4.
      ultraTab6:Key = "Mallar".
      ultraTab6:TabPage = THIS-OBJECT:TabMall.
      ultraTab6:Text = "".
      ultraTab6:ToolTipText = "Mallar".
      appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
      ultraTab5:Appearance = appearance5.
      ultraTab5:Key = "Sekretess".
      ultraTab5:TabPage = THIS-OBJECT:TabSek.
      ultraTab5:Text = "".
      ultraTab5:ToolTipText = "Sekretess".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 6 NO-UNDO.
      arrayvar0[1] = ultraTab2.
      arrayvar0[2] = ultraTab1.
      arrayvar0[3] = ultraTab3.
      arrayvar0[4] = ultraTab4.
      arrayvar0[5] = ultraTab6.
      arrayvar0[6] = ultraTab5.
      THIS-OBJECT:TabKalkylAdm:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabKalkylAdm:SelectedTabChanging:Subscribe(THIS-OBJECT:TabKalkylAdm_SelectedTabChanging).
      THIS-OBJECT:TabKalkylAdm:SelectedTabChanged:Subscribe(THIS-OBJECT:TabKalkylAdm_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* KalkylAdmShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabKalkylAdm).
      THIS-OBJECT:Name = "KalkylAdmShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1053, 556).
      CAST(THIS-OBJECT:TabKalkylAdm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkylAdm:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	

   METHOD PUBLIC VOID StartKalkAdm():
      
   END METHOD. 
     
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateComboTyptt().
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateFunctions().
      THIS-OBJECT:CreateRibbon().
      THIS-OBJECT:ValueGridSubKatKoderCh(?,?).
      THIS-OBJECT:ForetagAnpass().
      THIS-OBJECT:TabKalkylAdm:Tabs["Kataloger"]:Selected = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID CreateFunctions():
       /*Drag dropp*/
      THIS-OBJECT:DGAnvand = NEW Controls.Subclasses.DragDropGroup("Anvandare").
      THIS-OBJECT:DGAnvand:AddChild(THIS-OBJECT:KalkylAdmSekControl:GridAnvSek).
      THIS-OBJECT:DGAnvand:AddChild(THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek).
      
      THIS-OBJECT:DGHuvkat = NEW Controls.Subclasses.DragDropGroup("Kataloger").
      THIS-OBJECT:DGHuvkat:AddChild(THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel).
      THIS-OBJECT:DGHuvkat:AddChild(THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel).
      
      THIS-OBJECT:DGLopMallar = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGLopMallar:AddChild(THIS-OBJECT:KalkylAdmMallControl:GridLopMall).
      THIS-OBJECT:DGLopMallar:AddChild(THIS-OBJECT:KalkylAdmMallControl:GridValdMallar). 
      
       /* Button overs*/
      THIS-OBJECT:KalkylAdmSekControl:ButtonOverAnvMark:GuruKoppla(THIS-OBJECT:KalkylAdmSekControl:GridAnvSek,THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek).
      THIS-OBJECT:KalkylAdmSekControl:ButtonOverAnvMark:GuruDublett = FALSE.
      THIS-OBJECT:KalkylAdmSekControl:ButtonBackAnvMark:GuruKoppla(THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek,THIS-OBJECT:KalkylAdmSekControl:GridAnvSek).
      THIS-OBJECT:KalkylAdmSekControl:ButtonBackAnvMark:GuruRemoveFrom = TRUE.  
    
       /* Button overs*/
      THIS-OBJECT:KalkylAdmKatControl:ButtonOverHuvKat:GuruKoppla(THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel,THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel).
      THIS-OBJECT:KalkylAdmKatControl:ButtonOverHuvKat:GuruDublett = FALSE.
      THIS-OBJECT:KalkylAdmKatControl:ButtonBackHuvKat:GuruKoppla(THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel,THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel).
      THIS-OBJECT:KalkylAdmKatControl:ButtonBackHuvKat:GuruRemoveFrom = TRUE.  
      /* Button overs*/
      THIS-OBJECT:KalkylAdmMallControl:ButtonOverMall:GuruKoppla(THIS-OBJECT:KalkylAdmMallControl:GridLopMall,THIS-OBJECT:KalkylAdmMallControl:GridValdMallar).
      THIS-OBJECT:KalkylAdmMallControl:ButtonOverMall:GuruDublett = FALSE.
      THIS-OBJECT:KalkylAdmMallControl:ButtonBackMall:GuruKoppla(THIS-OBJECT:KalkylAdmMallControl:GridValdMallar,THIS-OBJECT:KalkylAdmMallControl:GridLopMall).
      THIS-OBJECT:KalkylAdmMallControl:ButtonBackMall:GuruRemoveFrom = TRUE.
      
        /* Button overs*/
      THIS-OBJECT:KalkylAdmKodControl:AddPrisButtonOverMark:GuruKoppla(THIS-OBJECT:KalkylAdmKodControl:GridAddPris,THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub).
      THIS-OBJECT:KalkylAdmKodControl:AddPrisButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:KalkylAdmKodControl:AddPrisButtonBackMark:GuruKoppla(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub,THIS-OBJECT:KalkylAdmKodControl:GridAddPris).
      THIS-OBJECT:KalkylAdmKodControl:AddPrisButtonBackMark:GuruRemoveFrom = TRUE.
      
      
      
        
   END METHOD.
   METHOD PUBLIC VOID CreateComboTyptt():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).                    
      IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 3 THEN DO:
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 0
         typtt.TYPC    = "Alla".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 1
         typtt.TYPC    = "Typ 1".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 2
         typtt.TYPC    = "Typ 2".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 3
         typtt.TYPC    = "Typ 3".           
      END.
      /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "cGRAN" OR Guru.Konstanter:globforetag = "VAST" THEN DO:*/
      IF Guru.Konstanter:varforetypchar[10] = "1" THEN DO:    
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 5
         typtt.TYPC    = "Sam B/F".        
      END.
      IF Guru.Konstanter:varforetypval[41] = 1 THEN DO:  
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 7
         typtt.TYPC    = "Nätreg N1".      
      END.
      
   END METHOD.
     
   METHOD PUBLIC VOID CreateRibbon():
        /*första ribbon tab*/
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      /*öriag tabbar om inte i Guru.ModuleManager*/
      /*     THIS-OBJECT:Root:ModuleManager:RibbonTab("kkkc","pcpp"). */
      /* Skapa ribbongrupp i kalkyltaben */
      
      THIS-OBJECT:RibbonGroupAdm = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Välj administration").
      THIS-OBJECT:RibbonGroupKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Katalog"). /**/
      THIS-OBJECT:RibbonGroupKatalog:Caption = "Kataloger".
      THIS-OBJECT:RibbonGroupDelKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Delkatalog").   /**/
      THIS-OBJECT:RibbonGroupDelKatalog:Caption = "Delkataloger".
      THIS-OBJECT:RibbonGroupPrisKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("PrisKatalog"). /**/
      THIS-OBJECT:RibbonGroupPrisKatalog:Caption = "Katalogpriser".
      THIS-OBJECT:RibbonGroupVisaPristyp = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visningsregler").
      THIS-OBJECT:RibbonGroupArbetskoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Arbetskoder").
      
      THIS-OBJECT:RibbonGroupSpara = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Spara inställningar").
      
      THIS-OBJECT:RibbonDummy = NEW Infragistics.Win.UltraWinToolbars.LabelTool("RD").
      THIS-OBJECT:RibbonDummy:SharedPropsInternal:Caption = " ".
      /*adm*/
      THIS-OBJECT:RibbonToolAdmKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmKod").
      THIS-OBJECT:RibbonToolAdmKoder:SharedPropsInternal:Caption = "Koder".
      THIS-OBJECT:RibbonToolAdmKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmKatalog").
      THIS-OBJECT:RibbonToolAdmKatalog:SharedPropsInternal:Caption = "Kataloger".
      THIS-OBJECT:RibbonToolAdmKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmDelKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmDelKatalog").
      THIS-OBJECT:RibbonToolAdmDelKatalog:SharedPropsInternal:Caption = "Delkataloger".
      THIS-OBJECT:RibbonToolAdmDelKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmPriser").
      THIS-OBJECT:RibbonToolAdmPriser:SharedPropsInternal:Caption = "Priser".
      THIS-OBJECT:RibbonToolAdmPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmSek = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmSek").
      THIS-OBJECT:RibbonToolAdmSek:SharedPropsInternal:Caption = "Sekretess".
      THIS-OBJECT:RibbonToolAdmSek:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmFrek = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmFrek").
      THIS-OBJECT:RibbonToolAdmFrek:SharedPropsInternal:Caption = "Frekvenstabeller".
      THIS-OBJECT:RibbonToolAdmFrek:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*Kataloger*/
      THIS-OBJECT:RibbonToolKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Katalog").
      THIS-OBJECT:RibbonToolKatalog:SharedPropsInternal:Caption = "Kataloger".
      THIS-OBJECT:RibbonToolKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolArtal = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visaartal").
      THIS-OBJECT:RibbonToolArtal:SharedPropsInternal:Caption = "Visa årtal".
      THIS-OBJECT:RibbonToolArtal:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSek = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visasek").
      THIS-OBJECT:RibbonToolSek:SharedPropsInternal:Caption = "Visa sekretess".
      THIS-OBJECT:RibbonToolSek:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolStyrande = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visadelk").
      THIS-OBJECT:RibbonToolStyrande:SharedPropsInternal:Caption = "Visa delkatalognr".
      THIS-OBJECT:RibbonToolStyrande:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSkapad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visaskapad").
      THIS-OBJECT:RibbonToolSkapad:SharedPropsInternal:Caption = "Visa skapad".
      THIS-OBJECT:RibbonToolSkapad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAvslutad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visaavsl").
      THIS-OBJECT:RibbonToolAvslutad:SharedPropsInternal:Caption = "Visa avslutad".
      THIS-OBJECT:RibbonToolAvslutad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*DelKataloger*/
      /* BEHÖVS EJ
      THIS-OBJECT:RibbonToolDelKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Delkatalog").
      THIS-OBJECT:RibbonToolDelKatalog:SharedPropsInternal:Caption = "Delkatalog".
      THIS-OBJECT:RibbonToolDelKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolKopplaDelKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KopplaDelKatalog").
      THIS-OBJECT:RibbonToolKopplaDelKatalog:SharedPropsInternal:Caption = "Koppla delkataloger".
      THIS-OBJECT:RibbonToolKopplaDelKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      */
      THIS-OBJECT:RibbonToolDelSkapad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visadelskapad").
      THIS-OBJECT:RibbonToolDelSkapad:SharedPropsInternal:Caption = "Visa skapad".
      THIS-OBJECT:RibbonToolDelSkapad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*
      THIS-OBJECT:RibbonToolDelAvslutad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visdelavsl").
      THIS-OBJECT:RibbonToolDelAvslutad:SharedPropsInternal:Caption = "Visa avslutad".
      THIS-OBJECT:RibbonToolDelAvslutad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      */
      /*prislistor*/
      THIS-OBJECT:RibbonToolPrisKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("PrisKatalog").
      THIS-OBJECT:RibbonToolPrisKatalog:SharedPropsInternal:Caption = "Katalogpriser".
      THIS-OBJECT:RibbonToolPrisKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSokBen = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("SokBenPris").
      THIS-OBJECT:RibbonToolSokBen:SharedPropsInternal:Caption = "Visa sökbenämning".
      THIS-OBJECT:RibbonToolSokBen:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolTaEgnaKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("TaEgnaKoder").
      THIS-OBJECT:RibbonToolTaEgnaKoder:SharedPropsInternal:Caption = "Visa egna koder".
      THIS-OBJECT:RibbonToolTaEgnaKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolTaEgnaPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("TaEgnaPriser").
      THIS-OBJECT:RibbonToolTaEgnaPriser:SharedPropsInternal:Caption = "Visa egna priser".
      THIS-OBJECT:RibbonToolTaEgnaPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*visnings regler*/
      THIS-OBJECT:RibbonToolVisningRegel = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisRegel").
      THIS-OBJECT:RibbonToolVisningRegel:SharedPropsInternal:Caption = "Visningsregler".
      THIS-OBJECT:RibbonToolVisningRegel:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*arbkoder*/
      THIS-OBJECT:RibbonToolVisaTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisaTyp").
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisaTyp:SharedProps:Visible = TRUE.
      THIS-OBJECT:RibbonToolVisaTyp:Key = "KalkylRibbonVisaTyp".
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width = 135.
      DEFINE VARIABLE wTemp AS Infragistics.Win.ValueListItem NO-UNDO.       
      FOR EACH typtt:
         wTemp = NEW Infragistics.Win.ValueListItem().
         wTemp:DisplayText = typtt.TYPC.
         wTemp:DataVALUE = NEW Helpers.ABLDataContainer().
         CAST(wTemp:DataVALUE, Helpers.ABLDataContainer):CreateData("Typ", typtt.TYPKALK).
         THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Add(wTemp).
      END. 
      THIS-OBJECT:RibbonToolFilterTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FilterTyp").
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Caption = "Märkning".
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      
      THIS-OBJECT:RibbonToolLopnr = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaLopnr").
      THIS-OBJECT:RibbonToolLopnr:SharedPropsInternal:Caption = "Visa löpnummerpriser".
      THIS-OBJECT:RibbonToolLopnr:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      THIS-OBJECT:RibbonToolLopnr:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens-kommentar".
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolTypKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaTypKalk").
      THIS-OBJECT:RibbonToolTypKalk:SharedPropsInternal:Caption = "Visa kalkyltyp".
      THIS-OBJECT:RibbonToolTypKalk:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      THIS-OBJECT:RibbonToolTypKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
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
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmKoder,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmKatalog,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmDelKatalog,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmPriser,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmSek,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmFrek,THIS-OBJECT:RibbonGroupAdm).
                
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKatalog,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolArtal,RibbonGroupKatalog).
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSek,RibbonGroupKatalog).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkapad,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAvslutad,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolStyrande,RibbonGroupKatalog).
      /*BEHÖVS EJ
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelKatalog,RibbonGroupDelKatalog).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelSkapad,RibbonGroupDelKatalog).
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelAvslutad,RibbonGroupDelKatalog).
      */
      /*BEHÖVS EJ
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKopplaDelKatalog,RibbonGroupDelKatalog).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolPrisKatalog,RibbonGroupPrisKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSokBen,RibbonGroupPrisKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolTaEgnaKoder,RibbonGroupPrisKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolTaEgnaPriser,RibbonGroupPrisKatalog).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisningRegel,RibbonGroupVisaPristyp).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolTypKalk,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLopnr,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFrekvensKom,RibbonGroupArbetskoder).
      /*
      THIS-OBJECT:RibbonGroupArbetskoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolManual,THIS-OBJECT:RibbonGroupSpara).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs,RibbonGroupSpara).
      /*nästan alla start med true*/
      ASSIGN 
      THIS-OBJECT:RibbonToolAdmKoder:Checked = FALSE
      THIS-OBJECT:RibbonToolAdmKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmDelKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmPriser:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmSek:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmFrek:Checked = TRUE
      
      
      THIS-OBJECT:RibbonToolKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolArtal:Checked = TRUE
      THIS-OBJECT:RibbonToolSek:Checked = TRUE
      THIS-OBJECT:RibbonToolStyrande:Checked = TRUE
      THIS-OBJECT:RibbonToolSkapad:Checked = TRUE
      THIS-OBJECT:RibbonToolAvslutad:Checked = TRUE
      /*BEHÖVS EJ
      THIS-OBJECT:RibbonToolDelKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolKopplaDelKatalog:Checked = TRUE
      */
      THIS-OBJECT:RibbonToolDelSkapad:Checked = TRUE
      /*
      THIS-OBJECT:RibbonToolDelAvslutad:Checked = TRUE
      */
      THIS-OBJECT:RibbonToolPrisKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolSokBen:Checked = TRUE
      THIS-OBJECT:RibbonToolTaEgnaKoder:Checked = TRUE
      THIS-OBJECT:RibbonToolTaEgnaPriser:Checked = TRUE
      
      THIS-OBJECT:RibbonToolVisningRegel:Checked = TRUE
      
      THIS-OBJECT:RibbonToolLopnr:Checked = FALSE.
      THIS-OBJECT:RibbonToolFrekvensKom:Checked = TRUE.
      THIS-OBJECT:RibbonToolTypKalk:Checked = TRUE.
      
      /*eventS triggers*/
      THIS-OBJECT:RibbonToolKatalog:ToolClick:SUBSCRIBE(RibbonKatalogChanged).
      THIS-OBJECT:RibbonToolArtal:ToolClick:SUBSCRIBE(RibbonArtalChanged).
      THIS-OBJECT:RibbonToolSek:ToolClick:SUBSCRIBE(RibbonSekKatalogChanged).
      THIS-OBJECT:RibbonToolStyrande:ToolClick:SUBSCRIBE(RibbonStyrandeChanged).
      THIS-OBJECT:RibbonToolSkapad:ToolClick:SUBSCRIBE(RibbonSkapKatalogChanged).
      THIS-OBJECT:RibbonToolAvslutad:ToolClick:SUBSCRIBE(RibbonAvsKatalogChanged).
      /*BEHÖVS EJ
      THIS-OBJECT:RibbonToolDelKatalog:ToolClick:SUBSCRIBE(RibbonDelKatalogChanged).
      THIS-OBJECT:RibbonToolKopplaDelKatalog:ToolClick:SUBSCRIBE(RibbonKopplaDelKatalogChanged).
      */
      THIS-OBJECT:RibbonToolDelSkapad:ToolClick:SUBSCRIBE(RibbonSkapDelKatalogChanged).
      /*
      THIS-OBJECT:RibbonToolDelAvslutad:ToolClick:SUBSCRIBE(RibbonAvsDelKatalogChanged).
      */
      THIS-OBJECT:RibbonToolPrisKatalog:ToolClick:SUBSCRIBE(RibbonPrisKatChanged).
      THIS-OBJECT:RibbonToolSokBen:ToolClick:SUBSCRIBE(RibbonSokBenChanged).
      THIS-OBJECT:RibbonToolTaEgnaKoder:ToolClick:SUBSCRIBE(RibbonTaEgnaKoderChanged).
      THIS-OBJECT:RibbonToolTaEgnaPriser:ToolClick:SUBSCRIBE(RibbonTaEgnaPriserChanged).
      
      THIS-OBJECT:RibbonToolVisningRegel:ToolClick:SUBSCRIBE(RibbonVisningRegelChanged).
      
      THIS-OBJECT:RibbonToolVisaTyp:ToolValueChanged:Subscribe(RibbonUpdateTyp).
      THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder). 
      THIS-OBJECT:RibbonToolFrekvensKom:ToolClick:Subscribe(THIS-OBJECT:RibbonFrekvensKomChanged).
      THIS-OBJECT:RibbonToolTypKalk:ToolClick:Subscribe(THIS-OBJECT:RibbonTypKalkChanged).
      
      THIS-OBJECT:RibbonToolLopnr:ToolClick:Subscribe(THIS-OBJECT:RibbonLopnrChanged).
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:SUBSCRIBE(SparaDefs).
      THIS-OBJECT:RibbonToolManual:ToolClick:Subscribe(Manual).
      
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      THIS-OBJECT:RefreshMarkning(TRUE).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupAdm).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupKatalog).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupDelKatalog).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupPrisKatalog).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupVisaPristyp).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupArbetskoder).
      
   END METHOD.
   
   METHOD PUBLIC VOID Manual(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      DEFINE VARIABLE path AS CHARACTER.
     
      path = SEARCH("Manualer\Kap5 KalkylAdm.pdf").
      System.Diagnostics.Process:Start(path).
   END METHOD.
   
   METHOD PUBLIC VOID RibbonKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKatalog:Checked EQ TRUE THEN DO:
         /*THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = FALSE.
         THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = FALSE.*/
                  
      END.
      ELSE DO:
         /*IF THIS-OBJECT:splitContainerKatalogSubKat:Panel2Collapsed = TRUE THEN  THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = TRUE.*/
        
      END.
    
   END METHOD.
   METHOD PUBLIC VOID RibbonLopnrChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolLopnr:Checked = TRUE THEN THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE.   
      IF THIS-OBJECT:RibbonToolLopnr:Checked = FALSE THEN THIS-OBJECT:RibbonToolFrekvensKom:Checked = TRUE.
      THIS-OBJECT:KalkylAdmKodControl:splitContainerLopsubBild:Panel1Collapsed = FALSE.      
   END METHOD.
   
   METHOD PUBLIC VOID RibbonFrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         ASSIGN
         THIS-OBJECT:RibbonToolLopnr:Checked                     = FALSE .
         /*
         THIS-OBJECT:KalkylAdmKodControl:GridFrekvens:VISIBLE                           = TRUE
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:VISIBLE                        = FALSE
                 THIS-OBJECT:KalkylAdmKodControl:splitContainerFrekPris:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkylAdmKodControl:splitContainerFrekPris:Panel2Collapsed = TRUE.
        
         */
         THIS-OBJECT:KalkylAdmKodControl:splitContainerLopSubFrek:Panel1Collapsed = TRUE.
         THIS-OBJECT:KalkylAdmKodControl:splitContainerLopSubFrek:Panel2Collapsed = FALSE.
         THIS-OBJECT:ValueGridLopnrCh(?,?).
         IF THIS-OBJECT:KalkAdmdbControl:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:KalkylAdmKodControl:ValdKodKommentar:Text = THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         END.
                      
      END.   
      ELSE DO:
         ASSIGN
         THIS-OBJECT:KalkylAdmKodControl:splitContainerLopSubFrek:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkylAdmKodControl:splitContainerLopSubFrek:Panel2Collapsed = TRUE.
         /*
         THIS-OBJECT:KalkylAdmKodControl:GridFrekvens:VISIBLE                           = FALSE
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:VISIBLE                        = TRUE.
          THIS-OBJECT:KalkylAdmKodControl:splitContainerFrekPris:Panel1Collapsed = TRUE .
         THIS-OBJECT:KalkylAdmKodControl:splitContainerFrekPris:Panel2Collapsed = FALSE.
         */
         
      END.      
      THIS-OBJECT:KalkylAdmKodControl:splitContainerLopsubBild:Panel1Collapsed = FALSE.
   END METHOD.
   
   METHOD PUBLIC VOID RibbonTypKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolTypKalk:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:DisplayLayout:Bands[0]:Columns["TYPKALK"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:DisplayLayout:Bands[0]:Columns["TYPKALK"]:HIDDEN =  TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonArtalChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolArtal:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = FALSE.                  
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonSekKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolSek:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["SEKRETESS"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["SEKRETESS"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonStyrandeChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolStyrande:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonSkapKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolSkapad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonAvsKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolAvslutad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RibbonDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*
      IF THIS-OBJECT:RibbonToolDelKatalog:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerKatalogSubKat:Panel2Collapsed = FALSE.
         THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = FALSE.
         IF THIS-OBJECT:RibbonToolKatalog:Checked = FALSE THEN THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerSubKatKoder:Panel1Collapsed = FALSE.
      END.
      ELSE DO:
         IF THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = TRUE THEN  THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerKatalogSubKat:Panel2Collapsed = TRUE.
         
         IF THIS-OBJECT:splitContainerSubKatKoder:Panel2Collapsed = TRUE THEN  THIS-OBJECT:splitContainerSubKatKoder:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerSubKatKoder:Panel1Collapsed = TRUE.
      END.
      */
      
   END METHOD.
   METHOD PUBLIC VOID RibbonKopplaDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*BEHÖVS EJ
      IF THIS-OBJECT:RibbonToolKopplaDelKatalog:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerPrisKopDel:Panel2Collapsed = FALSE. 
         THIS-OBJECT:splitContainerSubKatPrisDel:Panel2Collapsed = FALSE.       
      END.
      ELSE DO:
         IF THIS-OBJECT:splitContainerPrisKopDel:Panel1Collapsed = TRUE THEN THIS-OBJECT:splitContainerSubKatPrisDel:Panel2Collapsed = TRUE.
         THIS-OBJECT:splitContainerPrisKopDel:Panel2Collapsed = TRUE.
      END.
      */
   END METHOD.
   METHOD PUBLIC VOID RibbonSkapDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:RibbonToolDelSkapad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonAvsDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolDelAvslutad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
      END.
   END METHOD.

   METHOD PUBLIC VOID RibbonPrisKatChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolPrisKatalog:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:splitContainerKatalog:Panel2Collapsed = FALSE.
         THIS-OBJECT:KalkylAdmPrisControl:splitContainerPris:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkylAdmKodControl:splitContainerLopsubBild:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkylAdmKatControl:splitContainerSubKatPrisDel:Panel2Collapsed = FALSE.              
      END.
      ELSE DO:
         IF THIS-OBJECT:KalkylAdmPrisControl:splitContainerPris:Panel2Collapsed = TRUE THEN  THIS-OBJECT:KalkylAdmPrisControl:splitContainerKatalog:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkylAdmPrisControl:splitContainerPris:Panel1Collapsed = TRUE.
         THIS-OBJECT:KalkylAdmKodControl:splitContainerLopsubBild:Panel1Collapsed = TRUE.
                 
      END.
      
   END METHOD.
   METHOD PUBLIC VOID RibbonSokBenChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolSokBen:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["SOKBENAMNING"]:HIDDEN = FALSE.      
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["SOKBENAMNING"]:HIDDEN = TRUE.         
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonTaEgnaKoderChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolTaEgnaKoder:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENKODUPP"]:HIDDEN = FALSE.         
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENKODUPP"]:HIDDEN = TRUE.         
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonTaEgnaPriserChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolTaEgnaPriser:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENPRISUPP"]:HIDDEN = FALSE.         
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENPRISUPP"]:HIDDEN = TRUE.        
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonVisningRegelChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolVisningRegel:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:splitContainerVisningBild:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkylAdmPrisControl:splitContainerKatalog:Panel2Collapsed = FALSE.
      END.
      ELSE DO:
         IF THIS-OBJECT:KalkylAdmPrisControl:splitContainerVisningBild:Panel1Collapsed = TRUE THEN  THIS-OBJECT:KalkylAdmPrisControl:splitContainerKatalog:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkylAdmPrisControl:splitContainerVisningBild:Panel1Collapsed = TRUE.
      END.
   END METHOD.
   /*retunerar typ = 1 typ = 2 osv*/
   METHOD PUBLIC INTEGER GetVisaTyp():
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 THEN DO:
         RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetInteger("Typ").
      END.
      RETURN -1.
   END METHOD.
   /*öppnar arbetskoder ned rätt query för vald typ*/
   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      IF THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:Initierad = FALSE THEN RETURN.                    
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 THEN DO:
         THIS-OBJECT:RefreshArbKoder(?,?).
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning(INPUT ladda AS LOGICAL):
      DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
      IF ladda = TRUE THEN DO:
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Clear().
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Alla").
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Ej märkta").
         /*
         IF THIS-OBJECT:GetVisaTyp() = 0 THEN Q = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:MarkningTTh:TABLE + " NO-LOCK".
         ELSE Q = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:MarkningTTh:TABLE + " WHERE TYP = " + STRING(THIS-OBJECT:GetVisaTyp()) + " NO-LOCK".
         */
         Q = "FOR EACH " + THIS-OBJECT:KalkAdmdbControl:MarkningTTh:TABLE + " NO-LOCK".
         CREATE QUERY mQ.
         mQ:SET-BUFFERS(THIS-OBJECT:KalkAdmdbControl:MarkningTTh).
         mQ:QUERY-PREPARE(Q).
         mQ:QUERY-OPEN().
         mQ:GET-FIRST().
         DO WHILE mQ:QUERY-OFF-END = FALSE:
            THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add(THIS-OBJECT:KalkAdmdbControl:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE).
            mQ:GET-NEXT().
         END.
         mQ:QUERY-CLOSE().
         THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[0].
      END.   
      
   /*THIS-OBJECT:WasMarkningMade = TRUE.*/
   /*END.*/
   END METHOD.
   
   METHOD PUBLIC CHARACTER GetVisaMarkning():
      RETURN THIS-OBJECT:RibbonToolFilterTyp:Text.
   END METHOD.
    
   METHOD PUBLIC VOID RefreshArbKoder(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE Typ        AS INTEGER   NO-UNDO INITIAL 1.
      DEFINE VARIABLE Markning   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE DoAll      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE DoUnmarked AS LOGICAL   NO-UNDO.
     
      Typ = THIS-OBJECT:GetVisaTyp().
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      IF Markning EQ "Alla" OR Markning EQ "" THEN DoAll = TRUE.
      ELSE DoAll = FALSE.
      IF Markning EQ "Ej märkta" THEN DoUnmarked = TRUE.
      ELSE DoUnmarked = FALSE.
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruTom().
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruTom().
      IF DoAll EQ TRUE THEN DO:
         IF Typ = 0 THEN THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFiltrera("").
         ELSE THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFiltrera("TYPKALK = " + STRING(Typ)).
         THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFirstrow().
         THIS-OBJECT:ValueGridArbKoderCh(?,?).
      END.
      ELSE DO:
         IF DoUnmarked EQ TRUE THEN DO:
            IF Typ = 0 THEN THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFiltrera("MARKNING = ''").
            ELSE THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = ''").
            THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFirstrow().
            THIS-OBJECT:ValueGridArbKoderCh(?,?).
         END.
         ELSE DO:
            IF Typ = 0 THEN THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFiltrera("MARKNING = '" + Markning + "'").
            ELSE THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '" + Markning + "'").
            THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruFirstrow().
            THIS-OBJECT:ValueGridArbKoderCh(?,?).
         END.
         IF THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:tBS:Count EQ 0 THEN DO:
            THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruTom().
            THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruTom().
            RETURN.
         END.      
      END.
      
      /*
      IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE EQ TRUE THEN DO:
         IF Typ = 0 THEN DO:
            THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST() NO-ERROR.
            THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruFiltrera("ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         END.   
         ELSE DO:
           THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE TYPKALK = " + STRING(Typ)) NO-ERROR.
           THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         END.
         GridLopnr:GuruFirstrow(). 
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruFirstrow().
      END.
      */
   END METHOD.
   
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "KalkylAdm",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT 0,INPUT "") .
   END METHOD.
   METHOD PUBLIC VOID ForetagAnpass():
      THIS-OBJECT:RibbonGroupAdm:VISIBLE   = FALSE.
      THIS-OBJECT:TabKalkylAdm:Tabs["Visa"]:VISIBLE   = FALSE.
      /*RUN SKAPKATEKG.P PERSISTENT SET katfrekinH.       */ 
/*      THIS-OBJECT:toolStripLasFrekvens:VISIBLE   = TRUE.
      THIS-OBJECT:toolStripLasKataLog:VISIBLE   = TRUE.
      THIS-OBJECT:toolStripLasInP2kom:VISIBLE   = TRUE.*/
      /*IF Guru.Konstanter:appcon THEN DO:
         THIS-OBJECT:toolStripLasFrekvens:VISIBLE   = FALSE.
         THIS-OBJECT:toolStripLasKataLog:VISIBLE   = FALSE.
         THIS-OBJECT:toolStripLasInP2kom:VISIBLE   = FALSE.
         
      END.   
      ELSE DO:
        
         RUN SKAPKATEKG.P PERSISTENT SET katfrekinH.
        
         THIS-OBJECT:toolStripLasFrekvens:VISIBLE   = TRUE.
         THIS-OBJECT:toolStripLasKataLog:VISIBLE   = TRUE.
         THIS-OBJECT:toolStripLasInP2kom:VISIBLE   = TRUE.
      END.*/   
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridKatalog().  
      THIS-OBJECT:CreateGridSubKatalog().
      THIS-OBJECT:CreateGridKatPris().
      THIS-OBJECT:CreateGridVisning().
      
      THIS-OBJECT:CreateGridArbKoder().
      THIS-OBJECT:CreateGridLopnr().
      THIS-OBJECT:CreateGridLopnumsub().
      THIS-OBJECT:CreateGridAnvSek().
      THIS-OBJECT:CreateGridFrekvens().
      THIS-OBJECT:CreateGridMallar().
      THIS-OBJECT:CreateGridValdMallar().
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruReopen().
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruFirstrow().  
      THIS-OBJECT:ValueGridKatalogCh(?,?).
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruFirstrow().
      THIS-OBJECT:ValueGridSubKatalogCh(?,?).
      THIS-OBJECT:KalkylAdmPrisControl:GridVisning:GuruReopen().
      THIS-OBJECT:KalkylAdmSekControl:GridAnvSek:GuruReopen().
      
      THIS-OBJECT:KalkylAdmMallControl:GridMallar:GuruReopen().
      THIS-OBJECT:KalkylAdmMallControl:GridMallar:GuruFirstrow().
      THIS-OBJECT:ValueGridMallarCh(?,?). 
      THIS-OBJECT:ValueGridSubMallCh(?,?).
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("MALLNR"," Mallnr ",TRUE,30).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("TYP","Typ",FALSE,30).
      THIS-OBJECT:KalkylAdmMallControl:GridMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth)).
      THIS-OBJECT:KalkylAdmMallControl:GridMallar:TEXT = "Mallar".
      THIS-OBJECT:KalkylAdmMallControl:GridMallar:tBS:PositionChanged:Subscribe(ValueGridMallarCh).
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridFrekvens():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("FREKOD"," Arbkod ",TRUE,55).
      rubrikergrid[2] = NEW Controls.GridRubrik("FREKNR","Löpnr",TRUE,45).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,600).
      THIS-OBJECT:KalkylAdmKodControl:GridFrekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkAdmdbControl:FrekvensTTh)).
      THIS-OBJECT:KalkylAdmKodControl:GridFrekvens:TEXT = "Frekvenskoder".  
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridKatalog():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[2] = NEW Controls.GridRubrik("KATALOGTYP","Kat.typ",TRUE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("VISARTAL","Årtal",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("SEKRETESS","Sekretess",FALSE,63,"LOG").
      rubrikergrid[5] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,75).
      rubrikergrid[6] = NEW Controls.GridRubrik("AVSLUTAD","Avslutad",TRUE,60,"LOG").
      rubrikergrid[7] = NEW Controls.GridRubrik("HKLOGSUBID","Styrande delkatalog",TRUE,60).
      rubrikergrid[8] = NEW Controls.GridRubrik("KLOGID","NUM",TRUE,60).
      THIS-OBJECT:KalkylAdmSekControl:GridKatSek:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:KalkAdmdbControl:KatalogTTh)).  
      THIS-OBJECT:KalkylAdmSekControl:GridKatSek:TEXT = "Välj katalog för sekretess".      
      THIS-OBJECT:KalkylAdmSekControl:GridKatSek:tBS:PositionChanged:Subscribe(ValueGridKatSekCh).
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[2] = NEW Controls.GridRubrik("KATALOGTYP","Kat.typ",FALSE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("VISARTAL","Årtal",FALSE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,75).
      rubrikergrid[5] = NEW Controls.GridRubrik("AVSLUTAD","Avslutad",FALSE,60,"LOG").
      rubrikergrid[6] = NEW Controls.GridRubrik("HKLOGSUBID","Styrande delkatalog",FALSE,60).   
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:KalkAdmdbControl:KatalogTTh)).  
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:TEXT = "Kataloger - markera katalog du vill koppla till delkatalog".
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:tBS:PositionChanged:Subscribe(ValueGridKatalogCh). 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridSubKatalog():
      /*lägg till och ta bort delkataloger*/ 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,75).
     /* rubrikergrid[4] = NEW Controls.GridRubrik("AVSLUTAD","Avslutad",FALSE,60,"LOG").*/
      THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth)).  
      THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:TEXT = "Delkataloger".
      THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:tBS:PositionChanged:Subscribe(ValueGridSubKatDelCh).
      
      /*subkat sätt pris*/ 
      rubrikergrid[1] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,75).
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth)).  
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:TEXT = "Delkataloger".      
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:tBS:PositionChanged:Subscribe(ValueGridSubKatalogCh).
    
      /* subkat vid arbetskoder läsin katalog + frekvens  */
      THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth)).  
      THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:TEXT = "Delkataloger". 
      THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:tBS:PositionChanged:Subscribe(ValueGridSubKatKoderCh).
     
     /*subkat vid mall*/ 
      THIS-OBJECT:KalkylAdmMallControl:GridSubMall:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth)).  
      THIS-OBJECT:KalkylAdmMallControl:GridSubMall:TEXT = "Delkataloger".
      THIS-OBJECT:KalkylAdmMallControl:GridSubMall:tBS:PositionChanged:Subscribe(ValueGridSubMallCh).
      
      
      /* subkat vid arbetskoder läsin katalog + frekvens*/
      rubrikergrid[1] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,75).
      rubrikergrid[4] = NEW Controls.GridRubrik("AVSLUTAD","Avslutad",FALSE,60,"LOG").
      THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkAdmdbControl:Katalogsubtth)).  
      THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:TEXT = "Vald katalog består av följande delkataloger".
      
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridKatPris():
     
      {GridRubrikListaVarExtent.i}  
      rubrikergrid[1] = NEW Controls.GridRubrik("KPID","PrisId",TRUE,60).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("PRIS","Pris",FALSE,60,"DEC").
      rubrikergrid[4] = NEW Controls.GridRubrik("SOKBENAMNING","Sökbenämning",TRUE,160).
      rubrikergrid[5] = NEW Controls.GridRubrik("EGENKODUPP","Egna koder",FALSE,90).
      rubrikergrid[6] = NEW Controls.GridRubrik("EGENPRISUPP","Egna priser",FALSE,90).
      rubrikergrid[7] = NEW Controls.GridRubrik("KVID","Visningsregel",FALSE,80).
      rubrikergrid[8] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:KalkAdmdbControl:PriserTTh)).  
      THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:TEXT = "Katalogpriser".  
      IF Guru.Konstanter:globanv = {setuser.I} THEN .
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["KPID"]:Hidden = false.
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = false.
      END.   
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS","Pris",TRUE,60,"DEC").
      THIS-OBJECT:KalkylAdmKodControl:GridAddPris:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkAdmdbControl:PriserTTh)).
      THIS-OBJECT:KalkylAdmKodControl:GridAddPris:TEXT = "Katalogpriser".
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridVisning():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("KVID","Visningsregel",TRUE,60).
      rubrikergrid[2] = NEW Controls.GridRubrik("EABER","EA-mängd",FALSE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("TIMTYP","Timmar",FALSE,80).
      rubrikergrid[4] = NEW Controls.GridRubrik("KOSTTYP","Kostnad",TRUE,80).
      rubrikergrid[5] = NEW Controls.GridRubrik("ORDNINGTIMMAR","Ord.timmar",FALSE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("ORDNINGKOSTNAD","Ord.kostnad",FALSE,70).
      THIS-OBJECT:KalkylAdmPrisControl:GridVisning:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:KalkAdmdbControl:VisningTTh)).  
      THIS-OBJECT:KalkylAdmPrisControl:GridVisning:TEXT = "Visningsregler".      
        
        
   END METHOD.
   METHOD PUBLIC VOID CreateGridArbKoder():
     
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",FALSE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,64).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,100).
                                                           
      rubrikergrid[5] = NEW Controls.GridRubrik("TYPKALK","Typ",FALSE,50).
     /*
      rubrikergrid[5] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      */
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh)).  
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:TEXT = "Arbetskoder".
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:tBS:PositionChanged:Subscribe(ValueGridArbKoderCh).  
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      /*CCC LENA
      rubrikergrid[5] = NEW Controls.GridRubrik("TYPKALK","Typ",TRUE,50).*/
      THIS-OBJECT:KalkylAdmMallControl:GridArbKodMall:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh)).  
      THIS-OBJECT:KalkylAdmMallControl:GridArbKodMall:TEXT = "Arbetskoder".
      THIS-OBJECT:KalkylAdmMallControl:GridArbKodMall:tBS:PositionChanged:Subscribe(ValueGridArbKodMallCh).
      
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:DisplayLayout:Bands[0]:Columns["MARKNING"]:Hidden = FALSE.
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:DisplayLayout:Bands[0]:Columns["MARKSUB"]:Hidden = FALSE.        
     
       THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ExtramenuItemAdd("Kopiera Arbetskoder").
       THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KopieraArbkoder_Click).
      
       THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ExtramenuItemAdd("Klistra in Arbetskoder").
       THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KlistraArbkoder_Click).
       
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopnr():
      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("LOPNR","Löpnr",FALSE,40, "KALKLOP").
      rubrikergrid[2] = NEW Controls.GridRubrik("ENHET","Enh",FALSE,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,325).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,100).
      rubrikergrid[5] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("TKOST", "Tot.Kostnad", TRUE , 90,"DEC").
      
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh)).
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:TEXT = "Löpnummer".
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:tBS:PositionChanged:Subscribe(ValueGridLopnrCh).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35, "KALKLOP").
      rubrikergrid[2] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE ,100).
      rubrikergrid[5] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      
      THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 4, INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh)).
      THIS-OBJECT:KalkylAdmMallControl:GridLopMall:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:KalkylAdmMallControl:GridLopMall:TEXT = "Löpnummer".
      THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruOrderby("BY ARBKOD BY LOPNR").
         
   END METHOD.   
   METHOD PUBLIC VOID CreateGridValdMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("MALLNR","Mallar",TRUE,45).
      THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:KalkAdmdbControl:KalkmallKodertth)).
      /*default filter  sortering*/     
      THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:Text = "Kalkylkoder för mallen".      
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridLopnumsub():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 125).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris", TRUE, 100,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("TIMMAR", "Timmar", FALSE, 50,"DEC").
      rubrikergrid[4] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE, 75,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("POSTKOST", "Tot.Kostnad", TRUE , 90,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("KPID","PrisId",TRUE,60).
      rubrikergrid[7] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:KalkAdmdbControl:LopsubTTh)).  
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:DisplayLayout:Bands[0]:Columns["KPID"]:Hidden = TRUE.
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TEXT = "Pris och timmar per löpnummer Totalkostnad ". 
     
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:SetTitle(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TEXT).
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruNoAutoTitle(TRUE).
                   
   END METHOD.
   METHOD PUBLIC VOID GridLopnumSubTitle():
      DEFINE VARIABLE minuskoll AS INTEGER NO-UNDO.
      minuskoll = INDEX(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TITLE,"-").
      IF minuskoll > 0 THEN THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TEXT = SUBSTRING(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TITLE,1,minuskoll) + " " + STRING(ROUND(THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot,2)).
      ELSE THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TEXT = THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:TITLE + " " + STRING(ROUND(THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot,2)).
      
   END METHOD.   
   METHOD PUBLIC VOID CreateGridAnvSek():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ANVANDARE", "Användare", TRUE, 100).
      rubrikergrid[2] = NEW Controls.GridRubrik("AV-NAMN", "Namn", TRUE, 200).
      rubrikergrid[3] = NEW Controls.GridRubrik("AV-LEVEL", "Anv.Niv", TRUE, 50).
      THIS-OBJECT:KalkylAdmSekControl:GridAnvSek:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT Guru.Konstanter:AnvandareTTh)).  
      THIS-OBJECT:KalkylAdmSekControl:GridAnvSek:GuruFiltrera("anvandartemp.AV-LEVEL NE " + STRING(0)).
      
      THIS-OBJECT:KalkylAdmSekControl:GridAnvSek:TEXT = "Användare".
      THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:KalkAdmdbControl:kalkanvtth)).  
      THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek:TEXT = "Valda användare".     
               
   END METHOD.
   
   METHOD PUBLIC VOID KlistraArbkoder_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH2       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
     
      IF THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GridKlistraIn()  = FALSE THEN RETURN. 
      IF THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GridKlistraIn() = FALSE THEN RETURN.
      queryvar = "FOR EACH " + THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE: 
         IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ArbKoderKoll(INPUT THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                                    INPUT THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                                    INPUT ?) = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
            RETURN.
         END.
         qH:GET-NEXT().  
      END.
      queryvar = "FOR EACH " + THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         
         THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-CREATE().
         rid =  THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID.
         THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-COPY(THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:ttBufferHandleKopieraKlistra).
         THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:RECID.
         THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE.
         
         queryvar2 = "FOR EACH " + THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra:TABLE.
         qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra,queryvar).
         qH2:GET-FIRST().
         DO WHILE qH2:QUERY-OFF-END = FALSE:
            THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-CREATE().
            THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-COPY(THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra).
            THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:LopposterTTh:RECID.
            THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE.
           
            qH2:GET-NEXT().  
         END.
         queryvar2 = "FOR EACH " + THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra:TABLE.
         qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra,queryvar).
         qH2:GET-FIRST().
         DO WHILE qH2:QUERY-OFF-END = FALSE:
            THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-CREATE().
            THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-COPY(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra).
            THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:LopsubTTh:RECID.
            THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE.
           
            qH2:GET-NEXT().  
         END.
         qH:GET-NEXT().  
      END.
     
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
      THIS-OBJECT:ValueGridSubKatKoderCh(?,?). 
      
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruRepositionTo(rid).      
      
         
   END METHOD.
   METHOD PUBLIC VOID KopieraArbkoder_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      /*skapar kopior av markerade poster*/
     DEFINE VARIABLE rid AS ROWID NO-UNDO.
     
     IF THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GridKopiera() = FALSE THEN RETURN.
     THIS-OBJECT:KalkylAdmKodControl:GridLopnr:KopiKlistraTable().
     THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:KopiKlistraTable().
     rid =  THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID.
     THIS-OBJECT:KalkylAdmKodControl:GridLopnr:tqH:GET-FIRST().
     DO WHILE THIS-OBJECT:KalkylAdmKodControl:GridLopnr:tqH:QUERY-OFF-END = FALSE:
        THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra:BUFFER-CREATE().
        THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra:BUFFER-COPY(THIS-OBJECT:KalkylAdmKodControl:GridLopnr:Guruegenskap:ttBufferHandle).
        THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkylAdmKodControl:GridLopnr:ttBufferHandleKopieraKlistra:RECID.
        THIS-OBJECT:ValueGridLopnrCh(?,?).
        
        THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:tqH:GET-FIRST().
        DO WHILE THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:tqH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra:BUFFER-CREATE().
           THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra:BUFFER-COPY(THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:Guruegenskap:ttBufferHandle).
           THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:ttBufferHandleKopieraKlistra:RECID.
           THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:tqH:GET-NEXT(). 
        END. 
        
        THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruFiltreraTom().
        THIS-OBJECT:KalkylAdmKodControl:GridLopnr:tqH:GET-NEXT(). 
     END.
     
     THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruReopen().
   
      
      
   END METHOD.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabKalkylAdm_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
		DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
		cTo = e:TAB:KEY.
	
      IF cfrom = "Koder" THEN  
      THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
      IF cfrom = "Priser" THEN  
      THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
      IF cfrom = "Kataloger" THEN  
      THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
	   IF cfrom = "Mallar" THEN  
      THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmMallControl:GridSubMall:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
	
			
		IF cTo = "Koder" THEN DO:
		   
         THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:GuruRadvaljare(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:ROWID).
		   
		   THIS-OBJECT:ValueGridSubKatKoderCh(?,?).  
		   THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruGetCurrentRowid(). 
		  
		   /*delkataloger kalogprser arbetskoder
		   */
         THIS-OBJECT:RibbonGroupKatalog:Visible = FALSE. 
         THIS-OBJECT:RibbonGroupDelKatalog:Visible = TRUE.
         THIS-OBJECT:RibbonGroupPrisKatalog:Visible = TRUE.
         THIS-OBJECT:RibbonGroupVisaPristyp:Visible = FALSE.
         THIS-OBJECT:RibbonGroupArbetskoder:Visible = TRUE.
		END.
		IF cTo = "Priser" THEN DO:
		   /*
		   THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
		   */
		   THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruRadvaljare(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:ROWID).
		   
		   THIS-OBJECT:ValueGridSubKatalogCh(?,?).
/*		   katalogpriser visningsregler*/
         THIS-OBJECT:RibbonGroupKatalog:Visible = FALSE. 
         THIS-OBJECT:RibbonGroupDelKatalog:Visible = FALSE.
         THIS-OBJECT:RibbonGroupPrisKatalog:Visible = TRUE.
         THIS-OBJECT:RibbonGroupVisaPristyp:Visible = TRUE.
         THIS-OBJECT:RibbonGroupArbetskoder:Visible = FALSE.
      END.
      IF cTo = "Kataloger" THEN DO:
         THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:KalkylAdmKatControl:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE)) NO-ERROR.
         /*
         THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
         */
         THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:GuruRadvaljare(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:ROWID).
         THIS-OBJECT:ValueGridKatalogCh(?,?).
         THIS-OBJECT:ValueGridSubKatDelCh(?,?).         
         /*kataloger delkataloger*/
         THIS-OBJECT:RibbonGroupKatalog:Visible = TRUE. 
         THIS-OBJECT:RibbonGroupDelKatalog:Visible = TRUE.
         THIS-OBJECT:RibbonGroupPrisKatalog:Visible = FALSE.
         THIS-OBJECT:RibbonGroupVisaPristyp:Visible = FALSE.
         THIS-OBJECT:RibbonGroupArbetskoder:Visible = FALSE.
         
      END.
      IF cTo = "Mallar" THEN DO:
         THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(THIS-OBJECT:KalkylAdmMallControl:GridMallar:ActiveRow:Cells["MALLNR"]:VALUE)) NO-ERROR.  
         /*        
         THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:KalkylAdmMallControl:GridSubMall:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
         */
         THIS-OBJECT:KalkylAdmMallControl:GridSubMall:GuruRadvaljare(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:ROWID).
         THIS-OBJECT:ValueGridSubMallCh(?,?).
         /*delkatloger*/ 
         THIS-OBJECT:RibbonGroupKatalog:Visible = FALSE. 
         THIS-OBJECT:RibbonGroupDelKatalog:Visible = FALSE.
         THIS-OBJECT:RibbonGroupPrisKatalog:Visible = FALSE.
         THIS-OBJECT:RibbonGroupVisaPristyp:Visible = FALSE.
         THIS-OBJECT:RibbonGroupArbetskoder:Visible = FALSE.
      END.
      IF cTo = "Sekretess" THEN DO:
         THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:KalkylAdmSekControl:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE)) NO-ERROR.
         THIS-OBJECT:ValueGridKatSekCh(?,?).
         /*kataloger*/
         THIS-OBJECT:RibbonGroupKatalog:Visible = TRUE. 
         THIS-OBJECT:RibbonGroupDelKatalog:Visible = FALSE.
         THIS-OBJECT:RibbonGroupPrisKatalog:Visible = FALSE.
         THIS-OBJECT:RibbonGroupVisaPristyp:Visible = FALSE.
         THIS-OBJECT:RibbonGroupArbetskoder:Visible = FALSE.
      END.
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabKalkylAdm_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
		DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
      
      cTo = e:TAB:KEY.
      cFrom = THIS-OBJECT:TabKalkylAdm:SelectedTab:KEY NO-ERROR.
      
      
     /*BEHÖVS EJ
      IF cTo = "Delkataloger" THEN THIS-OBJECT:RibbonToolKopplaDelKatalog:SharedPropsInternal:Visible = TRUE. 
      ELSE THIS-OBJECT:RibbonToolKopplaDelKatalog:SharedPropsInternal:Visible = FALSE.
      */
        
		RETURN.

	END METHOD.


   METHOD PRIVATE VOID ValueGridSubKatDelCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
         /*THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruReopen().
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruReopen().  
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruReopen().  
         THIS-OBJECT:KalkylAdmMallControl:GridSubMall:GuruReopen().*/     
   END METHOD.
                
   METHOD PRIVATE VOID ValueGridSubKatalogCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:AVAILABLE THEN DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:GuruTom().
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:GuruFiltrera("kalkylprisertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE)).
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:GuruFirstrow().   
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruTom().
      END.     
   END METHOD.
   
   /*ROB*/
   METHOD PUBLIC VOID ValueGridKatalogCh(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      IF THIS-OBJECT:KalkAdmdbControl:KatalogTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruTom().
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruFiltrera("kalkylkatalogsubtt.KLOGID = " + THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE).
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruFirstrow().
         THIS-OBJECT:UppdateraGriddar("subkat").
      END. 
      ELSE DO:
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruTom().
      END.   
   END METHOD.
                      
   METHOD PUBLIC VOID ValueGridSubKatKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKoder(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
         THIS-OBJECT:RefreshArbKoder(?,?).
         THIS-OBJECT:ValueGridArbKoderCh(?,?).              
         THIS-OBJECT:KalkylAdmKodControl:GridAddPris:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridAddPris:GuruFiltrera("kalkylprisertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE)).
         THIS-OBJECT:KalkylAdmKodControl:GridAddPris:GuruFirstrow().
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridAddPris:GuruTom().
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridArbKodMallCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchLopar(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE).
         THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruFiltrera("kalkylloppostertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
         "kalkylloppostertt.ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").              
         THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruFirstrow().
                           
      END.
      ELSE DO:
         
         THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruTom().         
      END.    
   END METHOD.
   METHOD PUBLIC VOID ValueGridArbKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchLopar(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE).
         THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruFiltrera("kalkylloppostertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
         "kalkylloppostertt.ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").              
         THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruFirstrow(). 
         THIS-OBJECT:ValueGridLopnrCh(?,?).           
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKodControl:GridLopnr:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruTom().
      END.    
   END METHOD.
   METHOD PUBLIC VOID ValueGridLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      
      IF THIS-OBJECT:KalkAdmdbControl:LopposterTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruFiltrera("kalkyllopsubtt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
         "kalkyllopsubtt.ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND kalkyllopsubtt.LOPNR = " + STRING(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruFirstrow().
         IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
            THIS-OBJECT:KalkylAdmKodControl:ValdKodKommentar:Text = THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchFrekvens(INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
            INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
            THIS-OBJECT:KalkylAdmKodControl:GridFrekvens:GuruReopen().
         END. 
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KodKostTotSetGet(INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
         INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE, INPUT FALSE).
         THIS-OBJECT:GridLopnumsubTitle().    
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:GridFrekvens:GuruTom().
         THIS-OBJECT:KalkylAdmKodControl:ValdKodKommentar:Text = "".
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      IF THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:AVAILABLE THEN DO:
         THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:GuruFirstrow().
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmMallControl:GridValdMallar:GuruTom().            
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridSubMallCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKoder(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
         THIS-OBJECT:KalkylAdmMallControl:GridArbKodMall:GuruFiltrera("kalkylarbkodertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE)).
         THIS-OBJECT:KalkylAdmMallControl:GridArbKodMall:GuruFirstrow().
         THIS-OBJECT:ValueGridArbKodMallCh(?,?).
         IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchLopar(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE).
            THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruFiltrera("kalkylloppostertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
            "kalkylloppostertt.ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").              
            THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruFirstrow(). 
         END. 
         ELSE THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruTom(). 
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmMallControl:GridArbKodMall:GuruTom().
         THIS-OBJECT:KalkylAdmMallControl:GridLopMall:GuruTom().      
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridKatSekCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      
      IF THIS-OBJECT:KalkAdmdbControl:KatalogTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek:GuruFiltrera("kalkanvtt.KLOGID = " + STRING(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE)).
         THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek:GuruFirstrow().        
      END.
      ELSE DO:
         THIS-OBJECT:KalkylAdmSekControl:GridValdaAnvSek:GuruTom().                 
      END.        
   END METHOD.
  
  /*För synkroniserade listor*/ 
  METHOD PUBLIC VOID UppdateraGriddar(INPUT typ AS CHARACTER): 
      IF typ = "kat" THEN DO:
         THIS-OBJECT:KalkylAdmSekControl:GridKatSek:GuruReopen().
         THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruReopen().  
      END.
      IF typ = "subkat" THEN DO:
         THIS-OBJECT:KalkylAdmKatControl:GridSubKatDel:GuruReopen().
         THIS-OBJECT:KalkylAdmKatControl:GridKatHuvDel:GuruReopen().
         THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruReopen().
         THIS-OBJECT:KalkylAdmKodControl:GridSubKatKoder:GuruReopen().  
           
         THIS-OBJECT:KalkylAdmMallControl:GridSubMall:GuruReopen().
      END.
   END METHOD.
   
   METHOD PUBLIC VOID LasGriddar():
      /*
      MESSAGE "GRIDDAR LÅSTA"
      VIEW-AS ALERT-BOX.
      */
      THIS-OBJECT:KalkylAdmKodControl:GridArbKoder:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmKodControl:GridLopnumsub:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmKodControl:GridLopnr:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmPrisControl:GridKatPris:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:KalkylAdmPrisControl:GridVisning:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
   END METHOD.
   
	DESTRUCTOR PUBLIC KalkylAdmShell ( ):
	   IF VALID-HANDLE(katfrekinH) THEN DELETE PROCEDURE katfrekinH. 
		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
   
  
   
END CLASS.
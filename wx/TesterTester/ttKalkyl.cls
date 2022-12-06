 
/*------------------------------------------------------------------------
   File        : Kalkyl.cls
   Purpose     : Make sure stuff works
   Syntax      : 
   Description : 
   Author(s)   : elpfh
   Created     : Fri Feb 10 12:57:57 CET 2012
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.
USING Microsoft.Office.Interop.Excel.*.


CLASS Modules.Kalkyl.ttKalkyl INHERITS UserControl: 
   DEFINE PRIVATE VARIABLE m_Kalkyl_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkyl_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkyl_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkyl_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBerMtrl                AS Controls.Button                                                NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonKonvKalkyl             AS Controls.Button                                                NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonKopieraKalkyl          AS Controls.Button                                                NO-UNDO.
   DEFINE PRIVATE VARIABLE comboPriserUppf              AS Controls.ComboSimpleLabel                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE imageList1                   AS System.Windows.Forms.ImageList                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE KonvTillTyp                  AS Controls.ComboSimpleLabel                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE KopHuvudOmrade               AS Controls.Combo                                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE KopLabelKatalog              AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE KopHuvudKatalog              AS Controls.ComboSimple                                           NO-UNDO.
   DEFINE PRIVATE VARIABLE KopLabelOmr                  AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE CurrentMatris                AS INTEGER                                                        NO-UNDO.
      
   DEFINE PRIVATE VARIABLE LabelUtf                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelOmr                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelKatalog                 AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelAnm                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   
   
   
   DEFINE PRIVATE VARIABLE splitContainerArbkoder       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaValj       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaVald       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaKoder      AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaArbKoder   AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFaktorerPriser AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFriKalk        AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerHuv            AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalk           AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopnr          AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrl           AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrlValj       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFelmedd                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKopiera                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaExcel                    AS Controls.GuruExcelEmbed                                        NO-UNDO.
   
   DEFINE PRIVATE VARIABLE ultraAvtal                   AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   
   DEFINE PRIVATE VARIABLE components                   AS System.ComponentModel.IContainer                               NO-UNDO.
   
   DEFINE PRIVATE VARIABLE label1                       AS System.Windows.Forms.Label                                     NO-UNDO.
  
   DEFINE PRIVATE VARIABLE EgnaKoderStrip               AS System.Windows.Forms.ToolStrip                                 NO-UNDO.
  
   DEFINE PRIVATE VARIABLE EgenTaBort                   AS System.Windows.Forms.ToolStripButton                           NO-UNDO.
   DEFINE PRIVATE VARIABLE EgenSkapa                    AS System.Windows.Forms.ToolStripButton                           NO-UNDO. 
   
   DEFINE PRIVATE VARIABLE ButtonMallar                 AS Controls.Button                                                NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonExcel                  AS Controls.Button                                                NO-UNDO.
   
   DEFINE PRIVATE VARIABLE MatButtonOverMark            AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE MatButtonBackMark            AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE EgnaButtonOverMark           AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE EgnaButtonOverAll            AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE EgnaButtonBackMark           AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE EgnaButtonBackAll            AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylButtonOverMark         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylButtonOverAll          AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylButtonBackMark         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylButtonBackAll          AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOverMarkMallar         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBackMarkMallar         AS Controls.ButtonOver                                            NO-UNDO.
   
   DEFINE PRIVATE VARIABLE GridFrekvens                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridValdaMallar              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridMallarKoder              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridMallar                   AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridVolymBer                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridFriKalk                  AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridPriser                   AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridFaktorer                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridMaterial                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridEgetMaterial             AS Controls.Grid                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridEgnaKoder                AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridLopKoder                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridKalkylKoder              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridArbetsKoder              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridValdaUnderKoder          AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridUnderKoder               AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridPersonal                 AS Controls.Grid                                                  NO-UNDO.   
   DEFINE PRIVATE VARIABLE GridFelMedd                  AS Controls.Grid                                                  NO-UNDO.
   /* Drag & Dropp*/
   DEFINE         VARIABLE DGKoder                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGEgna                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMat                        AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMall                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   
   DEFINE PRIVATE VARIABLE AvtalText                    AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.
   DEFINE PRIVATE VARIABLE KoderAnmarkning              AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.
   DEFINE PRIVATE VARIABLE KodKommentar                 AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudAnmarkning              AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.  
   DEFINE PRIVATE VARIABLE ultraTextEditor2             AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HuvudTyp                     AS Controls.ComboSimpleLabel                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudKatalog                 AS Controls.ComboSimple                                           NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudUtfardare               AS Controls.Combo                                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudOmrade                  AS Controls.Combo                                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE ComboAvtal                   AS Controls.Combo                                                 NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HuvudKalknr                  AS Controls.FillinInteger                                         NO-UNDO.
   DEFINE PUBLIC  VARIABLE HuvudBenamning               AS Controls.FillinChar                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudAnsvarig                AS Controls.FillinChar                                            NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE TabAvtalskalkyl              AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabEgna                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabImportMallar              AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFaktorer                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkyl                    AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylHuvud               AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVolymber                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMateriel                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager                   AS Infragistics.Win.UltraWinTabControl.UltraTabControl            NO-UNDO. 
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1  AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   
   /* Ribbongrupper */
   DEFINE         VARIABLE RibbonTabKalkyl              AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabKoder               AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabEgnaKoder           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabMat                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisaKoder           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisning             AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisningVisa         AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisningEgenskaper   AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   /* uppföljning
   DEFINE         VARIABLE RibbonTabUppfoljning        AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   */
   /* Ribbonkontroller */
   /* Dummy, lägg denna där du vill fylla ut */
   DEFINE         VARIABLE RibbonDummy                  AS Infragistics.Win.UltraWinToolbars.LabelTool                    NO-UNDO.
   /* info */
   DEFINE         VARIABLE RibbonToolInfTyp             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolOmrade             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolKat                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   /* Koder */
   DEFINE PUBLIC  VARIABLE RibbonToolMatris             AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFriKalk            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFriGenerera        AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAnm           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /* visning */
   
   DEFINE PUBLIC  VARIABLE RibbonToolGenerera           AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE         VARIABLE rbt                          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.  
    
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUt            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisHuvud           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisAnmarkning      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisUtforlig        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisEgnaFaktorer    AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisEgnaPriser      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisVisa            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisSummera         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGruppera        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisPriser          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatSpec         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatris          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGroupMatris     AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   /* uppfölj
   DEFINE PUBLIC  VARIABLE RibbonToolUppfPriser        AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
   */
   /* Material */
   DEFINE PUBLIC  VARIABLE RibbonToolLev                AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE         VARIABLE RibbonToolEnr                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolSok                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPanelEmpty               AS System.Windows.Forms.Panel                                     NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPanelWaiting             AS System.Windows.Forms.Panel                                     NO-UNDO.
   
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   DEFINE PUBLIC  VARIABLE GuruDefaultVALUEsTTh         AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE OmradeTTh                    AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE AnvandareTTh                 AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE PersonalTTh                  AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE LeverantorTTh                AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE MaterialTTh                  AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE kalktmtrlTTh                 AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE MarkningTTh                  AS HANDLE                                                         NO-UNDO.
 
   /* Kalkyl */
   DEFINE         VARIABLE HuvudTTh                     AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE KatalogTTh                   AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE PriserTTh                    AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE VisningTTh                   AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE ValdaPriserTTh               AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE LopposterTTh                 AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE ArbetskoderTTh               AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE KoderTTh                     AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE kalkantalTTh                 AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE kalkkostnadTTh               AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE kalkrubrikTTh                AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE kalkaonrTTh                  AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE EgnaPriserTTh                AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE FaktorerTTh                  AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE Avtalskalktth                AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE AvtalKodertth                AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE KalkylimportTTh              AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE KalkmallHuvudtth             AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE KalkmallKodertth             AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE KalkmallValdtth              AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE VolymBerTTh                  AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE FrekvensTTh                  AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE tiduth                       AS HANDLE                                                         NO-UNDO.
   DEFINE         VARIABLE berkalkmtrltth               AS HANDLE                                                         NO-UNDO.
   
   DEFINE PRIVATE VARIABLE KalkylLoaded                 AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PRIVATE VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   DEFINE PRIVATE VARIABLE PassedOnMatris               AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PRIVATE VARIABLE NyKalkyl                     AS LOGICAL                                                        NO-UNDO INITIAL FALSE.
   DEFINE         VARIABLE cSenasteArbKod               AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteLopNr                AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE iSenasteMatris               AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE cSenasteMtrl                 AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteMtrlMatris           AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE ExelKommando                 AS CHARACTER                                                      NO-UNDO.   
   DEFINE         VARIABLE WasExcelShown                AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   /* ttHandles ifrån stuff */
   DEFINE         VARIABLE KalkDummy                    AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   
   DEFINE TEMP-TABLE typtt NO-UNDO
      FIELD ATAT    AS CHARACTER
      FIELD TYPKALK AS INTEGER
      FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
   DEFINE TEMP-TABLE konvtyptt NO-UNDO
      FIELD ATAT    AS CHARACTER
      FIELD TYPKALK AS INTEGER
      FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
      
   DEFINE TEMP-TABLE ufpris NO-UNDO
      FIELD UTYP AS INTEGER
      FIELD TYPC AS CHARACTER LABEL "Pris".
   DEFINE PUBLIC PROPERTY KalkNrvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Omradevar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   CONSTRUCTOR PUBLIC ttKalkyl ( INPUT r AS Guru.Root ):
      SUPER ().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Root:Logger:WriteLine("Skapar kalkylobjekt..").
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InitializeTTs().
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Gör kopplingar till databasemanager..").
      THIS-OBJECT:AnvandareTTh = THIS-OBJECT:Root:DatabaseManager:Global:AnvandareTTh.
      THIS-OBJECT:ArbetskoderTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ArbetskoderTTh.
      THIS-OBJECT:AvtalKodertth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:AvtalKodertth.
      THIS-OBJECT:Avtalskalktth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalskalktth.
      THIS-OBJECT:EgnaPriserTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:EgnaPriserTTh.
      THIS-OBJECT:FaktorerTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:FaktorerTTh.
      THIS-OBJECT:FrekvensTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensTTh.
      THIS-OBJECT:HuvudTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:HuvudTTh.
      THIS-OBJECT:KalkaonrTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkaonrTTh.
      THIS-OBJECT:KalkmallHuvudtth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkmallHuvudtth.
      THIS-OBJECT:KalkmallKodertth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkmallKodertth.
      THIS-OBJECT:KalkmallValdtth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkmallValdtth.
      THIS-OBJECT:kalktmtrlTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalktmtrlTTh.
      THIS-OBJECT:kalktmtrlTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalktmtrlTTh.
      THIS-OBJECT:KalkylimportTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkylimportTTh.
      THIS-OBJECT:KatalogTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KatalogTTh.
      THIS-OBJECT:KoderTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KoderTTh.
      THIS-OBJECT:LeverantorTTh = THIS-OBJECT:Root:DatabaseManager:Global:LeverantorTTh.
      THIS-OBJECT:LopposterTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:LopposterTTh.
      THIS-OBJECT:MarkningTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:MarkningTTh.
      THIS-OBJECT:MaterialTTh = THIS-OBJECT:Root:DatabaseManager:Global:MaterialTTh.
      THIS-OBJECT:OmradeTTh = THIS-OBJECT:Root:DatabaseManager:Global:OmradeTTh.
      THIS-OBJECT:PersonalTTh = THIS-OBJECT:Root:DatabaseManager:Global:PersonalTTh.
      THIS-OBJECT:PriserTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:PriserTTh.
      THIS-OBJECT:ValdaPriserTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ValdaPriserTTh.
      THIS-OBJECT:VisningTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisningTTh.
      THIS-OBJECT:VolymBerTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBerTTh.
      THIS-OBJECT:tiduth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:tiduth.
      THIS-OBJECT:berkalkmtrltth = THIS-OBJECT:Root:DatabaseManager:Kalkyl:berkalkmtrltth.
      
      
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC ttKalkyl ( ):

   END DESTRUCTOR.


   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBerMtrl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:Root:WindowManager:Visa(FALSE).
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrl().
		
      THIS-OBJECT:Root:WindowManager:Visa(TRUE).
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonExcel_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
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
         TITLE          "Välj den excelfil som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(50) + "      " + 
         THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(51), THIS-OBJECT:Root:LanguageManager:GetString(52), System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Warning).
      tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      IF res:ToString() = "Yes" THEN THIS-OBJECT:ExcelImportLista(1,fildir).
      IF res:ToString() = "No" THEN THIS-OBJECT:ExcelImportLista(2,fildir).
      IF res:ToString() = "Cancel" THEN RETURN.
      THIS-OBJECT:FelMeddCheck().
      
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonKonvKalkyl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KonverteraKalkyl(INPUT INTEGER(STRING(THIS-OBJECT:KonvTillTyp:GuruCombo:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
      IF felmed = TRUE THEN RETURN.
      THIS-OBJECT:UpdateRibbonShow().  
      
      THIS-OBJECT:SparaDefs(?,?).
      THIS-OBJECT:ImportLista().
      THIS-OBJECT:FelMeddCheck().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonKopieraKalkyl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Kopierakalkyl(INPUT INTEGER(STRING(THIS-OBJECT:KopHuvudKatalog:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
      IF felmed = TRUE THEN RETURN.
      THIS-OBJECT:ImportLista().
      THIS-OBJECT:FelMeddCheck().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ImportMallar().
      THIS-OBJECT:FelMeddCheck().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackMarkMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridValdaMallar:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:ButtonBackMarkMallar:GuruOver().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ButtonOverMarkMallar:GuruOver().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFaktorer_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:FaktorerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SavePrisFakt().
		
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFaktorer_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
		
      RETURN.

   END METHOD.

	
   /*------------------------------------------------------------------------------
      Purpose:  																	  
      Notes:  																	  
------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaMallar THEN 
      DO:
         THIS-OBJECT:ButtonBackMarkMallar_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridMaterial_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridEgetMaterial THEN 
      DO:
         THIS-OBJECT:MatButtonBackMark_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridPriser_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SavePrisFakt().
		
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridPriser_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
		
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMallar THEN 
      DO:
         
         THIS-OBJECT:ButtonOverMarkMallar_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridVolymBer_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:VolymberTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBer().
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["TOTVOLYM"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["TILLAGGVOLYM"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("TILLAGGVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["SKYDDFYLLNING"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["YTBELAGGD"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("YTBELAGGD"):BUFFER-VALUE.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KoderAnmarkning_AfterExitEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:KoderAnmarkning:Text).
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      RETURN.

   END METHOD.
	
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC LOGICAL NewKalkyl():
      /* skapa kalkyl i databas */
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKalkyl() = FALSE THEN 
      DO:
         RETURN FALSE.
      END.   
      /* Hitta rätt katalog*/
      THIS-OBJECT:HuvudKatalog:GuruFiltrera().
      KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK).
      THIS-OBJECT:HuvudKatalog:VALUE = THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      THIS-OBJECT:HuvudOmrade:VALUE = THIS-OBJECT:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:HuvudUtfardare:VALUE = THIS-OBJECT:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:HuvudTyp:GuruCombo:VALUE = 2.
      THIS-OBJECT:comboPriserUppf:GuruCombo:VALUE = 1.
      
      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      THIS-OBJECT:LaddaLabels().
      THIS-OBJECT:VisaTabs("ALLA", FALSE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY, TRUE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY, TRUE).
      THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
   END METHOD.
   METHOD PRIVATE LOGICAL SaveHuvud():
      THIS-OBJECT:HuvudTTh:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE  =     STRING(THIS-OBJECT:HuvudBenamning:GuruText:Text).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE     =     STRING(THIS-OBJECT:HuvudKalknr:GuruText:Text).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE    =      STRING(THIS-OBJECT:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE    =     STRING(THIS-OBJECT:HuvudTyp:GuruCombo:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE       =      STRING(THIS-OBJECT:comboPriserUppf:GuruCombo:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE     =     STRING(THIS-OBJECT:HuvudOmrade:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE     =         STRING(THIS-OBJECT:HuvudKatalog:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE  = STRING(THIS-OBJECT:HuvudUtfardare:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:HuvudAnmarkning:Text).
      DEFINE VARIABLE ErrorText AS CHARACTER NO-UNDO.
      DEFINE VARIABLE IsError   AS LOGICAL   NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SaveHuvud(OUTPUT ErrorText, OUTPUT IsError).
      IF IsError = TRUE THEN 
      DO:
         System.Windows.Forms.MessageBox:Show(ErrorText).
         IsError = FALSE.
         RETURN FALSE. 
      END.
      RETURN TRUE. 
   END METHOD.
   METHOD PUBLIC LOGICAL ApplyCreatedKalkyl():
      IF THIS-OBJECT:SaveHuvud() = FALSE THEN 
      DO:
         RETURN FALSE.
      END.   
      THIS-OBJECT:NyKalkyl = FALSE.
      ASSIGN 
         THIS-OBJECT:HuvudKalknr:GuruText:Text = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
         THIS-OBJECT:HuvudKalknr:VISIBLE       = TRUE. 
      IF THIS-OBJECT:LoadKalkyl(INTEGER(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)),STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE), TRUE) = FALSE THEN RETURN FALSE.
      ELSE RETURN TRUE.
   END METHOD.

   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:Logger:WriteLine("Laddar kalkyl #" + STRING(nr) + " ifrån område " + omr + ".").
      THIS-OBJECT:KalkNrvar = nr.
      THIS-OBJECT:Omradevar = omr.
      /* Ladda från databas + DEFAULT*/
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchKalkyl(nr, omr) = FALSE THEN RETURN FALSE.  
      THIS-OBJECT:kalkaonrTTh:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN 
      DO:
         ASSIGN 
             Guru.GlobalaVariabler:plusaonr = ?
            Guru.GlobalaVariabler:plusdnr  = ?.
      END.
      ELSE 
      DO:
         ASSIGN 
             Guru.GlobalaVariabler:plusaonr = THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE
            Guru.GlobalaVariabler:plusdnr  = THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("DELNR"):BUFFER-VALUE.
      END.   
      /*anders
           THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:GuruDefaultsTTh.
           */
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
      /*THIS-OBJECT:GuruDefaultVALUEsTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:GuruDefaultVALUEsTT.*/
      /* Spärra "skapany"-mode */
      ASSIGN
         THIS-OBJECT:HuvudKatalog:ENABLED = FALSE
         THIS-OBJECT:HuvudOmrade:ENABLED  = FALSE
         THIS-OBJECT:HuvudTyp:ENABLED     = FALSE.
      /* VVVVSätt alla ändringar på grids, Combos osv som behövs efter laddning*/
      
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
     
      IF nydef = TRUE THEN 
      DO:
         /*SÄTTER DEFAULT VÄRDE VID NYKALK*/
         THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
            "KalkylRibbonVisaTyp",STRING(THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN 
         DO:
            THIS-OBJECT:MarkningTTh:FIND-FIRST("WHERE markfiltertt.TYP = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE).
            THIS-OBJECT:RibbonToolFilterTyp:Text = THIS-OBJECT:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE.
            THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "FilterTyp",STRING(2), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
         END. 
      END.
      THIS-OBJECT:ApplyLoadedKalkyl().
      THIS-OBJECT:KalkylLoaded = TRUE.
      THIS-OBJECT:RibbonToolFrekvensKom:CHECKED = FALSE.
      THIS-OBJECT:FrekvensKomChanged(?,?).
      THIS-OBJECT:ValueArbetsKoderCh(?,?).
      RETURN TRUE.
   END METHOD.

   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateRibbon().
      THIS-OBJECT:ForetagAnpass().
   END METHOD.
   /* Fullständiggör formen när kalkyl laddats*/
   METHOD PUBLIC VOID ApplyLoadedKalkyl():
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:RibbonTabKalkyl:Caption = "Kalkyl " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE).
     
     
      /* Sätter Combos */
      /*GÖR ÄVEN REF PÅ ARBKODER*/
      THIS-OBJECT:HuvudTyp:GuruCombo:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE.
      THIS-OBJECT:comboPriserUppf:GuruCombo:VALUE  = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE.
      
      
      /*
      THIS-OBJECT:RibbonToolUppfPriser:SelectedItem = THIS-OBJECT:RibbonToolUppfPriser:VALUEList:VALUEListItems[THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE - 1].
     */
      THIS-OBJECT:HuvudOmrade:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      
      THIS-OBJECT:KopHuvudOmrade:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
     
      THIS-OBJECT:HuvudKatalog:GuruFiltrera().
      THIS-OBJECT:KopHuvudKatalog:GuruFiltrera().
      THIS-OBJECT:HuvudKatalog:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      THIS-OBJECT:KopHuvudKatalog:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
     
      THIS-OBJECT:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "'")  NO-ERROR. 
      /*THIS-OBJECT:HuvudUtfardare:GuruFiltrera().*/
      THIS-OBJECT:HuvudUtfardare:VALUE = THIS-OBJECT:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:HuvudKatalog:Text.
      /* Hitta kalkylansvarig*/
      THIS-OBJECT:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
      IF THIS-OBJECT:PersonalTTh:AVAILABLE THEN 
      DO:                   
         THIS-OBJECT:GridPersonal:GuruRepositionTo(THIS-OBJECT:PersonalTTh:ROWID).              
      END.
      /* Filtrera arbetskoder efter kalkyltyp */
      THIS-OBJECT:GridArbetsKoder:GuruFiltrera("kalkylarbkodertt.TYPKALK = " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
      /* Hitta första rows i grejer - behövs detta verkligen ? */
      /* Döp om matris rätt */
      matrisname = THIS-OBJECT:GetMatrisName().
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      /* Sätter värden */
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
      THIS-OBJECT:LaddaLabels().
      /* Ribbongrejjer börjar här! */
      /* Uppdatera matris*/
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Caption = GetMatrisName().
      THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Clear().
      THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add("Alla").
      THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 1").
      THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 2").
      IF HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE NE 7 THEN 
      DO:
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 3").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 4").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 5").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 6").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 7").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 8").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 9").
         THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems:Add(GetMatrisName() + " 10").
      END.
      THIS-OBJECT:RibbonToolMatris:VALUE = THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems[0].
      THIS-OBJECT:RibbonToolMatris:SelectedIndex = 0.
      THIS-OBJECT:CurrentMatris = -1.
      THIS-OBJECT:RibbonToolVisMatris:VALUEList = THIS-OBJECT:RibbonToolMatris:VALUEList:Clone().
      THIS-OBJECT:RibbonToolVisMatris:VALUE = THIS-OBJECT:RibbonToolMatris:VALUE.
      /* Visa  */
      THIS-OBJECT:UpdateRibbonShow().
      THIS-OBJECT:VisaTabs("ALLA", TRUE).
      THIS-OBJECT:GridPriser:GuruReopen().
      THIS-OBJECT:GridFaktorer:GuruReopen().
      
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN 
      DO:
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.         
      END.    
      ELSE THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
      
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
   /*
   Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabEgnaKoder).
   Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabUppfoljning).
   */
      
   /* Ladda defaults
      
   DEFINE VARIABLE DVLink AS Helpers.DVLink NO-UNDO.
   DVLink = NEW Helpers.DVLink( THIS-OBJECT:GuruDefaultsTT, THIS-OBJECT:GuruDefaultVALUEsTT, "KALKYL", THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar, "" ).
   DVLink:AddToolGroup(THIS-OBJECT:RibbonTabVisningEgenskaper).
   DVLink:AddToolGroup(THIS-OBJECT:RibbonTabVisningVisa).
   DVLink:LoadIteration().
   DELETE OBJECT DVLink NO-ERROR.
   TestIterate(THIS-OBJECT:RibbonTabVisningVisa).
   TestIterate(THIS-OBJECT:RibbonTabVisningEgenskaper).
   TestIterate(THIS-OBJECT:RibbonTabUppfoljning).*/
   END METHOD.
   METHOD PUBLIC VOID LaddaLabels():
      THIS-OBJECT:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR.
      ASSIGN
         THIS-OBJECT:LabelOmr:TEXT                  = Guru.Konstanter:gomrk
         THIS-OBJECT:KopLabelOmr:TEXT               = Guru.Konstanter:gomrk
         THIS-OBJECT:HuvudBenamning:GuruLabel:Text  = "Benämning"
         THIS-OBJECT:HuvudBenamning:GuruText:Text   = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
         THIS-OBJECT:HuvudAnmarkning:Text           = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE
         THIS-OBJECT:HuvudKalknr:GuruLabel:Text     = "Kalkylnr."
         THIS-OBJECT:HuvudKalknr:GuruText:Text      = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
         THIS-OBJECT:HuvudKalknr:GuruText:Enabled   = FALSE          
         THIS-OBJECT:HuvudTyp:GuruLabel:Text        = "Kalkyltyp"
         THIS-OBJECT:comboPriserUppf:GuruLabel:Text = "Välj priser för uppföljning"
         THIS-OBJECT:KonvTillTyp:GuruLabel:Text     = " Konvertera till Kalkyltyp"
         THIS-OBJECT:HuvudAnsvarig:GuruLabel:Text   = "Ansvarig"
         THIS-OBJECT:HuvudAnsvarig:GuruText:Text    = THIS-OBJECT:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + THIS-OBJECT:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
   END METHOD.
   
   /*anders
METHOD PUBLIC VOID TestIterate(toolgroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
   /*  visning */
   DEFINE VARIABLE iterator     AS INTEGER                                    NO-UNDO.
   DEFINE VARIABLE ToolType     AS CHARACTER                                  NO-UNDO.
   DEFINE VARIABLE ToolVALUE    AS CHARACTER                                  NO-UNDO.
   DEFINE VARIABLE ToolKey      AS CHARACTER                                  NO-UNDO.
   DEFINE VARIABLE DasTool      AS Infragistics.Win.UltraWinToolbars.ToolBase NO-UNDO.
   DEFINE VARIABLE DebugMessage AS CHARACTER                                  NO-UNDO.
   REPEAT iterator = 0 TO toolgroup:Tools:Count - 1 :
      DasTool = toolgroup:Tools[iterator].
      ToolKey  = DasTool:KEY.
      ToolType = DasTool:GetType():FullName.
      ToolVALUE = "???".
      IF ToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN 
      DO:
         ToolVALUE = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
      END.
        
      IF ToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
         ToolVALUE = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
      END.
      DebugMessage = DebugMessage + "~n" + ToolKey + ", " + ToolType + ", " + ToolVALUE.
   END.
   MESSAGE DebugMessage
      VIEW-AS ALERT-BOX.
END METHOD.
*/
   /* Skapar alla griddar */
   METHOD PUBLIC VOID CreateGrids():
      /* Huvud */
      THIS-OBJECT:CreateGridPersonal().
      /* Kalkyl*/
      THIS-OBJECT:CreateGridArbetsKoder().
      THIS-OBJECT:CreateGridLopNummer().
      THIS-OBJECT:CreateGridKalkylKoder().
      THIS-OBJECT:CreateGridFriKoder().
      THIS-OBJECT:CreateGridFrekvens().
      /* Egna koder*/
      THIS-OBJECT:CreateGridEgnaKoder().
      THIS-OBJECT:CreateGridUnderKoder().
      THIS-OBJECT:CreateGridValdaUnderKoder().
      /* Materiel */
      THIS-OBJECT:CreateGridMateriel().
      THIS-OBJECT:CreateGridEgetMateriel().
      /* Faktorer, priser*/
      THIS-OBJECT:CreateGridFaktorer().
      THIS-OBJECT:CreateGridEgnaPriser().
      THIS-OBJECT:CreateGridMallar().
      THIS-OBJECT:CreateGridMallarKoder().
      THIS-OBJECT:CreateGridValdaMallar().
      THIS-OBJECT:CreateGridVolymBer().
      THIS-OBJECT:CreateGridFelMedd().
      
   END METHOD.

   METHOD PUBLIC VOID CreateGridFunctions():
      /* Drag dropp*/
      THIS-OBJECT:DGKoder = NEW Controls.Subclasses.DragDropGroup("Koder").
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:GridLopKoder).
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:GridKalkylKoder).
      THIS-OBJECT:DGEgna = NEW Controls.Subclasses.DragDropGroup("Egna").
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:GridUnderKoder).
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:DGMat = NEW Controls.Subclasses.DragDropGroup("Material").
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridMaterial).
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridEgetMaterial).
      THIS-OBJECT:DGMall = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:GridMallar).
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:GridValdaMallar).
      /* Button overs*/
      THIS-OBJECT:KalkylButtonOverMark:GuruKoppla(THIS-OBJECT:GridLopKoder,THIS-OBJECT:GridKalkylKoder).
      THIS-OBJECT:KalkylButtonOverMark:GuruDublett = TRUE.
      THIS-OBJECT:KalkylButtonOverAll:GuruKoppla(THIS-OBJECT:GridLopKoder,THIS-OBJECT:GridKalkylKoder).
      THIS-OBJECT:KalkylButtonOverAll:GuruDublett = TRUE.
      THIS-OBJECT:KalkylButtonOverAll:GuruAllaover = TRUE.
      THIS-OBJECT:KalkylButtonBackMark:GuruKoppla(THIS-OBJECT:GridKalkylKoder,THIS-OBJECT:GridLopKoder).
      THIS-OBJECT:KalkylButtonBackMark:GuruRemoveFrom = TRUE.  
      THIS-OBJECT:KalkylButtonBackAll:GuruKoppla(THIS-OBJECT:GridKalkylKoder,THIS-OBJECT:GridLopKoder).              
      THIS-OBJECT:KalkylButtonBackAll:GuruRemoveFrom = TRUE.
      THIS-OBJECT:KalkylButtonBackAll:GuruAllaover = TRUE.
      /* Egna koder börjar här*/
      THIS-OBJECT:EgnaButtonOverMark:GuruKoppla(THIS-OBJECT:GridUnderKoder,THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:EgnaButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:EgnaButtonOverAll:GuruKoppla(THIS-OBJECT:GridUnderKoder, THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:EgnaButtonOverAll:GuruDublett = FALSE.
      THIS-OBJECT:EgnaButtonOverAll:GuruAllaover = TRUE.
      THIS-OBJECT:EgnaButtonBackMark:GuruKoppla(THIS-OBJECT:GridValdaUnderKoder, THIS-OBJECT:GridUnderKoder).
      THIS-OBJECT:EgnaButtonBackMark:GuruRemoveFrom = TRUE.
      THIS-OBJECT:EgnaButtonBackAll:GuruKoppla(THIS-OBJECT:GridValdaUnderKoder, THIS-OBJECT:GridUnderKoder).
      THIS-OBJECT:EgnaButtonBackAll:GuruRemoveFrom = TRUE.
      THIS-OBJECT:EgnaButtonBackAll:GuruAllaover = TRUE.
      /*materialtab*/
      THIS-OBJECT:MatButtonOverMark:GuruKoppla(THIS-OBJECT:GridMaterial,THIS-OBJECT:GridEgetMaterial).
      THIS-OBJECT:MatButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:MatButtonBackMark:GuruKoppla(THIS-OBJECT:GridEgetMaterial, THIS-OBJECT:GridMaterial).
      THIS-OBJECT:MatButtonBackMark:GuruRemoveFrom = TRUE.
      /*mallar*/
      THIS-OBJECT:ButtonOverMarkMallar:GuruKoppla(THIS-OBJECT:GridMallar,THIS-OBJECT:GridValdaMallar).
      THIS-OBJECT:ButtonOverMarkMallar:GuruDublett = TRUE.
      
      THIS-OBJECT:ButtonBackMarkMallar:GuruKoppla(THIS-OBJECT:GridValdaMallar, THIS-OBJECT:GridMallar).
      THIS-OBJECT:ButtonBackMarkMallar:GuruRemoveFrom = TRUE.
   END METHOD.

   /* Huvud */
   METHOD PUBLIC VOID CreateGridPersonal():
      DEFINE VARIABLE personallist  AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE personalPKOD  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE personalFNAMN AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE personalENAMN AS Controls.GridRubrik      NO-UNDO. 
      personallist = NEW Controls.GridRubrikLista().
      personallist:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Global:PersonalTTh.            
      personalPKOD = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      personallist:addRubrik(personalPKOD).
      personalFNAMN = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      personallist:addRubrik(personalFNAMN).
      personalENAMN = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      personallist:addRubrik(personalENAMN).
      THIS-OBJECT:GridPersonal:GuruInitGrid(personallist).  
      THIS-OBJECT:GridPersonal:Text = "Kalkylansvarig".
      /*egna event*/
      THIS-OBJECT:GridPersonal:tBS:PositionChanged:Subscribe(ValuePersCh).
   END METHOD.

   /* Kalkyl*/
   METHOD PUBLIC VOID CreateGridArbetsKoder():
      DEFINE VARIABLE arbkoderlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE arbkoderArb     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE arbkoderBen     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE arbkoderMark    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE arbkoderMarksub AS Controls.GridRubrik      NO-UNDO.
      arbkoderlist = NEW Controls.GridRubrikLista().
      arbkoderlist:ttBufferHandle = THIS-OBJECT:ArbetskoderTTh.
      arbkoderArb = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      arbkoderlist:addRubrik(arbkoderArb).
      arbkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      arbkoderlist:addRubrik(arbkoderBen).
      arbkoderMark = NEW Controls.GridRubrik("MARKNING","Märkning",TRUE,64).
      arbkoderlist:addRubrik(arbkoderMark).
      arbkoderMarksub = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,125).
      arbkoderlist:addRubrik(arbkoderMarksub).
      DEBUGGER:SET-BREAK().
      GridArbetsKoder:GuruInitGrid(arbkoderlist).  
      GridArbetsKoder:TEXT = "Arbetskoder".      
      GridArbetsKoder:tBS:PositionChanged:Subscribe(ValueArbetsKoderCh).
   /*
   THIS-OBJECT:GridArbetsKoder:GuruSetColumnFgColor("ARBKOD", System.Drawing.Color:Red).
   */
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopNummer():
      DEFINE VARIABLE lopkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE lopkoderLop  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE lopkoderEnh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE lopkoderBen  AS Controls.GridRubrik      NO-UNDO.
      lopkoderlist = NEW Controls.GridRubrikLista().
      lopkoderlist:ttBufferHandle = THIS-OBJECT:LopposterTTh.
      lopkoderlist:multiselect = TRUE.   
      lopkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35).
      lopkoderlist:addRubrik(lopkoderLop).
      lopkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      lopkoderlist:addRubrik(lopkoderBen).
      lopkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      lopkoderlist:addRubrik(lopkoderEnh).
      THIS-OBJECT:GridLopKoder:GuruInitGrid(lopkoderlist).
      THIS-OBJECT:GridLopKoder:TEXT = "Löpnummer".
      THIS-OBJECT:GridLopKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:GridLopKoder:tBS:PositionChanged:Subscribe(ValueLopnrCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKalkylKoder():
      DEFINE VARIABLE kalkylkoderlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkylkoderArb     AS Controls.GridRubrik      NO-UNDO.   
      DEFINE VARIABLE kalkylkoderLop     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderEnh     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderBen     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderMat     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderAnt     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderSum     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderFSum    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderMark    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderSubmark AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderRisk    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderVinst   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderNum       AS Controls.GridRubrik      NO-UNDO.
      kalkylkoderlist = NEW Controls.GridRubrikLista().
      kalkylkoderlist:ttBufferHandle = THIS-OBJECT:KoderTTh.
      kalkylkoderlist:multiselect = TRUE. 
      /* 
      egnakoderNum = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      kalkylkoderlist:addRubrik(egnakoderNum).
      */ 
      kalkylkoderMat = NEW Controls.GridRubrik("MATRIS","MATRIS",FALSE,35).
      kalkylkoderlist:addRubrik(kalkylkoderMat). 
      kalkylkoderArb = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      kalkylkoderlist:addRubrik(kalkylkoderArb).
      kalkylkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      kalkylkoderlist:addRubrik(kalkylkoderLop).
      kalkylkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      kalkylkoderlist:addRubrik(kalkylkoderEnh).
      kalkylkoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      kalkylkoderlist:addRubrik(kalkylkoderAnt).
      kalkylkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      kalkylkoderlist:addRubrik(kalkylkoderBen).
      kalkylkoderSum = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55).
      kalkylkoderlist:addRubrik(kalkylkoderSum).
      kalkylkoderFSum = NEW Controls.GridRubrik("FRITOTKOST","Frikalk. Summa",TRUE,55).
      kalkylkoderlist:addRubrik(kalkylkoderFSum).
      kalkylkoderMark = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      kalkylkoderlist:addRubrik(kalkylkoderMark).
      kalkylkoderSubmark = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,80).
      kalkylkoderlist:addRubrik(kalkylkoderSubmark).
      kalkylkoderRisk = NEW Controls.GridRubrik("RISK","Risk i %",FALSE,75).
      kalkylkoderlist:addRubrik(kalkylkoderRisk).
      kalkylkoderVinst = NEW Controls.GridRubrik("VINST","Vinst i %",FALSE,75).
      kalkylkoderlist:addRubrik(kalkylkoderVinst).
      THIS-OBJECT:GridKalkylKoder:GuruInitGrid(kalkylkoderlist). 
      THIS-OBJECT:GridKalkylKoder:TEXT = "Kalkylkoder".  
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.   
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Format = "0".
      THIS-OBJECT:KoderAnmarkning:VISIBLE = FALSE.
      /*default filter  sortering*/     
      THIS-OBJECT:GridKalkylKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:GridKalkylKoder:tBS:PositionChanged:Subscribe(ValueKalkKoderCh).
      THIS-OBJECT:GridKalkylKoder:GuruFirstrow().
   END METHOD.

   METHOD PUBLIC VOID CreateGridFrekvens():
      DEFINE VARIABLE kalkylkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkylkoderArb  AS Controls.GridRubrik      NO-UNDO.   
      DEFINE VARIABLE kalkylkoderLop  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderEnh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderBen  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderAnt  AS Controls.GridRubrik      NO-UNDO.
      kalkylkoderlist = NEW Controls.GridRubrikLista().
      kalkylkoderlist:ttBufferHandle = THIS-OBJECT:FrekvensTTh.
      kalkylkoderArb = NEW Controls.GridRubrik("FREKOD"," Arbkod ",TRUE,55).      
      kalkylkoderlist:addRubrik(kalkylkoderArb).
      kalkylkoderLop = NEW Controls.GridRubrik("FREKNR","Löpnr",TRUE,45).
      kalkylkoderlist:addRubrik(kalkylkoderLop).
      kalkylkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      kalkylkoderlist:addRubrik(kalkylkoderEnh).
      kalkylkoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      kalkylkoderlist:addRubrik(kalkylkoderAnt).
      kalkylkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,600).
      kalkylkoderlist:addRubrik(kalkylkoderBen).
      THIS-OBJECT:GridFrekvens:GuruInitGrid(kalkylkoderlist). 
      THIS-OBJECT:GridFrekvens:TEXT = "Frekvenskoder".  
   END METHOD.

   METHOD PUBLIC VOID CreateGridFriKoder():
      DEFINE VARIABLE frikalkList     AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE frikalkBen      AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkTim      AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkKost     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkPris     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkfTim     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkfKost    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkfPris    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE frikalkfBen     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserNum  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserNums AS Controls.GridRubrik      NO-UNDO.
      frikalklist = NEW Controls.GridRubrikLista().
      frikalklist:ttBufferHandle = THIS-OBJECT:ValdaPriserTTh.
      frikalklist:multiselect = FALSE.
      /*
      valdapriserNum = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      frikalkList:AddRubrik(valdapriserNUM).
      valdapriserNums = NEW Controls.GridRubrik("NUMsubid", "##", TRUE ,35).
      frikalkList:AddRubrik(valdapriserNUMs).
      */
      frikalkfBen = NEW Controls.GridRubrik("FRIBENAMNING", "Fri benämning", false, 100).
      frikalkList:addRubrik(frikalkfBen).
      frikalkfTim = NEW Controls.GridRubrik("FRITIMMAR", "Fri timmar", false, 50).
      frikalkList:addRubrik(frikalkfTim).
      frikalkfPris = NEW Controls.GridRubrik("FRIPRIS", "Fri pris/h", false, 75).
      frikalkList:addRubrik(frikalkfPris).
      frikalkfKost = NEW Controls.GridRubrik("FRIKOSTNAD", "Fri kostnad", false, 75).
      frikalkList:addRubrik(frikalkfKost).
      frikalkBen = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 100).
      frikalkList:addRubrik(frikalkBen).
      frikalkTim = NEW Controls.GridRubrik("TIMMAR", "Timmar", true, 50).
      frikalkList:addRubrik(frikalkTim).
      frikalkPris = NEW Controls.GridRubrik("PRIS", "Pris/h", true, 75).
      frikalkList:addRubrik(frikalkPris).     
      frikalkKost = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", true, 75).
      frikalkList:addRubrik(frikalkKost).
      THIS-OBJECT:GridFriKalk:GuruInitGrid(frikalklist).
      THIS-OBJECT:GridFriKalk:Text = "Frikalkylkoder".
      THIS-OBJECT:GridFriKalk:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridFriKalk:GuruFirstrow().
      /*
            THIS-OBJECT:splitContainerKalk:SplitterDistance = THIS-OBJECT:ClientSize:Width * 0.35.
            THIS-OBJECT:splitContainerFriKalk:SplitterDistance =  THIS-OBJECT:splitContainerFriKalk:Width / 2.
        */   
      THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRIBENAMNING", System.Drawing.Color:LightBlue).
      THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRIPRIS", System.Drawing.Color:LightBlue).
      THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRIKOSTNAD", System.Drawing.Color:LightBlue).
      THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRITIMMAR", System.Drawing.Color:LightBlue).
   /*
    MESSAGE Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])
    VIEW-AS ALERT-BOX.
    THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRIBENAMNING", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
    THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRIPRIS", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
    THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRIKOSTNAD", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
    THIS-OBJECT:GridFriKalk:GuruSetColumnBgColor("FRITIMMAR", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
    */
   END METHOD.
   
   /* Egna koder*/
   METHOD PUBLIC VOID CreateGridEgnaKoder():
      THIS-OBJECT:DontCheck = TRUE.
      DEFINE VARIABLE egnakoderlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE egnakoderArb     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderNum     AS Controls.GridRubrik      NO-UNDO.      
      DEFINE VARIABLE egnakoderLop     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderEnh     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderBen     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderMat     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderAnt     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderSum     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderMark    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderSubmark AS Controls.GridRubrik      NO-UNDO.
      egnakoderlist = NEW Controls.GridRubrikLista().
      egnakoderlist:ttBufferHandle = THIS-OBJECT:KoderTTh.
      egnakoderlist:multiselect = FALSE.  
      egnakoderlist:filter = "kalknumtt.ARBKOD = 'EGEN'".
      /*
      egnakoderNum = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      egnakoderlist:addRubrik(egnakoderNum).
      */ 
      egnakoderMat = NEW Controls.GridRubrik("MATRIS","MATRIS",FALSE,35).
      egnakoderlist:addRubrik(egnakoderMat). 
      egnakoderArb = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      egnakoderlist:addRubrik(egnakoderArb).
      egnakoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      egnakoderlist:addRubrik(egnakoderLop).
      egnakoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      egnakoderlist:addRubrik(egnakoderEnh).
      egnakoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45).
      egnakoderlist:addRubrik(egnakoderAnt).
      egnakoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,250).
      egnakoderlist:addRubrik(egnakoderBen).
      egnakoderSum = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55).
      egnakoderlist:addRubrik(egnakoderSum).
      egnakoderMark = NEW Controls.GridRubrik("MARKNING","Märkning",false,100).
      egnakoderlist:addRubrik(egnakoderMark).
      egnakoderSubmark = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",false,80).
      egnakoderlist:addRubrik(egnakoderSubmark).
      /*init av grid*/
      THIS-OBJECT:GridEgnaKoder:GuruInitGrid(egnakoderlist). 
      GridEgnaKoder:TEXT = "Egna koder".     
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:GridEgnaKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:GridEgnaKoder:tBS:PositionChanged:Subscribe(ValueEgnaKoderCh).
      THIS-OBJECT:GridEgnaKoder:GuruFirstrow().
      THIS-OBJECT:DontCheck = false.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridUnderKoder():
      DEFINE VARIABLE kalkpriserlist     AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkpriserKPID     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserKVID     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserSokB     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserBenm     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserPris     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserEgenPris AS Controls.GridRubrik      NO-UNDO.
      kalkpriserlist = NEW Controls.GridRubrikLista().
      kalkpriserlist:ttBufferHandle = THIS-OBJECT:PriserTTh.
      kalkpriserlist:multiselect = TRUE.
      kalkpriserlist:filter = "EGENKODUPP = TRUE".
      kalkpriserBenm = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 250).
      kalkpriserlist:addRubrik(kalkpriserBenm).
      kalkpriserPris = NEW Controls.GridRubrik("PRIS", "Pris/enhet", TRUE, 100).
      kalkpriserlist:addRubrik(kalkpriserPris).
      THIS-OBJECT:GridUnderKoder:GuruInitGrid(kalkpriserlist).
      THIS-OBJECT:GridUnderKoder:TEXT = "Underkoder".
      THIS-OBJECT:GridUnderKoder:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridUnderKoder:GuruFirstrow().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaUnderKoder():
      DEFINE VARIABLE valdapriserlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE valdapriserBenm    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserTimmar  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserPris    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserKostnad AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserNum     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserNums    AS Controls.GridRubrik      NO-UNDO.
      valdapriserlist = NEW Controls.GridRubrikLista().
      valdapriserlist:ttBufferHandle = THIS-OBJECT:ValdaPriserTTh.
      valdapriserlist:multiselect = TRUE.
      valdapriserlist:filter = "".
      /*
      valdapriserNum = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      valdapriserlist:AddRubrik(valdapriserNUM).
      valdapriserNums = NEW Controls.GridRubrik("NUMSUBID", "##", TRUE ,35).
      valdapriserlist:AddRubrik(valdapriserNUMs).
      */
      valdapriserBenm = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      valdapriserlist:AddRubrik(valdapriserBenm).
      
      valdapriserTimmar = NEW Controls.GridRubrik("TIMMAR", "Antal",FALSE,45,"DEC").
      valdapriserlist:AddRubrik(valdapriserTimmar).
      valdapriserPris = NEW Controls.GridRubrik("PRIS", "Pris/enhet", FALSE ,100).
      valdapriserlist:AddRubrik(valdapriserPris).
      valdapriserKostnad = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE ,100).
      valdapriserlist:AddRubrik(valdapriserKostnad).
      THIS-OBJECT:GridValdaUnderKoder:GuruInitGrid(valdapriserlist).
      THIS-OBJECT:GridValdaUnderKoder:TEXT = "Valda underkoder".
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruOrderby("BY BENAMNING").
   END METHOD.

   /* Materiel */
   METHOD PUBLIC VOID CreateGridMateriel():
      DEFINE VARIABLE matlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE matben  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE matenh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE matenr  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE matnet  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE matkpr  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE matlev  AS Controls.GridRubrik      NO-UNDO.
      
      matlist = NEW Controls.GridRubrikLista().
      matlist:ttBufferHandle = THIS-OBJECT:MaterialTTh.
      matlist:multiselect = TRUE.
      matlist:filter = "".
      matenr = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk , TRUE ,75).
      matlist:AddRubrik(matenr).
      matben = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      matlist:AddRubrik(matben).
      matenh = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      matlist:AddRubrik(matenh).
      matnet = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,100).
      matlist:AddRubrik(matnet).
      matlev = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      matlist:AddRubrik(matlev).
      /*
      matkpr = NEW Controls.GridRubrik("kund", "färg", TRUE ,50).
      matlist:AddRubrik(matkpr).
      */
      
      THIS-OBJECT:GridMaterial:GuruInitGrid(matlist).
      THIS-OBJECT:GridMaterial:Text = "Materiel".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN 
      DO:
         THIS-OBJECT:GridMaterial:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
     
      THIS-OBJECT:GridMaterial:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgetMateriel():
      DEFINE VARIABLE ematlist  AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE ematben   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematenh   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematenr   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematnet   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematlev   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematkpr   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematmat   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE ematantal AS Controls.GridRubrik      NO-UNDO.
      ematlist = NEW Controls.GridRubrikLista().
      ematlist:ttBufferHandle = THIS-OBJECT:kalktmtrlTTh.
      ematlist:multiselect = TRUE.
      ematlist:filter = "".
      ematmat = NEW Controls.GridRubrik("MATRIS", "Matris.", TRUE ,50).
      ematlist:AddRubrik(ematmat).   
      ematenr = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,75).
      ematlist:AddRubrik(ematenr).
      ematben = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      ematlist:AddRubrik(ematben).
      ematenh = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      ematlist:AddRubrik(ematenh).
      ematantal = NEW Controls.GridRubrik("BERKVANT", "Antal", FALSE,50).
      ematlist:AddRubrik(ematantal).
      ematnet = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,100).
      ematlist:AddRubrik(ematnet).
      
      /*
      ematkpr = NEW Controls.GridRubrik("KPRIS", "Kundpris", TRUE ,50).
      ematlist:AddRubrik(ematkpr).
      */
      ematlev = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      ematlist:AddRubrik(ematlev).
      /* +kundpris, +levid, +antal*/
      /*
       ematkpr = NEW Controls.GridRubrik("kund", "färg", TRUE ,50).
      ematlist:AddRubrik(ematkpr).
      */
      THIS-OBJECT:GridEgetMaterial:GuruInitGrid(ematlist).
      THIS-OBJECT:GridEgetMaterial:Text = "Materielspecifikation".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN 
      DO:
         THIS-OBJECT:GridEgetMaterial:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      THIS-OBJECT:GridEgetMaterial:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).     
   END METHOD.

   /* Faktorer och priser*/
   METHOD PUBLIC VOID CreateGridFaktorer():
      DEFINE VARIABLE Headers    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE hBenamning AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hFaktor    AS Controls.GridRubrik      NO-UNDO.
      Headers = NEW Controls.GridRubrikLista().
      Headers:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:FaktorerTTh.
      hBenamning = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      Headers:addRubrik(hBenamning).
      hFaktor = NEW Controls.GridRubrik("FAKTOR","Faktor",FALSE,75).
      Headers:addRubrik(hFaktor).
      THIS-OBJECT:GridFaktorer:GuruInitGrid(Headers).  
      THIS-OBJECT:GridFaktorer:Text = "Faktorer".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMallarKoder():
      DEFINE VARIABLE kalkylkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkylkoderArb  AS Controls.GridRubrik      NO-UNDO.   
      DEFINE VARIABLE kalkylkoderLop  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderEnh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderBen  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderAnt  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hTyp            AS Controls.GridRubrik      NO-UNDO.
      kalkylkoderlist = NEW Controls.GridRubrikLista().
      kalkylkoderlist:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkmallKodertth.
      kalkylkoderArb = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      kalkylkoderlist:addRubrik(kalkylkoderArb).
      kalkylkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      kalkylkoderlist:addRubrik(kalkylkoderLop).
      kalkylkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      kalkylkoderlist:addRubrik(kalkylkoderEnh).
      kalkylkoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC").
      kalkylkoderlist:addRubrik(kalkylkoderAnt).
      kalkylkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      kalkylkoderlist:addRubrik(kalkylkoderBen).
      THIS-OBJECT:GridMallarKoder:GuruInitGrid(kalkylkoderlist). 
      /*default filter  sortering*/     
      THIS-OBJECT:GridMallarKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:GridMallarKoder:Text = "Kalkylkoder för mallen".
   END METHOD.

   METHOD PUBLIC VOID CreateGridMallar():
      DEFINE VARIABLE hHeaders   AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE hBenamning AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hTyp       AS Controls.GridRubrik      NO-UNDO.
      hHeaders = NEW Controls.GridRubrikLista().
      hHeaders:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkmallHuvudtth.
      hBenamning = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      hHeaders:addRubrik(hBenamning).
      hTyp = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      hHeaders:addRubrik(hTyp).
      THIS-OBJECT:GridMallar:GuruInitGrid(hHeaders).  
      THIS-OBJECT:GridMallar:Text = "Kalkylmallar".
      GridMallar:tBS:PositionChanged:Subscribe(ValueMallarCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaMallar():
      DEFINE VARIABLE hHeaders   AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE hBenamning AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hTyp       AS Controls.GridRubrik      NO-UNDO.
      hHeaders = NEW Controls.GridRubrikLista().
      hHeaders:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkmallValdtth.
      hBenamning = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      hHeaders:addRubrik(hBenamning).
      hTyp = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      hHeaders:addRubrik(hTyp).
      THIS-OBJECT:GridValdaMallar:GuruInitGrid(hHeaders).  
      THIS-OBJECT:GridValdaMallar:Text = "Valda Kalkylmallar".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgnaPriser():
      DEFINE VARIABLE hHeaders   AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE hBenamning AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hPris      AS Controls.GridRubrik      NO-UNDO.
      hHeaders = NEW Controls.GridRubrikLista().
      hHeaders:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:EgnaPriserTTh.
      hBenamning = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      hHeaders:addRubrik(hBenamning).
      hPris = NEW Controls.GridRubrik("PRIS","Pris",FALSE,75).
      hHeaders:addRubrik(hPris).
      THIS-OBJECT:GridPriser:GuruInitGrid(hHeaders).  
      THIS-OBJECT:GridPriser:Text = "Egna priser".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridVolymBer():
      DEFINE VARIABLE hSchl    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchb    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchd    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchm    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchr    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchtv   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchtlv  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hSchsf   AS Controls.GridRubrik      NO-UNDO.   
      DEFINE VARIABLE hSchyt   AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hHeaders AS Controls.GridRubrikLista NO-UNDO.
      hHeaders = NEW Controls.GridRubrikLista().
      hHeaders:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymberTTh.
      hSchl = NEW Controls.GridRubrik("SCHAKTLANGD","Schaktlängd i meter",FALSE,100).
      hHeaders:addRubrik(hSchl).
      hSchb = NEW Controls.GridRubrik("SCHAKTBREDD","Schaktbredd i centimeter",FALSE,140).
      hHeaders:addRubrik(hSchb).
      hSchd = NEW Controls.GridRubrik("SCHAKTDJUP","Schaktdjup i centimeter",FALSE,135).
      hHeaders:addRubrik(hSchd).
      hSchm = NEW Controls.GridRubrik("SVARMARK","Svår mark",FALSE,70).
      hHeaders:addRubrik(hSchm).
      hSchr = NEW Controls.GridRubrik("RORFOR","Rörförläggning",FALSE,90).
      hHeaders:addRubrik(hSchr).
      hSchtv = NEW Controls.GridRubrik("TOTVOLYM","Totalt m3",TRUE,70).
      hHeaders:addRubrik(hSchtv).
      hSchtlv = NEW Controls.GridRubrik("TILLAGGVOLYM","Tilläggsfyllning m3",TRUE,110).
      hHeaders:addRubrik(hSchtlv).
      hSchsf = NEW Controls.GridRubrik("SKYDDFYLLNING","Skyddsfyllning m3",TRUE,105).
      hHeaders:addRubrik(hSchsf).
      hSchyt = NEW Controls.GridRubrik("YTBELAGGD","Ytbeläggning m2",TRUE,100).
      hHeaders:addRubrik(hSchyt).
      THIS-OBJECT:GridVolymBer:GuruInitGrid(hHeaders).  
      THIS-OBJECT:GridVolymBer:Text = "Volymberäkningar".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFelMedd():
      DEFINE VARIABLE hut      AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE hHeaders AS Controls.GridRubrikLista NO-UNDO.
      hHeaders = NEW Controls.GridRubrikLista().
      hHeaders:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Kalkyl:tiduth.
      hut = NEW Controls.GridRubrik("UT","Meddelande",TRUE,400).
      hHeaders:addRubrik(hut).
      THIS-OBJECT:GridFelMedd:GuruInitGrid(hHeaders).  
      THIS-OBJECT:GridFelMedd:Text = "Fel vid inläsning!".
   END METHOD.
   
   
   /* Skapar alla Combos */
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboTyp().
      THIS-OBJECT:CreatecomboPriserUppf().
      THIS-OBJECT:CreateComboOmrade().
      THIS-OBJECT:KopCreateComboOmrade().
      THIS-OBJECT:CreateComboKatalog().
      THIS-OBJECT:CreateComboKopKatalog().
      THIS-OBJECT:CreateComboUtfardare().
      
   END METHOD. 

   METHOD PUBLIC VOID CreatecomboPriserUppf():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE ufpris:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Priser".
      cmbTyplist:addRubrik(typK). 
      CREATE ufpris.
      ASSIGN 
         ufpris.UTYP = 1
         ufpris.TYPC = "EBR-katalog".
      CREATE ufpris.
      ASSIGN 
         ufpris.UTYP = 2
         ufpris.TYPC = "Eget materiel".
      THIS-OBJECT:comboPriserUppf:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "UTYP", "TYPC").
      
   END METHOD.
   METHOD PUBLIC VOID CreateComboTyp():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).                    
      IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 3 THEN 
      DO:
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
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "VAST" THEN 
      DO: 
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 5
            typtt.TYPC    = "Sam B/F".        
      END.
      IF Guru.Konstanter:varforetypval[41] = 1 THEN 
      DO:  
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 7
            typtt.TYPC    = "Nätreg N1".      
      END.
      THIS-OBJECT:HuvudTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
   END METHOD.
   METHOD PUBLIC VOID CreateComboKonvTyp():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      EMPTY TEMP-TABLE konvtyptt NO-ERROR. 
      THIS-OBJECT:KonvTillTyp:GuruCombo:Clear().
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE konvtyptt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:HuvudKatalog:VALUE) ).
      IF INTEGER(STRING(THIS-OBJECT:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE)) <= 2002 THEN.
      ELSE 
      DO: 
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "1" THEN 
         DO:
            CREATE konvtyptt.
            ASSIGN 
               konvtyptt.TYPKALK = 2
               konvtyptt.TYPC    = "Typ 2".
            IF Guru.Konstanter:varforetypval[41] = 1 THEN 
            DO:  
               CREATE konvtyptt.
               ASSIGN 
                  konvtyptt.TYPKALK = 7
                  konvtyptt.TYPC    = "Nätreg N1".      
            END.
         END.
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN 
         DO:
            CREATE konvtyptt.
            ASSIGN 
               konvtyptt.TYPKALK = 1
               konvtyptt.TYPC    = "Typ 1".
         END.   
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "2" THEN 
         DO:
            IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "VAST" THEN 
            DO: 
               CREATE konvtyptt.
               ASSIGN 
                  konvtyptt.TYPKALK = 3
                  konvtyptt.TYPC    = "Typ 3".        
            END.
         END.
      END.    
      FIND FIRST konvtyptt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE konvtyptt THEN 
      DO:
         CREATE konvtyptt.
         ASSIGN 
            konvtyptt.TYPKALK = 99
            konvtyptt.TYPC    = "Det går inte att konvertera denna kalkyl!".  
      END.
      THIS-OBJECT:KonvTillTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
      THIS-OBJECT:KonvTillTyp:GuruCombo:VALUE = konvtyptt.TYPKALK.
   END METHOD.
   METHOD PUBLIC VOID CreateComboOmrade():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = THIS-OBJECT:OmradeTTh.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = Guru.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).  
      THIS-OBJECT:HuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").
   END METHOD.
   METHOD PUBLIC VOID KopCreateComboOmrade():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = THIS-OBJECT:OmradeTTh.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = Guru.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).  
      THIS-OBJECT:KopHuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").
   END METHOD.

   METHOD PUBLIC VOID CreateComboKatalog():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KatalogTTh.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      THIS-OBJECT:HuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
   END METHOD.
   METHOD PUBLIC VOID CreateComboKopKatalog():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KatalogTTh.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      THIS-OBJECT:KopHuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
   END METHOD.
   METHOD PUBLIC VOID CreateComboUtfardare():
      DEFINE VARIABLE cmbUtflist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE UtfK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE UtfnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbUtflist = NEW Controls.GridRubrikLista().
      cmbUtflist:ttBufferHandle = THIS-OBJECT:AnvandareTTh.
      IF Guru.Konstanter:globforetag = "ELPA" THEN.
      ELSE cmbUtflist:filter = "anvandartemp.AV-lEVEL > 0".
      UtfK = NEW Controls.GridRubrik().
      UtfK:Falt = "ANVANDARE".
      UtfK:Rubrik = "Användare".
      cmbUtflist:addRubrik(UtfK).
      UtfnamnK = NEW Controls.GridRubrik().
      UtfnamnK:Falt = "AV-NAMN".
      UtfnamnK:Rubrik = "Namn".
      cmbUtflist:addRubrik(UtfnamnK).       
      THIS-OBJECT:HuvudUtfardare:GuruInitCombo(cmbUtflist, "ANVANDARE", "AV-NAMN").
   END METHOD.
   METHOD PUBLIC VOID CreateComboAvtal():
      DEFINE VARIABLE cmbAvtallist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katar        AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE avtalar      AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE avtaltxt     AS Controls.GridRubrik      NO-UNDO.
      cmbAvtallist = NEW Controls.GridRubrikLista().
      cmbAvtallist:ttBufferHandle = THIS-OBJECT:Avtalskalktth.
      katar = NEW Controls.GridRubrik().
      katar:Falt = "KATAR".
      katar:Rubrik = "Årtal för KLG".
      cmbAvtallist:addRubrik(katar).
      avtalar = NEW Controls.GridRubrik().
      avtalar:Falt = "AVTALAR".
      avtalar:Rubrik = "Årtal för avtal".
      cmbAvtallist:addRubrik(avtalar).
      avtaltxt = NEW Controls.GridRubrik().
      avtaltxt:Falt = "AVTALTXT".
      avtaltxt:Rubrik = "Välj avtalskalkylmall för din kund.".
      cmbAvtallist:addRubrik(AVTALTXT).  
      THIS-OBJECT:ComboAvtal:GuruInitCombo(cmbAvtallist,"ID","AVTALTXTHELA").
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackAlla_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):     
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridKalkylKoder:Posn() = -1 THEN RETURN. 
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(43), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveKoderAll().
      THIS-OBJECT:KalkylButtonBackAll:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackAllaPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridEgnaKoder:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:GridValdaUnderKoder:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(43),THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveValdaAlla().
      THIS-OBJECT:EgnaButtonBackAll:GuruOver().
      THIS-OBJECT:BerEgenKod().      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC VOID BerEgenKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK)  NO-ERROR.
      IF THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN      
         THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
    
   END METHOD.

   METHOD PUBLIC VOID BerKalkKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN
         THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
      IF THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE THEN 
      DO:
         IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE
            THEN THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
      END.
   END METHOD.

   METHOD PRIVATE VOID ButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridKalkylKoder:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:DontCheck = TRUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveKoderSelected().
      THIS-OBJECT:KalkylButtonBackMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      THIS-OBJECT:DontCheck = FALSE.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridEgnaKoder:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:GridValdaUnderKoder:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:DontCheck = TRUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveValdaSelected().
      THIS-OBJECT:EgnaButtonBackMark:GuruOver().      
      THIS-OBJECT:BerEgenKod().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      THIS-OBJECT:DontCheck = FALSE.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverAlla_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
      antalvar = 1.
      THIS-OBJECT:KalkylButtonOverAll:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKoderRows(antalvar).
      THIS-OBJECT:KalkylButtonOverAll:GuruOverefteruppdat().
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverAllaPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:EgnaButtonOverAll:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateValdaRows().
      THIS-OBJECT:EgnaButtonOverAll:GuruOverefteruppdat().
      THIS-OBJECT:BerEgenKod().
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOvermark_ClickE( INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:ButtonOvermark_Click(1).
      RETURN.
   END METHOD.

   METHOD PRIVATE VOID ButtonOvermark_Click(INPUT antalvar AS DECIMAL):
      THIS-OBJECT:KalkylButtonOverMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKoderRows(antalvar).
      THIS-OBJECT:KalkylButtonOverMark:GuruOverefteruppdat().
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmpe =  THIS-OBJECT:GridEgnaKoder:ActiveRow.     
      THIS-OBJECT:EgnaButtonOverMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateValdaRows().      
      THIS-OBJECT:EgnaButtonOverMark:GuruOverefteruppdat().            
      THIS-OBJECT:BerEgenKod().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgetMaterial_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMaterial THEN 
      DO:
         THIS-OBJECT:MatButtonOverMark_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterCellActivate( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:GridEgnaKoder:ActiveRow = ? THEN RETURN.
      FilterUnderKoder().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      THIS-OBJECT:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKod().
      THIS-OBJECT:BerEgenKod().        
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterRowActivate( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      FilterUnderKoder().
   END METHOD.
   
   METHOD PUBLIC VOID FilterUnderKoder():
      /* Om det finns en aktiv row..*/
      IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 AND THIS-OBJECT:GridEgnaKoder:ActiveRow NE ? THEN 
      DO:
         /* Uppdatera valda underkoderna, och återställ kontrollerna*/
         IF TRIM(STRING(THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE)) = "" THEN RETURN.
         IF THIS-OBJECT:DontCheck = FALSE THEN 
         DO:
            THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE )).
            THIS-OBJECT:GridValdaUnderKoder:GuruFirstrow().
            THIS-OBJECT:GridValdaUnderKoder:Enabled = TRUE.
            THIS-OBJECT:EgnaButtonOverAll:Enabled = TRUE.
            THIS-OBJECT:EgnaButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:EgnaButtonBackAll:Enabled = TRUE.
            THIS-OBJECT:EgnaButtonBackMark:Enabled = TRUE.
         END.  
      END.
      ELSE 
      DO:
         /* Annars disablea kontrollerna och filtrera efter något dumt som döljer rubbet */
         THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera("NUM = -1").
         THIS-OBJECT:GridValdaUnderKoder:Enabled = FALSE.
         THIS-OBJECT:EgnaButtonOverAll:Enabled = FALSE.
         THIS-OBJECT:EgnaButtonOverMark:Enabled = FALSE.
         THIS-OBJECT:EgnaButtonBackAll:Enabled = FALSE.
         THIS-OBJECT:EgnaButtonBackMark:Enabled = FALSE.
      END.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      THIS-OBJECT:FilterUnderKoder().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFriKalk_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKodSub().
      THIS-OBJECT:BerKalkKod().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  	skickar data till db efter cell uppdat																  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKalkylKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      /*THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.*/
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKod().
      THIS-OBJECT:BerKalkKod().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().     
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKodSub().
      THIS-OBJECT:BerEgenKod().
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKalkylKoder_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      DEFINE VARIABLE num AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN 
      DO:
         IF THIS-OBJECT:GridFriKalk:GuruEgenskap NE ? THEN 
         DO:
            IF THIS-OBJECT:GridKalkylKoder:ActiveRow = ? THEN 
            DO:
               THIS-OBJECT:GridKalkylKoder:GuruFirstRow().		         
            END.   
            IF THIS-OBJECT:GridKalkylKoder:ActiveRow NE ? THEN 
            DO:
               IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells NE ? THEN 
               DO:
                  num = THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE.
                  THIS-OBJECT:GridFriKalk:GuruFiltrera("NUM = " + STRING(num) + " AND EGENKODUPP = TRUE").
               END.
            END.
         END.
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:    gör då samma som buttenover                                                  
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKalkylKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopKoder THEN 
      DO:
         THIS-OBJECT:ButtonOvermark_ClickE(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridLopKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKalkylKoder THEN 
      DO:
         THIS-OBJECT:ButtonBackmark_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaUnderKoder THEN 
      DO:
         THIS-OBJECT:ButtonBackmarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridUnderKoder THEN 
      DO:
         THIS-OBJECT:ButtonOvermarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID HuvudTyp_GuruValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:UpdateRibbonShow().
      RETURN.

   END METHOD.

   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Control = THIS-OBJECT.      
      THIS-OBJECT:CreateForm().
      RETURN TRUE.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID MatButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridEgetMaterial:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:MatButtonBackMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID MatButtonOverMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:MatButtonOverMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreatevalMaterial(THIS-OBJECT:KalkNrvar,THIS-OBJECT:Omradevar).
      THIS-OBJECT:MatButtonOverMark:GuruOverefteruppdat().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabKopiera_Paint( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.PaintEventArgs ):
		
      RETURN.

   END METHOD.

   METHOD PUBLIC VOID UpdateMatrisE(sender AS System.Object, args AS System.EventArgs):
      THIS-OBJECT:UpdateMatris().
   END METHOD.

   METHOD PUBLIC VOID UpdateMatris():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO. 
      mat = 1.
      IF THIS-OBJECT:RibbonToolMatris:Text = "Alla" THEN mat = -1.
      IF THIS-OBJECT:RibbonToolMatris:Text = GetMatrisName() + " 1" THEN mat = 1.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 2" THEN mat = 2.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 3" THEN mat = 3.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 4" THEN mat = 4.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 5" THEN mat = 5.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 6" THEN mat = 6.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 7" THEN mat = 7.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 8" THEN mat = 8.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 9" THEN mat = 9.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 10" THEN mat = 10.
      THIS-OBJECT:CurrentMatris = mat.
      IF THIS-OBJECT:NyKalkyl = FALSE THEN 
      DO:
         IF mat = -1 THEN 
         DO:
            THIS-OBJECT:GridEgetMaterial:GuruFiltrera("").
            THIS-OBJECT:GridEgnaKoder:GuruFiltrera("ARBKOD = 'EGEN'" ).
            THIS-OBJECT:GridKalkylKoder:GuruFiltrera("").
         END.
         ELSE 
         DO:
            THIS-OBJECT:GridEgetMaterial:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
            THIS-OBJECT:KalkylButtonOverAll:Enabled = TRUE.
            THIS-OBJECT:KalkylButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:EgenSkapa:Enabled = TRUE.
            THIS-OBJECT:GridEgnaKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)+ " AND ARBKOD = 'EGEN'" ).
            THIS-OBJECT:GridKalkylKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
         END.
         THIS-OBJECT:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:FilterUnderKoder().
      END.      
   END METHOD.
   
   METHOD PUBLIC VOID CreateRibbon():
      /* Skapa ribbongrupp i kalkyltaben */
      THIS-OBJECT:Rbt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Bilder").
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      THIS-OBJECT:RibbonTabKalkyl = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkyl").
      THIS-OBJECT:RibbonTabKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Koder").
      THIS-OBJECT:RibbonTabEgnaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Egna Koder").
      THIS-OBJECT:RibbonTabMat = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sök material").
      THIS-OBJECT:RibbonTabVisaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visning av koder").
      THIS-OBJECT:RibbonTabVisaKoder:Visible = FALSE.
      THIS-OBJECT:RibbonTabVisning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkylvisning").
      THIS-OBJECT:RibbonTabVisningVisa = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Inkludera i visning").
      THIS-OBJECT:RibbonTabVisningEgenskaper = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visningsinställningar").
      /*
      THIS-OBJECT:RibbonTabUppfoljning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Uppföljning").
      */
      /* Skapa verktyg*/
      THIS-OBJECT:RibbonDummy = NEW Infragistics.Win.UltraWinToolbars.LabelTool("RD").
      THIS-OBJECT:RibbonDummy:SharedPropsInternal:Caption = " ". 
      /* Global*/
      THIS-OBJECT:RibbonToolMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("KodMatris").
      THIS-OBJECT:RibbonToolMatris:Key = "KalkylRibbonMatris".
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width = 135.
      
      THIS-OBJECT:RibbonToolVisaTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisaTyp").
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisaTyp:SharedProps:Visible = TRUE.
      THIS-OBJECT:RibbonToolVisaTyp:Key = "KalkylRibbonVisaTyp".
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width.
      DEFINE VARIABLE wTemp AS Infragistics.Win.VALUEListItem NO-UNDO.
      FOR EACH typtt:
         wTemp = NEW Infragistics.Win.VALUEListItem().
         wTemp:DisplayText = typtt.TYPC.
         wTemp:DataVALUE = NEW Helpers.ABLDataContainer().
         CAST(wTemp:DataVALUE, Helpers.ABLDataContainer):CreateData("Typ", typtt.TYPKALK).
         THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems:Add(wTemp).
      END. 
      THIS-OBJECT:RibbonToolFilterTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FilterTyp").
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Caption = "Märkning".
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width.
      /* kalkyl*/
      THIS-OBJECT:RibbonToolInfTyp = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKalkTyp").
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Caption = "Kalkyl".
      THIS-OBJECT:RibbonToolInfTyp:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolOmrade = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoOmrade").
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Caption = Guru.Konstanter:gomrk.
      THIS-OBJECT:RibbonToolOmrade:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKatalog").
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Caption = "Katalog".
      THIS-OBJECT:RibbonToolKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:HuvudKatalog:Text.
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Width = 192.
      /* Koder */
      THIS-OBJECT:RibbonToolFriKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FriKalkActive").
      THIS-OBJECT:RibbonToolFriKalk:SharedPropsInternal:Caption = "Frikalkyl".
      THIS-OBJECT:RibbonToolFriKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFriGenerera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FriGen").
      THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Caption = "Återställ frikalkyl".
      THIS-OBJECT:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolFriGenerera,INPUT "bilder/kalkyl-frikalkyl.gif").
      THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = FALSE.
      THIS-OBJECT:RibbonToolKoderAnm = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KoderAnmarkning").
      THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:Caption = "Anm. för koder".
      THIS-OBJECT:RibbonToolKoderAnm:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens-kommentar".
                                                                         
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /* Visning */
     
      THIS-OBJECT:RibbonToolGenerera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaGen").
      THIS-OBJECT:RibbonToolGenerera:SharedPropsInternal:Caption = "Generera kalkyl".
      THIS-OBJECT:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolGenerera,INPUT "bilder/kalkyl-generate.gif").
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Caption = "Skriv Ut".
      THIS-OBJECT:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSkrivUt,INPUT "bilder/kalkyl-print.gif").
      
      /* inkludera i visning */
      THIS-OBJECT:RibbonToolVisHuvud = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisHuvud").
      THIS-OBJECT:RibbonToolVisHuvud:SharedPropsInternal:Caption = "Kalkylhuvud".
      THIS-OBJECT:RibbonToolVisHuvud:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisAnmarkning = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisAnmarkning").
      THIS-OBJECT:RibbonToolVisAnmarkning:SharedPropsInternal:Caption = "Anmärkning".
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisUtforlig = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisAnmUtf").
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Caption = "Utförlig anm.".
      THIS-OBJECT:RibbonToolVisUtforlig:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = FALSE.
      THIS-OBJECT:RibbonToolVisEgnaFaktorer = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisFaktorer").
      THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Caption = "Egna faktorer".
      THIS-OBJECT:RibbonToolVisEgnaFaktorer:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisEgnaPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisEgnaPriser").
      THIS-OBJECT:RibbonToolVisEgnaPriser:SharedPropsInternal:Caption = "Egna Priser".
      THIS-OBJECT:RibbonToolVisEgnaPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /* visningsinställningar */
      THIS-OBJECT:RibbonToolVisVisa = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisVisafor").
      THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisVisa:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Grundkalkyl")).
      THIS-OBJECT:RibbonToolVisVisa:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Frikalkyl")).
      THIS-OBJECT:RibbonToolVisVisa:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Riskkalkyl")).
      THIS-OBJECT:RibbonToolVisVisa:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Vinstkalkyl")).
      THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Width = 140.
      
      THIS-OBJECT:RibbonToolVisPriser = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisPriser").
      THIS-OBJECT:RibbonToolVisPriser:SharedPropsInternal:Caption = "Priser".
      THIS-OBJECT:RibbonToolVisPriser:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("EBR-katalog")).
      THIS-OBJECT:RibbonToolVisPriser:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Eget materiel")).
      THIS-OBJECT:RibbonToolVisPriser:SharedPropsInternal:Width = 140.
      
      THIS-OBJECT:RibbonToolVisMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisMatris").
      THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Caption = "Matris".
      THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Width = 140.
      
      THIS-OBJECT:RibbonToolVisSummera = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisSummera").
      THIS-OBJECT:RibbonToolVisSummera:SharedPropsInternal:Caption = "Summera koder".
      THIS-OBJECT:RibbonToolVisSummera:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisGruppera = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisGruppera").
      THIS-OBJECT:RibbonToolVisGruppera:SharedPropsInternal:Caption = "Gruppera koder".
      THIS-OBJECT:RibbonToolVisGruppera:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisGroupMatris = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisGruppMatris").
      THIS-OBJECT:RibbonToolVisGroupMatris:SharedPropsInternal:Caption = "Gruppera matris".
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      
      THIS-OBJECT:RibbonToolVisMatSpec = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisMatSpec").
      THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Caption = "Mtrl.spec".
      THIS-OBJECT:RibbonToolVisMatSpec:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolSparaDefs = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveDefs").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Caption = "Spara inställningar".
      /*
      THIS-OBJECT:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      */
       THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Caption = "".
      THIS-OBJECT:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/AF_logo.gif").
          
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = TRUE.

      /*
      /* Uppföljning */
      THIS-OBJECT:RibbonToolUppfPriser = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("UppfPriser").
      THIS-OBJECT:RibbonToolUppfPriser:SharedPropsInternal:Caption = "Priser".
      THIS-OBJECT:RibbonToolUppfPriser:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("EBR-katalog")).
      THIS-OBJECT:RibbonToolUppfPriser:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Eget materiel")).
      THIS-OBJECT:RibbonToolUppfPriser:SharedPropsInternal:Width = 130.
       /*
      THIS-OBJECT:RibbonToolUppfPriser:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Från beredning")).
      */
      */
     
      /* Material */
      THIS-OBJECT:RibbonToolLev = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("MatLever").
      THIS-OBJECT:RibbonToolLev:SharedPropsInternal:Caption = "Leverantör:".
      /* Använd detta för inladdning = THIS-OBJECT:RibbonToolMatris:VALUEList:VALUEListItems[1]:DataVALUE */
      THIS-OBJECT:RibbonToolEnr = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatEnr").
      THIS-OBJECT:RibbonToolEnr:SharedPropsInternal:Caption = Guru.Konstanter:genk.
      THIS-OBJECT:RibbonToolSok = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatSoek").
      THIS-OBJECT:RibbonToolSok:SharedPropsInternal:Caption = "Benämning:".
      /* Förbered för ribbon */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMatris).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolInfTyp).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolOmrade).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKat).           
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLev).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnr).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSok).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriKalk).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriGenerera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKoderAnm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFrekvensKom).
      /* Preppa Visning*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGenerera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUt).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisHuvud).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisAnmarkning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisUtforlig).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisEgnaFaktorer).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisEgnaPriser).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisSummera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGruppera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisPriser).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatSpec).
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolUppfPriser).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatris).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGroupMatris).
      DEBUGGER:SET-BREAK().
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs).
      /* Lägg till verktygen i gruppen ordning */
      THIS-OBJECT:RibbonTabKoder:Tools:Add( THIS-OBJECT:RibbonToolMatris ).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolVisaTyp).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFilterTyp).
      
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolKoderAnm).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFrekvensKom).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFriKalk).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFriGenerera).
      THIS-OBJECT:RibbonTabEgnaKoder:Tools:Add( THIS-OBJECT:RibbonToolMatris ).
      
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolEnr).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolSok).
     
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolLev).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolInfTyp).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolOmrade).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolKat).
      
      THIS-OBJECT:RibbonTabVisning:Tools:Add(THIS-OBJECT:RibbonToolGenerera).
      THIS-OBJECT:RibbonTabVisning:Tools:Add(THIS-OBJECT:RibbonToolSkrivUt).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisHuvud).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisAnmarkning).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisUtforlig).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisEgnaFaktorer).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisEgnaPriser).
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisVisa).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisPriser).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisMatris).
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisSummera).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisGruppera).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisGroupMatris).   
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisMatSpec).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonDummy).

      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolSparaDefs).
      
      /*
      THIS-OBJECT:RibbonTabUppfoljning:Tools:Add(THIS-OBJECT:RibbonToolUppfPriser).
      */
      /* binda events*/
      THIS-OBJECT:RibbonToolSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolEnr:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolGenerera:ToolClick:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      THIS-OBJECT:RibbonToolSkrivUt:ToolClick:Subscribe(THIS-OBJECT:SkrivUtE).
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolClick:Subscribe(THIS-OBJECT:VisAnmarkningChanged).
      THIS-OBJECT:RibbonToolVisPriser:ToolValueChanged:Subscribe(THIS-OBJECT:VisPriserChanged).
      THIS-OBJECT:RibbonToolVisPriser:SelectedItem = THIS-OBJECT:RibbonToolVisPriser:VALUEList:VALUEListItems[0].
      THIS-OBJECT:RibbonToolVisVisa:SelectedItem = THIS-OBJECT:RibbonToolVisVisa:VALUEList:VALUEListItems[0].
      THIS-OBJECT:RibbonToolVisaTyp:ToolValueChanged:Subscribe(RibbonUpdateTyp).
      THIS-OBJECT:RibbonToolFriGenerera:ToolClick:Subscribe(THIS-OBJECT:RestoreFriKoder).
      THIS-OBJECT:RibbonToolFriKalk:ToolClick:Subscribe(THIS-OBJECT:FriKalkChanged).
      
      THIS-OBJECT:RibbonToolKoderAnm:ToolClick:Subscribe(THIS-OBJECT:KoderAnmChanged).
      THIS-OBJECT:RibbonToolFrekvensKom:ToolClick:Subscribe(THIS-OBJECT:FrekvensKomChanged).
      THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder).
      /*
      THIS-OBJECT:RibbonToolUppfPriser:SelectedItem = THIS-OBJECT:RibbonToolUppfPriser:VALUEList:VALUEListItems[0].
      THIS-OBJECT:RibbonToolUppfPriser:ToolValueChanged:Subscribe(RibbonUpdateUppfPriser).
      */
      THIS-OBJECT:RibbonToolMatris:ToolValueChanged:Subscribe(UpdateMatrisE).
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolClick:Subscribe(THIS-OBJECT:GruppMatrisChanged).
     
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:Subscribe(SparaDefs).
      /* splatter */
     
      THIS-OBJECT:LoadRibbonLevs().
   END METHOD.
   
   METHOD PUBLIC VOID RibbonImage(INPUT vad AS INTEGER, INPUT rbt1 AS Infragistics.Win.UltraWinToolbars.ButtonTool,INPUT path AS CHARACTER):
      rbt = rbt1.
      path = SEARCH(path).
      IF path NE ? THEN 
      DO:
         IF vad = 1 THEN THIS-OBJECT:rbt:SharedPropsInternal:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).
         IF vad = 2 THEN DO:
            THIS-OBJECT:rbt:SharedPropsInternal:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).
            THIS-OBJECT:rbt:SharedPropsInternal:AppearancesSmall:ResetAppearance().          
         END.   
      END.
      IF vad = 4 THEN THIS-OBJECT:rbt:SharedPropsInternal:AppearancesLarge:ResetAppearance().  
      IF vad = 5 THEN THIS-OBJECT:rbt:SharedPropsInternal:AppearancesSmall:ResetAppearance().
   /*
 MESSAGE THIS-OBJECT:rbt:ToString() THIS-OBJECT:rbt:SharedPropsInternal:AppearancesLarge:HasAppearance  
    THIS-OBJECT:rbt:SharedPropsInternal:AppearancesSmall:HasAppearance
    /*
    HasPropertyBeenSet( it:SharedPropsInternal:AppearancesSmall:ToString()Appearance)
               it:SharedPropsInternal:Appearance:Image = 2.
    */
    VIEW-AS ALERT-BOX.
  */
   END METHOD.
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "Kalkyl",INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar) .
   /*
   DEFINE VARIABLE DVLink AS Helpers.DVLink NO-UNDO.
   DVLink = NEW Helpers.DVLink( THIS-OBJECT:GuruDefaultsTT, THIS-OBJECT:GuruDefaultVALUEsTT, "KALKYL", THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar, "" ).
   DVLink:AddToolGroup(THIS-OBJECT:RibbonTabVisningEgenskaper).
   DVLink:AddToolGroup(THIS-OBJECT:RibbonTabVisningVisa).
      
   DVLink:SaveIteration().
   DELETE OBJECT DVLink NO-ERROR.
   THIS-OBJECT:Root:DatabaseManager:Kalkyl:SaveDefaultVALUEs().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:TestDataSet().
   */
   END METHOD.

   METHOD PUBLIC VOID VisAnmarkningChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = THIS-OBJECT:RibbonToolVisAnmarkning:Checked.
   END METHOD.
   
   METHOD PUBLIC VOID VisPriserChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*THIS-OBJECT:RibbonToolVisPriser:VALUEList:VALUEListItems[THIS-OBJECT:RibbonToolVisPriser:VALUEList:SelectedIndex]*/
      IF THIS-OBJECT:RibbonToolVisPriser:Text EQ "EBR-katalog" THEN 
      DO:
         THIS-OBJECT:RibbonToolVisMatSpec:CHECKED = FALSE.
         THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = FALSE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = TRUE.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RestoreFriKoder(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchFriKoder(THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar).
      THIS-OBJECT:GridFriKalk:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID FrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN 
      DO:
         ASSIGN
            THIS-OBJECT:RibbonToolKoderAnm:Checked                     = FALSE 
            THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = FALSE
            THIS-OBJECT:KodKommentar:VISIBLE                           = TRUE
            THIS-OBJECT:GridFrekvens:VISIBLE                           = TRUE
            THIS-OBJECT:GridKalkylKoder:VISIBLE                        = FALSE.
         THIS-OBJECT:KodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         THIS-OBJECT:ValueLopnrCh(?,?). 
      END.   
      ELSE 
      DO:
         ASSIGN
            THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = TRUE
            THIS-OBJECT:KodKommentar:VISIBLE                           = FALSE
            THIS-OBJECT:GridFrekvens:VISIBLE                           = FALSE
            THIS-OBJECT:GridKalkylKoder:VISIBLE                        = TRUE.
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID KoderAnmChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN THIS-OBJECT:KoderAnmarkning:VISIBLE = TRUE.
      ELSE THIS-OBJECT:KoderAnmarkning:VISIBLE = FALSE.         
   END METHOD.
   
   METHOD PUBLIC VOID FriKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      GridKalkylKoder_AfterSelectChange(?,?).
      IF THIS-OBJECT:RibbonToolFriKalk:Checked EQ TRUE THEN 
      DO:
         THIS-OBJECT:GridKalkylKoder:Dock = System.Windows.Forms.DockStyle:Fill.
         THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE.
         THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:VISIBLE = FALSE.
         THIS-OBJECT:RibbonToolKoderAnm:Checked = FALSE. 
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = FALSE.
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 1.
         IF THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE  THEN.
         ELSE 
         DO:
            System.Windows.Forms.MessageBox:Show("Du kan bara gå till Frikalkyl via fliken Kakyl!").
            THIS-OBJECT:RibbonToolFriKalk:Checked = FALSE.
            RETURN.
         END.
         THIS-OBJECT:GridFriKalk:GuruReopen().
         THIS-OBJECT:splitContainerFriKalk:Panel2Collapsed = FALSE.
         THIS-OBJECT:splitContainerKalk:Panel1Collapsed = TRUE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = FALSE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = FALSE.
         THIS-OBJECT:VisaTabs("FRIKALK", FALSE).
         THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:GridKalkylKoder:Dock = System.Windows.Forms.DockStyle:Left.
         THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 0.
         THIS-OBJECT:splitContainerFriKalk:Panel2Collapsed = TRUE.
         THIS-OBJECT:splitContainerKalk:Panel1Collapsed = FALSE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
         THIS-OBJECT:VisaTabs("FRIKALK", TRUE).
         THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = FALSE.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID GruppMatrisChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:RibbonToolVisGroupMatris:Checked EQ TRUE THEN 
      DO:
         THIS-OBJECT:RibbonToolVisMatris:Text = "Alla".         
      END.
      ELSE 
      DO:
         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning():
      DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
      THIS-OBJECT:RibbonToolFilterTyp:VALUEList:VALUEListItems:Clear().
      THIS-OBJECT:RibbonToolFilterTyp:VALUEList:VALUEListItems:Add("Alla").
      THIS-OBJECT:RibbonToolFilterTyp:VALUEList:VALUEListItems:Add("Ej märkta").
      Q = "FOR EACH " + THIS-OBJECT:MarkningTTh:TABLE + " WHERE TYP = " + STRING(THIS-OBJECT:GetVisaTyp()) + " NO-LOCK".
      CREATE QUERY mQ.
      mQ:SET-BUFFERS(THIS-OBJECT:MarkningTTh).
      mQ:QUERY-PREPARE(Q).
      mQ:QUERY-OPEN().
      mQ:GET-FIRST().
      DO WHILE mQ:QUERY-OFF-END = FALSE:
         THIS-OBJECT:RibbonToolFilterTyp:VALUEList:VALUEListItems:Add(THIS-OBJECT:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE).
         mQ:GET-NEXT().
      END.
      mQ:QUERY-CLOSE().
      THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:VALUEList:VALUEListItems[1].
      THIS-OBJECT:RefreshArbKoder(?,?).
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
      DEFINE VARIABLE DoReturn   AS LOGICAL   INITIAL FALSE NO-UNDO.
      Typ = THIS-OBJECT:GetVisaTyp().
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      IF Markning EQ "Alla" OR Markning EQ "" THEN DoAll = TRUE.
      ELSE DoAll = FALSE.
      IF Markning EQ "Ej märkta" THEN DoUnmarked = TRUE.
      ELSE DoUnmarked = FALSE.
      GridLopKoder:GuruAvmarkera(). 
      IF DoAll EQ TRUE THEN 
      DO:
         THIS-OBJECT:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ)).
         THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
      END.
      ELSE 
      DO:
         IF DoUnmarked EQ TRUE THEN 
         DO:
            THIS-OBJECT:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = ''").
            THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
         END.
         ELSE 
         DO:
            THIS-OBJECT:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '" + Markning + "'").
            THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
         END.
         IF THIS-OBJECT:GridArbetsKoder:tBS:Count EQ 0 THEN 
         DO:
            THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:VALUEList:VALUEListItems[0].
            DoReturn = TRUE.
         END.      
      END.
      IF DoReturn EQ TRUE THEN RETURN.
      IF THIS-OBJECT:ArbetsKoderTTh:AVAILABLE EQ TRUE THEN 
      DO:
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TYPKALK = " + STRING(Typ)) NO-ERROR.
         THIS-OBJECT:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         GridLopKoder:GuruFirstrow(). 
      END.
      ELSE 
      DO:
         THIS-OBJECT:GridLopKoder:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:GridLopKoder:GuruFirstrow().
         IF THIS-OBJECT:KalkylLoaded EQ TRUE THEN 
         DO:
            System.Windows.Forms.MessageBox:Show("Inga arbetskoder funna!").
         END.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID UpdateRibbonShow():
      THIS-OBJECT:RibbonToolInfTyp:Text = THIS-OBJECT:HuvudTyp:GuruCombo:Text.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      i = 0.
      DO WHILE i LE THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems:Count :
         IF  THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems[i]:DisplayText = THIS-OBJECT:RibbonToolInfTyp:Text THEN 
         DO:
            THIS-OBJECT:RibbonToolVisaTyp:SelectedItem = THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems[i].
            RETURN.
         END.
         i = i + 1.
      END.      
   END METHOD.
   
   METHOD PUBLIC VOID LoadRibbonLevs():
      DEFINE VARIABLE w     AS Infragistics.Win.VALUEListItem NO-UNDO.
      DEFINE VARIABLE wData AS Helpers.ABLDataContainer       NO-UNDO.
      DEFINE VARIABLE qH    AS HANDLE                         NO-UNDO.
      DEFINE VARIABLE sw    AS Integer                        NO-UNDO INITIAL 0.
      DEFINE VARIABLE ssw   AS Integer                        NO-UNDO.
      THIS-OBJECT:RibbonToolLev:VALUEList:VALUEListItems:Clear().
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:LeverantorTTh, "FOR EACH " + THIS-OBJECT:LeverantorTTh:TABLE + " NO-LOCK").
      qH:GET-FIRST().
      
      DO WHILE qh:QUERY-OFF-END = FALSE:
         
         w = NEW Infragistics.Win.VALUEListItem().
         w:DisplayText = STRING(THIS-OBJECT:LeverantorTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE()).
         wData = NEW Helpers.ABLDataContainer().
         wData:CreateData("LEVKOD", STRING(THIS-OBJECT:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE())).
         w:DataVALUE = wData.
         IF STRING(THIS-OBJECT:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE()) = THIS-OBJECT:Root:DatabaseManager:Global:HuvudLeverantor THEN 
         DO:
            wData:CreateData("ISHUV", TRUE).
            ssw = sw.
         END.
         ELSE 
         DO:
            wData:CreateData("ISHUV", FALSE).
         END.
       
         THIS-OBJECT:RibbonToolLev:VALUEList:VALUEListItems:Add(w).
         sw = sw + 1 . 
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:RibbonToolLev:VALUEList:VALUEListItems:Add("Alla leverantörer").
      THIS-OBJECT:RibbonToolLev:SelectedIndex = ssw.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
   END METHOD.
   
   /* Ribbon events*/
   METHOD PUBLIC VOID RibbonSokUpdate(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
      DEFINE VARIABLE enr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ben AS CHARACTER NO-UNDO.
      IF e:KeyChar  = CHR(13) THEN 
      DO:
         IF STRING(THIS-OBJECT:RibbonToolLev:VALUEList:VALUEListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE) = "Alla leverantörer" THEN lev = "".
         ELSE lev = STRING( CAST(THIS-OBJECT:RibbonToolLev:VALUEList:VALUEListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). /*THIS-OBJECT:MatLeverantor:GuruCombo:SelectedRow:GetCellVALUE("LEVKOD").*/
         enr = THIS-OBJECT:RibbonToolEnr:Text.
         ben = THIS-OBJECT:RibbonToolSok:Text.
         THIS-OBJECT:Root:DatabaseManager:Global:FetchMaterial(lev, enr, ben).
         THIS-OBJECT:GridMaterial:GuruReopen().
         THIS-OBJECT:GridMaterial:GuruFirstrow().
         e:Handled = TRUE.
      END.
   END METHOD.

   METHOD PUBLIC INTEGER GetVisaTyp():
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems:Count > 0 THEN 
      DO:
         RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems[THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetInteger("Typ").
      END.
      RETURN -1.
   END METHOD.

   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      IF THIS-OBJECT:GridArbetsKoder:Initierad = FALSE THEN RETURN.
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems:Count > 0 THEN 
      DO:
         THIS-OBJECT:RefreshMarkning().
      END.
   END METHOD.
   /*
   METHOD PUBLIC VOID RibbonUpdateUppfPriser(sender AS System.Object, e AS System.EventArgs):
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE = 1 + THIS-OBJECT:RibbonToolUppfPriser:SelectedIndex.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
   END METHOD.  
     */ 
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
      appearance50 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance66 AS Infragistics.Win.Appearance NO-UNDO.
      appearance66 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance67 AS Infragistics.Win.Appearance NO-UNDO.
      appearance67 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance68 AS Infragistics.Win.Appearance NO-UNDO.
      appearance68 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance69 AS Infragistics.Win.Appearance NO-UNDO.
      appearance69 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance70 AS Infragistics.Win.Appearance NO-UNDO.
      appearance70 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance71 AS Infragistics.Win.Appearance NO-UNDO.
      appearance71 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance72 AS Infragistics.Win.Appearance NO-UNDO.
      appearance72 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance73 AS Infragistics.Win.Appearance NO-UNDO.
      appearance73 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance74 AS Infragistics.Win.Appearance NO-UNDO.
      appearance74 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance75 AS Infragistics.Win.Appearance NO-UNDO.
      appearance75 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance76 AS Infragistics.Win.Appearance NO-UNDO.
      appearance76 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance115 AS Infragistics.Win.Appearance NO-UNDO.
      appearance115 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance116 AS Infragistics.Win.Appearance NO-UNDO.
      appearance116 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance117 AS Infragistics.Win.Appearance NO-UNDO.
      appearance117 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance118 AS Infragistics.Win.Appearance NO-UNDO.
      appearance118 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance119 AS Infragistics.Win.Appearance NO-UNDO.
      appearance119 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance120 AS Infragistics.Win.Appearance NO-UNDO.
      appearance120 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance121 AS Infragistics.Win.Appearance NO-UNDO.
      appearance121 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance122 AS Infragistics.Win.Appearance NO-UNDO.
      appearance122 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance123 AS Infragistics.Win.Appearance NO-UNDO.
      appearance123 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance124 AS Infragistics.Win.Appearance NO-UNDO.
      appearance124 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance125 AS Infragistics.Win.Appearance NO-UNDO.
      appearance125 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance126 AS Infragistics.Win.Appearance NO-UNDO.
      appearance126 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
      appearance54 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance79 AS Infragistics.Win.Appearance NO-UNDO.
      appearance79 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance80 AS Infragistics.Win.Appearance NO-UNDO.
      appearance80 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance81 AS Infragistics.Win.Appearance NO-UNDO.
      appearance81 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance82 AS Infragistics.Win.Appearance NO-UNDO.
      appearance82 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance83 AS Infragistics.Win.Appearance NO-UNDO.
      appearance83 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance84 AS Infragistics.Win.Appearance NO-UNDO.
      appearance84 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance85 AS Infragistics.Win.Appearance NO-UNDO.
      appearance85 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance86 AS Infragistics.Win.Appearance NO-UNDO.
      appearance86 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance87 AS Infragistics.Win.Appearance NO-UNDO.
      appearance87 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance88 AS Infragistics.Win.Appearance NO-UNDO.
      appearance88 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance89 AS Infragistics.Win.Appearance NO-UNDO.
      appearance89 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance90 AS Infragistics.Win.Appearance NO-UNDO.
      appearance90 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance330 AS Infragistics.Win.Appearance NO-UNDO.
      appearance330 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance331 AS Infragistics.Win.Appearance NO-UNDO.
      appearance331 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance332 AS Infragistics.Win.Appearance NO-UNDO.
      appearance332 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance333 AS Infragistics.Win.Appearance NO-UNDO.
      appearance333 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance334 AS Infragistics.Win.Appearance NO-UNDO.
      appearance334 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance335 AS Infragistics.Win.Appearance NO-UNDO.
      appearance335 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance336 AS Infragistics.Win.Appearance NO-UNDO.
      appearance336 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance337 AS Infragistics.Win.Appearance NO-UNDO.
      appearance337 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance338 AS Infragistics.Win.Appearance NO-UNDO.
      appearance338 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance339 AS Infragistics.Win.Appearance NO-UNDO.
      appearance339 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance340 AS Infragistics.Win.Appearance NO-UNDO.
      appearance340 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance341 AS Infragistics.Win.Appearance NO-UNDO.
      appearance341 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance318 AS Infragistics.Win.Appearance NO-UNDO.
      appearance318 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance319 AS Infragistics.Win.Appearance NO-UNDO.
      appearance319 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance320 AS Infragistics.Win.Appearance NO-UNDO.
      appearance320 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance321 AS Infragistics.Win.Appearance NO-UNDO.
      appearance321 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance322 AS Infragistics.Win.Appearance NO-UNDO.
      appearance322 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance323 AS Infragistics.Win.Appearance NO-UNDO.
      appearance323 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance324 AS Infragistics.Win.Appearance NO-UNDO.
      appearance324 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance325 AS Infragistics.Win.Appearance NO-UNDO.
      appearance325 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance326 AS Infragistics.Win.Appearance NO-UNDO.
      appearance326 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance327 AS Infragistics.Win.Appearance NO-UNDO.
      appearance327 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance328 AS Infragistics.Win.Appearance NO-UNDO.
      appearance328 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance329 AS Infragistics.Win.Appearance NO-UNDO.
      appearance329 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance149 AS Infragistics.Win.Appearance NO-UNDO.
      appearance149 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.ttKalkyl").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance152 AS Infragistics.Win.Appearance NO-UNDO.
      appearance152 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance150 AS Infragistics.Win.Appearance NO-UNDO.
      appearance150 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance151 AS Infragistics.Win.Appearance NO-UNDO.
      appearance151 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance106 AS Infragistics.Win.Appearance NO-UNDO.
      appearance106 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
      appearance103 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
      appearance104 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance105 AS Infragistics.Win.Appearance NO-UNDO.
      appearance105 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance114 AS Infragistics.Win.Appearance NO-UNDO.
      appearance114 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance109 AS Infragistics.Win.Appearance NO-UNDO.
      appearance109 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance108 AS Infragistics.Win.Appearance NO-UNDO.
      appearance108 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance107 AS Infragistics.Win.Appearance NO-UNDO.
      appearance107 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance111 AS Infragistics.Win.Appearance NO-UNDO.
      appearance111 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance113 AS Infragistics.Win.Appearance NO-UNDO.
      appearance113 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance112 AS Infragistics.Win.Appearance NO-UNDO.
      appearance112 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance110 AS Infragistics.Win.Appearance NO-UNDO.
      appearance110 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance342 AS Infragistics.Win.Appearance NO-UNDO.
      appearance342 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance343 AS Infragistics.Win.Appearance NO-UNDO.
      appearance343 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance344 AS Infragistics.Win.Appearance NO-UNDO.
      appearance344 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance345 AS Infragistics.Win.Appearance NO-UNDO.
      appearance345 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance346 AS Infragistics.Win.Appearance NO-UNDO.
      appearance346 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance347 AS Infragistics.Win.Appearance NO-UNDO.
      appearance347 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance348 AS Infragistics.Win.Appearance NO-UNDO.
      appearance348 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance349 AS Infragistics.Win.Appearance NO-UNDO.
      appearance349 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance350 AS Infragistics.Win.Appearance NO-UNDO.
      appearance350 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance351 AS Infragistics.Win.Appearance NO-UNDO.
      appearance351 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance352 AS Infragistics.Win.Appearance NO-UNDO.
      appearance352 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance353 AS Infragistics.Win.Appearance NO-UNDO.
      appearance353 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance354 AS Infragistics.Win.Appearance NO-UNDO.
      appearance354 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance355 AS Infragistics.Win.Appearance NO-UNDO.
      appearance355 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance356 AS Infragistics.Win.Appearance NO-UNDO.
      appearance356 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance357 AS Infragistics.Win.Appearance NO-UNDO.
      appearance357 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance358 AS Infragistics.Win.Appearance NO-UNDO.
      appearance358 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance359 AS Infragistics.Win.Appearance NO-UNDO.
      appearance359 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance360 AS Infragistics.Win.Appearance NO-UNDO.
      appearance360 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance361 AS Infragistics.Win.Appearance NO-UNDO.
      appearance361 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance362 AS Infragistics.Win.Appearance NO-UNDO.
      appearance362 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance363 AS Infragistics.Win.Appearance NO-UNDO.
      appearance363 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance364 AS Infragistics.Win.Appearance NO-UNDO.
      appearance364 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance365 AS Infragistics.Win.Appearance NO-UNDO.
      appearance365 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance378 AS Infragistics.Win.Appearance NO-UNDO.
      appearance378 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance379 AS Infragistics.Win.Appearance NO-UNDO.
      appearance379 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance380 AS Infragistics.Win.Appearance NO-UNDO.
      appearance380 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance381 AS Infragistics.Win.Appearance NO-UNDO.
      appearance381 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance382 AS Infragistics.Win.Appearance NO-UNDO.
      appearance382 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance383 AS Infragistics.Win.Appearance NO-UNDO.
      appearance383 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance384 AS Infragistics.Win.Appearance NO-UNDO.
      appearance384 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance385 AS Infragistics.Win.Appearance NO-UNDO.
      appearance385 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance386 AS Infragistics.Win.Appearance NO-UNDO.
      appearance386 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance387 AS Infragistics.Win.Appearance NO-UNDO.
      appearance387 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance388 AS Infragistics.Win.Appearance NO-UNDO.
      appearance388 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance389 AS Infragistics.Win.Appearance NO-UNDO.
      appearance389 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance153 AS Infragistics.Win.Appearance NO-UNDO.
      appearance153 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance156 AS Infragistics.Win.Appearance NO-UNDO.
      appearance156 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance154 AS Infragistics.Win.Appearance NO-UNDO.
      appearance154 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance155 AS Infragistics.Win.Appearance NO-UNDO.
      appearance155 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance366 AS Infragistics.Win.Appearance NO-UNDO.
      appearance366 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance367 AS Infragistics.Win.Appearance NO-UNDO.
      appearance367 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance368 AS Infragistics.Win.Appearance NO-UNDO.
      appearance368 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance369 AS Infragistics.Win.Appearance NO-UNDO.
      appearance369 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance370 AS Infragistics.Win.Appearance NO-UNDO.
      appearance370 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance371 AS Infragistics.Win.Appearance NO-UNDO.
      appearance371 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance372 AS Infragistics.Win.Appearance NO-UNDO.
      appearance372 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance373 AS Infragistics.Win.Appearance NO-UNDO.
      appearance373 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance374 AS Infragistics.Win.Appearance NO-UNDO.
      appearance374 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance375 AS Infragistics.Win.Appearance NO-UNDO.
      appearance375 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance376 AS Infragistics.Win.Appearance NO-UNDO.
      appearance376 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance377 AS Infragistics.Win.Appearance NO-UNDO.
      appearance377 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance91 AS Infragistics.Win.Appearance NO-UNDO.
      appearance91 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance92 AS Infragistics.Win.Appearance NO-UNDO.
      appearance92 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance93 AS Infragistics.Win.Appearance NO-UNDO.
      appearance93 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance94 AS Infragistics.Win.Appearance NO-UNDO.
      appearance94 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance95 AS Infragistics.Win.Appearance NO-UNDO.
      appearance95 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance96 AS Infragistics.Win.Appearance NO-UNDO.
      appearance96 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance97 AS Infragistics.Win.Appearance NO-UNDO.
      appearance97 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance98 AS Infragistics.Win.Appearance NO-UNDO.
      appearance98 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance99 AS Infragistics.Win.Appearance NO-UNDO.
      appearance99 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance100 AS Infragistics.Win.Appearance NO-UNDO.
      appearance100 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance390 AS Infragistics.Win.Appearance NO-UNDO.
      appearance390 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance391 AS Infragistics.Win.Appearance NO-UNDO.
      appearance391 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance392 AS Infragistics.Win.Appearance NO-UNDO.
      appearance392 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance393 AS Infragistics.Win.Appearance NO-UNDO.
      appearance393 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance394 AS Infragistics.Win.Appearance NO-UNDO.
      appearance394 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance395 AS Infragistics.Win.Appearance NO-UNDO.
      appearance395 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance396 AS Infragistics.Win.Appearance NO-UNDO.
      appearance396 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance397 AS Infragistics.Win.Appearance NO-UNDO.
      appearance397 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance398 AS Infragistics.Win.Appearance NO-UNDO.
      appearance398 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance399 AS Infragistics.Win.Appearance NO-UNDO.
      appearance399 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance400 AS Infragistics.Win.Appearance NO-UNDO.
      appearance400 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance401 AS Infragistics.Win.Appearance NO-UNDO.
      appearance401 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance426 AS Infragistics.Win.Appearance NO-UNDO.
      appearance426 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance427 AS Infragistics.Win.Appearance NO-UNDO.
      appearance427 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance428 AS Infragistics.Win.Appearance NO-UNDO.
      appearance428 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance429 AS Infragistics.Win.Appearance NO-UNDO.
      appearance429 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance430 AS Infragistics.Win.Appearance NO-UNDO.
      appearance430 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance431 AS Infragistics.Win.Appearance NO-UNDO.
      appearance431 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance432 AS Infragistics.Win.Appearance NO-UNDO.
      appearance432 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance433 AS Infragistics.Win.Appearance NO-UNDO.
      appearance433 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance434 AS Infragistics.Win.Appearance NO-UNDO.
      appearance434 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance435 AS Infragistics.Win.Appearance NO-UNDO.
      appearance435 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance436 AS Infragistics.Win.Appearance NO-UNDO.
      appearance436 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance437 AS Infragistics.Win.Appearance NO-UNDO.
      appearance437 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance157 AS Infragistics.Win.Appearance NO-UNDO.
      appearance157 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance158 AS Infragistics.Win.Appearance NO-UNDO.
      appearance158 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance414 AS Infragistics.Win.Appearance NO-UNDO.
      appearance414 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance415 AS Infragistics.Win.Appearance NO-UNDO.
      appearance415 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance416 AS Infragistics.Win.Appearance NO-UNDO.
      appearance416 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance417 AS Infragistics.Win.Appearance NO-UNDO.
      appearance417 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance418 AS Infragistics.Win.Appearance NO-UNDO.
      appearance418 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance419 AS Infragistics.Win.Appearance NO-UNDO.
      appearance419 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance420 AS Infragistics.Win.Appearance NO-UNDO.
      appearance420 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance421 AS Infragistics.Win.Appearance NO-UNDO.
      appearance421 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance422 AS Infragistics.Win.Appearance NO-UNDO.
      appearance422 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance423 AS Infragistics.Win.Appearance NO-UNDO.
      appearance423 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance424 AS Infragistics.Win.Appearance NO-UNDO.
      appearance424 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance425 AS Infragistics.Win.Appearance NO-UNDO.
      appearance425 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance160 AS Infragistics.Win.Appearance NO-UNDO.
      appearance160 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance159 AS Infragistics.Win.Appearance NO-UNDO.
      appearance159 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance489 AS Infragistics.Win.Appearance NO-UNDO.
      appearance489 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance490 AS Infragistics.Win.Appearance NO-UNDO.
      appearance490 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance491 AS Infragistics.Win.Appearance NO-UNDO.
      appearance491 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance492 AS Infragistics.Win.Appearance NO-UNDO.
      appearance492 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance493 AS Infragistics.Win.Appearance NO-UNDO.
      appearance493 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance494 AS Infragistics.Win.Appearance NO-UNDO.
      appearance494 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance495 AS Infragistics.Win.Appearance NO-UNDO.
      appearance495 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance496 AS Infragistics.Win.Appearance NO-UNDO.
      appearance496 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance497 AS Infragistics.Win.Appearance NO-UNDO.
      appearance497 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance498 AS Infragistics.Win.Appearance NO-UNDO.
      appearance498 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance499 AS Infragistics.Win.Appearance NO-UNDO.
      appearance499 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance500 AS Infragistics.Win.Appearance NO-UNDO.
      appearance500 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance501 AS Infragistics.Win.Appearance NO-UNDO.
      appearance501 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance502 AS Infragistics.Win.Appearance NO-UNDO.
      appearance502 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance503 AS Infragistics.Win.Appearance NO-UNDO.
      appearance503 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance504 AS Infragistics.Win.Appearance NO-UNDO.
      appearance504 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance505 AS Infragistics.Win.Appearance NO-UNDO.
      appearance505 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance506 AS Infragistics.Win.Appearance NO-UNDO.
      appearance506 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance507 AS Infragistics.Win.Appearance NO-UNDO.
      appearance507 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance508 AS Infragistics.Win.Appearance NO-UNDO.
      appearance508 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance509 AS Infragistics.Win.Appearance NO-UNDO.
      appearance509 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance510 AS Infragistics.Win.Appearance NO-UNDO.
      appearance510 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance511 AS Infragistics.Win.Appearance NO-UNDO.
      appearance511 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance512 AS Infragistics.Win.Appearance NO-UNDO.
      appearance512 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance513 AS Infragistics.Win.Appearance NO-UNDO.
      appearance513 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance514 AS Infragistics.Win.Appearance NO-UNDO.
      appearance514 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance515 AS Infragistics.Win.Appearance NO-UNDO.
      appearance515 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance516 AS Infragistics.Win.Appearance NO-UNDO.
      appearance516 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance517 AS Infragistics.Win.Appearance NO-UNDO.
      appearance517 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance518 AS Infragistics.Win.Appearance NO-UNDO.
      appearance518 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance519 AS Infragistics.Win.Appearance NO-UNDO.
      appearance519 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance520 AS Infragistics.Win.Appearance NO-UNDO.
      appearance520 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance521 AS Infragistics.Win.Appearance NO-UNDO.
      appearance521 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance522 AS Infragistics.Win.Appearance NO-UNDO.
      appearance522 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance523 AS Infragistics.Win.Appearance NO-UNDO.
      appearance523 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance524 AS Infragistics.Win.Appearance NO-UNDO.
      appearance524 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
      appearance23 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
      appearance24 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
      appearance26 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
      appearance29 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
      appearance30 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
      appearance31 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
      appearance34 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
      appearance35 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
      appearance36 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
      appearance37 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
      appearance38 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
      appearance39 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
      appearance51 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
      appearance53 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
      appearance58 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
      appearance55 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
      appearance56 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
      appearance57 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance78 AS Infragistics.Win.Appearance NO-UNDO.
      appearance78 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance61 AS Infragistics.Win.Appearance NO-UNDO.
      appearance61 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance60 AS Infragistics.Win.Appearance NO-UNDO.
      appearance60 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance59 AS Infragistics.Win.Appearance NO-UNDO.
      appearance59 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance63 AS Infragistics.Win.Appearance NO-UNDO.
      appearance63 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance77 AS Infragistics.Win.Appearance NO-UNDO.
      appearance77 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance64 AS Infragistics.Win.Appearance NO-UNDO.
      appearance64 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance62 AS Infragistics.Win.Appearance NO-UNDO.
      appearance62 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
      appearance40 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
      appearance41 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
      appearance42 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
      appearance43 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
      appearance44 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
      appearance45 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
      appearance46 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
      appearance47 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
      appearance48 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
      appearance49 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance128 AS Infragistics.Win.Appearance NO-UNDO.
      appearance128 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance127 AS Infragistics.Win.Appearance NO-UNDO.
      appearance127 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance130 AS Infragistics.Win.Appearance NO-UNDO.
      appearance130 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance129 AS Infragistics.Win.Appearance NO-UNDO.
      appearance129 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance132 AS Infragistics.Win.Appearance NO-UNDO.
      appearance132 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance131 AS Infragistics.Win.Appearance NO-UNDO.
      appearance131 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance134 AS Infragistics.Win.Appearance NO-UNDO.
      appearance134 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance133 AS Infragistics.Win.Appearance NO-UNDO.
      appearance133 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance136 AS Infragistics.Win.Appearance NO-UNDO.
      appearance136 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance135 AS Infragistics.Win.Appearance NO-UNDO.
      appearance135 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance138 AS Infragistics.Win.Appearance NO-UNDO.
      appearance138 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance137 AS Infragistics.Win.Appearance NO-UNDO.
      appearance137 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance140 AS Infragistics.Win.Appearance NO-UNDO.
      appearance140 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance139 AS Infragistics.Win.Appearance NO-UNDO.
      appearance139 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab20 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab20 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance142 AS Infragistics.Win.Appearance NO-UNDO.
      appearance142 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance141 AS Infragistics.Win.Appearance NO-UNDO.
      appearance141 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance144 AS Infragistics.Win.Appearance NO-UNDO.
      appearance144 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance143 AS Infragistics.Win.Appearance NO-UNDO.
      appearance143 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance146 AS Infragistics.Win.Appearance NO-UNDO.
      appearance146 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance145 AS Infragistics.Win.Appearance NO-UNDO.
      appearance145 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance148 AS Infragistics.Win.Appearance NO-UNDO.
      appearance148 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance147 AS Infragistics.Win.Appearance NO-UNDO.
      appearance147 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
      ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("nyaknappen").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("nyaknappen").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance172 AS Infragistics.Win.Appearance NO-UNDO.
      appearance172 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance173 AS Infragistics.Win.Appearance NO-UNDO.
      appearance173 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabKalkylHuvud = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerHuv = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:comboPriserUppf = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:HuvudKalknr = NEW Controls.FillinInteger().
      THIS-OBJECT:HuvudUtfardare = NEW Controls.Combo().
      THIS-OBJECT:HuvudAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:LabelAnm = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudAnsvarig = NEW Controls.FillinChar().
      THIS-OBJECT:HuvudKatalog = NEW Controls.ComboSimple().
      THIS-OBJECT:LabelOmr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:HuvudOmrade = NEW Controls.Combo().
      THIS-OBJECT:LabelUtf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudTyp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:LabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:GridPersonal = NEW Controls.Grid().
      THIS-OBJECT:TabKalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerArbkoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridArbetsKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridLopKoder = NEW Controls.Grid().
      THIS-OBJECT:KalkylButtonOverAll = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylButtonBackAll = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerFriKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFrekvens = NEW Controls.Grid().
      THIS-OBJECT:KodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:KoderAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:GridKalkylKoder = NEW Controls.Grid().
      THIS-OBJECT:GridFriKalk = NEW Controls.Grid().
      THIS-OBJECT:TabEgna = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerEgnaKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaArbKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaKoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:EgenSkapa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:EgenTaBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridEgnaKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaVald = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaButtonOverAll = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaButtonBackAll = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridValdaUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:VisaPanelEmpty = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:VisaPanelWaiting = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
      THIS-OBJECT:TabFaktorer = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerFaktorerPriser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFaktorer = NEW Controls.Grid().
      THIS-OBJECT:GridPriser = NEW Controls.Grid().
      THIS-OBJECT:TabMateriel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerMtrlValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridMaterial = NEW Controls.Grid().
      THIS-OBJECT:MatButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:MatButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridEgetMaterial = NEW Controls.Grid().
      THIS-OBJECT:ButtonBerMtrl = NEW Controls.Button().
      THIS-OBJECT:TabImportMallar = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ButtonMallar = NEW Controls.Button().
      THIS-OBJECT:ButtonExcel = NEW Controls.Button().
      THIS-OBJECT:ButtonBackMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonOverMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:GridMallarKoder = NEW Controls.Grid().
      THIS-OBJECT:GridValdaMallar = NEW Controls.Grid().
      THIS-OBJECT:GridMallar = NEW Controls.Grid().
      THIS-OBJECT:TabAvtalskalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraAvtal = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:ComboAvtal = NEW Controls.Combo().
      THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:AvtalText = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:TabKopiera = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:buttonKonvKalkyl = NEW Controls.Button().
      THIS-OBJECT:KonvTillTyp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:KopLabelOmr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:KopHuvudOmrade = NEW Controls.Combo().
      THIS-OBJECT:KopLabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:KopHuvudKatalog = NEW Controls.ComboSimple().
      THIS-OBJECT:buttonKopieraKalkyl = NEW Controls.Button().
      THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
      THIS-OBJECT:TabFelmedd = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:GridFelMedd = NEW Controls.Grid().
      THIS-OBJECT:TabVolymber = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:GridVolymBer = NEW Controls.Grid().
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:TabKalkylHuvud:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:SuspendLayout().
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkyl:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:SuspendLayout().
      THIS-OBJECT:splitContainerArbkoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerArbkoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerArbkoder:SuspendLayout().
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:SuspendLayout().
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFriKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFriKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerFriKalk:SuspendLayout().
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KoderAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabEgna:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:SuspendLayout().
      THIS-OBJECT:EgnaKoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:SuspendLayout().
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabVisa:SuspendLayout().
      THIS-OBJECT:VisaPanelWaiting:SuspendLayout().
      THIS-OBJECT:TabFaktorer:SuspendLayout().
      THIS-OBJECT:splitContainerFaktorerPriser:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFaktorerPriser:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerFaktorerPriser:SuspendLayout().
      CAST(THIS-OBJECT:GridFaktorer, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridPriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabMateriel:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:SuspendLayout().
      CAST(THIS-OBJECT:GridMaterial, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridEgetMaterial, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabImportMallar:SuspendLayout().
      CAST(THIS-OBJECT:GridMallarKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridValdaMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabAvtalskalkyl:SuspendLayout().
      CAST(THIS-OBJECT:ComboAvtal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:AvtalText, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKopiera:SuspendLayout().
      CAST(THIS-OBJECT:KopHuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KopHuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabFelmedd:SuspendLayout().
      CAST(THIS-OBJECT:GridFelMedd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabVolymber:SuspendLayout().
      CAST(THIS-OBJECT:GridVolymBer, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabKalkylHuvud */
      /*  */
      THIS-OBJECT:TabKalkylHuvud:Controls:Add(THIS-OBJECT:splitContainerHuv).
      THIS-OBJECT:TabKalkylHuvud:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabKalkylHuvud:Name = "TabKalkylHuvud".
      THIS-OBJECT:TabKalkylHuvud:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* splitContainerHuv */
      /*  */
      THIS-OBJECT:splitContainerHuv:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerHuv:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerHuv:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerHuv:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerHuv:Name = "splitContainerHuv".
      /*  */
      /* splitContainerHuv.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:comboPriserUppf).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudKalknr).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudUtfardare).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudAnmarkning).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelAnm).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudAnsvarig).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudKatalog).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelOmr).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudBenamning).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudOmrade).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelUtf).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudTyp).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelKatalog).
      /*  */
      /* splitContainerHuv.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel2:Controls:Add(THIS-OBJECT:GridPersonal).
      THIS-OBJECT:splitContainerHuv:Size = NEW System.Drawing.Size(1145, 501).
      THIS-OBJECT:splitContainerHuv:SplitterDistance = 383.
      THIS-OBJECT:splitContainerHuv:TabIndex = 43.
      /*  */
      /* comboPriserUppf */
      /*  */
      THIS-OBJECT:comboPriserUppf:Location = NEW System.Drawing.Point(13, 247).
      THIS-OBJECT:comboPriserUppf:Name = "comboPriserUppf".
      THIS-OBJECT:comboPriserUppf:Size = NEW System.Drawing.Size(360, 43).
      THIS-OBJECT:comboPriserUppf:TabIndex = 43.
      /*  */
      /* HuvudKalknr */
      /*  */
      THIS-OBJECT:HuvudKalknr:Location = NEW System.Drawing.Point(13, 6).
      THIS-OBJECT:HuvudKalknr:Name = "HuvudKalknr".
      THIS-OBJECT:HuvudKalknr:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudKalknr:TabIndex = 39.
      /*  */
      /* HuvudUtfardare */
      /*  */
      THIS-OBJECT:HuvudUtfardare:CheckedListSettings:CheckStateMember = "".
      appearance50:BackColor = System.Drawing.SystemColors:Window.
      appearance50:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Appearance = appearance50.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance66:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:Appearance = appearance66.
      appearance67:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BandLabelAppearance = appearance67.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance68:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance68:BackColor2 = System.Drawing.SystemColors:Control.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:PromptAppearance = appearance68.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxRowScrollRegions = 1.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      appearance69:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveCellAppearance = appearance69.
      appearance70:BackColor = System.Drawing.SystemColors:Highlight.
      appearance70:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveRowAppearance = appearance70.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CardAreaAppearance = appearance71.
      appearance72:BorderColor = System.Drawing.Color:Silver.
      appearance72:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellAppearance = appearance72.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellPadding = 0.
      appearance73:BackColor = System.Drawing.SystemColors:Control.
      appearance73:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance73:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:GroupByRowAppearance = appearance73.
      appearance74:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderAppearance = appearance74.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowAppearance = appearance75.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:TemplateAddRowAppearance = appearance76.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudUtfardare:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudUtfardare:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudUtfardare:Location = NEW System.Drawing.Point(97, 171).
      THIS-OBJECT:HuvudUtfardare:Name = "HuvudUtfardare".
      THIS-OBJECT:HuvudUtfardare:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudUtfardare:TabIndex = 41.
      /*  */
      /* HuvudAnmarkning */
      /*  */
      THIS-OBJECT:HuvudAnmarkning:Location = NEW System.Drawing.Point(13, 325).
      THIS-OBJECT:HuvudAnmarkning:Multiline = TRUE.
      THIS-OBJECT:HuvudAnmarkning:Name = "HuvudAnmarkning".
      THIS-OBJECT:HuvudAnmarkning:Size = NEW System.Drawing.Size(361, 108).
      THIS-OBJECT:HuvudAnmarkning:TabIndex = 32.
      THIS-OBJECT:HuvudAnmarkning:Text = " ".
      /*  */
      /* LabelAnm */
      /*  */
      THIS-OBJECT:LabelAnm:Location = NEW System.Drawing.Point(13, 296).
      THIS-OBJECT:LabelAnm:Name = "LabelAnm".
      THIS-OBJECT:LabelAnm:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelAnm:TabIndex = 33.
      THIS-OBJECT:LabelAnm:Text = "Anmärkning".
      /*  */
      /* HuvudAnsvarig */
      /*  */
      THIS-OBJECT:HuvudAnsvarig:Location = NEW System.Drawing.Point(13, 210).
      THIS-OBJECT:HuvudAnsvarig:Name = "HuvudAnsvarig".
      THIS-OBJECT:HuvudAnsvarig:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudAnsvarig:TabIndex = 40.
      /*  */
      /* HuvudKatalog */
      /*  */
      THIS-OBJECT:HuvudKatalog:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudKatalog:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:HuvudKatalog:Location = NEW System.Drawing.Point(97, 104).
      THIS-OBJECT:HuvudKatalog:Name = "HuvudKatalog".
      THIS-OBJECT:HuvudKatalog:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:HuvudKatalog:TabIndex = 42.
      /*  */
      /* LabelOmr */
      /*  */
      appearance65:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelOmr:Appearance = appearance65.
      THIS-OBJECT:LabelOmr:Location = NEW System.Drawing.Point(13, 40).
      THIS-OBJECT:LabelOmr:Name = "LabelOmr".
      THIS-OBJECT:LabelOmr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelOmr:TabIndex = 38.
      /*  */
      /* HuvudBenamning */
      /*  */
      THIS-OBJECT:HuvudBenamning:Location = NEW System.Drawing.Point(13, 137).
      THIS-OBJECT:HuvudBenamning:Name = "HuvudBenamning".
      THIS-OBJECT:HuvudBenamning:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudBenamning:TabIndex = 35.
      THIS-OBJECT:HuvudBenamning:Tag = "".
      /*  */
      /* HuvudOmrade */
      /*  */
      THIS-OBJECT:HuvudOmrade:CheckedListSettings:CheckStateMember = "".
      appearance115:BackColor = System.Drawing.SystemColors:Window.
      appearance115:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Appearance = appearance115.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance116:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance116:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance116:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance116:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance116.
      appearance117:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance117.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance118:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance118:BackColor2 = System.Drawing.SystemColors:Control.
      appearance118:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance118:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance118.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance119:BackColor = System.Drawing.SystemColors:Window.
      appearance119:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance119.
      appearance120:BackColor = System.Drawing.SystemColors:Highlight.
      appearance120:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance120.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance121:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance121.
      appearance122:BorderColor = System.Drawing.Color:Silver.
      appearance122:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellAppearance = appearance122.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance123:BackColor = System.Drawing.SystemColors:Control.
      appearance123:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance123:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance123:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance123:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance123.
      appearance124:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance124.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance125:BackColor = System.Drawing.SystemColors:Window.
      appearance125:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowAppearance = appearance125.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance126:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance126.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudOmrade:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudOmrade:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudOmrade:Location = NEW System.Drawing.Point(97, 40).
      THIS-OBJECT:HuvudOmrade:Name = "HuvudOmrade".
      THIS-OBJECT:HuvudOmrade:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudOmrade:TabIndex = 37.
      /*  */
      /* LabelUtf */
      /*  */
      THIS-OBJECT:LabelUtf:ImageTransparentColor = System.Drawing.Color:Red.
      THIS-OBJECT:LabelUtf:Location = NEW System.Drawing.Point(13, 171).
      THIS-OBJECT:LabelUtf:Name = "LabelUtf".
      THIS-OBJECT:LabelUtf:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:LabelUtf:TabIndex = 30.
      THIS-OBJECT:LabelUtf:Text = "Utfärdare".
      /*  */
      /* HuvudTyp */
      /*  */
      THIS-OBJECT:HuvudTyp:Location = NEW System.Drawing.Point(13, 70).
      THIS-OBJECT:HuvudTyp:Name = "HuvudTyp".
      THIS-OBJECT:HuvudTyp:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudTyp:TabIndex = 36.
      THIS-OBJECT:HuvudTyp:GuruValueChanged:Subscribe(THIS-OBJECT:HuvudTyp_GuruValueChanged).
      /*  */
      /* LabelKatalog */
      /*  */
      appearance54:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelKatalog:Appearance = appearance54.
      THIS-OBJECT:LabelKatalog:Location = NEW System.Drawing.Point(13, 108).
      THIS-OBJECT:LabelKatalog:Name = "LabelKatalog".
      THIS-OBJECT:LabelKatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelKatalog:TabIndex = 31.
      THIS-OBJECT:LabelKatalog:Text = "Katalog".
      /*  */
      /* GridPersonal */
      /*  */
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPersonal:DisplayLayout:Appearance = appearance79.
      THIS-OBJECT:GridPersonal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPersonal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance80:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance80:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance80:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:Appearance = appearance80.
      appearance81:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance81.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance82:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance82:BackColor2 = System.Drawing.SystemColors:Control.
      appearance82:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance82:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:PromptAppearance = appearance82.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      appearance83:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveCellAppearance = appearance83.
      appearance84:BackColor = System.Drawing.SystemColors:Highlight.
      appearance84:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveRowAppearance = appearance84.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CardAreaAppearance = appearance85.
      appearance86:BorderColor = System.Drawing.Color:Silver.
      appearance86:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellAppearance = appearance86.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellPadding = 0.
      appearance87:BackColor = System.Drawing.SystemColors:Control.
      appearance87:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance87:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance87:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance87:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:GroupByRowAppearance = appearance87.
      appearance88:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderAppearance = appearance88.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowAppearance = appearance89.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:TemplateAddRowAppearance = appearance90.
      THIS-OBJECT:GridPersonal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridPersonal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridPersonal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridPersonal:GuruContext = ?.
      THIS-OBJECT:GridPersonal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridPersonal:Name = "GridPersonal".
      THIS-OBJECT:GridPersonal:Size = NEW System.Drawing.Size(400, 577).
      THIS-OBJECT:GridPersonal:TabIndex = 34.
      THIS-OBJECT:GridPersonal:Text = "grid1".
      /*  */
      /* TabKalkyl */
      /*  */
      THIS-OBJECT:TabKalkyl:Controls:Add(THIS-OBJECT:splitContainerKalk).
      THIS-OBJECT:TabKalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKalkyl:Name = "TabKalkyl".
      THIS-OBJECT:TabKalkyl:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* splitContainerKalk */
      /*  */
      THIS-OBJECT:splitContainerKalk:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalk:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalk:Name = "splitContainerKalk".
      /*  */
      /* splitContainerKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel1:Controls:Add(THIS-OBJECT:splitContainerArbkoder).
      /*  */
      /* splitContainerKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel2:Controls:Add(THIS-OBJECT:splitContainerFriKalk).
      THIS-OBJECT:splitContainerKalk:Size = NEW System.Drawing.Size(1145, 501).
      THIS-OBJECT:splitContainerKalk:SplitterDistance = 500.
      THIS-OBJECT:splitContainerKalk:TabIndex = 11.
      /*  */
      /* splitContainerArbkoder */
      /*  */
      THIS-OBJECT:splitContainerArbkoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerArbkoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerArbkoder:Name = "splitContainerArbkoder".
      THIS-OBJECT:splitContainerArbkoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerArbkoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerArbkoder:Panel1:Controls:Add(THIS-OBJECT:GridArbetsKoder).
      /*  */
      /* splitContainerArbkoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerArbkoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopnr).
      THIS-OBJECT:splitContainerArbkoder:Size = NEW System.Drawing.Size(498, 499).
      THIS-OBJECT:splitContainerArbkoder:SplitterDistance = 228.
      THIS-OBJECT:splitContainerArbkoder:TabIndex = 0.
      /*  */
      /* GridArbetsKoder */
      /*  */
      appearance330:BackColor = System.Drawing.SystemColors:Window.
      appearance330:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Appearance = appearance330.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance331:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance331:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance331:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance331:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:Appearance = appearance331.
      appearance332:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance332.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance333:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance333:BackColor2 = System.Drawing.SystemColors:Control.
      appearance333:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance333:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance333.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance334:BackColor = System.Drawing.SystemColors:Window.
      appearance334:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveCellAppearance = appearance334.
      appearance335:BackColor = System.Drawing.SystemColors:Highlight.
      appearance335:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveRowAppearance = appearance335.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance336:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CardAreaAppearance = appearance336.
      appearance337:BorderColor = System.Drawing.Color:Silver.
      appearance337:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellAppearance = appearance337.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellPadding = 0.
      appearance338:BackColor = System.Drawing.SystemColors:Control.
      appearance338:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance338:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance338:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance338:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:GroupByRowAppearance = appearance338.
      appearance339:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderAppearance = appearance339.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance340:BackColor = System.Drawing.SystemColors:Window.
      appearance340:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowAppearance = appearance340.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance341:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance341.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbetsKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbetsKoder:GuruContext = ?.
      THIS-OBJECT:GridArbetsKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbetsKoder:Name = "GridArbetsKoder".
      THIS-OBJECT:GridArbetsKoder:Size = NEW System.Drawing.Size(498, 228).
      THIS-OBJECT:GridArbetsKoder:TabIndex = 4.
      THIS-OBJECT:GridArbetsKoder:Text = "grid1".
      /*  */
      /* splitContainerLopnr */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnr:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnr:Name = "splitContainerLopnr".
      /*  */
      /* splitContainerLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel1:Controls:Add(THIS-OBJECT:GridLopKoder).
      /*  */
      /* splitContainerLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonOverAll).
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonBackAll).
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonOverMark).
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonBackMark).
      THIS-OBJECT:splitContainerLopnr:Size = NEW System.Drawing.Size(498, 267).
      THIS-OBJECT:splitContainerLopnr:SplitterDistance = 463.
      THIS-OBJECT:splitContainerLopnr:TabIndex = 0.
      /*  */
      /* GridLopKoder */
      /*  */
      THIS-OBJECT:GridLopKoder:AllowDrop = TRUE.
      appearance318:BackColor = System.Drawing.SystemColors:Window.
      appearance318:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Appearance = appearance318.
      THIS-OBJECT:GridLopKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance319:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance319:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance319:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance319:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:Appearance = appearance319.
      appearance320:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance320.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance321:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance321:BackColor2 = System.Drawing.SystemColors:Control.
      appearance321:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance321:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance321.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance322:BackColor = System.Drawing.SystemColors:Window.
      appearance322:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveCellAppearance = appearance322.
      appearance323:BackColor = System.Drawing.SystemColors:Highlight.
      appearance323:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveRowAppearance = appearance323.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance324:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CardAreaAppearance = appearance324.
      appearance325:BorderColor = System.Drawing.Color:Silver.
      appearance325:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellAppearance = appearance325.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellPadding = 0.
      appearance326:BackColor = System.Drawing.SystemColors:Control.
      appearance326:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance326:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance326:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance326:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:GroupByRowAppearance = appearance326.
      appearance327:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderAppearance = appearance327.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance328:BackColor = System.Drawing.SystemColors:Window.
      appearance328:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowAppearance = appearance328.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance329:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance329.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopKoder:GuruContext = ?.
      THIS-OBJECT:GridLopKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopKoder:Name = "GridLopKoder".
      THIS-OBJECT:GridLopKoder:Size = NEW System.Drawing.Size(463, 267).
      THIS-OBJECT:GridLopKoder:TabIndex = 5.
      THIS-OBJECT:GridLopKoder:Text = "grid2".
      THIS-OBJECT:GridLopKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridLopKoder_GuruAfterDropped).
      /*  */
      /* KalkylButtonOverAll */
      /*  */
      appearance149:Image = CAST(resources:GetObject("appearance149.Image"), System.Object).
      THIS-OBJECT:KalkylButtonOverAll:Appearance = appearance149.
      THIS-OBJECT:KalkylButtonOverAll:Location = NEW System.Drawing.Point(3, 3).
      THIS-OBJECT:KalkylButtonOverAll:Name = "KalkylButtonOverAll".
      THIS-OBJECT:KalkylButtonOverAll:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylButtonOverAll:TabIndex = 8.
      THIS-OBJECT:KalkylButtonOverAll:Text = "Alla väljs".
      THIS-OBJECT:KalkylButtonOverAll:Click:Subscribe(THIS-OBJECT:ButtonOverAlla_Click).
      /*  */
      /* KalkylButtonBackAll */
      /*  */
      appearance152:Image = CAST(resources:GetObject("appearance152.Image"), System.Object).
      THIS-OBJECT:KalkylButtonBackAll:Appearance = appearance152.
      THIS-OBJECT:KalkylButtonBackAll:Location = NEW System.Drawing.Point(2, 96).
      THIS-OBJECT:KalkylButtonBackAll:Name = "KalkylButtonBackAll".
      THIS-OBJECT:KalkylButtonBackAll:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylButtonBackAll:TabIndex = 10.
      THIS-OBJECT:KalkylButtonBackAll:Text = "Alla bort".
      THIS-OBJECT:KalkylButtonBackAll:Click:Subscribe(THIS-OBJECT:ButtonBackAlla_Click).
      /*  */
      /* KalkylButtonOverMark */
      /*  */
      appearance150:Image = CAST(resources:GetObject("appearance150.Image"), System.Object).
      THIS-OBJECT:KalkylButtonOverMark:Appearance = appearance150.
      THIS-OBJECT:KalkylButtonOverMark:Location = NEW System.Drawing.Point(2, 34).
      THIS-OBJECT:KalkylButtonOverMark:Name = "KalkylButtonOverMark".
      THIS-OBJECT:KalkylButtonOverMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylButtonOverMark:TabIndex = 7.
      THIS-OBJECT:KalkylButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:KalkylButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOvermark_ClickE).
      /*  */
      /* KalkylButtonBackMark */
      /*  */
      appearance151:Image = CAST(resources:GetObject("appearance151.Image"), System.Object).
      THIS-OBJECT:KalkylButtonBackMark:Appearance = appearance151.
      THIS-OBJECT:KalkylButtonBackMark:Location = NEW System.Drawing.Point(2, 65).
      THIS-OBJECT:KalkylButtonBackMark:Name = "KalkylButtonBackMark".
      THIS-OBJECT:KalkylButtonBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylButtonBackMark:TabIndex = 9.
      THIS-OBJECT:KalkylButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:KalkylButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMark_Click).
      /*  */
      /* splitContainerFriKalk */
      /*  */
      THIS-OBJECT:splitContainerFriKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFriKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFriKalk:Name = "splitContainerFriKalk".
      /*  */
      /* splitContainerFriKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFriKalk:Panel1:Controls:Add(THIS-OBJECT:GridFrekvens).
      THIS-OBJECT:splitContainerFriKalk:Panel1:Controls:Add(THIS-OBJECT:KodKommentar).
      THIS-OBJECT:splitContainerFriKalk:Panel1:Controls:Add(THIS-OBJECT:KoderAnmarkning).
      THIS-OBJECT:splitContainerFriKalk:Panel1:Controls:Add(THIS-OBJECT:GridKalkylKoder).
      /*  */
      /* splitContainerFriKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFriKalk:Panel2:Controls:Add(THIS-OBJECT:GridFriKalk).
      THIS-OBJECT:splitContainerFriKalk:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerFriKalk:Size = NEW System.Drawing.Size(639, 499).
      THIS-OBJECT:splitContainerFriKalk:SplitterDistance = 400.
      THIS-OBJECT:splitContainerFriKalk:TabIndex = 4.
      /*  */
      /* GridFrekvens */
      /*  */
      appearance106:BackColor = System.Drawing.SystemColors:Window.
      appearance106:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Appearance = appearance106.
      THIS-OBJECT:GridFrekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFrekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance103:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance103:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance103:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance103:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:Appearance = appearance103.
      appearance104:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance104.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance105:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance105:BackColor2 = System.Drawing.SystemColors:Control.
      appearance105:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance105:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance105.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance114:BackColor = System.Drawing.SystemColors:Window.
      appearance114:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveCellAppearance = appearance114.
      appearance109:BackColor = System.Drawing.SystemColors:Highlight.
      appearance109:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveRowAppearance = appearance109.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance108:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CardAreaAppearance = appearance108.
      appearance107:BorderColor = System.Drawing.Color:Silver.
      appearance107:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellAppearance = appearance107.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellPadding = 0.
      appearance111:BackColor = System.Drawing.SystemColors:Control.
      appearance111:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance111:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance111:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance111:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:GroupByRowAppearance = appearance111.
      appearance113:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderAppearance = appearance113.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance112:BackColor = System.Drawing.SystemColors:Window.
      appearance112:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowAppearance = appearance112.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance110:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance110.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFrekvens:Dock = System.Windows.Forms.DockStyle:Left.
      THIS-OBJECT:GridFrekvens:GuruContext = ?.
      THIS-OBJECT:GridFrekvens:Location = NEW System.Drawing.Point(1472, 0).
      THIS-OBJECT:GridFrekvens:Name = "GridFrekvens".
      THIS-OBJECT:GridFrekvens:Size = NEW System.Drawing.Size(862, 499).
      THIS-OBJECT:GridFrekvens:TabIndex = 35.
      THIS-OBJECT:GridFrekvens:Text = "grid1".
      /*  */
      /* KodKommentar */
      /*  */
      THIS-OBJECT:KodKommentar:Dock = System.Windows.Forms.DockStyle:Left.
      THIS-OBJECT:KodKommentar:Location = NEW System.Drawing.Point(1111, 0).
      THIS-OBJECT:KodKommentar:Multiline = TRUE.
      THIS-OBJECT:KodKommentar:Name = "KodKommentar".
      THIS-OBJECT:KodKommentar:ReadOnly = TRUE.
      THIS-OBJECT:KodKommentar:Size = NEW System.Drawing.Size(361, 499).
      THIS-OBJECT:KodKommentar:TabIndex = 34.
      THIS-OBJECT:KodKommentar:Text = " ".
      /*  */
      /* KoderAnmarkning */
      /*  */
      THIS-OBJECT:KoderAnmarkning:Dock = System.Windows.Forms.DockStyle:Left.
      THIS-OBJECT:KoderAnmarkning:Location = NEW System.Drawing.Point(750, 0).
      THIS-OBJECT:KoderAnmarkning:Multiline = TRUE.
      THIS-OBJECT:KoderAnmarkning:Name = "KoderAnmarkning".
      THIS-OBJECT:KoderAnmarkning:Size = NEW System.Drawing.Size(361, 499).
      THIS-OBJECT:KoderAnmarkning:TabIndex = 33.
      THIS-OBJECT:KoderAnmarkning:Text = " ".
      THIS-OBJECT:KoderAnmarkning:AfterExitEditMode:Subscribe(THIS-OBJECT:KoderAnmarkning_AfterExitEditMode).
      /*  */
      /* GridKalkylKoder */
      /*  */
      THIS-OBJECT:GridKalkylKoder:AllowDrop = TRUE.
      appearance342:BackColor = System.Drawing.SystemColors:Window.
      appearance342:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Appearance = appearance342.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance343:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance343:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance343:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance343:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:Appearance = appearance343.
      appearance344:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance344.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance345:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance345:BackColor2 = System.Drawing.SystemColors:Control.
      appearance345:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance345:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance345.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance346:BackColor = System.Drawing.SystemColors:Window.
      appearance346:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveCellAppearance = appearance346.
      appearance347:BackColor = System.Drawing.SystemColors:Highlight.
      appearance347:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveRowAppearance = appearance347.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance348:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CardAreaAppearance = appearance348.
      appearance349:BorderColor = System.Drawing.Color:Silver.
      appearance349:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellAppearance = appearance349.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellPadding = 0.
      appearance350:BackColor = System.Drawing.SystemColors:Control.
      appearance350:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance350:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance350:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance350:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:GroupByRowAppearance = appearance350.
      appearance351:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderAppearance = appearance351.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance352:BackColor = System.Drawing.SystemColors:Window.
      appearance352:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowAppearance = appearance352.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance353:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance353.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKalkylKoder:Dock = System.Windows.Forms.DockStyle:Left.
      THIS-OBJECT:GridKalkylKoder:GuruContext = ?.
      THIS-OBJECT:GridKalkylKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridKalkylKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridKalkylKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKalkylKoder:Name = "GridKalkylKoder".
      THIS-OBJECT:GridKalkylKoder:Size = NEW System.Drawing.Size(750, 499).
      THIS-OBJECT:GridKalkylKoder:TabIndex = 3.
      THIS-OBJECT:GridKalkylKoder:Text = "grid3".
      THIS-OBJECT:GridKalkylKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterCellUpdate).
      THIS-OBJECT:GridKalkylKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridKalkylKoder_GuruAfterDropped).
      THIS-OBJECT:GridKalkylKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterSelectChange).
      /*  */
      /* GridFriKalk */
      /*  */
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      appearance8:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Appearance = appearance8.
      THIS-OBJECT:GridFriKalk:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFriKalk:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance5:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:Appearance = appearance5.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance7:BackColor2 = System.Drawing.SystemColors:Control.
      appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance11:BackColor = System.Drawing.SystemColors:Highlight.
      appearance11:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveRowAppearance = appearance11.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance10:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CardAreaAppearance = appearance10.
      appearance9:BorderColor = System.Drawing.Color:Silver.
      appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellAppearance = appearance9.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellPadding = 0.
      appearance13:BackColor = System.Drawing.SystemColors:Control.
      appearance13:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance13:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance13:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance13:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:GroupByRowAppearance = appearance13.
      appearance15:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderAppearance = appearance15.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowAppearance = appearance14.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFriKalk:Dock = System.Windows.Forms.DockStyle:Right.
      THIS-OBJECT:GridFriKalk:GuruContext = ?.
      THIS-OBJECT:GridFriKalk:Location = NEW System.Drawing.Point(-604, 0).
      THIS-OBJECT:GridFriKalk:Name = "GridFriKalk".
      THIS-OBJECT:GridFriKalk:Size = NEW System.Drawing.Size(700, 100).
      THIS-OBJECT:GridFriKalk:TabIndex = 0.
      THIS-OBJECT:GridFriKalk:Text = "grid1".
      THIS-OBJECT:GridFriKalk:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFriKalk_AfterCellUpdate).
      /*  */
      /* TabEgna */
      /*  */
      THIS-OBJECT:TabEgna:Controls:Add(THIS-OBJECT:splitContainerEgnaKoder).
      THIS-OBJECT:TabEgna:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabEgna:Name = "TabEgna".
      THIS-OBJECT:TabEgna:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* splitContainerEgnaKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaKoder:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaKoder:Name = "splitContainerEgnaKoder".
      THIS-OBJECT:splitContainerEgnaKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaArbKoder).
      /*  */
      /* splitContainerEgnaKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaValj).
      THIS-OBJECT:splitContainerEgnaKoder:Size = NEW System.Drawing.Size(1145, 501).
      THIS-OBJECT:splitContainerEgnaKoder:SplitterDistance = 148.
      THIS-OBJECT:splitContainerEgnaKoder:TabIndex = 18.
      /*  */
      /* splitContainerEgnaArbKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaArbKoder:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerEgnaArbKoder:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaArbKoder:Name = "splitContainerEgnaArbKoder".
      THIS-OBJECT:splitContainerEgnaArbKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaArbKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:Controls:Add(THIS-OBJECT:EgnaKoderStrip).
      /*  */
      /* splitContainerEgnaArbKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:Controls:Add(THIS-OBJECT:GridEgnaKoder).
      THIS-OBJECT:splitContainerEgnaArbKoder:Size = NEW System.Drawing.Size(1145, 148).
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterDistance = 25.
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerEgnaArbKoder:TabIndex = 14.
      /*  */
      /* EgnaKoderStrip */
      /*  */
      THIS-OBJECT:EgnaKoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:EgenSkapa.
      arrayvar0[2] = THIS-OBJECT:EgenTaBort.
      THIS-OBJECT:EgnaKoderStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:EgnaKoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:EgnaKoderStrip:Name = "EgnaKoderStrip".
      THIS-OBJECT:EgnaKoderStrip:Size = NEW System.Drawing.Size(1145, 25).
      THIS-OBJECT:EgnaKoderStrip:TabIndex = 13.
      THIS-OBJECT:EgnaKoderStrip:Text = "toolStrip1".
      /*  */
      /* EgenSkapa */
      /*  */
      THIS-OBJECT:EgenSkapa:Image = CAST(resources:GetObject("EgenSkapa.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenSkapa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenSkapa:Name = "EgenSkapa".
      THIS-OBJECT:EgenSkapa:Size = NEW System.Drawing.Size(98, 22).
      THIS-OBJECT:EgenSkapa:Text = "Lägg till egen".
      THIS-OBJECT:EgenSkapa:Click:Subscribe(THIS-OBJECT:SkapaNyEgen_Click).
      /*  */
      /* EgenTaBort */
      /*  */
      THIS-OBJECT:EgenTaBort:Image = CAST(resources:GetObject("EgenTaBort.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenTaBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenTaBort:Name = "EgenTaBort".
      THIS-OBJECT:EgenTaBort:Size = NEW System.Drawing.Size(118, 22).
      THIS-OBJECT:EgenTaBort:Text = "Ta bort markerad".
      THIS-OBJECT:EgenTaBort:Click:Subscribe(THIS-OBJECT:TaBortEgen_Click).
      /*  */
      /* GridEgnaKoder */
      /*  */
      appearance354:BackColor = System.Drawing.SystemColors:Window.
      appearance354:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Appearance = appearance354.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance355:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance355:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance355:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance355:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:Appearance = appearance355.
      appearance356:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance356.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance357:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance357:BackColor2 = System.Drawing.SystemColors:Control.
      appearance357:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance357:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance357.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance358:BackColor = System.Drawing.SystemColors:Window.
      appearance358:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveCellAppearance = appearance358.
      appearance359:BackColor = System.Drawing.SystemColors:Highlight.
      appearance359:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveRowAppearance = appearance359.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance360:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CardAreaAppearance = appearance360.
      appearance361:BorderColor = System.Drawing.Color:Silver.
      appearance361:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellAppearance = appearance361.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellPadding = 0.
      appearance362:BackColor = System.Drawing.SystemColors:Control.
      appearance362:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance362:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance362:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance362:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:GroupByRowAppearance = appearance362.
      appearance363:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderAppearance = appearance363.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance364:BackColor = System.Drawing.SystemColors:Window.
      appearance364:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowAppearance = appearance364.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance365:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance365.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgnaKoder:GuruContext = ?.
      THIS-OBJECT:GridEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgnaKoder:Name = "GridEgnaKoder".
      THIS-OBJECT:GridEgnaKoder:Size = NEW System.Drawing.Size(1145, 122).
      THIS-OBJECT:GridEgnaKoder:TabIndex = 12.
      THIS-OBJECT:GridEgnaKoder:Text = "grid1".
      THIS-OBJECT:GridEgnaKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellUpdate).
      THIS-OBJECT:GridEgnaKoder:AfterCellActivate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellActivate).
      THIS-OBJECT:GridEgnaKoder:AfterRowActivate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterRowActivate).
      THIS-OBJECT:GridEgnaKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterSelectChange).
      /*  */
      /* splitContainerEgnaValj */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerEgnaValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaValj:Name = "splitContainerEgnaValj".
      /*  */
      /* splitContainerEgnaValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel1:Controls:Add(THIS-OBJECT:GridUnderKoder).
      /*  */
      /* splitContainerEgnaValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaVald).
      THIS-OBJECT:splitContainerEgnaValj:Size = NEW System.Drawing.Size(1145, 349).
      THIS-OBJECT:splitContainerEgnaValj:SplitterDistance = 381.
      THIS-OBJECT:splitContainerEgnaValj:TabIndex = 0.
      /*  */
      /* GridUnderKoder */
      /*  */
      THIS-OBJECT:GridUnderKoder:AllowDrop = TRUE.
      appearance378:BackColor = System.Drawing.SystemColors:Window.
      appearance378:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Appearance = appearance378.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance379:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance379:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance379:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance379:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance379.
      appearance380:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance380.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance381:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance381:BackColor2 = System.Drawing.SystemColors:Control.
      appearance381:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance381:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance381.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance382:BackColor = System.Drawing.SystemColors:Window.
      appearance382:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance382.
      appearance383:BackColor = System.Drawing.SystemColors:Highlight.
      appearance383:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance383.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance384:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance384.
      appearance385:BorderColor = System.Drawing.Color:Silver.
      appearance385:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellAppearance = appearance385.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance386:BackColor = System.Drawing.SystemColors:Control.
      appearance386:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance386:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance386:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance386:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance386.
      appearance387:TextHAlignAsString = "Left".
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance387.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance388:BackColor = System.Drawing.SystemColors:Window.
      appearance388:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowAppearance = appearance388.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance389:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance389.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridUnderKoder:Name = "GridUnderKoder".
      THIS-OBJECT:GridUnderKoder:Size = NEW System.Drawing.Size(379, 347).
      THIS-OBJECT:GridUnderKoder:TabIndex = 10.
      THIS-OBJECT:GridUnderKoder:Text = "grid1".
      THIS-OBJECT:GridUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridUnderKoder_GuruAfterDropped).
      /*  */
      /* splitContainerEgnaVald */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaVald:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerEgnaVald:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaVald:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaVald:Name = "splitContainerEgnaVald".
      /*  */
      /* splitContainerEgnaVald.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonOverAll).
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonBackAll).
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonOverMark).
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonBackMark).
      /*  */
      /* splitContainerEgnaVald.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel2:Controls:Add(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:splitContainerEgnaVald:Size = NEW System.Drawing.Size(758, 347).
      THIS-OBJECT:splitContainerEgnaVald:SplitterDistance = 30.
      THIS-OBJECT:splitContainerEgnaVald:TabIndex = 0.
      /*  */
      /* EgnaButtonOverAll */
      /*  */
      appearance153:Image = CAST(resources:GetObject("appearance153.Image"), System.Object).
      THIS-OBJECT:EgnaButtonOverAll:Appearance = appearance153.
      THIS-OBJECT:EgnaButtonOverAll:Location = NEW System.Drawing.Point(4, 3).
      THIS-OBJECT:EgnaButtonOverAll:Name = "EgnaButtonOverAll".
      THIS-OBJECT:EgnaButtonOverAll:Size = NEW System.Drawing.Size(24, 25).
      THIS-OBJECT:EgnaButtonOverAll:TabIndex = 14.
      THIS-OBJECT:EgnaButtonOverAll:Text = "Alla väljs".
      THIS-OBJECT:EgnaButtonOverAll:Click:Subscribe(THIS-OBJECT:ButtonOverAllaPriser_Click).
      /*  */
      /* EgnaButtonBackAll */
      /*  */
      appearance156:Image = CAST(resources:GetObject("appearance156.Image"), System.Object).
      THIS-OBJECT:EgnaButtonBackAll:Appearance = appearance156.
      THIS-OBJECT:EgnaButtonBackAll:Location = NEW System.Drawing.Point(4, 94).
      THIS-OBJECT:EgnaButtonBackAll:Name = "EgnaButtonBackAll".
      THIS-OBJECT:EgnaButtonBackAll:Size = NEW System.Drawing.Size(23, 24).
      THIS-OBJECT:EgnaButtonBackAll:TabIndex = 16.
      THIS-OBJECT:EgnaButtonBackAll:Text = "Alla bort".
      THIS-OBJECT:EgnaButtonBackAll:Click:Subscribe(THIS-OBJECT:ButtonBackAllaPriser_Click).
      /*  */
      /* EgnaButtonOverMark */
      /*  */
      appearance154:Image = CAST(resources:GetObject("appearance154.Image"), System.Object).
      THIS-OBJECT:EgnaButtonOverMark:Appearance = appearance154.
      THIS-OBJECT:EgnaButtonOverMark:Location = NEW System.Drawing.Point(4, 34).
      THIS-OBJECT:EgnaButtonOverMark:Name = "EgnaButtonOverMark".
      THIS-OBJECT:EgnaButtonOverMark:Size = NEW System.Drawing.Size(23, 25).
      THIS-OBJECT:EgnaButtonOverMark:TabIndex = 13.
      THIS-OBJECT:EgnaButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:EgnaButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOverMarkPriser_Click).
      /*  */
      /* EgnaButtonBackMark */
      /*  */
      appearance155:Image = CAST(resources:GetObject("appearance155.Image"), System.Object).
      THIS-OBJECT:EgnaButtonBackMark:Appearance = appearance155.
      THIS-OBJECT:EgnaButtonBackMark:Location = NEW System.Drawing.Point(4, 65).
      THIS-OBJECT:EgnaButtonBackMark:Name = "EgnaButtonBackMark".
      THIS-OBJECT:EgnaButtonBackMark:Size = NEW System.Drawing.Size(23, 23).
      THIS-OBJECT:EgnaButtonBackMark:TabIndex = 15.
      THIS-OBJECT:EgnaButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:EgnaButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMarkPriser_Click).
      /*  */
      /* GridValdaUnderKoder */
      /*  */
      THIS-OBJECT:GridValdaUnderKoder:AllowDrop = TRUE.
      appearance366:BackColor = System.Drawing.SystemColors:Window.
      appearance366:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Appearance = appearance366.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance367:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance367:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance367:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance367:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance367.
      appearance368:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance368.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance369:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance369:BackColor2 = System.Drawing.SystemColors:Control.
      appearance369:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance369:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance369.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance370:BackColor = System.Drawing.SystemColors:Window.
      appearance370:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance370.
      appearance371:BackColor = System.Drawing.SystemColors:Highlight.
      appearance371:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance371.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance372:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance372.
      appearance373:BorderColor = System.Drawing.Color:Silver.
      appearance373:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellAppearance = appearance373.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance374:BackColor = System.Drawing.SystemColors:Control.
      appearance374:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance374:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance374:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance374:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance374.
      appearance375:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance375.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance376:BackColor = System.Drawing.SystemColors:Window.
      appearance376:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowAppearance = appearance376.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance377:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance377.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridValdaUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaUnderKoder:Name = "GridValdaUnderKoder".
      THIS-OBJECT:GridValdaUnderKoder:Size = NEW System.Drawing.Size(724, 347).
      THIS-OBJECT:GridValdaUnderKoder:TabIndex = 11.
      THIS-OBJECT:GridValdaUnderKoder:Text = "grid1".
      THIS-OBJECT:GridValdaUnderKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridValdaUnderKoder_AfterCellUpdate).
      THIS-OBJECT:GridValdaUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaUnderKoder_GuruAfterDropped).
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:VisaExcel).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:VisaPanelEmpty).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:VisaPanelWaiting).
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(358, 44).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(522, 330).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      /*  */
      /* VisaPanelEmpty */
      /*  */
      THIS-OBJECT:VisaPanelEmpty:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:VisaPanelEmpty:Location = NEW System.Drawing.Point(40, 293).
      THIS-OBJECT:VisaPanelEmpty:Name = "VisaPanelEmpty".
      THIS-OBJECT:VisaPanelEmpty:Size = NEW System.Drawing.Size(279, 221).
      THIS-OBJECT:VisaPanelEmpty:TabIndex = 2.
      /*  */
      /* VisaPanelWaiting */
      /*  */
      THIS-OBJECT:VisaPanelWaiting:BackColor = System.Drawing.SystemColors:Info.
      THIS-OBJECT:VisaPanelWaiting:Controls:Add(THIS-OBJECT:label1).
      THIS-OBJECT:VisaPanelWaiting:Location = NEW System.Drawing.Point(30, 110).
      THIS-OBJECT:VisaPanelWaiting:Name = "VisaPanelWaiting".
      THIS-OBJECT:VisaPanelWaiting:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:VisaPanelWaiting:TabIndex = 1.
      /*  */
      /* label1 */
      /*  */
      THIS-OBJECT:label1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:label1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 8.25, System.Drawing.FontStyle:Italic, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:label1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:label1:Name = "label1".
      THIS-OBJECT:label1:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:label1:TabIndex = 0.
      THIS-OBJECT:label1:Text = "Genererar kalkyl, var god vänta..".
      THIS-OBJECT:label1:TextAlign = System.Drawing.ContentAlignment:MiddleCenter.
      THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
      /*  */
      /* TabFaktorer */
      /*  */
      THIS-OBJECT:TabFaktorer:Controls:Add(THIS-OBJECT:splitContainerFaktorerPriser).
      THIS-OBJECT:TabFaktorer:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFaktorer:Name = "TabFaktorer".
      THIS-OBJECT:TabFaktorer:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* splitContainerFaktorerPriser */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFaktorerPriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFaktorerPriser:Name = "splitContainerFaktorerPriser".
      /*  */
      /* splitContainerFaktorerPriser.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Panel1:Controls:Add(THIS-OBJECT:GridFaktorer).
      /*  */
      /* splitContainerFaktorerPriser.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Panel2:Controls:Add(THIS-OBJECT:GridPriser).
      THIS-OBJECT:splitContainerFaktorerPriser:Size = NEW System.Drawing.Size(1145, 501).
      THIS-OBJECT:splitContainerFaktorerPriser:SplitterDistance = 597.
      THIS-OBJECT:splitContainerFaktorerPriser:TabIndex = 2.
      /*  */
      /* GridFaktorer */
      /*  */
      appearance91:BackColor = System.Drawing.SystemColors:Window.
      appearance91:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Appearance = appearance91.
      THIS-OBJECT:GridFaktorer:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFaktorer:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance92:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance92:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance92:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance92:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:Appearance = appearance92.
      appearance93:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:BandLabelAppearance = appearance93.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance94:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance94:BackColor2 = System.Drawing.SystemColors:Control.
      appearance94:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance94:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:PromptAppearance = appearance94.
      THIS-OBJECT:GridFaktorer:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance95:BackColor = System.Drawing.SystemColors:Window.
      appearance95:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveCellAppearance = appearance95.
      appearance96:BackColor = System.Drawing.SystemColors:Highlight.
      appearance96:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveRowAppearance = appearance96.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance97:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CardAreaAppearance = appearance97.
      appearance98:BorderColor = System.Drawing.Color:Silver.
      appearance98:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellAppearance = appearance98.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellPadding = 0.
      appearance99:BackColor = System.Drawing.SystemColors:Control.
      appearance99:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance99:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance99:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance99:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:GroupByRowAppearance = appearance99.
      appearance100:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderAppearance = appearance100.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance101:BackColor = System.Drawing.SystemColors:Window.
      appearance101:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:RowAppearance = appearance101.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance102:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:TemplateAddRowAppearance = appearance102.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFaktorer:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFaktorer:GuruContext = ?.
      THIS-OBJECT:GridFaktorer:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFaktorer:Name = "GridFaktorer".
      THIS-OBJECT:GridFaktorer:Size = NEW System.Drawing.Size(597, 501).
      THIS-OBJECT:GridFaktorer:TabIndex = 0.
      THIS-OBJECT:GridFaktorer:Text = "grid1".
      THIS-OBJECT:GridFaktorer:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFaktorer_AfterCellUpdate).
      THIS-OBJECT:GridFaktorer:InitializeLayout:Subscribe(THIS-OBJECT:GridFaktorer_InitializeLayout).
      /*  */
      /* GridPriser */
      /*  */
      appearance390:BackColor = System.Drawing.SystemColors:Window.
      appearance390:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPriser:DisplayLayout:Appearance = appearance390.
      THIS-OBJECT:GridPriser:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPriser:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance391:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance391:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance391:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance391:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:Appearance = appearance391.
      appearance392:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:BandLabelAppearance = appearance392.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance393:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance393:BackColor2 = System.Drawing.SystemColors:Control.
      appearance393:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance393:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:PromptAppearance = appearance393.
      THIS-OBJECT:GridPriser:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPriser:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance394:BackColor = System.Drawing.SystemColors:Window.
      appearance394:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveCellAppearance = appearance394.
      appearance395:BackColor = System.Drawing.SystemColors:Highlight.
      appearance395:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveRowAppearance = appearance395.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance396:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CardAreaAppearance = appearance396.
      appearance397:BorderColor = System.Drawing.Color:Silver.
      appearance397:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellAppearance = appearance397.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellPadding = 0.
      appearance398:BackColor = System.Drawing.SystemColors:Control.
      appearance398:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance398:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance398:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance398:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:GroupByRowAppearance = appearance398.
      appearance399:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderAppearance = appearance399.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance400:BackColor = System.Drawing.SystemColors:Window.
      appearance400:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:RowAppearance = appearance400.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance401:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:TemplateAddRowAppearance = appearance401.
      THIS-OBJECT:GridPriser:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridPriser:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridPriser:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridPriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridPriser:GuruContext = ?.
      THIS-OBJECT:GridPriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridPriser:Name = "GridPriser".
      THIS-OBJECT:GridPriser:Size = NEW System.Drawing.Size(544, 501).
      THIS-OBJECT:GridPriser:TabIndex = 1.
      THIS-OBJECT:GridPriser:Text = "grid1".
      THIS-OBJECT:GridPriser:AfterCellUpdate:Subscribe(THIS-OBJECT:GridPriser_AfterCellUpdate).
      THIS-OBJECT:GridPriser:InitializeLayout:Subscribe(THIS-OBJECT:GridPriser_InitializeLayout).
      /*  */
      /* TabMateriel */
      /*  */
      THIS-OBJECT:TabMateriel:Controls:Add(THIS-OBJECT:splitContainerMtrl).
      THIS-OBJECT:TabMateriel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMateriel:Name = "TabMateriel".
      THIS-OBJECT:TabMateriel:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* splitContainerMtrl */
      /*  */
      THIS-OBJECT:splitContainerMtrl:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrl:Name = "splitContainerMtrl".
      /*  */
      /* splitContainerMtrl.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrl:Panel1:Controls:Add(THIS-OBJECT:splitContainerMtrlValj).
      /*  */
      /* splitContainerMtrl.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrl:Panel2:Controls:Add(THIS-OBJECT:GridEgetMaterial).
      THIS-OBJECT:splitContainerMtrl:Panel2:Controls:Add(THIS-OBJECT:ButtonBerMtrl).
      THIS-OBJECT:splitContainerMtrl:Size = NEW System.Drawing.Size(1145, 501).
      THIS-OBJECT:splitContainerMtrl:SplitterDistance = 400.
      THIS-OBJECT:splitContainerMtrl:TabIndex = 19.
      /*  */
      /* splitContainerMtrlValj */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlValj:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerMtrlValj:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerMtrlValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlValj:Name = "splitContainerMtrlValj".
      /*  */
      /* splitContainerMtrlValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Panel1:Controls:Add(THIS-OBJECT:GridMaterial).
      /*  */
      /* splitContainerMtrlValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Panel2:Controls:Add(THIS-OBJECT:MatButtonOverMark).
      THIS-OBJECT:splitContainerMtrlValj:Panel2:Controls:Add(THIS-OBJECT:MatButtonBackMark).
      THIS-OBJECT:splitContainerMtrlValj:Size = NEW System.Drawing.Size(398, 499).
      THIS-OBJECT:splitContainerMtrlValj:SplitterDistance = 369.
      THIS-OBJECT:splitContainerMtrlValj:TabIndex = 1.
      /*  */
      /* GridMaterial */
      /*  */
      THIS-OBJECT:GridMaterial:AllowDrop = TRUE.
      appearance426:BackColor = System.Drawing.SystemColors:Window.
      appearance426:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMaterial:DisplayLayout:Appearance = appearance426.
      THIS-OBJECT:GridMaterial:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMaterial:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance427:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance427:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance427:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance427:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:Appearance = appearance427.
      appearance428:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:BandLabelAppearance = appearance428.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance429:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance429:BackColor2 = System.Drawing.SystemColors:Control.
      appearance429:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance429:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:PromptAppearance = appearance429.
      THIS-OBJECT:GridMaterial:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMaterial:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance430:BackColor = System.Drawing.SystemColors:Window.
      appearance430:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:ActiveCellAppearance = appearance430.
      appearance431:BackColor = System.Drawing.SystemColors:Highlight.
      appearance431:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:ActiveRowAppearance = appearance431.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance432:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CardAreaAppearance = appearance432.
      appearance433:BorderColor = System.Drawing.Color:Silver.
      appearance433:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CellAppearance = appearance433.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CellPadding = 0.
      appearance434:BackColor = System.Drawing.SystemColors:Control.
      appearance434:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance434:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance434:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance434:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:GroupByRowAppearance = appearance434.
      appearance435:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:HeaderAppearance = appearance435.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance436:BackColor = System.Drawing.SystemColors:Window.
      appearance436:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:RowAppearance = appearance436.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance437:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:TemplateAddRowAppearance = appearance437.
      THIS-OBJECT:GridMaterial:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMaterial:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMaterial:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMaterial:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMaterial:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMaterial:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMaterial:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMaterial:GuruContext = ?.
      THIS-OBJECT:GridMaterial:GuruDraggable = TRUE.
      THIS-OBJECT:GridMaterial:GuruDroppable = TRUE.
      THIS-OBJECT:GridMaterial:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMaterial:Name = "GridMaterial".
      THIS-OBJECT:GridMaterial:Size = NEW System.Drawing.Size(369, 499).
      THIS-OBJECT:GridMaterial:TabIndex = 0.
      THIS-OBJECT:GridMaterial:Text = "grid1".
      THIS-OBJECT:GridMaterial:GuruAfterDropped:Subscribe(THIS-OBJECT:GridMaterial_GuruAfterDropped).
      /*  */
      /* MatButtonOverMark */
      /*  */
      appearance157:Image = CAST(resources:GetObject("appearance157.Image"), System.Object).
      THIS-OBJECT:MatButtonOverMark:Appearance = appearance157.
      THIS-OBJECT:MatButtonOverMark:Location = NEW System.Drawing.Point(-1, 3).
      THIS-OBJECT:MatButtonOverMark:Name = "MatButtonOverMark".
      THIS-OBJECT:MatButtonOverMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:MatButtonOverMark:TabIndex = 10.
      THIS-OBJECT:MatButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:MatButtonOverMark:Click:Subscribe(THIS-OBJECT:MatButtonOverMark_Click).
      /*  */
      /* MatButtonBackMark */
      /*  */
      appearance158:Image = CAST(resources:GetObject("appearance158.Image"), System.Object).
      THIS-OBJECT:MatButtonBackMark:Appearance = appearance158.
      THIS-OBJECT:MatButtonBackMark:Location = NEW System.Drawing.Point(-1, 34).
      THIS-OBJECT:MatButtonBackMark:Name = "MatButtonBackMark".
      THIS-OBJECT:MatButtonBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:MatButtonBackMark:TabIndex = 11.
      THIS-OBJECT:MatButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:MatButtonBackMark:Click:Subscribe(THIS-OBJECT:MatButtonBackMark_Click).
      /*  */
      /* GridEgetMaterial */
      /*  */
      THIS-OBJECT:GridEgetMaterial:AllowDrop = TRUE.
      appearance414:BackColor = System.Drawing.SystemColors:Window.
      appearance414:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Appearance = appearance414.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance415:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance415:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance415:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance415:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:Appearance = appearance415.
      appearance416:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:BandLabelAppearance = appearance416.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance417:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance417:BackColor2 = System.Drawing.SystemColors:Control.
      appearance417:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance417:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:PromptAppearance = appearance417.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance418:BackColor = System.Drawing.SystemColors:Window.
      appearance418:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:ActiveCellAppearance = appearance418.
      appearance419:BackColor = System.Drawing.SystemColors:Highlight.
      appearance419:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:ActiveRowAppearance = appearance419.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance420:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CardAreaAppearance = appearance420.
      appearance421:BorderColor = System.Drawing.Color:Silver.
      appearance421:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CellAppearance = appearance421.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CellPadding = 0.
      appearance422:BackColor = System.Drawing.SystemColors:Control.
      appearance422:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance422:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance422:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance422:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:GroupByRowAppearance = appearance422.
      appearance423:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:HeaderAppearance = appearance423.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance424:BackColor = System.Drawing.SystemColors:Window.
      appearance424:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:RowAppearance = appearance424.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance425:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:TemplateAddRowAppearance = appearance425.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgetMaterial:Dock = System.Windows.Forms.DockStyle:Left.
      THIS-OBJECT:GridEgetMaterial:GuruContext = ?.
      THIS-OBJECT:GridEgetMaterial:GuruDraggable = TRUE.
      THIS-OBJECT:GridEgetMaterial:GuruDroppable = TRUE.
      THIS-OBJECT:GridEgetMaterial:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgetMaterial:Name = "GridEgetMaterial".
      THIS-OBJECT:GridEgetMaterial:Size = NEW System.Drawing.Size(650, 499).
      THIS-OBJECT:GridEgetMaterial:TabIndex = 1.
      THIS-OBJECT:GridEgetMaterial:Text = "grid1".
      THIS-OBJECT:GridEgetMaterial:GuruAfterDropped:Subscribe(THIS-OBJECT:GridEgetMaterial_GuruAfterDropped).
      /*  */
      /* ButtonBerMtrl */
      /*  */
      THIS-OBJECT:ButtonBerMtrl:Location = NEW System.Drawing.Point(656, 34).
      THIS-OBJECT:ButtonBerMtrl:Name = "ButtonBerMtrl".
      THIS-OBJECT:ButtonBerMtrl:Size = NEW System.Drawing.Size(80, 50).
      THIS-OBJECT:ButtonBerMtrl:TabIndex = 2.
      THIS-OBJECT:ButtonBerMtrl:Text = "Berednings-Materiel".
      THIS-OBJECT:ButtonBerMtrl:Click:Subscribe(THIS-OBJECT:ButtonBerMtrl_Click).
      /*  */
      /* TabImportMallar */
      /*  */
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:ButtonMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:ButtonExcel).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:ButtonBackMarkMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:ButtonOverMarkMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:GridMallarKoder).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:GridValdaMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:GridMallar).
      THIS-OBJECT:TabImportMallar:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabImportMallar:Name = "TabImportMallar".
      THIS-OBJECT:TabImportMallar:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* ButtonMallar */
      /*  */
      THIS-OBJECT:ButtonMallar:Location = NEW System.Drawing.Point(864, 195).
      THIS-OBJECT:ButtonMallar:Name = "ButtonMallar".
      THIS-OBJECT:ButtonMallar:Size = NEW System.Drawing.Size(86, 23).
      THIS-OBJECT:ButtonMallar:TabIndex = 16.
      THIS-OBJECT:ButtonMallar:Text = "Koppla Mallar".
      THIS-OBJECT:ButtonMallar:Click:Subscribe(THIS-OBJECT:ButtonMallar_Click).
      /*  */
      /* ButtonExcel */
      /*  */
      THIS-OBJECT:ButtonExcel:Location = NEW System.Drawing.Point(864, 235).
      THIS-OBJECT:ButtonExcel:Name = "ButtonExcel".
      THIS-OBJECT:ButtonExcel:Size = NEW System.Drawing.Size(86, 23).
      THIS-OBJECT:ButtonExcel:TabIndex = 15.
      THIS-OBJECT:ButtonExcel:Text = "Import Excel".
      THIS-OBJECT:ButtonExcel:Click:Subscribe(THIS-OBJECT:ButtonExcel_Click).
      /*  */
      /* ButtonBackMarkMallar */
      /*  */
      appearance160:Image = CAST(resources:GetObject("appearance160.Image"), System.Object).
      THIS-OBJECT:ButtonBackMarkMallar:Appearance = appearance160.
      THIS-OBJECT:ButtonBackMarkMallar:Location = NEW System.Drawing.Point(268, 152).
      THIS-OBJECT:ButtonBackMarkMallar:Name = "ButtonBackMarkMallar".
      THIS-OBJECT:ButtonBackMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackMarkMallar:TabIndex = 13.
      THIS-OBJECT:ButtonBackMarkMallar:Text = "Markerade bort".
      THIS-OBJECT:ButtonBackMarkMallar:Click:Subscribe(THIS-OBJECT:ButtonBackMarkMallar_Click).
      /*  */
      /* ButtonOverMarkMallar */
      /*  */
      appearance159:Image = CAST(resources:GetObject("appearance159.Image"), System.Object).
      THIS-OBJECT:ButtonOverMarkMallar:Appearance = appearance159.
      THIS-OBJECT:ButtonOverMarkMallar:Location = NEW System.Drawing.Point(268, 114).
      THIS-OBJECT:ButtonOverMarkMallar:Name = "ButtonOverMarkMallar".
      THIS-OBJECT:ButtonOverMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverMarkMallar:TabIndex = 11.
      THIS-OBJECT:ButtonOverMarkMallar:Text = "Markerade väljs".
      THIS-OBJECT:ButtonOverMarkMallar:Click:Subscribe(THIS-OBJECT:ButtonOverMarkMallar_Click).
      /*  */
      /* GridMallarKoder */
      /*  */
      appearance489:BackColor = System.Drawing.SystemColors:Window.
      appearance489:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Appearance = appearance489.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance490:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance490:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance490:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance490:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:Appearance = appearance490.
      appearance491:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance491.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance492:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance492:BackColor2 = System.Drawing.SystemColors:Control.
      appearance492:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance492:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance492.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance493:BackColor = System.Drawing.SystemColors:Window.
      appearance493:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveCellAppearance = appearance493.
      appearance494:BackColor = System.Drawing.SystemColors:Highlight.
      appearance494:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveRowAppearance = appearance494.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance495:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CardAreaAppearance = appearance495.
      appearance496:BorderColor = System.Drawing.Color:Silver.
      appearance496:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellAppearance = appearance496.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellPadding = 0.
      appearance497:BackColor = System.Drawing.SystemColors:Control.
      appearance497:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance497:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance497:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance497:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:GroupByRowAppearance = appearance497.
      appearance498:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderAppearance = appearance498.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance499:BackColor = System.Drawing.SystemColors:Window.
      appearance499:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:RowAppearance = appearance499.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance500:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance500.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallarKoder:GuruContext = ?.
      THIS-OBJECT:GridMallarKoder:Location = NEW System.Drawing.Point(19, 364).
      THIS-OBJECT:GridMallarKoder:Name = "GridMallarKoder".
      THIS-OBJECT:GridMallarKoder:Size = NEW System.Drawing.Size(352, 194).
      THIS-OBJECT:GridMallarKoder:TabIndex = 2.
      THIS-OBJECT:GridMallarKoder:Text = "grid1".
      /*  */
      /* GridValdaMallar */
      /*  */
      THIS-OBJECT:GridValdaMallar:AllowDrop = TRUE.
      appearance501:BackColor = System.Drawing.SystemColors:Window.
      appearance501:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Appearance = appearance501.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance502:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance502:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance502:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance502:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:Appearance = appearance502.
      appearance503:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance503.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance504:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance504:BackColor2 = System.Drawing.SystemColors:Control.
      appearance504:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance504:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance504.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance505:BackColor = System.Drawing.SystemColors:Window.
      appearance505:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveCellAppearance = appearance505.
      appearance506:BackColor = System.Drawing.SystemColors:Highlight.
      appearance506:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveRowAppearance = appearance506.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance507:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CardAreaAppearance = appearance507.
      appearance508:BorderColor = System.Drawing.Color:Silver.
      appearance508:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellAppearance = appearance508.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellPadding = 0.
      appearance509:BackColor = System.Drawing.SystemColors:Control.
      appearance509:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance509:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance509:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance509:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:GroupByRowAppearance = appearance509.
      appearance510:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderAppearance = appearance510.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance511:BackColor = System.Drawing.SystemColors:Window.
      appearance511:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:RowAppearance = appearance511.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance512:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance512.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaMallar:GuruContext = ?.
      THIS-OBJECT:GridValdaMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaMallar:Location = NEW System.Drawing.Point(420, 43).
      THIS-OBJECT:GridValdaMallar:Name = "GridValdaMallar".
      THIS-OBJECT:GridValdaMallar:Size = NEW System.Drawing.Size(222, 297).
      THIS-OBJECT:GridValdaMallar:TabIndex = 1.
      THIS-OBJECT:GridValdaMallar:Text = "grid1".
      THIS-OBJECT:GridValdaMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaMallar_GuruAfterDropped).
      /*  */
      /* GridMallar */
      /*  */
      THIS-OBJECT:GridMallar:AllowDrop = TRUE.
      appearance513:BackColor = System.Drawing.SystemColors:Window.
      appearance513:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallar:DisplayLayout:Appearance = appearance513.
      THIS-OBJECT:GridMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance514:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance514:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance514:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance514:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:Appearance = appearance514.
      appearance515:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance515.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance516:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance516:BackColor2 = System.Drawing.SystemColors:Control.
      appearance516:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance516:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance516.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance517:BackColor = System.Drawing.SystemColors:Window.
      appearance517:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveCellAppearance = appearance517.
      appearance518:BackColor = System.Drawing.SystemColors:Highlight.
      appearance518:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveRowAppearance = appearance518.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance519:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CardAreaAppearance = appearance519.
      appearance520:BorderColor = System.Drawing.Color:Silver.
      appearance520:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellAppearance = appearance520.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellPadding = 0.
      appearance521:BackColor = System.Drawing.SystemColors:Control.
      appearance521:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance521:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance521:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance521:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:GroupByRowAppearance = appearance521.
      appearance522:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderAppearance = appearance522.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance523:BackColor = System.Drawing.SystemColors:Window.
      appearance523:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowAppearance = appearance523.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance524:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance524.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallar:GuruContext = ?.
      THIS-OBJECT:GridMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridMallar:Location = NEW System.Drawing.Point(19, 43).
      THIS-OBJECT:GridMallar:Name = "GridMallar".
      THIS-OBJECT:GridMallar:Size = NEW System.Drawing.Size(222, 297).
      THIS-OBJECT:GridMallar:TabIndex = 0.
      THIS-OBJECT:GridMallar:Text = "grid1".
      THIS-OBJECT:GridMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridMallar_GuruAfterDropped).
      /*  */
      /* TabAvtalskalkyl */
      /*  */
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:ultraAvtal).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:ComboAvtal).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:ultraTextEditor2).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:AvtalText).
      THIS-OBJECT:TabAvtalskalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabAvtalskalkyl:Name = "TabAvtalskalkyl".
      THIS-OBJECT:TabAvtalskalkyl:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* ultraAvtal */
      /*  */
      THIS-OBJECT:ultraAvtal:ImageTransparentColor = System.Drawing.Color:Red.
      THIS-OBJECT:ultraAvtal:Location = NEW System.Drawing.Point(62, 117).
      THIS-OBJECT:ultraAvtal:Name = "ultraAvtal".
      THIS-OBJECT:ultraAvtal:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:ultraAvtal:TabIndex = 43.
      THIS-OBJECT:ultraAvtal:Text = "Kalkylavtal".
      /*  */
      /* ComboAvtal */
      /*  */
      THIS-OBJECT:ComboAvtal:CheckedListSettings:CheckStateMember = "".
      appearance16:BackColor = System.Drawing.SystemColors:Window.
      appearance16:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Appearance = appearance16.
      THIS-OBJECT:ComboAvtal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:ComboAvtal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance17:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance17:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance17:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance17:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:Appearance = appearance17.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance18.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance20:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance20:BackColor2 = System.Drawing.SystemColors:Control.
      appearance20:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance20:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:PromptAppearance = appearance20.
      THIS-OBJECT:ComboAvtal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:ComboAvtal:DisplayLayout:MaxRowScrollRegions = 1.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      appearance21:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:ActiveCellAppearance = appearance21.
      appearance22:BackColor = System.Drawing.SystemColors:Highlight.
      appearance22:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:ActiveRowAppearance = appearance22.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CardAreaAppearance = appearance23.
      appearance24:BorderColor = System.Drawing.Color:Silver.
      appearance24:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CellAppearance = appearance24.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CellPadding = 0.
      appearance25:BackColor = System.Drawing.SystemColors:Control.
      appearance25:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance25:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance25:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance25:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:GroupByRowAppearance = appearance25.
      appearance26:TextHAlignAsString = "Left".
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:HeaderAppearance = appearance26.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:RowAppearance = appearance27.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:TemplateAddRowAppearance = appearance28.
      THIS-OBJECT:ComboAvtal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:ComboAvtal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:ComboAvtal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:ComboAvtal:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:ComboAvtal:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:ComboAvtal:Location = NEW System.Drawing.Point(149, 118).
      THIS-OBJECT:ComboAvtal:Name = "ComboAvtal".
      THIS-OBJECT:ComboAvtal:Size = NEW System.Drawing.Size(514, 22).
      THIS-OBJECT:ComboAvtal:TabIndex = 42.
      /*  */
      /* ultraTextEditor2 */
      /*  */
      THIS-OBJECT:ultraTextEditor2:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 15, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:ultraTextEditor2:Location = NEW System.Drawing.Point(62, 49).
      THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2".
      THIS-OBJECT:ultraTextEditor2:Size = NEW System.Drawing.Size(601, 33).
      THIS-OBJECT:ultraTextEditor2:TabIndex = 6.
      THIS-OBJECT:ultraTextEditor2:Text = "Mallarna ska ge dig en hjälp att få med alla grundkalkylkoder!".
      /*  */
      /* AvtalText */
      /*  */
      THIS-OBJECT:AvtalText:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 15, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:AvtalText:Location = NEW System.Drawing.Point(62, 13).
      THIS-OBJECT:AvtalText:Name = "AvtalText".
      THIS-OBJECT:AvtalText:Size = NEW System.Drawing.Size(601, 33).
      THIS-OBJECT:AvtalText:TabIndex = 5.
      THIS-OBJECT:AvtalText:Text = "Har du tagit del av de kalkylmallar som finns tillgängliga?".
      /*  */
      /* TabKopiera */
      /*  */
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:buttonKonvKalkyl).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KonvTillTyp).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KopLabelOmr).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KopHuvudOmrade).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KopLabelKatalog).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KopHuvudKatalog).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:buttonKopieraKalkyl).
      THIS-OBJECT:TabKopiera:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKopiera:Name = "TabKopiera".
      THIS-OBJECT:TabKopiera:Size = NEW System.Drawing.Size(1145, 501).
      THIS-OBJECT:TabKopiera:Paint:Subscribe(THIS-OBJECT:TabKopiera_Paint).
      /*  */
      /* buttonKonvKalkyl */
      /*  */
      THIS-OBJECT:buttonKonvKalkyl:Location = NEW System.Drawing.Point(662, 289).
      THIS-OBJECT:buttonKonvKalkyl:Name = "buttonKonvKalkyl".
      THIS-OBJECT:buttonKonvKalkyl:Size = NEW System.Drawing.Size(86, 23).
      THIS-OBJECT:buttonKonvKalkyl:TabIndex = 48.
      THIS-OBJECT:buttonKonvKalkyl:Text = "Konvertera Kalkyl".
      THIS-OBJECT:buttonKonvKalkyl:Click:Subscribe(THIS-OBJECT:buttonKonvKalkyl_Click).
      /*  */
      /* KonvTillTyp */
      /*  */
      THIS-OBJECT:KonvTillTyp:Location = NEW System.Drawing.Point(517, 76).
      THIS-OBJECT:KonvTillTyp:Name = "KonvTillTyp".
      THIS-OBJECT:KonvTillTyp:Size = NEW System.Drawing.Size(423, 28).
      THIS-OBJECT:KonvTillTyp:TabIndex = 47.
      /*  */
      /* KopLabelOmr */
      /*  */
      appearance29:TextHAlignAsString = "Left".
      THIS-OBJECT:KopLabelOmr:Appearance = appearance29.
      THIS-OBJECT:KopLabelOmr:Location = NEW System.Drawing.Point(57, 81).
      THIS-OBJECT:KopLabelOmr:Name = "KopLabelOmr".
      THIS-OBJECT:KopLabelOmr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:KopLabelOmr:TabIndex = 46.
      /*  */
      /* KopHuvudOmrade */
      /*  */
      THIS-OBJECT:KopHuvudOmrade:CheckedListSettings:CheckStateMember = "".
      appearance30:BackColor = System.Drawing.SystemColors:Window.
      appearance30:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Appearance = appearance30.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance31:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance31:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance31:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance31:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance31.
      appearance32:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance32.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance33:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance33:BackColor2 = System.Drawing.SystemColors:Control.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance33.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance34:BackColor = System.Drawing.SystemColors:Window.
      appearance34:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance34.
      appearance35:BackColor = System.Drawing.SystemColors:Highlight.
      appearance35:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance35.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance36:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance36.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      appearance37:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CellAppearance = appearance37.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance38:BackColor = System.Drawing.SystemColors:Control.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance38.
      appearance39:TextHAlignAsString = "Left".
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance39.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:RowAppearance = appearance51.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance52.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:KopHuvudOmrade:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:KopHuvudOmrade:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:KopHuvudOmrade:Location = NEW System.Drawing.Point(174, 82).
      THIS-OBJECT:KopHuvudOmrade:Name = "KopHuvudOmrade".
      THIS-OBJECT:KopHuvudOmrade:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:KopHuvudOmrade:TabIndex = 45.
      /*  */
      /* KopLabelKatalog */
      /*  */
      appearance53:TextHAlignAsString = "Left".
      THIS-OBJECT:KopLabelKatalog:Appearance = appearance53.
      THIS-OBJECT:KopLabelKatalog:Location = NEW System.Drawing.Point(57, 160).
      THIS-OBJECT:KopLabelKatalog:Name = "KopLabelKatalog".
      THIS-OBJECT:KopLabelKatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:KopLabelKatalog:TabIndex = 44.
      THIS-OBJECT:KopLabelKatalog:Text = "Katalog".
      /*  */
      /* KopHuvudKatalog */
      /*  */
      THIS-OBJECT:KopHuvudKatalog:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:KopHuvudKatalog:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:KopHuvudKatalog:Location = NEW System.Drawing.Point(174, 156).
      THIS-OBJECT:KopHuvudKatalog:Name = "KopHuvudKatalog".
      THIS-OBJECT:KopHuvudKatalog:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:KopHuvudKatalog:TabIndex = 43.
      /*  */
      /* buttonKopieraKalkyl */
      /*  */
      THIS-OBJECT:buttonKopieraKalkyl:ImageList = THIS-OBJECT:imageList1.
      THIS-OBJECT:buttonKopieraKalkyl:Location = NEW System.Drawing.Point(215, 289).
      THIS-OBJECT:buttonKopieraKalkyl:Name = "buttonKopieraKalkyl".
      THIS-OBJECT:buttonKopieraKalkyl:Size = NEW System.Drawing.Size(86, 23).
      THIS-OBJECT:buttonKopieraKalkyl:TabIndex = 0.
      THIS-OBJECT:buttonKopieraKalkyl:Text = "Kopiera Kalkyl".
      THIS-OBJECT:buttonKopieraKalkyl:Click:Subscribe(THIS-OBJECT:buttonKopieraKalkyl_Click).
      /*  */
      /* imageList1 */
      /*  */
      THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream"), System.Windows.Forms.ImageListStreamer).
      THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
      THIS-OBJECT:imageList1:Images:SetKeyName(0, "kalkyl-info.jpg").
      THIS-OBJECT:imageList1:Images:SetKeyName(1, "kalkyl-volym.jpg").
      /*  */
      /* TabFelmedd */
      /*  */
      THIS-OBJECT:TabFelmedd:Controls:Add(THIS-OBJECT:GridFelMedd).
      THIS-OBJECT:TabFelmedd:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFelmedd:Name = "TabFelmedd".
      THIS-OBJECT:TabFelmedd:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* GridFelMedd */
      /*  */
      appearance58:BackColor = System.Drawing.SystemColors:Window.
      appearance58:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Appearance = appearance58.
      THIS-OBJECT:GridFelMedd:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFelMedd:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance55:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance55:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance55:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance55:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:Appearance = appearance55.
      appearance56:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:BandLabelAppearance = appearance56.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance57:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance57:BackColor2 = System.Drawing.SystemColors:Control.
      appearance57:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance57:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:PromptAppearance = appearance57.
      THIS-OBJECT:GridFelMedd:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance78:BackColor = System.Drawing.SystemColors:Window.
      appearance78:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveCellAppearance = appearance78.
      appearance61:BackColor = System.Drawing.SystemColors:Highlight.
      appearance61:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveRowAppearance = appearance61.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance60:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CardAreaAppearance = appearance60.
      appearance59:BorderColor = System.Drawing.Color:Silver.
      appearance59:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellAppearance = appearance59.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellPadding = 0.
      appearance63:BackColor = System.Drawing.SystemColors:Control.
      appearance63:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance63:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance63:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance63:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:GroupByRowAppearance = appearance63.
      appearance77:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderAppearance = appearance77.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance64:BackColor = System.Drawing.SystemColors:Window.
      appearance64:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:RowAppearance = appearance64.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFelMedd:GuruContext = ?.
      THIS-OBJECT:GridFelMedd:Location = NEW System.Drawing.Point(269, 94).
      THIS-OBJECT:GridFelMedd:Name = "GridFelMedd".
      THIS-OBJECT:GridFelMedd:Size = NEW System.Drawing.Size(550, 341).
      THIS-OBJECT:GridFelMedd:TabIndex = 0.
      THIS-OBJECT:GridFelMedd:Text = "grid1".
      /*  */
      /* TabVolymber */
      /*  */
      THIS-OBJECT:TabVolymber:Controls:Add(THIS-OBJECT:GridVolymBer).
      THIS-OBJECT:TabVolymber:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVolymber:Name = "TabVolymber".
      THIS-OBJECT:TabVolymber:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* GridVolymBer */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridVolymBer:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridVolymBer:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance4:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance4:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance4:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:Appearance = appearance4.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:BandLabelAppearance = appearance40.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance41:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance41:BackColor2 = System.Drawing.SystemColors:Control.
      appearance41:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:PromptAppearance = appearance41.
      THIS-OBJECT:GridVolymBer:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridVolymBer:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance42:BackColor = System.Drawing.SystemColors:Window.
      appearance42:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:ActiveCellAppearance = appearance42.
      appearance43:BackColor = System.Drawing.SystemColors:Highlight.
      appearance43:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:ActiveRowAppearance = appearance43.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance44:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CardAreaAppearance = appearance44.
      appearance45:BorderColor = System.Drawing.Color:Silver.
      appearance45:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CellAppearance = appearance45.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CellPadding = 0.
      appearance46:BackColor = System.Drawing.SystemColors:Control.
      appearance46:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance46:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance46:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance46:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:GroupByRowAppearance = appearance46.
      appearance47:TextHAlignAsString = "Left".
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:HeaderAppearance = appearance47.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance48:BackColor = System.Drawing.SystemColors:Window.
      appearance48:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:RowAppearance = appearance48.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance49:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:TemplateAddRowAppearance = appearance49.
      THIS-OBJECT:GridVolymBer:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridVolymBer:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridVolymBer:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridVolymBer:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridVolymBer:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridVolymBer:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridVolymBer:GuruContext = ?.
      THIS-OBJECT:GridVolymBer:Location = NEW System.Drawing.Point(150, 116).
      THIS-OBJECT:GridVolymBer:Name = "GridVolymBer".
      THIS-OBJECT:GridVolymBer:Size = NEW System.Drawing.Size(950, 190).
      THIS-OBJECT:GridVolymBer:TabIndex = 0.
      THIS-OBJECT:GridVolymBer:Text = "grid1".
      THIS-OBJECT:GridVolymBer:AfterCellUpdate:Subscribe(THIS-OBJECT:GridVolymBer_AfterCellUpdate).
      /*  */
      /* TabManager */
      /*  */
      appearance2:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance2.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFaktorer).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabMateriel).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabImportMallar).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVolymber).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkylHuvud).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabEgna).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabAvtalskalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFelmedd).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKopiera).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 52).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1149, 551).
      THIS-OBJECT:TabManager:TabIndex = 44.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance128:Image = CAST(resources:GetObject("appearance128.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance128.
      appearance127:Image = CAST(resources:GetObject("appearance127.Image"), System.Object).
      ultraTab13:Appearance = appearance127.
      ultraTab13:Key = "Kalkylhuvud".
      ultraTab13:TabPage = THIS-OBJECT:TabKalkylHuvud.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Kalkylhuvud".
      appearance130:Image = CAST(resources:GetObject("appearance130.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance130.
      appearance129:Image = CAST(resources:GetObject("appearance129.Image"), System.Object).
      ultraTab14:Appearance = appearance129.
      ultraTab14:Key = "Kalkyl".
      ultraTab14:TabPage = THIS-OBJECT:TabKalkyl.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Kalkylera".
      appearance132:Image = CAST(resources:GetObject("appearance132.Image"), System.Object).
      ultraTab15:ActiveAppearance = appearance132.
      appearance131:Image = CAST(resources:GetObject("appearance131.Image"), System.Object).
      ultraTab15:Appearance = appearance131.
      appearance3:BackColor = System.Drawing.SystemColors:Control.
      ultraTab15:ClientAreaAppearance = appearance3.
      ultraTab15:Key = "Egna koder".
      ultraTab15:TabPage = THIS-OBJECT:TabEgna.
      ultraTab15:Text = "".
      ultraTab15:ToolTipText = "Gör Egna koder".
      appearance134:Image = CAST(resources:GetObject("appearance134.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance134.
      appearance133:Image = CAST(resources:GetObject("appearance133.Image"), System.Object).
      ultraTab16:Appearance = appearance133.
      ultraTab16:Key = "Visa Kalkyl".
      ultraTab16:TabPage = THIS-OBJECT:TabVisa.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "Visa kalkyl".
      appearance136:Image = CAST(resources:GetObject("appearance136.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance136.
      appearance135:Image = CAST(resources:GetObject("appearance135.Image"), System.Object).
      ultraTab17:Appearance = appearance135.
      ultraTab17:Key = "FaktorerPriser".
      ultraTab17:TabPage = THIS-OBJECT:TabFaktorer.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "Lägg in egna Faktorer/Priser".
      appearance138:Image = CAST(resources:GetObject("appearance138.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance138.
      appearance137:Image = CAST(resources:GetObject("appearance137.Image"), System.Object).
      ultraTab19:Appearance = appearance137.
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "Eget Materiel".
      appearance140:Image = CAST(resources:GetObject("appearance140.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance140.
      appearance139:BackColor = System.Drawing.Color:Transparent.
      appearance139:Image = CAST(resources:GetObject("appearance139.Image"), System.Object).
      ultraTab22:Appearance = appearance139.
      ultraTab22:Key = "ImportMallar".
      ultraTab22:TabPage = THIS-OBJECT:TabImportMallar.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "Import/Mallar".
      appearance142:Image = CAST(resources:GetObject("appearance142.Image"), System.Object).
      ultraTab20:ActiveAppearance = appearance142.
      appearance141:Image = CAST(resources:GetObject("appearance141.Image"), System.Object).
      ultraTab20:Appearance = appearance141.
      ultraTab20:Key = "Avtalskalkyl".
      ultraTab20:TabPage = THIS-OBJECT:TabAvtalskalkyl.
      ultraTab20:Text = "".
      ultraTab20:ToolTipText = "Avtalskalkyl".
      appearance144:Image = CAST(resources:GetObject("appearance144.Image"), System.Object).
      ultraTab2:ActiveAppearance = appearance144.
      appearance143:Image = CAST(resources:GetObject("appearance143.Image"), System.Object).
      ultraTab2:Appearance = appearance143.
      ultraTab2:Key = "KopieraKalkyl".
      ultraTab2:TabPage = THIS-OBJECT:TabKopiera.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Kopiera/Konvertera kalkyler".
      appearance146:Image = CAST(resources:GetObject("appearance146.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance146.
      appearance145:Image = CAST(resources:GetObject("appearance145.Image"), System.Object).
      ultraTab1:Appearance = appearance145.
      ultraTab1:Key = "FelMedd".
      ultraTab1:TabPage = THIS-OBJECT:TabFelmedd.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Felmeddelanden".
      appearance148:Image = CAST(resources:GetObject("appearance148.Image"), System.Object).
      ultraTab24:ActiveAppearance = appearance148.
      appearance147:Image = CAST(resources:GetObject("appearance147.Image"), System.Object).
      ultraTab24:Appearance = appearance147.
      ultraTab24:Key = "Volymber".
      ultraTab24:TabPage = THIS-OBJECT:TabVolymber.
      ultraTab24:Text = "".
      ultraTab24:ToolTipText = "Volymberäkning av schaktmassor".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 11 NO-UNDO.
      arrayvar1[1] = ultraTab13.
      arrayvar1[2] = ultraTab14.
      arrayvar1[3] = ultraTab15.
      arrayvar1[4] = ultraTab16.
      arrayvar1[5] = ultraTab17.
      arrayvar1[6] = ultraTab19.
      arrayvar1[7] = ultraTab22.
      arrayvar1[8] = ultraTab20.
      arrayvar1[9] = ultraTab2.
      arrayvar1[10] = ultraTab1.
      arrayvar1[11] = ultraTab24.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar1).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 501).
      /*  */
      /* ultraToolbarsManager1 */
      /*  */
      THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
      THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
      ultraToolbar1:DockedColumn = 0.
      ultraToolbar1:DockedRow = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
      arrayvar2[1] = buttonTool1.
      ultraToolbar1:NonInheritedTools:AddRange(arrayvar2).
      ultraToolbar1:Text = "UltraToolbar1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
      arrayvar3[1] = ultraToolbar1.
      THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar3).
      buttonTool2:SharedPropsInternal:Caption = "ButtonTool1".
      appearance172:BackColor = System.Drawing.Color:Lime.
      appearance172:Image = CAST(resources:GetObject("appearance172.Image"), System.Object).
      buttonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance172.
      appearance173:BackColor = System.Drawing.Color:Red.
      appearance173:Image = CAST(resources:GetObject("appearance173.Image"), System.Object).
      buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance173.
      buttonTool3:SharedPropsInternal:Caption = "nyaknappen".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar4[1] = buttonTool2.
      arrayvar4[2] = buttonTool3.
      THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar4).
      /*  */
      /* m_Kalkyl_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 52).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:Name = "m_Kalkyl_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 551).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_Kalkyl_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(1149, 52).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:Name = "m_Kalkyl_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 551).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_Kalkyl_Toolbars_Dock_Area_Top */
      /*  */
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:Name = "m_Kalkyl_Toolbars_Dock_Area_Top".
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(1149, 52).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_Kalkyl_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 603).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:Name = "m_Kalkyl_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(1149, 0).
      THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* ttKalkyl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Top).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkyl_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Name = "ttKalkyl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabKalkylHuvud:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkoder:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnr:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnr:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFriKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFriKalk:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerFriKalk:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFriKalk:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KoderAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabEgna:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:PerformLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaVald:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaVald:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabVisa:ResumeLayout(FALSE).
      THIS-OBJECT:VisaPanelWaiting:ResumeLayout(FALSE).
      THIS-OBJECT:TabFaktorer:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktorerPriser:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktorerPriser:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktorerPriser:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFaktorer, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridPriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabMateriel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMaterial, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridEgetMaterial, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabImportMallar:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMallarKoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridValdaMallar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabAvtalskalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:TabAvtalskalkyl:PerformLayout().
      CAST(THIS-OBJECT:ComboAvtal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:AvtalText, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKopiera:ResumeLayout(FALSE).
      THIS-OBJECT:TabKopiera:PerformLayout().
      CAST(THIS-OBJECT:KopHuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KopHuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabFelmedd:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFelMedd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabVolymber:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridVolymBer, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManager:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

   

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
      THIS-OBJECT:Invalidate().
      THIS-OBJECT:TabManager:Invalidate().
      PROCESS EVENTS.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      DEFINE VARIABLE path  AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFrom AS CHARACTER NO-UNDO.

      cTo = e:TAB:KEY.
      cFrom = THIS-OBJECT:TabManager:SelectedTab:KEY NO-ERROR.
     
      IF cFrom EQ ? THEN cFrom = "".  
      IF cFrom = "Kalkylhuvud" THEN 
      DO:         
         IF THIS-OBJECT:NyKalkyl = TRUE THEN 
         DO:
            IF THIS-OBJECT:ApplyCreatedKalkyl() EQ FALSE THEN 
            DO:
               e:Cancel = TRUE.
            END. 
            THIS-OBJECT:SparaDefs(?,?).           
         END.
         ELSE 
         DO:
            IF THIS-OBJECT:SaveHuvud() = FALSE THEN 
            DO:
               e:Cancel = TRUE.
            END.   
         END.  
      END.       
      IF cTo = "Visa Kalkyl" THEN 
      DO:
         IF THIS-OBJECT:WasExcelShown NE TRUE THEN 
         DO:
            IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 THEN 
            DO:
               THIS-OBJECT:GenereraAvtalsKalkyl().            
            END.
            ELSE 
            DO:   
               THIS-OBJECT:VisningKalkIni(OUTPUT path).
               THIS-OBJECT:GenereraKalkylVisning().
               THIS-OBJECT:WasExcelShown = TRUE.                 
            END.   
         END.         
         THIS-OBJECT:RibbonTabVisning:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.      
      IF cTo = "FaktorerPriser" THEN 
      DO:
      END.      
      IF cTo = "Kalkyl" THEN 
      DO:         
         THIS-OBJECT:GridKalkylKoder:GuruReopen().
         THIS-OBJECT:GridFriKalk:GuruReopen().
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabKoder:Visible = FALSE.
      END.      
      IF cTo = "Egna koder" THEN 
      DO:
         THIS-OBJECT:GridEgnaKoder:GuruReopen().
         THIS-OBJECT:GridUnderKoder:GuruReopen().
          
         /*ANDERS*/
         THIS-OBJECT:GridEgnaKoder:ValueChanged(). 
         THIS-OBJECT:GridUnderKoder:ValueChanged(). 
         THIS-OBJECT:RibbonTabEgnaKoder:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabEgnaKoder:Visible = FALSE.
      END.      
      IF cTo = "Materiel" THEN 
      DO:
         THIS-OBJECT:RibbonTabMat:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabMat:Visible = FALSE.
      END.
      IF cTo = "ImportMallar" THEN 
      DO:
         THIS-OBJECT:KalkmallHuvudtth:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN.
         ELSE 
         DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:MallarHmt().
            THIS-OBJECT:GridMallar:GuruReopen().
            THIS-OBJECT:GridMallar:GuruFirstRow().
         END.   
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.  
      END.
      IF cTo = "Avtalskalkyl" THEN 
      DO:
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 4.
      END.    
      IF cTo = "Volymber" THEN 
      DO:
         THIS-OBJECT:VolymBerTTh:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:VolymBerTTh:AVAILABLE THEN.
         ELSE 
         DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBerHmt().
            THIS-OBJECT:GridVolymBer:GuruReopen().
            THIS-OBJECT:GridVolymBer:GuruFirstRow().
         END.   
      END.
                
      IF cTo = "KopieraKalkyl" THEN THIS-OBJECT:CreateComboKonvTyp().
      IF cTo = "Felmeddelanden" THEN 
      DO:
         THIS-OBJECT:GridFelMedd:GuruReopen().
         THIS-OBJECT:GridFelMedd:GuruFirstRow().           
      END.
      PROCESS EVENTS.
   END METHOD.
   METHOD PUBLIC VOID VisningKalkIni( OUTPUT path AS CHARACTER).
      IF THIS-OBJECT:VisaExcel:embedvar = TRUE THEN 
      DO:         
         THIS-OBJECT:SetVisningsBild(0).
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
         THIS-OBJECT:VisaExcel:Initialize(path).        
         THIS-OBJECT:VisaExcel:Fix().
      END.   
      ELSE THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).    
   END METHOD.
   METHOD PUBLIC VOID Kill().
      
   END METHOD.

   METHOD PRIVATE VOID InitializeForm ():
   /* FINNS I * 2GURUJUNI*/     
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID SkapaNyEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      THIS-OBJECT:DontCheck = TRUE.
      rid = THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateEgenKod().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridEgnaKoder:GuruRepositionto(rid).     
      THIS-OBJECT:DontCheck= FALSE.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      /* INGA KVAR*/
      IF THIS-OBJECT:GridEgnaKoder:Posn() = -1 THEN RETURN.  
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveEgenKod().
      GridEgnaKoder:GuruBortrader(FALSE).
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      THIS-OBJECT:FilterUnderKoder().
      RETURN.
   
   END METHOD.

   METHOD PUBLIC VOID TabRefreshKalkylHuvud():
      
   END METHOD.

   METHOD PRIVATE VOID ValueArbetsKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN 
      DO:
         THIS-OBJECT:GridLopKoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ "'").
         THIS-OBJECT:GridLopKoder:GuruFirstrow().
         THIS-OBJECT:ValueLopnrCh(?,?). 
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValueMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN 
      DO:
         THIS-OBJECT:GridMallarKoder:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:GridMallarKoder:GuruFirstrow().
      END.
   END METHOD.
   
   METHOD PRIVATE INTEGER GetCurrentType():
      RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:VALUEList:VALUEListItems[THIS-OBJECT:RibbonToolVisaTyp:VALUEList:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer):GetInteger("Typ").
   END METHOD.

   METHOD PRIVATE CHARACTER GetMatrisName():
      DEFINE VARIABLE mName AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:HuvudTTh:Buffer-field("TYPKALK"):Buffer-VALUE = 7 THEN mName = "Period".
      ELSE mName = "Matris".
      RETURN mName.
   END METHOD.

   METHOD PRIVATE VOID ValueLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN 
      DO:
         THIS-OBJECT:KodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:FerekvensHmt(INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
            INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
            INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
         THIS-OBJECT:GridFrekvens:GuruReopen().
      END.   
   /*
   THIS-OBJECT:FrekvensKomChanged(?,?).
   */
   END METHOD.
   
   METHOD PRIVATE VOID ValueKalkKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      DEFINE VARIABLE q     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE exrid AS RECID     NO-UNDO.
      /*Se filtrera i Grid*/ 
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN 
      DO:
         IF THIS-OBJECT:DontCheck = TRUE THEN.
         ELSE 
         DO:
            IF STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN 
            DO: 
               exrid = INTEGER(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE)).           
               THIS-OBJECT:GridEgnaKoder:GuruRepositionTo( THIS-OBJECT:KoderTTh:ROWID /* ROWID(kalknumtt) */).
               q = STRING("WHERE kalknumtt.TTRECID = " + STRING(exrid)).
               THIS-OBJECT:KoderTTh:FIND-FIRST(q, NO-LOCK) NO-ERROR.
               /*FIND FIRST kalknumtt WHERE kalknumtt.TTRECID = exrid NO-LOCK NO-ERROR.*/
               THIS-OBJECT:FilterUnderKoder().  
            END. 
         END.
         IF KoderTTh:AVAILABLE THEN THIS-OBJECT:KoderAnmarkning:Text  = THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValueEgnaKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 THEN 
      DO:
         
      END.    
   END METHOD.
   
   METHOD PRIVATE VOID ValuePersCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/   
      THIS-OBJECT:HuvudAnsvarig:GuruText:Text = THIS-OBJECT:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + THIS-OBJECT:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.          
   END METHOD.
      
   METHOD PRIVATE VOID VisaTabs (INPUT vilka AS CHARACTER, INPUT visavar AS LOGICAL ):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY = vilka THEN 
      DO:
         THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[2] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = visavar.
      END.   
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY = vilka THEN 
      DO:
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[4] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = visavar.
      END.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Egna koder"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:KEY = vilka THEN 
      DO:
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[6] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = visavar.
      END.   
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:KEY = vilka THEN 
      DO:         
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[11] = TRUE THEN 
         DO: 
            THIS-OBJECT:kalkaonrTTh:FIND-FIRST() NO-ERROR.
            IF THIS-OBJECT:kalkaonrTTh:AVAILABLE THEN 
            DO:
               IF THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN.
               ELSE 
               DO:
                  IF STRING(THIS-OBJECT:HuvudTyp:GuruCombo:VALUE) = "2" THEN 
                  DO:
                     IF Guru.konstanter:globforetag = "ELPA" OR Guru.konstanter:globforetag = "GRAN" OR  Guru.konstanter:globforetag = "LIMO" THEN 
                     DO:                       
                        queryvar =  "FOR EACH " + THIS-OBJECT:Avtalskalktth:TABLE. 
                        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Avtalskalktth,queryvar).
                        qH:GET-FIRST().
                        IF THIS-OBJECT:Avtalskalktth:AVAILABLE THEN 
                        DO:
                           THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
                           THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = visavar.
                        END.                                  
                        ELSE 
                        DO:
                           KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK).
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchAvtal(INPUT THIS-OBJECT:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE ).                                               
                           THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
                           qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Avtalskalktth,queryvar).
                           qH:GET-FIRST().
                           IF THIS-OBJECT:Avtalskalktth:AVAILABLE THEN  
                           DO:
                              THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = visavar.
                              THIS-OBJECT:RibbonToolVisVisa:VALUEList:VALUEListItems:Add(NEW Infragistics.Win.VALUEListItem("Avtalskalkyl")).
                              THIS-OBJECT:CreateComboAvtal().
                           END.
                           DO WHILE qH:QUERY-OFF-END = FALSE:
                              IF THIS-OBJECT:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE = THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("KATAR"):BUFFER-VALUE THEN 
                              DO:
                                 THIS-OBJECT:ComboAvtal:VALUE = STRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE).                              
                              END.   
                              qH:GET-NEXT().
                           END.
                           THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).                          
                        END.
                     END.
                  END.
               END.
            END.            
         END.   
      END.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Materiel"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["ImportMallar"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Felmedd"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["Felmedd"]:VISIBLE   = visavar.   
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Volymber"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["Volymber"]:VISIBLE   = visavar.                    
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN 
      DO:
         THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE = FALSE. 
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
      END.   
   END METHOD.   

   METHOD PUBLIC VOID SetVisningsBild(i AS INTEGER):
      /* 
         0 = inget genererat än
         1 = genererar
         2 = resultat
      */
      IF i EQ 0 THEN 
      DO:
         THIS-OBJECT:VisaPanelEmpty:Visible = TRUE.
         THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:VisaExcel:Visible = FALSE.
         THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 1 THEN 
      DO:
         THIS-OBJECT:VisaPanelWaiting:Visible = TRUE.
         THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:VisaExcel:Visible = FALSE.
         THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 2 THEN 
      DO:
         THIS-OBJECT:VisaExcel:Visible = TRUE.
         THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
   END METHOD.

   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 THEN 
      DO:
         THIS-OBJECT:GenereraAvtalsKalkyl().
         RETURN.
      END.   
      IF THIS-OBJECT:VisaExcel:Visible EQ TRUE THEN 
      DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF res:ToString()  EQ "Yes" THEN 
         DO:
            THIS-OBJECT:GenereraKalkylVisning().   
         END.
      END.
      ELSE 
      DO:
         THIS-OBJECT:GenereraKalkylVisning().
      END.
   END METHOD.
    
   METHOD PUBLIC VOID SetVisningExceldoc(INPUT vad AS CHARACTER, OUTPUT path AS CHARACTER):
      THIS-OBJECT:VisaExcel:direkticell = FALSE. 
      THIS-OBJECT:VisaExcel:embedvar = TRUE.        
      IF vad = "VKALK" THEN path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xls").
            
   END METHOD.   
   METHOD PUBLIC VOID GenereraKalkylVisning():
      DEFINE VARIABLE path              AS CHARACTER NO-UNDO.
      DEFINE VARIABLE totbermtrl        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE findfel           AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH             AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE link              AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rubrikrad         AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE radefterrubrikrad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE matrisrader       AS CHARACTER NO-UNDO.   
      DEFINE VARIABLE kostnadstartcol   AS CHARACTER NO-UNDO.
      ASSIGN 
         cSenasteArbKod     = ""
         iSenasteLopNr      = 0        
         iSenasteMatris     = 0        
         cSenasteMtrl       = ""       
         iSenasteMtrlMatris = 0.
      IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 THEN 
      DO:
         THIS-OBJECT:GenereraAvtalsKalkyl().
         RETURN.
      END.  
      
      IF THIS-OBJECT:VisaExcel:embedvar = TRUE THEN 
      DO:
         {EUROPEANSET.I}
         IF THIS-OBJECT:WasExcelShown = FALSE THEN THIS-OBJECT:VisningKalkIni(OUTPUT path).
         ELSE THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
      END.
      ELSE 
      DO:
         numformat = SESSION:NUMERIC-FORMAT.
         SESSION:NUMERIC-FORMAT = "AMERICAN".
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
      END.   
      THIS-OBJECT:WasExcelShown = TRUE.
      
      /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i slutet också! */
      DEFINE VARIABLE TimerTest AS System.Diagnostics.Stopwatch NO-UNDO.
      TimerTest = NEW System.Diagnostics.Stopwatch().
      TimerTest:Reset().
      TimerTest:Start().
     
      
      THIS-OBJECT:VisaExcel:iSistaColnum = 1.
      IF THIS-OBJECT:VisaExcel:embedvar = TRUE THEN 
      DO:
         THIS-OBJECT:VisaExcel:ScreenUpp(FALSE).
         THIS-OBJECT:SetVisningsBild(1).
         THIS-OBJECT:VisaExcel:Reload(path).         
      END.  
      ELSE THIS-OBJECT:VisaExcel:OpenExcel(path).
     
     
      /*INGA FAKTORER OM INTE GRUNDKALKYL*/
      IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) > 0 THEN 
      DO:
         THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE.
         THIS-OBJECT:RibbonToolVisEgnaPriser:CHECKED = FALSE.          
      END. 
      /*INTE GRUPPERA OM BARA EN MATRIS*/
      IF VisMatris() = 0 THEN.
      ELSE 
      DO:
         THIS-OBJECT:RibbonToolVisGroupMatris:CHECKED = FALSE.
      END.   
      /*BERÄKNA KALYLEN*/
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
      THIS-OBJECT:kalkrubrikTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkrubrikTTh.
      
      {LOGGORc.I}
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      link = ?.
      IF link NE ? THEN 
      DO: 
         IF THIS-OBJECT:VisaExcel:embedvar = TRUE THEN 
         DO:  
            THIS-OBJECT:VisaExcel:GetWorkSheet():PageSetup:LeftHeaderPicture:Filename = System.IO.Path:GetFullPath(link).
         END.
         ELSE THIS-OBJECT:VisaExcel:imageexcel(INPUT link).     
         THIS-OBJECT:VisaExcel:iRad = 1.
      END.
      THIS-OBJECT:VisaExcel:ColumnRad().
      /* Skapa huvud */
      THIS-OBJECT:ExcelHuv().    
      /*ANMÄRKNING*/
      THIS-OBJECT:ExcelAnm().
      /* --- Börja göra listan här --- */
      /* Skapa rubrikerna */
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      rubrikrad = THIS-OBJECT:VisaExcel:ColRad. 
      
      THIS-OBJECT:VisaExcel:Rowdown().  
      radefterrubrikrad = THIS-OBJECT:VisaExcel:ColRad.
      matrisrader = radefterrubrikrad.
      THIS-OBJECT:VisaExcel:Rowdup().  
      THIS-OBJECT:VisaExcel:DataOut("Kod").
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Benämning").
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Antal").
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Enh").
      /*LÄGG UT RUBRIKER FÖR TIMMAR*/
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = FALSE NO-LOCK BY ORDNING". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE = THIS-OBJECT:VisaExcel:cColname.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      kostnadstartcol = "".
      /*LÄGG UT RUBRIKER FÖR KOSTNAD*/
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = TRUE NO-LOCK BY ORDNING".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = THIS-OBJECT:VisaExcel:cColname.
         IF kostnadstartcol = "" THEN 
         DO:
            IF THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "EA" THEN.
            ELSE kostnadstartcol = THIS-OBJECT:VisaExcel:cColname.
         END.   
         qH:GET-NEXT(). 
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Summa").
      /* Gör rubriker bold */
      THIS-OBJECT:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:VisaExcel:iRad) ,THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ). 
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:Border("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
      THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
      /*OM MATRIS*/
      IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE. 
      ELSE 
      DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE + " WHERE kalknumtt.MATRIS = " + STRING(VisMatris()).      
      END.    
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
      DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR".
      END.
      ELSE 
      DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
      END.       
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
      qH:GET-FIRST().
      
    
      DO WHILE qH:QUERY-OFF-END = FALSE:           
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().      
         /*    GRUPPERA MATRIS*/    

         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
         DO: 
            THIS-OBJECT:ExcelGruppMatris(INPUT 1, INPUT matrisrader, INPUT kostnadstartcol ).
            IF iSenasteMatris = 0 THEN matrisrader = matrisrader.
            ELSE IF iSenasteMatris NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE THEN 
               DO:
                  matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:VisaExcel:Getrad(THIS-OBJECT:VisaExcel:ColRad))).
               END.   
            /*OM NY MATRIS*/
            iSenasteMatris = THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
         END.
         /*GRUPPERA KODER*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppera") = "yes" THEN 
         DO:
            THIS-OBJECT:ExcelGruppKoder().         
         END.
         /*SUMMERA SAMMA KOD*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisSummera") = "yes" THEN 
         DO:
            IF VisSummera(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
         END.    
         ELSE THIS-OBJECT:VisaExcel:Rowdown().
         /*UT MED ARB KOD MM*/  
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
         /*UT MED TIMMARNA*/
         queryvar =  "FOR EACH " + THIS-OBJECT:kalkantalTTh:TABLE + " WHERE kalkantal.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = FALSE  AND BENAMNING = '" + THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN
               THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).                  
            subqH:GET-NEXT(NO-LOCK).
         END.
         /*UT MED KOSTNAD*/
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkkostnadTTh:TABLE + " WHERE kalkkostnad.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = TRUE  AND BENAMNING = '" + THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN 
            DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN 
               DO:
                  THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE)).
               END.
               ELSE THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).    
            END.
            /*UT MED EA*/
            THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "EA"') NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN 
            DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN 
               DO:
                  THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("FRIEAMANGD"):BUFFER-VALUE)).
               END.
               ELSE THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE)).             
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.           
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).         
         /*SUMMERA HORISONT*/
         THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
         THIS-OBJECT:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad), THIS-OBJECT:VisaExcel:ColRad,0).
         /*RISKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 2 THEN 
         DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE = 0 THEN.
            ELSE 
            DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(ROUND(DECIMAL(THIS-OBJECT:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE) / 100),0))).
            END.   
         END.   
         /*VINSTKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 3 THEN 
         DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE = 0 THEN.
            ELSE 
            DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(ROUND(DECIMAL(THIS-OBJECT:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE) / 100),0))).
            END.   
         END.  
         qH:GET-NEXT(NO-LOCK).
      END.
      /*GRUPPERA MATRIS*/     
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
      DO:
         THIS-OBJECT:ExcelGruppMatris(INPUT 2,INPUT matrisrader, INPUT kostnadstartcol).       
         matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:VisaExcel:Getrad(THIS-OBJECT:VisaExcel:ColRad)) + 1).                                
      END.
      /*EGET MATRIEL SUMMA*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN.
      ELSE 
      DO:        
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE 
         DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisPriser") = "1" THEN 
            DO:
               THIS-OBJECT:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN 
               DO:  
                  THIS-OBJECT:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  IF VisMatris() = 0 THEN                             
                     THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(?))).
                  ELSE THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(VisMatris()))).
                  THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).
               END.                       
            END.            
         END.
      END.
      /*Berednings mtrl*/   
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN 
      DO:
         THIS-OBJECT:VisaExcel:cColname = "B".
         THIS-OBJECT:VisaExcel:ColumnRad().
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:DataOut("Materielkostnad från beredning").
         THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
         IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN 
         DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlHmt(OUTPUT totbermtrl).
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE 
            DO:
               THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(totbermtrl)).
               THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).              
            END.                   
         END.           
      END.
      
      /*  
        THIS-OBJECT:VisaExcel:valueDataOut2("a7","1.5").
        */
      
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      /* 
    Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(),"C" + STRING(THIS-OBJECT:VisaExcel:iRad) , "N" + STRING(THIS-OBJECT:VisaExcel:iRad) ):Merge( true ).
    */
      
      /*KOLUMNBREDD*/
      THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
      THIS-OBJECT:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:VisaExcel:iRad),45).
      THIS-OBJECT:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:VisaExcel:iRad),10).
      THIS-OBJECT:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:VisaExcel:iRad),10).
      THIS-OBJECT:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:VisaExcel:iRad),15).
      THIS-OBJECT:VisaExcel:BordHoriz(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:VisaExcel:BordHorizW(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlBorderWeight:xlThin"). 
      THIS-OBJECT:VisaExcel:BordVert(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:VisaExcel:BordVertW(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlBorderWeight:xlThin").
      
      /*SUMMERA ALLA KOLUMNER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN.
      ELSE 
      DO:
         THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "E".
         THIS-OBJECT:VisaExcel:ColumnRad().
         REPEAT:
            THIS-OBJECT:VisaExcel:Summa(THIS-OBJECT:VisaExcel:cColname + THIS-OBJECT:VisaExcel:Getrad(radefterrubrikrad), THIS-OBJECT:VisaExcel:ColRad,0).
            IF THIS-OBJECT:VisaExcel:iColnum = THIS-OBJECT:VisaExcel:iSistaColnum THEN LEAVE.
            THIS-OBJECT:VisaExcel:ColRight().
         END.              
      END.
      /*ANDERS
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(),rubrikrad , "C" + STRING(THIS-OBJECT:VisaExcel:iRad) ):Merge( true ).
      */
      
      /*FONTER*/
      THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
      THIS-OBJECT:VisaExcel:Border(rubrikrad, THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad)). 
      
      /*BILAGA EGNA PRISER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisEgnaPriser") = "yes" THEN 
      DO:
         THIS-OBJECT:ExcelBilagor(1).
      END.
      /*BILAGA EGNA FAKTORER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisFaktorer") = "yes" THEN 
      DO:
         THIS-OBJECT:ExcelBilagor(2).       
      END.   
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisFaktorer") = "yes" OR 
         /*BILAGA EGNA Priser*/
         THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisEgnaPriser") = "yes" THEN 
      DO:
         THIS-OBJECT:VisaExcel:ColWidth("C1",THIS-OBJECT:VisaExcel:ColWidth("A1")).
      END.   
      /*BILAGA EGNet mtrl*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisMatSpec") = "yes" THEN 
      DO:
         THIS-OBJECT:ExcelBilagor(3).      
      END.   
      /*BILAGA mtrl från ber*/
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN 
      DO:
         THIS-OBJECT:ExcelBilagor(4).      
      END.
      THIS-OBJECT:VisaExcel:ScreenUpp(TRUE).
      THIS-OBJECT:VisaExcel:valueDataOutTT().
      
      IF VisaExcel:embedvar = TRUE THEN 
      DO:
         THIS-OBJECT:VisaExcel:ViewType(INPUT 3).
         THIS-OBJECT:VisaExcel:ExcelAppHandle:ActiveWorkbook:Activate().
         THIS-OBJECT:VisaExcel:Fix().
         THIS-OBJECT:SetVisningsBild(2).
      END.   
      ELSE THIS-OBJECT:VisaExcel:SlutExcel().
      {EUROPEANAMERICANRESET.I}
      /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i början också! */
      MESSAGE TimerTest:ElapsedMilliseconds
         VIEW-AS ALERT-BOX. 
   END METHOD.

   METHOD PRIVATE VOID ExcelHuv():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisHuvud") = "yes" THEN 
      DO:
         THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:RibbonToolVisVisa:Text).
         THIS-OBJECT:VisaExcel:Rowdown().             
         THIS-OBJECT:VisaExcel:DataOut("Kalkyl-nr.: ").        
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkNrvar)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:Root:DatabaseManager:Kalkyl:PlnrAonr()).
         IF VisMatris() = 0 THEN. 
         ELSE 
         DO:
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(GetMatrisName() + " " + STRING(VisMatris())).
         END. 
         THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ).   
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:DataOut("Kalkyltyp: ").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:RibbonToolInfTyp:Text)).
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:Rowdown().   
         THIS-OBJECT:VisaExcel:DataOut(Guru.Konstanter:gomrl + ": ").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:Omradevar)).
         THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().      
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelAnm():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisAnmarkning") = "yes" THEN 
      DO:
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().
         THIS-OBJECT:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
         THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
      END.
   END METHOD.

   METHOD PRIVATE VOID ExcelGruppMatris(vad AS INTEGER,matrisrader AS CHARACTER,kostnadstartcol AS CHARACTER):
      IF vad = 1 THEN 
      DO:
         /*LÄGGER UT MATRIS RADEN*/
         IF VisGruppMatris(THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN 
         DO:
            /*EGET MATRIEL SUMMA*/
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE 
            DO:
               IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisPriser") = "1" THEN 
               DO:
                  THIS-OBJECT:VisaExcel:Rowdown().
                  THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
                  IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN 
                  DO:                                               
                     THIS-OBJECT:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:VisaExcel:iRad),"Materielkostnad från eget materiel").
                     THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(iSenasteMatris))).                  
                  END.              
                  THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).
               END.
            END.
            THIS-OBJECT:VisaExcel:Rowdown().            
            THIS-OBJECT:VisaExcel:DataOut("Summa " + GetMatrisName() + " " + STRING(iSenasteMatris)). 
            THIS-OBJECT:VisaExcel:cColname = "E".
            THIS-OBJECT:VisaExcel:ColumnRad().
            /*SUMMERA ALLA KOLUMNER*/
            REPEAT:
               THIS-OBJECT:VisaExcel:Summa(THIS-OBJECT:VisaExcel:cColname + THIS-OBJECT:VisaExcel:Getrad(matrisrader), THIS-OBJECT:VisaExcel:ColRad,0).
               IF THIS-OBJECT:VisaExcel:iColnum = THIS-OBJECT:VisaExcel:iSistaColnum THEN LEAVE.
               THIS-OBJECT:VisaExcel:ColRight().
            END.
            THIS-OBJECT:VisaExcel:Rowdown().
            THIS-OBJECT:VisaExcel:cColname = "A".
            THIS-OBJECT:VisaExcel:ColumnRad().                                     
         END.
      END.
      /*SISTA MATRISEN?*/
      IF vad = 2 THEN 
      DO:
         /*EGET MATRIEL SUMMA*/
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE 
         DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisPriser") = "1" THEN 
            DO:
               THIS-OBJECT:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN 
               DO:                                               
                  THIS-OBJECT:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(iSenasteMatris))).                  
               END.              
               THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).
            END.
         END.
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().
         THIS-OBJECT:VisaExcel:Rowdown().            
         THIS-OBJECT:VisaExcel:DataOut("Summa " + GetMatrisName()+ " " + STRING(iSenasteMatris)). 
         THIS-OBJECT:VisaExcel:cColname = "E".
         THIS-OBJECT:VisaExcel:ColumnRad().
         /*SUMMERA ALLA KOLUMNER*/
         REPEAT:
            THIS-OBJECT:VisaExcel:Summa(THIS-OBJECT:VisaExcel:cColname + THIS-OBJECT:VisaExcel:Getrad(matrisrader), THIS-OBJECT:VisaExcel:ColRad,0).
            IF THIS-OBJECT:VisaExcel:iColnum = THIS-OBJECT:VisaExcel:iSistaColnum THEN LEAVE.
            THIS-OBJECT:VisaExcel:ColRight().
         END.  
         THIS-OBJECT:VisaExcel:Rowdown().                
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelGruppKoder():
      IF VisGruppera(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = FALSE THEN 
      DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN 
         DO:
            THIS-OBJECT:VisaExcel:DataOut(STRING("Egna Koder")).
         END.   
         ELSE 
         DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
         END.
      END.      
   END METHOD.
   
   METHOD PRIVATE VOID ExcelBilagor(vad AS INTEGER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      IF vad = 1 THEN 
      DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().                    
         THIS-OBJECT:VisaExcel:DataOut("Egna Priser").
         THIS-OBJECT:VisaExcel:FontExcel(THIS-OBJECT:VisaExcel:ColRad,THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
         queryvar =  "FOR EACH " + THIS-OBJECT:EgnaPriserTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:EgnaPriserTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:VisaExcel:cColname = "E" THEN 
            DO:
               THIS-OBJECT:VisaExcel:Rowdown().
               THIS-OBJECT:VisaExcel:cColname = "A".
               THIS-OBJECT:VisaExcel:ColumnRad().         
            END.
            THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
            THIS-OBJECT:VisaExcel:ColRight().
            subqH:GET-NEXT(NO-LOCK).
         END.       
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 2 THEN 
      DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().                    
         THIS-OBJECT:VisaExcel:DataOut("Egna Faktorer").
         THIS-OBJECT:VisaExcel:FontExcel(THIS-OBJECT:VisaExcel:ColRad,THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
         queryvar =  "FOR EACH " + THIS-OBJECT:FaktorerTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:FaktorerTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:VisaExcel:cColname = "E" THEN 
            DO:
               THIS-OBJECT:VisaExcel:Rowdown().
               THIS-OBJECT:VisaExcel:cColname = "A".
               THIS-OBJECT:VisaExcel:ColumnRad().         
            END.
            THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:FaktorerTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE)).
            THIS-OBJECT:VisaExcel:ColRight().
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 3 THEN 
      DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().                    
         THIS-OBJECT:VisaExcel:DataOut("MaterielSpecifikation").
         THIS-OBJECT:VisaExcel:FontExcel(THIS-OBJECT:VisaExcel:ColRad,THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().         
         THIS-OBJECT:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Antal").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
         DO:
            THIS-OBJECT:VisaExcel:DataOut(GetMatrisName()).
            THIS-OBJECT:VisaExcel:ColRight().
         END.   
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE 
         DO:
            THIS-OBJECT:VisaExcel:DataOut("Pris").
            THIS-OBJECT:VisaExcel:ColRight().
         END.
         THIS-OBJECT:VisaExcel:DataOut("Summa").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
         DO:
            queryvar = "FOR EACH " + THIS-OBJECT:kalktmtrlTTh:TABLE + " BY MATRIS BY ENR". 
         END.
         ELSE IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:kalktmtrlTTh:TABLE + " BY ENR".
            ELSE queryvar = "FOR EACH " + THIS-OBJECT:kalktmtrlTTh:TABLE + " WHERE MATRIS = " + STRING(VisMatris()) + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalktmtrlTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
            DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
            END.
            ELSE 
            DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
            END.   
            THIS-OBJECT:VisaExcel:cColname = "A".
            THIS-OBJECT:VisaExcel:ColumnRad().         
            THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE)).         
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:VisaExcel:ColRight().
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl","VisGruppMatris") = "yes" THEN 
            DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE)).        
               THIS-OBJECT:VisaExcel:ColRight().
            END.   
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE 
            DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE)).
               THIS-OBJECT:VisaExcel:ColRight().
               THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).            
               THIS-OBJECT:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 4 THEN 
      DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().                    
         THIS-OBJECT:VisaExcel:DataOut("Materieltillägg från beredning").
         THIS-OBJECT:VisaExcel:FontExcel(THIS-OBJECT:VisaExcel:ColRad,THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().         
         THIS-OBJECT:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Antal").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:VisaExcel:ColRight().
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE 
         DO:
            THIS-OBJECT:VisaExcel:DataOut("Pris").
            THIS-OBJECT:VisaExcel:ColRight().
         END.
         THIS-OBJECT:VisaExcel:DataOut("Summa").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
         queryvar = "FOR EACH " + THIS-OBJECT:berkalkmtrlttH:TABLE + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:berkalkmtrltth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF VisSummeraMtrl(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
            THIS-OBJECT:VisaExcel:cColname = "A".
            THIS-OBJECT:VisaExcel:ColumnRad().         
            THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).         
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:VisaExcel:ColRight().
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE 
            DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
               THIS-OBJECT:VisaExcel:ColRight().
               THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("TOTPRIS"):BUFFER-VALUE)).            
               THIS-OBJECT:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.   
   END METHOD.

   METHOD PRIVATE LOGICAL VisSummeraMtrl(cIN AS CHARACTER,iIN AS INTEGER):
      IF iIN = ? THEN 
      DO:
         IF cIN = cSenasteMtrl THEN RETURN TRUE.
         ELSE 
         DO:
            ASSIGN  
               cSenasteMtrl = cIN.         
            RETURN FALSE.
         END.
      END.
      ELSE 
      DO:
         IF cIN = cSenasteMtrl AND iIN = iSenasteMtrlMatris THEN RETURN TRUE.
         ELSE 
         DO:
            ASSIGN  
               cSenasteMtrl       = cIN
               iSenasteMtrlMatris = iIN.
            RETURN FALSE.
         END.
      END.      
   END METHOD.

   METHOD PRIVATE LOGICAL VisSummera(cIN AS CHARACTER,iIN AS INTEGER):
      IF cIN = cSenasteArbKod AND iIN = iSenasteLopNr THEN RETURN TRUE.
      ELSE 
      DO:
         ASSIGN  
            cSenasteArbKod = cIN
            iSenasteLopNr  = iIN.
         RETURN FALSE.
      END.   
   END METHOD.
   METHOD PRIVATE LOGICAL VisGruppera(cIN AS CHARACTER):
      IF cIN = cSenasteArbKod THEN RETURN TRUE.
      ELSE 
      DO:
         ASSIGN  
            cSenasteArbKod = cIN.      
         RETURN FALSE.
      END.   
       
   END METHOD.

   METHOD PRIVATE LOGICAL VisGruppMatris(iIN AS INTEGER):  
      IF iIN = iSenasteMatris THEN RETURN TRUE.
      ELSE 
      DO:
         ASSIGN
            cSenasteArbKod = ""  
            iSenasteLopNr  = 0. 
        
         IF iSenasteMatris = 0 THEN RETURN TRUE.
         ELSE RETURN FALSE.        
      END.       
   END METHOD.
   
   METHOD PRIVATE INTEGER VisMatris():
      DEFINE VARIABLE mCH AS CHARACTER NO-UNDO.
      DEFINE VARIABLE tCH AS CHARACTER NO-UNDO.
      mCH = GetMatrisName().
      tCH = THIS-OBJECT:RibbonToolVisMatris:Text.
      IF THIS-OBJECT:RibbonToolVisMatris:Text = "Alla" THEN RETURN 0.
      ELSE 
      DO:
         RETURN INTEGER(TRIM(SUBSTRING(tCH,LENGTH(mCH) + 1))).         
      END.       
   END METHOD.
   METHOD PRIVATE VOID ExcelImportLista(vad AS INTEGER,filnamn AS CHARACTER):
      {AMERICANSET.I}
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:embedvar = FALSE.
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:ReadExcelStart(INPUT filnamn).   
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF invar NE "" THEN 
         DO:
            IF THIS-OBJECT:VisaExcel:cColname = "A" THEN 
            DO:
               KalkylimportTTh:BUFFER-CREATE().
               IF vad = 2 THEN 
               DO:
                  klangd = LENGTH(invar).
                  THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 2))).
                  THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 1),2)) NO-ERROR.
               END.
               ELSE 
               DO:
                  THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
            END.
            IF THIS-OBJECT:VisaExcel:cColname = "B" AND vad = 1 THEN 
            DO:
               THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
            END.    
            IF THIS-OBJECT:VisaExcel:cColname = "C" THEN 
            DO:
               invar = REPLACE(invar,",",".").
               THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
            END.      
         END.
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "D" THEN 
         DO:
            THIS-OBJECT:VisaExcel:cColname = "A". 
            THIS-OBJECT:VisaExcel:Rowdown().
         END.            
      END.  
      THIS-OBJECT:VisaExcel:SlutReadExcel().
      {EUROPEANAMERICANRESET.I}
      THIS-OBJECT:ImportLista().
   END METHOD.
   
   METHOD PRIVATE VOID ImportMallar():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.  
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkmallValdtth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkmallValdtth,queryvar).
      qH:GET-FIRST().
      IF THIS-OBJECT:KalkmallValdtth:AVAILABLE THEN.
      ELSE RETURN.
      DO WHILE qH:QUERY-OFF-END = FALSE:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkmallKodertth:TABLE + " WHERE KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallValdtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE).     
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkmallKodertth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            KalkylimportTTh:BUFFER-CREATE().
            KalkylimportTTh:BUFFER-COPY(KalkmallKodertth). 
            subqH:GET-NEXT(NO-LOCK).  
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:ImportLista().
      KalkmallValdtth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:GridValdaMallar:GuruReopen().
   END METHOD.
   
   METHOD PRIVATE VOID ImportLista():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      THIS-OBJECT:GridArbetsKoder:GuruFiltrera(" ").
      THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkylimportTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkylimportTTh,queryvar).
      qH:GET-FIRST().
      
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN 
         DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            THIS-OBJECT:GridArbetsKoder:GuruRepositionTo(THIS-OBJECT:ArbetskoderTTh:ROWID).
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
            THIS-OBJECT:GridLopKoder:GuruRepositionTo(THIS-OBJECT:LopposterTTh:ROWID).
            THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR. 
            IF THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE > 0 THEN THIS-OBJECT:CurrentMatris = THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
            ELSE THIS-OBJECT:CurrentMatris = -1.
            THIS-OBJECT:ButtonOvermark_Click(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE).
         END.   
         ELSE 
         DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").         
         END.   
         qH:GET-NEXT().
         THIS-OBJECT:GridArbetsKoder:GuruAvmarkera().
         THIS-OBJECT:GridLopKoder:GuruAvmarkera(). 
      END.  
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:RefreshArbKoder(?,?).
   END METHOD.
   METHOD PUBLIC VOID GenereraAvtalsKalkyl():
      {AMERICANSET.I}
      DEFINE VARIABLE resid AS INTEGER                           NO-UNDO.
      DEFINE VARIABLE res   AS System.Windows.Forms.DialogResult NO-UNDO. 
      OS-DELETE VALUE(ExelKommando) NO-ERROR.     
      
      THIS-OBJECT:Avtalskalktth:FIND-FIRST("WHERE ID = "  + STRING(THIS-OBJECT:ComboAvtal:VALUE)) NO-ERROR.
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(47) + " " +
         THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXT"):BUFFER-VALUE  +  " Katalog år " + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("KATAR"):BUFFER-VALUE + 
         " Avtals år " + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALAR"):BUFFER-VALUE, "Avtalskalkyl", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Information).
      IF res:ToString()  EQ "Yes" THEN.
      ELSE RETURN.
      tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:embedvar = FALSE.
      IF Guru.Konstanter:globforetag = "GRAN" THEN 
      DO:
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalkalkao(INPUT THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, TRUE).      
      END.   
      ELSE THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalkalkao(INPUT THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, FALSE).
      THIS-OBJECT:HamtaAvtalsKalkyl(OUTPUT resid, OUTPUT ExelKommando).
      IF resid = ? THEN RETURN.
      IF Guru.Konstanter:globforetag = "GRAN" THEN 
      DO:   
         THIS-OBJECT:EonesAvtal().    
      END.
      ELSE IF Guru.Konstanter:globforetag = "FORS" THEN 
         DO:        
            THIS-OBJECT:InfraAvtal().       
         END. 
         ELSE IF Guru.Konstanter:globforetag = "LIMO" THEN 
            DO:        
               THIS-OBJECT:LimoAvtal().       
            END.   
      IF Guru.Konstanter:globforetag = "ELPA" THEN 
      DO: 
         IF ExelKommando MATCHES "*eones*" THEN THIS-OBJECT:EonesAvtal(). 
         IF ExelKommando MATCHES "*Infratek*" THEN THIS-OBJECT:InfraAvtal().
         IF ExelKommando MATCHES "*linjemontage*" THEN THIS-OBJECT:LimoAvtal().   
      END.
      {EUROPEANAMERICANRESET.I}
      THIS-OBJECT:FelMeddCheck().
    
   /*
   OS-DELETE VALUE(ExelKommando) NO-ERROR. 
   */   
   END METHOD.
   METHOD PRIVATE CHARACTER  TraffColf(INPUT ordvar AS INTEGER) :
      IF ordvar = 1 THEN RETURN  "G". 
      ELSE IF ordvar = 1 THEN RETURN "G".
         ELSE IF ordvar = 2 THEN RETURN "H".
            ELSE IF ordvar = 3 THEN RETURN "I".
               ELSE IF ordvar = 4 THEN RETURN "J".
                  ELSE IF ordvar = 5 THEN RETURN "K".
                     ELSE IF ordvar = 6 THEN RETURN "L".
                        ELSE IF ordvar = 7 THEN RETURN "M".
                           ELSE IF ordvar = 8 THEN RETURN "N".
                              ELSE IF ordvar = 9 THEN RETURN "O".
                                 ELSE IF ordvar = 10 THEN RETURN "P".
   END METHOD.
   METHOD PRIVATE VOID EonesAvtal():
      DEFINE VARIABLE sterad      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:EonesAvtal(INPUT-OUTPUT sterad,INPUT-OUTPUT slerad,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                   
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:VisaExcel:ValjBlad(6).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN THIS-OBJECT:VisaExcel:ValjBlad(12).
         ELSE THIS-OBJECT:VisaExcel:ValjBlad(flik1). 
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:DataOut("F4", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:VisaExcel:DataOut("F3",SUBSTRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,40)).      
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN 
      DO:
         THIS-OBJECT:VisaExcel:DataOut("F7",utfardatvar).
         THIS-OBJECT:VisaExcel:DataOut("F8",beredarvar).
         THIS-OBJECT:VisaExcel:DataOut("F9",refvar).
      END. 
      ELSE 
      DO:
         THIS-OBJECT:VisaExcel:DataOut("F8",utfardatvar).
         THIS-OBJECT:VisaExcel:DataOut("F9",beredarvar).
         THIS-OBJECT:VisaExcel:DataOut("F10",refvar).
      END. 
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:VisaExcel:ValjBlad(6).
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN THIS-OBJECT:VisaExcel:ValjBlad(12).
            ELSE THIS-OBJECT:VisaExcel:ValjBlad(flik1).
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "133" AND THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 20 THEN 
         DO:
            traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " m",INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ESP" THEN 
            DO:
               traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
            END.      
            ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN 
               DO:
                  traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
               END.
               ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EBR" THEN 
                  DO:
                     traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
                  END.
                  ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKJ" THEN 
                     DO:
                        traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
                     END.
                     ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKL" THEN 
                        DO:            
                           traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
                        END.
                        ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" THEN 
                           DO:
                              traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
                           END.
                           ELSE traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         IF traffcol = ? OR traffcol = "" THEN 
         DO:          
            THIS-OBJECT:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END. 
         ELSE 
         DO:
            IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN   THIS-OBJECT:VisaExcel:ValjBlad( 7).
            ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN THIS-OBJECT:VisaExcel:ValjBlad( 13).
               ELSE THIS-OBJECT:VisaExcel:ValjBlad( flik2). 
            traffcol = REPLACE(traffcol,"$","").         
            IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN 
            DO:
               traffcol = TraffColf(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,2)) + 10).
            END.            
            ELSE traffcol = TraffColf(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,2)) + 7).                            
            THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:VisaExcel:AddKom(INPUT traffcol,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
            IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN 
            DO:
               traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
               traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                
            END.
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN   THIS-OBJECT:VisaExcel:ValjBlad( 6).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN THIS-OBJECT:VisaExcel:ValjBlad( 12).
         ELSE THIS-OBJECT:VisaExcel:ValjBlad(INPUT flik1). 
      queryvar = "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).              
         IF traffcol NE "" THEN 
         DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE). 
               traffcol = REPLACE(traffcol,"$","").
               IF pristypvar = "BEREDARE" THEN 
               DO: /*F1*/     
                  traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
               END.
               IF pristypvar = "MONTÖR" THEN 
               DO:     /*F2*/ 
                  traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
               END.
               IF pristypvar = "MATERIEL" THEN 
               DO:     
                  traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
               END.
               IF pristypvar = "ÖVRIGKOSTNAD" THEN 
               DO:     
                  traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
               END.                                    
               IF pristypvar = "MASKIN1" THEN 
               DO: /*F3*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,"Ledning").                  
               END.
               IF pristypvar = "MASKIN2" THEN 
               DO:  /*F4*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,"Kabel").                  
               END.
               IF pristypvar = "MASKIN3" THEN 
               DO:  /*F5*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,"RUNT.OM.MASK").                  
               END.
               IF pristypvar = "MASKIN4" THEN 
               DO: /*F6*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,"Lastbil").                  
               END.
               IF pristypvar = "MASKIN5" THEN 
               DO: /*F7*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,"Kranbil").                  
               END.                 
               subqH:GET-NEXT(NO-LOCK).
            END.
         END.
         ELSE 
         DO:
            THIS-OBJECT:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").            
         END.   
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:VisaExcel:SlutExcel().
   END METHOD.
   
   METHOD PRIVATE VOID InfraAvtal():
      DEFINE VARIABLE sterad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InfraAvtal(INPUT-OUTPUT sterad1,INPUT-OUTPUT slerad1,OUTPUT sterad2,INPUT-OUTPUT slerad2,INPUT-OUTPUT sterad3,
         INPUT-OUTPUT slerad3,INPUT-OUTPUT frist,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar ,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN THIS-OBJECT:VisaExcel:ValjBlad(3).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:VisaExcel:ValjBlad(4).
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2011" THEN THIS-OBJECT:VisaExcel:ValjBlad(5).
            ELSE THIS-OBJECT:VisaExcel:ValjBlad(5).   
      THIS-OBJECT:VisaExcel:DataOut("H6", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:VisaExcel:DataOut("E4",ortnamn).
      THIS-OBJECT:VisaExcel:DataOut("E7",kontakt).      
      THIS-OBJECT:VisaExcel:DataOut("E6",refvar).      
      THIS-OBJECT:VisaExcel:DataOut("E5",refvar). 
      THIS-OBJECT:VisaExcel:DataOut("H8",arbannsv).    
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN.
            ELSE 
            DO:
               IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN 
               DO:
                  traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B912",TRUE).
               END.
               ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN 
                  DO:
                     traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B914",TRUE).               
                  END.
                  ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN 
                     DO:
                        traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B1056",TRUE).
                     END.         
                     ELSE 
                     DO:            
                        IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" THEN 
                        DO:
                           traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B" + STRING(sterad1),"B" + STRING(slerad1),TRUE).               
                        END.
                        ELSE 
                        DO:
                           traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B" + STRING(sterad1),"B" + STRING(slerad1),TRUE).                  
                        END.   
                     END.           
               IF traffcol = ? OR traffcol = "" THEN 
               DO:
                  THIS-OBJECT:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
               END.   
               ELSE 
               DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                   
               END.                
            END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN 
         DO:
            THIS-OBJECT:VisaExcel:ValjBlad(2).
            traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B84","B108",TRUE).
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.            
            ELSE 
            DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE).
                  IF pristypvar = "BEREDARE" THEN 
                  DO: /*F1*/     
                     traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN 
                  DO:     /*F2*/ 
                     traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN 
                  DO:     
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                  DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN 
                  DO: /*F3*/ 
                     traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN2" THEN 
                  DO:  /*F4*/ 
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                      
                  END.                                 
                  subqH:GET-NEXT(NO-LOCK).
               END.          
               THIS-OBJECT:VisaExcel:ValjBlad(3).
               traffcol = "C" + STRING(804 + INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                       
            END.         
         END.
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN 
            DO:
               THIS-OBJECT:VisaExcel:ValjBlad(3).
               traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B18","B42",TRUE).
               IF traffcol = ? OR traffcol = "" THEN 
               DO:
                  THIS-OBJECT:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
               END.   
               ELSE 
               DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
                  subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
                  subqH:GET-FIRST().   
                  DO WHILE subqH:QUERY-OFF-END = FALSE:         
                     pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE).
                     IF pristypvar = "BEREDARE" THEN 
                     DO: /*F1*/     
                        traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MONTÖR" THEN 
                     DO:     /*F2*/ 
                        traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MATERIEL" THEN 
                     DO:     
                        traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                     DO:     
                        traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     IF pristypvar = "MASKIN1" THEN 
                     DO: /*F3*/ 
                        traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                     END.
                     IF pristypvar = "MASKIN2" THEN 
                     DO:  /*F4*/ 
                        traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                     END.
                     IF pristypvar = "MASKIN3" THEN 
                     DO:  /*F5*/ 
                        traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                           
                     END.
                     IF pristypvar = "MASKIN4" THEN 
                     DO: /*F6*/ 
                        traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                     END.
                     subqH:GET-NEXT(NO-LOCK).           
                  END.            
                  THIS-OBJECT:VisaExcel:ValjBlad(4).
                  IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN traffcol = "C" + STRING(872 + INTEGER(SUBSTRING(traffcol,2))).
                  ELSE traffcol = "C" + STRING(873 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).            
               END.
            END.        
            ELSE 
            DO:
               /*20112011 och framåt om ej annat framkommer*/
               THIS-OBJECT:VisaExcel:ValjBlad(4).
               traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "C" + STRING(sterad2),"C" + STRING(slerad2),TRUE).
               IF traffcol = ? OR traffcol = "" THEN 
               DO:
                  THIS-OBJECT:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
               END.   
               ELSE 
               DO:
                  traffcol = REPLACE(traffcol,"$","").            
                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
                  subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
                  subqH:GET-FIRST().   
                  DO WHILE subqH:QUERY-OFF-END = FALSE:         
                     pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE).
                     IF pristypvar = "BEREDARE" THEN 
                     DO: /*F1*/     
                        traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MONTÖR" THEN 
                     DO:     /*F2*/ 
                        traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MATERIEL" THEN 
                     DO:     
                        traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                     DO:     
                        traffcol = "R" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     IF pristypvar = "MASKIN1" THEN 
                     DO: /*F3*/ 
                        traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                     END.
                     IF pristypvar = "MASKIN2" THEN 
                     DO:  /*F4*/ 
                        traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                     IF pristypvar = "MASKIN3" THEN 
                     DO:  /*F5*/ 
                        traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                     END.
                     IF pristypvar = "MASKIN4" THEN 
                     DO: /*F6*/ 
                        traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                     subqH:GET-NEXT(NO-LOCK).           
                  END. 
                  THIS-OBJECT:VisaExcel:ValjBlad(5).                     
                  IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN 
                  DO:
                     /*startrad för "FRI"-koder*/                           
                     traffcol = "C" + STRING(1012 + INTEGER(SUBSTRING(traffcol,2))).     
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  END.
                  ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112012" THEN 
                     DO:
                        /*startrad för "FRI"-koder*/               
                        traffcol = "C" + STRING(1013 + INTEGER(SUBSTRING(traffcol,2))).     
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                        traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).               
                     END.            
                     ELSE 
                     DO:            
                        /*startrad för "FRI"-koder 1037 + 20 = 1057*/
                        traffcol = "C" + STRING(frist + INTEGER(SUBSTRING(traffcol,2))).     
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                        traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                  
                     END.   
               END.         
            END.                
         traffcol = "".
         qH:GET-NEXT().        
      END.   
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'EON'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN 
         DO:   
            THIS-OBJECT:VisaExcel:ValjBlad(2).
            traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B27","B31",TRUE).                    
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            IF traffcol NE "" THEN 
            DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN 
                  DO:     
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                  DO:     
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:VisaExcel:ValjBlad(3).
               traffcol = "C" + STRING(806 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                          
            END.         
         END.
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN 
            DO:
               THIS-OBJECT:VisaExcel:ValjBlad(3).                
               traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B4","B13",TRUE).
               IF traffcol = ? OR traffcol = "" THEN 
               DO:
                  THIS-OBJECT:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").              
               END.
               ELSE 
               DO:
                  traffcol = REPLACE(traffcol,"$","").
                  queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
                  subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
                  subqH:GET-FIRST().   
                  DO WHILE subqH:QUERY-OFF-END = FALSE:         
                     pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE).
                     IF pristypvar = "MATERIEL" THEN 
                     DO:     
                        traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                     DO:     
                        traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     subqH:GET-NEXT(NO-LOCK).           
                  END. 
                  THIS-OBJECT:VisaExcel:ValjBlad(4).
                  traffcol = "C" + STRING(879 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                             
               END.
            END.
            ELSE 
            DO:
               /*20112011 och framåt om ej annat framkommer*/
               THIS-OBJECT:VisaExcel:ValjBlad(4).
               traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad3),INPUT "C" + STRING(slerad3),TRUE).       
               IF traffcol = ? OR traffcol = "" THEN 
               DO:
                  THIS-OBJECT:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
               END.
               ELSE 
               DO:
                  traffcol = REPLACE(traffcol,"$","").
                  queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
                  subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
                  subqH:GET-FIRST().   
                  DO WHILE subqH:QUERY-OFF-END = FALSE:         
                     pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE).
                     IF pristypvar = "MATERIEL" THEN 
                     DO:     
                        traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                     DO:     
                        traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     subqH:GET-NEXT(NO-LOCK).           
                  END. 
                  THIS-OBJECT:VisaExcel:ValjBlad(5).
                  traffcol = "C" + STRING(1019 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  THIS-OBJECT:VisaExcel:ValjBlad(5).                          
               END.
            END.                  
         traffcol = "".    
         qH:GET-NEXT().    
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:VisaExcel:SlutExcel().
   END METHOD.
   
   METHOD PRIVATE VOID  LimoAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:LimoAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
      THIS-OBJECT:VisaExcel:ValjBlad(4).    
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:DataOut("A3",refvar + " " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
      THIS-OBJECT:VisaExcel:DataOut("A5",STRING(TODAY,"9999-99-99")). 
      THIS-OBJECT:VisaExcel:DataOut("C3", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:VisaExcel:DataOut("C5",beredarvar).
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:VisaExcel:ValjBlad(4).         
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TKOD"  THEN 
         DO: 
            traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT SUBSTRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,6),INPUT "B10", "B950",TRUE).                    
         END.      
         ELSE traffcol = THIS-OBJECT:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "B10", "B950",TRUE).
         IF traffcol = ? OR traffcol = "" THEN 
         DO:
            THIS-OBJECT:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END.
         ELSE 
         DO:   
            THIS-OBJECT:VisaExcel:ValjBlad(4).                 
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
            IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:VisaExcel:AddKom(INPUT traffcol,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).                    
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:VisaExcel:SlutExcel().
   END METHOD.
   
   METHOD PRIVATE VOID  HamtaAvtalsKalkyl(OUTPUT resid AS INTEGER,OUTPUT ExelKommandout AS CHARACTER):
      DEFINE VARIABLE blobproch AS HANDLE                            NO-UNDO.
      DEFINE VARIABLE kommando  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE globanv   AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE res       AS System.Windows.Forms.DialogResult NO-UNDO.   
      RUN DYNBLOBC.P PERSISTENT SET blobproch. 
      RUN blobfil_UI IN blobproch (INPUT THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE, OUTPUT resid,OUTPUT ExelKommandout).
      IF resid = ? THEN 
      DO:                 
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(48) + " ", "Avtalskalkyl", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).           
         IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
         RETURN.    
      END.
      kommando = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando) NO-ERROR.
      IF  Guru.GlobalaVariabler:plusaonr = ? THEN 
      DO:  
                                        
         kommando = kommando + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.      
      END.
      ELSE 
      DO:
         kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.      
      END.
      OS-COPY VALUE(ExelKommandout) VALUE(kommando).
      OS-DELETE VALUE(ExelKommandout) NO-ERROR.
      ExelKommandout = kommando.
      RUN deleteproc_UI IN blobproch.
      IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
      THIS-OBJECT:VisaExcel:OpenExcel(ExelKommandout). 
   END METHOD.
   
   METHOD PUBLIC VOID ForetagAnpass():
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN 
      DO:
         THIS-OBJECT:ButtonBerMtrl:VISIBLE = TRUE.
      END.
      ELSE THIS-OBJECT:ButtonBerMtrl:VISIBLE = FALSE.
      
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      THIS-OBJECT:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:tiduth:AVAILABLE THEN THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.
      
   END METHOD.
   METHOD PUBLIC VOID ApplyLanguage():
      THIS-OBJECT:EgenSkapa:Text = THIS-OBJECT:Root:LanguageManager:GetString(41).
   END METHOD.
      
END CLASS.

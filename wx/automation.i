&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :               automation.i
                                ============
                    This set of functions is delivered for free
                    and without any responsibility of the creator.
                    You can use all of the features as long as you
                    leave this information in the source program.


    Description :   I recommend that you do not change any function
                    in this include because of the difficult update
                    to the next versions.
                    It's better to add functionality outside this
                    program in a different include.
                    

    Author(s)   :               Marcel FONDACCI (4GL)
                                m.fondacci@4gl.fr
                                
                                Web: www.4gl.fr
    Notes       :  Last version 2.4 from 7-mar-2000 :
                        - Bug in newFile() that does not return
                          a correct com-handle.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOP Auto4GL           2.4      

/*  OFFICE AUTOMATION
    ===============*/
    
    Def var DOCUMENT        as COM-Handle NO-UNDO.
    Def var SELECTION       as COM-Handle NO-UNDO.

    Def var APPLICATION     as COM-Handle NO-UNDO.

Def var wdGoToBookmark          as int no-undo initial -1.
Def var wdCell                  as int no-undo initial 12.
Def var wdRow                   as int no-undo initial 10.
Def var wdSendToNewDocument     as int no-undo initial 0.
Def var wdSendToPrinter         as int no-undo initial 1.
Def var wdSendToFax             as int no-undo initial 3.
Def var wdSendToEmail           as int no-undo initial 2.

Def var wdAlignParagraphCenter     as int no-undo initial 1.
Def var wdAlignParagraphJustify    as int no-undo initial 3.
Def var wdAlignParagraphLeft       as int no-undo initial 0.
Def var wdAlignParagraphRight      as int no-undo initial 2.

Def var wdBorderBottom             as int no-undo initial -3.
Def var wdBorderLeft               as int no-undo initial -2.
Def var wdBorderRight              as int no-undo initial -4.
Def var wdBorderTop                as int no-undo initial -1.

Def var wdLineWidth025pt           as int no-undo initial 2.
Def var wdLineWidth050pt           as int no-undo initial 4.
Def var wdLineWidth075pt           as int no-undo initial 6.
Def var wdLineWidth100pt           as int no-undo initial 8.

Def var wdPageBreak             as int no-undo initial 7.
Def var wdSectionBreakNextPage  as int no-undo initial 2.
Def var wdGotoSection           as int no-undo initial 0.

Def var wdCharacter             as int no-undo initial 1.
Def var wdLowerCase             as int no-undo initial 0.
Def var wdLine                  as int no-undo initial 5.
Def var wdScreen                as int no-undo initial 7.
Def var wdMove                  as int no-undo initial 0.
Def var wdStory                 as int no-undo initial 6.
def var wdSection               as int no-undo initial 8.

Def var wdPrintView             as int no-undo initial 3.
Def var wdSeekCurrentPageHeader as int no-undo initial 9.
Def var wdSeekCurrentPageFooter as int no-undo initial 10.
Def var wdNormalView            as int no-undo initial 1.
Def var wdAdjustNone            as int no-undo initial 0.

Def var wdDoNotSaveChanges      as int no-undo initial 0.
Def var wdSaveChanges           as int no-undo initial -1.

Def var wdTableFormatClassic1   as int no-undo initial 4.
Def var wdTableFormatColumns1   as int no-undo initial 11.
Def var wdTableFormatGrid1      as int no-undo initial 16.
Def var wdTableFormatNone       as int no-undo initial 0.
Def var wdTableFormatSimple2    as int no-undo initial 2.

Def var wdAllowOnlyFormFields   as int no-undo initial 2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addBookMark Procedure 
FUNCTION addBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ A as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addIndexEntry Procedure 
FUNCTION addIndexEntry RETURNS LOGICAL
  ( /* parameter-definitions */ A as CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BackSpace Procedure 
FUNCTION BackSpace RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Borders Procedure 
FUNCTION Borders RETURNS LOGICAL
  ( /* parameter-definitions */ wBordure as int, wOmbrage as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CentimetersToPoints Procedure 
FUNCTION CentimetersToPoints RETURNS DECIMAL
  ( /* parameter-definitions */ i as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deleteBookMark Procedure 
FUNCTION deleteBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD draftMode Procedure 
FUNCTION draftMode RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndFBold Procedure 
FUNCTION EndFBold RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndFItalic Procedure 
FUNCTION EndFItalic RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndFUnderline Procedure 
FUNCTION EndFUnderline RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndKey Procedure 
FUNCTION EndKey RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndOfDoc Procedure 
FUNCTION EndOfDoc RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndOfLine Procedure 
FUNCTION EndOfLine RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExistBookMark Procedure 
FUNCTION ExistBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ a as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExpandSectionSelection Procedure 
FUNCTION ExpandSectionSelection RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FBold Procedure 
FUNCTION FBold RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileClose Procedure 
FUNCTION FileClose RETURNS LOGICAL
  ( /* parameter-definitions */ Withsave as logical  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FilePrint Procedure 
FUNCTION FilePrint RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileQuit Procedure 
FUNCTION FileQuit RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileSave Procedure 
FUNCTION FileSave RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileSaveAs Procedure 
FUNCTION FileSaveAs RETURNS LOGICAL
  ( /* parameter-definitions */ a as Char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FItalic Procedure 
FUNCTION FItalic RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FNormal Procedure 
FUNCTION FNormal RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatParagraph Procedure 
FUNCTION FormatParagraph RETURNS LOGICAL
  ( /* parameter-definitions */ X as decimal, Y as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatTable Procedure 
FUNCTION FormatTable RETURNS LOGICAL
  ( /* parameter-definitions */ I as int,
                                tableFormat as int,
                                ApplyBorders as logical
                                
                                 )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FUnderline Procedure 
FUNCTION FUnderline RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetAutoVersion Procedure 
FUNCTION GetAutoVersion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GotoBookMark Procedure 
FUNCTION GotoBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ A AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD gotoNextSection Procedure 
FUNCTION gotoNextSection RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertCR Procedure 
FUNCTION InsertCR RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertDateTime Procedure 
FUNCTION InsertDateTime RETURNS LOGICAL
  ( /* parameter-definitions */ A as char, asField as logical)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD insertFile Procedure 
FUNCTION insertFile RETURNS LOGICAL
  ( /* parameter-definitions */ A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertImage Procedure 
FUNCTION InsertImage RETURNS LOGICAL
  ( /* parameter-definitions */ A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertRow Procedure 
FUNCTION InsertRow RETURNS LOGICAL
  ( /* parameter-definitions */ I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertSection Procedure 
FUNCTION InsertSection RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD insertSectionSkip Procedure 
FUNCTION insertSectionSkip RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertTable Procedure 
FUNCTION InsertTable RETURNS INT
  ( /* parameter-definitions */ X as int, Y as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InsertText Procedure 
FUNCTION InsertText RETURNS LOGICAL
  ( /* parameter-definitions */ A as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isAutomation Procedure 
FUNCTION isAutomation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Justify Procedure 
FUNCTION Justify RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LeftJustify Procedure 
FUNCTION LeftJustify RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LowerCase Procedure 
FUNCTION LowerCase RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MailToDoc Procedure 
FUNCTION MailToDoc RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MailToPrinter Procedure 
FUNCTION MailToPrinter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MoveDown Procedure 
FUNCTION MoveDown RETURNS LOGICAL
  ( /* parameter-definitions */ I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MoveLeft Procedure 
FUNCTION MoveLeft RETURNS LOGICAL
  ( /* parameter-definitions */ I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD moveRight Procedure 
FUNCTION moveRight RETURNS LOGICAL
  ( /* parameter-definitions */ I as INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewFile Procedure 
FUNCTION NewFile RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextColumn Procedure 
FUNCTION NextColumn RETURNS Logical
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextPageFooter Procedure 
FUNCTION NextPageFooter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextPageHeader Procedure 
FUNCTION NextPageHeader RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextRow Procedure 
FUNCTION NextRow RETURNS Logical
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextWindow Procedure 
FUNCTION NextWindow RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD noDraftMode Procedure 
FUNCTION noDraftMode RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NormalView Procedure 
FUNCTION NormalView RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenDOC Procedure 
FUNCTION OpenDOC RETURNS LOGICAL
  ( /* parameter-definitions */ A AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenDOCReadOnly Procedure 
FUNCTION OpenDOCReadOnly RETURNS LOGICAL
  ( /* parameter-definitions */ A AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenWord Procedure 
FUNCTION OpenWord RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PageFooter Procedure 
FUNCTION PageFooter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PageHeader Procedure 
FUNCTION PageHeader RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PageView Procedure 
FUNCTION PageView RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PCenter Procedure 
FUNCTION PCenter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PreviousWindow Procedure 
FUNCTION PreviousWindow RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProtectDocument Procedure 
FUNCTION ProtectDocument RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RightJustify Procedure 
FUNCTION RightJustify RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetFont Procedure 
FUNCTION SetFont RETURNS LOGICAL
  ( /* parameter-definitions */ A as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSize Procedure 
FUNCTION SetSize RETURNS LOGICAL
  ( /* parameter-definitions */ I as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startOfDoc Procedure 
FUNCTION startOfDoc RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidthColumn Procedure 
FUNCTION WidthColumn RETURNS LOGICAL
  ( /* parameter-definitions */ i as int, j as int, Decimal-ColumnWidth as decimal)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WindowClose Procedure 
FUNCTION WindowClose RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 35.81
         WIDTH              = 204.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 



/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addBookMark Procedure 
FUNCTION addBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ A as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Selection:BookMarks:Add ( 
                               A
                                ).
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addIndexEntry Procedure 
FUNCTION addIndexEntry RETURNS LOGICAL
  ( /* parameter-definitions */ A as CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Add an entry index
    Notes:  
------------------------------------------------------------------------------*/
Def var hRange as com-handle no-undo.

  hRange = Selection:Range.
  
  Document:Indexes:MarkEntry( hRange, A BY-VARIANT-POINTER).
  
  Release Object hRange.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BackSpace Procedure 
FUNCTION BackSpace RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:Range:Delete(, -1).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Borders Procedure 
FUNCTION Borders RETURNS LOGICAL
  ( /* parameter-definitions */ wBordure as int, wOmbrage as int) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
  
With ActiveDocument.Paragraphs(1).Borders    
            .Shadow = True    
            .DistanceFromBottom = numéro    
            .DistanceFromTop = numéro    
            .DistanceFromLeft = numéro    
            .DistanceFromRight = numéro
            End With
With Selection.Paragraphs.Shading    
            .Texture = WdTextureIndex    
            .BackgroundPatternColorIndex = WdColorIndex    
            .ForegroundPatternColorIndex = WdColorIndex
            End With
With ActiveDocument.Paragraphs(1)    
            .Borders(WdBorderType)
            .LineStyle = WdLineStyle    
            .Borders(WdBorderType)
            .LineWidth = WdLineWidth    
            .Borders(WdBorderType)
            .ColorIndex = WdColorIndex
            End With
With Dialogs(wdDialogFormatBordersAndShading)    
            .DefaultTab = WdWordDialogTab    
            .Show
            End With
            
*/            

    Document:Paragraphs(1):Borders(wdBorderBottom):LineWidth = wBordure.
    Document:Paragraphs(1):Borders(wdBorderTop):LineWidth    = wBordure.
    Document:Paragraphs(1):Borders(wdBorderLeft):LineWidth   = wBordure.
    Document:Paragraphs(1):Borders(wdBorderRight):LineWidth  = wBordure.
    
    Document:Paragraphs(1):Shadow = true.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CentimetersToPoints Procedure 
FUNCTION CentimetersToPoints RETURNS DECIMAL
  ( /* parameter-definitions */ i as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN I * 28.35 .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deleteBookMark Procedure 
FUNCTION deleteBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ A as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Document:BookMarks:Item( A BY-VARIANT-POINTER ):Delete().
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION draftMode Procedure 
FUNCTION draftMode RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Draft mode for viewing
    Notes:  
------------------------------------------------------------------------------*/
  Application:ActiveWindow:View:draft = true.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndFBold Procedure 
FUNCTION EndFBold RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  Selection:Font:Bold = False.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndFItalic Procedure 
FUNCTION EndFItalic RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  Selection:Font:Italic = False.

  RETURN TRUE.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndFUnderline Procedure 
FUNCTION EndFUnderline RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:Font:underline = False.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndKey Procedure 
FUNCTION EndKey RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:EndKey(wdLine).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndOfDoc Procedure 
FUNCTION EndOfDoc RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:Endkey( wdStory ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndOfLine Procedure 
FUNCTION EndOfLine RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:Endkey( wdLine ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExistBookMark Procedure 
FUNCTION ExistBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ a as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
    ActiveDocument.Bookmarks.Exists(nom)
*/

  RETURN Document:Bookmarks:Exists( a ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExpandSectionSelection Procedure 
FUNCTION ExpandSectionSelection RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:ExtendMode = true.
  Selection:Expand( wdSection BY-VARIANT-POINTER).
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FBold Procedure 
FUNCTION FBold RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:Font:Bold = True.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileClose Procedure 
FUNCTION FileClose RETURNS LOGICAL
  ( /* parameter-definitions */ Withsave as logical  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
 <com-handle>: Close ( 
          <anytype>-SaveChanges    BY-VARIANT-POINTER,
          <anytype>-OriginalFormat BY-VARIANT-POINTER,
          <anytype>-RouteDocument  BY-VARIANT-POINTER ).
*/
          
  Document:Close( If WithSave then wdSaveChanges else wdDoNotSaveChanges ).       
  Release Object Selection.
  Release Object Document.
  
  If Application:Documents:Count > 0 then
              Document    =   Application:ActiveDocument.
              
  If valid-Handle(Document) then
          SELECTION   =   Document:Application:Selection.
      
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FilePrint Procedure 
FUNCTION FilePrint RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
NO-RETURN-VALUE <com-handle>: PrintOut ( 
          <anytype>-Background BY-VARIANT-POINTER,
          <anytype>-Append BY-VARIANT-POINTER,
          <anytype>-Range BY-VARIANT-POINTER,
          <anytype>-OutputFileName BY-VARIANT-POINTER,
          <anytype>-From BY-VARIANT-POINTER,
          <anytype>-To BY-VARIANT-POINTER,
          <anytype>-Item BY-VARIANT-POINTER,
          <anytype>-Copies BY-VARIANT-POINTER,
          <anytype>-Pages BY-VARIANT-POINTER,
          <anytype>-PageType BY-VARIANT-POINTER,
          <anytype>-PrintToFile BY-VARIANT-POINTER,
          <anytype>-Collate BY-VARIANT-POINTER,
          <anytype>-ActivePrinterMacGX BY-VARIANT-POINTER,
          <anytype>-ManualDuplexPrint BY-VARIANT-POINTER ).
*/
NO-RETURN-VALUE Document:PrintOut (  ).
                  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileQuit Procedure 
FUNCTION FileQuit RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  NO-RETURN-VALUE  Application:QUIT().
  
  If Valid-Handle(Selection) then
            Release Object Selection.
  If Valid-Handle(Document) then
            Release Object Document.
  If Valid-Handle(Application) then
            Release Object Application.              
                        
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileSave Procedure 
FUNCTION FileSave RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
NO-RETURN-VALUE Document:Save (  ).

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileSaveAs Procedure 
FUNCTION FileSaveAs RETURNS LOGICAL
  ( /* parameter-definitions */ a as Char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
NO-RETURN-VALUE <com-handle>: SaveAs ( 
          <anytype>-FileName BY-VARIANT-POINTER,
          <anytype>-FileFormat BY-VARIANT-POINTER,
          <anytype>-LockComments BY-VARIANT-POINTER,
          <anytype>-Password BY-VARIANT-POINTER,
          <anytype>-AddToRecentFiles BY-VARIANT-POINTER,
          <anytype>-WritePassword BY-VARIANT-POINTER,
          <anytype>-ReadOnlyRecommended BY-VARIANT-POINTER,
          <anytype>-EmbedTrueTypeFonts BY-VARIANT-POINTER,
          <anytype>-SaveNativePictureFormat BY-VARIANT-POINTER,
          <anytype>-SaveFormsData BY-VARIANT-POINTER,
          <anytype>-SaveAsAOCELetter BY-VARIANT-POINTER ).
*/

NO-RETURN-VALUE Document:SaveAs ( A BY-VARIANT-POINTER ).         

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FItalic Procedure 
FUNCTION FItalic RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
Selection.Font.Italic = True
*/

  Selection:Font:Italic = True.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FNormal Procedure 
FUNCTION FNormal RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:Font:Bold = False.
  Selection:Font:Underline = False.
  Selection:Font:Italic = False.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormatParagraph Procedure 
FUNCTION FormatParagraph RETURNS LOGICAL
  ( /* parameter-definitions */ X as decimal, Y as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
    With Selection.ParagraphFormat
        .LeftIndent = CentimetersToPoints(2)
        .RightIndent = CentimetersToPoints(3)
        .SpaceBefore = 0
        .SpaceBeforeAuto = False
        .SpaceAfter = 0
        .SpaceAfterAuto = False
        .LineSpacingRule = wdLineSpaceSingle
        .Alignment = wdAlignParagraphLeft
        .WidowControl = True
        .KeepWithNext = False
        .KeepTogether = False
        .PageBreakBefore = False
        .NoLineNumber = False
        .Hyphenation = True
        .FirstLineIndent = CentimetersToPoints(0)
        .OutlineLevel = wdOutlineLevelBodyText
        .CharacterUnitLeftIndent = 0
        .CharacterUnitRightIndent = 0
        .CharacterUnitFirstLineIndent = 0
        .LineUnitBefore = 0
        .LineUnitAfter = 0
    End With
End Sub
*/

  Selection:ParagraphFormat:LeftIndent = X.
  Selection:ParagraphFormat:LeftIndent = Y.
    
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormatTable Procedure 
FUNCTION FormatTable RETURNS LOGICAL
  ( /* parameter-definitions */ I as int,
                                tableFormat as int,
                                ApplyBorders as logical
                                
                                 ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

NO-RETURN-VALUE Document:Tables:Item(I):AutoFormat ( 
          TableFormat BY-VARIANT-POINTER,
          ApplyBorders BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER,
          False BY-VARIANT-POINTER
/*        ,ApplyShading BY-VARIANT-POINTER,
 *        ApplyFont BY-VARIANT-POINTER,
 *        ApplyColor BY-VARIANT-POINTER,
 *        ApplyHeadingRows BY-VARIANT-POINTER,
 *        ApplyLastRow BY-VARIANT-POINTER,
 *        ApplyFirstColumn BY-VARIANT-POINTER,
 *        ApplyLastColumn BY-VARIANT-POINTER,
 *        AutoFit BY-VARIANT-POINTER*/ 
 ).
          
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FUnderline Procedure 
FUNCTION FUnderline RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:Font:underline = True.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetAutoVersion Procedure 
FUNCTION GetAutoVersion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Return the Automation.i version
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "{&Auto4GL}".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GotoBookMark Procedure 
FUNCTION GotoBookMark RETURNS LOGICAL
  ( /* parameter-definitions */ A AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 

         
  If not valid-Handle(Selection) then   do :
                    Message "GOTO" A "impossible à réaliser," skip
                            "Aucune sélection n'est active !"
                            view-as alert-box error.
                    return FALSE.
                    end.
    
  Selection:GoTo ( 
                    wdGoToBookmark BY-VARIANT-POINTER,,,
                    A              BY-VARIANT-POINTER
                  ).   
       
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION gotoNextSection Procedure 
FUNCTION gotoNextSection RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:GotoNext( wdGotoSection ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertCR Procedure 
FUNCTION InsertCR RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:TypeParagraph (  ).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertDateTime Procedure 
FUNCTION InsertDateTime RETURNS LOGICAL
  ( /* parameter-definitions */ A as char, asField as logical) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
NO-RETURN-VALUE Selection:InsertDateTime ( 
          A BY-VARIANT-POINTER,
          asField BY-VARIANT-POINTER
/*        <anytype>-InsertAsFullWidth BY-VARIANT-POINTER,
 *        <anytype>-DateLanguage BY-VARIANT-POINTER,
 *        <anytype>-CalendarType BY-VARIANT-POINTER */
          ).
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION insertFile Procedure 
FUNCTION insertFile RETURNS LOGICAL
  ( /* parameter-definitions */ A as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* InsertFile ( 
 *        Character-FileName,
 *        <anytype>-Range BY-VARIANT-POINTER,
 *        <anytype>-ConfirmConversions BY-VARIANT-POINTER,
 *        <anytype>-Link BY-VARIANT-POINTER,
 *        <anytype>-Attachment BY-VARIANT-POINTER ).*/

  Selection:InsertFile( A,
                        ,
                        ,
                        false BY-VARIANT-POINTER,
                        false BY-VARIANT-POINTER
                        ).        
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertImage Procedure 
FUNCTION InsertImage RETURNS LOGICAL
  ( /* parameter-definitions */ A as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

        Selection:InlineShapes:AddPicture ( 
          A,
          TRUE BY-VARIANT-POINTER,
          FALSE BY-VARIANT-POINTER ).
          
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertRow Procedure 
FUNCTION InsertRow RETURNS LOGICAL
  ( /* parameter-definitions */ I as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
If I > 0 then
  Selection:InsertRows( I by-variant-pointer).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertSection Procedure 
FUNCTION InsertSection RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    NO-RETURN-VALUE  Selection:Range:InsertBreak(wdSectionBreakNextPage BY-VARIANT-POINTER ).
 
    RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION insertSectionSkip Procedure 
FUNCTION insertSectionSkip RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:insertBreak( wdSectionBreakNextPage BY-VARIANT-POINTER ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertTable Procedure 
FUNCTION InsertTable RETURNS INT
  ( /* parameter-definitions */ X as int, Y as int) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
expression.Add(Range, NumRows, NumColumns)

newDoc.Tables.Add(Selection.Range, 3, 5)

*/
Def var hRange as com-handle no-undo.

    hRange = Selection:Range.
    
    Document:Tables:Add ( 
          hRange BY-POINTER,
          X,
          Y ).
          
  Release object hRange.          
          
  RETURN Document:TABLES:COUNT.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InsertText Procedure 
FUNCTION InsertText RETURNS LOGICAL
  ( /* parameter-definitions */ A as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF A <> ? then
              Selection:TypeText ( A ).   

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isAutomation Procedure 
FUNCTION isAutomation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
  CREATE "Word.application.8" Application NO-ERROR.
  
  IF NOT ERROR-STATUS:ERROR then do :
                FileQuit().
                Return TRUE.
                end.
  else
                Return False.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Justify Procedure 
FUNCTION Justify RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:Paragraphs:Alignment = wdAlignParagraphJustify.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LeftJustify Procedure 
FUNCTION LeftJustify RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:Paragraphs:Alignment = wdAlignParagraphLeft.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LowerCase Procedure 
FUNCTION LowerCase RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:range:case( wdLowerCase ).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MailToDoc Procedure 
FUNCTION MailToDoc RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
FusionVersDoc   Documents(nom).MailMerge.Destination = wdSendToNewDocument
FusionVersImprimante    ActiveDocument.MailMerge.Destination = wdSendToPrinter

*/

    Document:MailMerge:Destination = wdSendToNewDocument.
    Document:MailMerge:Execute (  ).
  

  
  Document  = Application:ActiveDocument.
  Selection = Application:Selection.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MailToPrinter Procedure 
FUNCTION MailToPrinter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
FusionVersDoc   Documents(nom).MailMerge.Destination = wdSendToNewDocument
FusionVersImprimante    ActiveDocument.MailMerge.Destination = wdSendToPrinter

*/

    Document:MailMerge:Destination = wdSendToPrinter.
    Document:MailMerge:Execute (  ).
  

  
  Document  = Application:ActiveDocument.
  Selection = Application:Selection.
  
  RETURN TRUE.   /* Function return value. */
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MoveDown Procedure 
FUNCTION MoveDown RETURNS LOGICAL
  ( /* parameter-definitions */ I as int ) :
/*------------------------------------------------------------------------------
  Purpose:  I = # of movedown to do
    Notes:  
------------------------------------------------------------------------------*/
  Selection:MoveDown( wdScreen, I, wdMove).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MoveLeft Procedure 
FUNCTION MoveLeft RETURNS LOGICAL
  ( /* parameter-definitions */ I as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:MoveLeft( wdCharacter  BY-VARIANT-POINTER, I  BY-VARIANT-POINTER).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION moveRight Procedure 
FUNCTION moveRight RETURNS LOGICAL
  ( /* parameter-definitions */ I as INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Selection:MoveRight( wdCharacter  BY-VARIANT-POINTER, I  BY-VARIANT-POINTER).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewFile Procedure 
FUNCTION NewFile RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  If Valid-Handle(Selection) then
            Release Object Selection.
  If Valid-Handle(Document) then
            Release Object Document.
            
  Document    =   Application:Documents:Add (,,,true BY-VARIANT-POINTER ).

  SELECTION   =   Document:Application:Selection.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextColumn Procedure 
FUNCTION NextColumn RETURNS Logical
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
Selection.Move Unit:=wdCell, Count:=1
*/


    Selection:MoveRight ( 
          wdCell BY-VARIANT-POINTER,
          1 BY-VARIANT-POINTER ).
          
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextPageFooter Procedure 
FUNCTION NextPageFooter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  NextPageHeader().
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextPageHeader Procedure 
FUNCTION NextPageHeader RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Application:ActiveWindow:View:NextHeaderFooter (  ).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextRow Procedure 
FUNCTION NextRow RETURNS Logical
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Selection:Move ( 
          wdRow BY-VARIANT-POINTER,
          1 BY-VARIANT-POINTER ).
          
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextWindow Procedure 
FUNCTION NextWindow RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
Def var nWindow as INT        NO-UNDO.

  nWindow = Application:ActiveWindow:WindowNumber.  /* Current Window number */
  If nWindow = Application:Windows:Count then do :
               If nWindow = 1 then
                               Return FALSE.
               else
                               nWindow = 0.
               END.
               

    nWindow = nWindow + 1.

    Application:Windows:Item(nWindow by-Variant-pointer):Activate().
    Document    = Application:Documents:Item( nWindow by-Variant-pointer).  
    SELECTION   =   Document:Application:Selection.
    Return TRUE.            

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION noDraftMode Procedure 
FUNCTION noDraftMode RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  reset the draft mode
    Notes:  
------------------------------------------------------------------------------*/
  Application:ActiveWindow:View:draft = false.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NormalView Procedure 
FUNCTION NormalView RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Application:ActiveWindow:View:Type = wdNormalView.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenDOC Procedure 
FUNCTION OpenDOC RETURNS LOGICAL
  ( /* parameter-definitions */ A AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Document    =   Application:Documents:Open ( A ).
  
  SELECTION   =   Document:Application:Selection.
    
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenDOCReadOnly Procedure 
FUNCTION OpenDOCReadOnly RETURNS LOGICAL
  ( /* parameter-definitions */ A AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Document    =   Application:Documents:Open ( A, , True ).
  
  SELECTION   =   Document:Application:Selection.
    
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenWord Procedure 
FUNCTION OpenWord RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  CREATE "Word.application.8" Application NO-ERROR.
  
  RETURN (NOT ERROR-STATUS:ERROR).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PageFooter Procedure 
FUNCTION PageFooter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Application:ActiveWindow:View:Type      = wdPrintView.
  Application:ActiveWindow:View:SeekView  = wdSeekCurrentPageFooter.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PageHeader Procedure 
FUNCTION PageHeader RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Application:ActiveWindow:View:Type      = wdPrintView.
  Application:ActiveWindow:View:SeekView  = wdSeekCurrentPageHeader.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PageView Procedure 
FUNCTION PageView RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  Application:ActiveWindow:View:Type = wdPrintView.
  
  RETURN TRUE.   /* Function return value. */
 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PCenter Procedure 
FUNCTION PCenter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
Selection.Paragraphs.Alignment = wdAlignParagraphCenter
*/

  Selection:Paragraphs:Alignment = wdAlignParagraphCenter.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PreviousWindow Procedure 
FUNCTION PreviousWindow RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /* Get the current window number
     ===========================*/

Def var nWindow as INT        NO-UNDO.

  nWindow = Application:ActiveWindow:WindowNumber.  /* Current Window number */
  If nWindow = 1 then DO :
               IF Application:Windows:Count = 1 then
                               Return FALSE.
               else
                                nWindow = Application:Windows:Count + 1.
               END.
               

    nWindow = nWindow - 1.

    Application:Windows:Item(nWindow by-Variant-pointer):Activate().
    Document = Application:Documents:Item( nWindow by-Variant-pointer).  
    SELECTION   =   Document:Application:Selection.
    Return TRUE.
       

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProtectDocument Procedure 
FUNCTION ProtectDocument RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
    True protect the document for any modification.      
*/

NO-RETURN-VALUE Document:Protect ( 
          wdAllowOnlyFormFields).        

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RightJustify Procedure 
FUNCTION RightJustify RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:Paragraphs:Alignment = wdAlignParagraphRight.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetFont Procedure 
FUNCTION SetFont RETURNS LOGICAL
  ( /* parameter-definitions */ A as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:Font:Name = A.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSize Procedure 
FUNCTION SetSize RETURNS LOGICAL
  ( /* parameter-definitions */ I as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Selection:Font:Size = I.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startOfDoc Procedure 
FUNCTION startOfDoc RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
          
  Selection:HomeKey( wdStory BY-VARIANT-POINTER, wdMove BY-VARIANT-POINTER).
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidthColumn Procedure 
FUNCTION WidthColumn RETURNS LOGICAL
  ( /* parameter-definitions */ i as int, j as int, Decimal-ColumnWidth as decimal) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
ActiveDocument.Tables(1).Columns.SetWidth ColumnWidth:=num, 


Selection.Tables(1).Columns(1).PreferredWidth = CentimetersToPoints(2.2)
NO-RETURN-VALUE <com-handle>: SetWidth ( 
          Decimal-ColumnWidth AS FLOAT,
          Integer-RulerStyle ).
          
*/

  NO-RETURN-VALUE Document:Tables:Item( I ):columns:Item( J ):SetWidth ( 
          Decimal-ColumnWidth AS FLOAT,
          wdAdjustNone ).
  RETURN True.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WindowClose Procedure 
FUNCTION WindowClose RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  Application:ActiveWindow:Close().
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


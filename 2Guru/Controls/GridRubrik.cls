 
 /*------------------------------------------------------------------------
    File        : GridRubrik
    Purpose     : 
    Syntax      : egenskaper för en rubrik
    Description : 
    Author(s)   : elpao
    Created     : Thu May 19 16:26:53 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


/* En rubrik i en Grid */
CLASS Controls.GridRubrik : 
   DEFINE PUBLIC VARIABLE comboList AS Infragistics.Win.ValueList NO-UNDO.
   
  
   DEFINE PUBLIC PROPERTY Ordning AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      
   DEFINE PUBLIC PROPERTY OrdningExtent AS INTEGER NO-UNDO {GridRubrikListaExtent.i}
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY Falt AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY FaltExtent AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}
      PUBLIC GET.
      PUBLIC SET.   
   DEFINE PUBLIC PROPERTY FaltFgColor AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      /*
   DEFINE PUBLIC PROPERTY FaltBolt AS LOGICAL  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   */
   DEFINE PUBLIC PROPERTY Rubrik AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY RubrikExtent AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}
      PUBLIC GET.
      PUBLIC SET.   
   DEFINE PUBLIC PROPERTY Bredd AS INTEGER INITIAL 20 NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY Typvar AS CHARACTER NO-UNDO /*"DEC" "INT" "TTRECID" "DATETIME" "LOG" */
      PUBLIC GET.
      PUBLIC SET.
      
 DEFINE PUBLIC PROPERTY DecFormat AS CHARACTER NO-UNDO /*"KALKYL" "EKG" */
     PUBLIC GET.
     PUBLIC SET.
   
   /* Sätts till true om rubriken inte ska visas*/
  DEFINE PUBLIC PROPERTY Gomma AS LOGICAL INITIAL FALSE NO-UNDO
     PUBLIC GET.
     PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY Readonly AS LOGICAL INITIAL TRUE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY DATATYP AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY FORMDATATYP AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      
   /* FUNKAR INTE
    DEFINE PUBLIC PROPERTY AntalExtent AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      */
    DEFINE PUBLIC PROPERTY ExtentNumber AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.  
      /*
   DEFINE PUBLIC PROPERTY Valbar AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.*/
     
   
   CONSTRUCTOR GridRubrik ():
      comboList = NEW Infragistics.Win.ValueList().
   END CONSTRUCTOR. 
   
   /*
   CONSTRUCTOR GridRubrik (antalextentvar AS INTEGER, falT AS CHARACTER , rubriK AS CHARACTER {GridRubrikListaExtent.i}, readonlY AS LOGICAL, bredD AS INTEGER):     
                            
     DEFINE VARIABLE iext AS INTEGER NO-UNDO.
      ASSIGN 
      THIS-OBJECT:Falt = falT
      THIS-OBJECT:Readonly = readonlY
      THIS-OBJECT:Bredd = bredD.
      THIS-OBJECT:AntalExtent = antalextentvar.
      comboList = NEW Infragistics.Win.ValueList().
      iext = 1.
      
      DO WHILE iext <= antalextentvar:
         THIS-OBJECT:OrdningExtent[iext] = iext.
         THIS-OBJECT:FaltExtent[iext] = falT + "[" + STRING(iext) + "]".
         THIS-OBJECT:RubrikExtent[iext] = rubriK[iext].
         iext = iext + 1.
      END.   

      
   END CONSTRUCTOR.
     */   
   CONSTRUCTOR GridRubrik (falT AS CHARACTER, rubriK AS CHARACTER, readonlY AS LOGICAL, bredD AS INTEGER):      
     
      ASSIGN 
      THIS-OBJECT:Falt = falT
      THIS-OBJECT:Rubrik = rubriK
      THIS-OBJECT:Readonly = readonlY
      THIS-OBJECT:Bredd = bredD.
      comboList = NEW Infragistics.Win.ValueList().
      
   END CONSTRUCTOR.
   CONSTRUCTOR GridRubrik (falT AS CHARACTER, rubriK AS CHARACTER, readonlY AS LOGICAL, bredD AS INTEGER, typvaR AS CHARACTER):      
      ASSIGN 

      THIS-OBJECT:Falt = falT
      THIS-OBJECT:Rubrik = rubriK
      THIS-OBJECT:Readonly = readonlY
      THIS-OBJECT:Bredd = bredD.
      THIS-OBJECT:Typvar = typvaR.
      
      comboList = NEW Infragistics.Win.ValueList().
      
   END CONSTRUCTOR.
   
   CONSTRUCTOR GridRubrik (falT AS CHARACTER, rubriK AS CHARACTER, readonlY AS LOGICAL, bredD AS INTEGER, typvaR AS CHARACTER, decFormat AS CHARACTER):      
      ASSIGN 

      THIS-OBJECT:Falt = falT
      THIS-OBJECT:Rubrik = rubriK
      THIS-OBJECT:Readonly = readonlY
      THIS-OBJECT:Bredd = bredD.
      THIS-OBJECT:Typvar = typvaR.
      THIS-OBJECT:DecFormat = decFormat.
      
      comboList = NEW Infragistics.Win.ValueList().
      
   END CONSTRUCTOR.
   
   CONSTRUCTOR GridRubrik (falT AS CHARACTER, extentvar AS INTEGER, rubriK AS CHARACTER, readonlY AS LOGICAL, bredD AS INTEGER, typvaR AS CHARACTER, decFormat AS CHARACTER):      
      ASSIGN 
      THIS-OBJECT:Extentnumber = extentvar
      THIS-OBJECT:Falt = falT
      THIS-OBJECT:Rubrik = rubriK
      THIS-OBJECT:Readonly = readonlY
      THIS-OBJECT:Bredd = bredD.
      THIS-OBJECT:Typvar = typvaR.
      THIS-OBJECT:DecFormat = decFormat.
      THIS-OBJECT:FaltExtent[extentvar] = falT + "[" + STRING(extentvar) + "]".
      comboList = NEW Infragistics.Win.ValueList().
      
   END CONSTRUCTOR.
   /* funkar inte med extent se GridRubrikLista
   CONSTRUCTOR GridRubrik (antalextentvar AS INTEGER, falT AS CHARACTER , rubriK AS CHARACTER {GridRubrikListaExtent.i}, readonlY AS LOGICAL, bredD AS INTEGER,typvaR AS CHARACTER, decFormat AS CHARACTER):      
     DEFINE VARIABLE iext AS INTEGER NO-UNDO.
      ASSIGN 
      THIS-OBJECT:Falt = falT
      THIS-OBJECT:Readonly = readonlY
      THIS-OBJECT:Bredd = bredD.
      THIS-OBJECT:AntalExtent = antalextentvar.
      THIS-OBJECT:Typvar = typvaR.
      THIS-OBJECT:DecFormat = decFormat.
      comboList = NEW Infragistics.Win.ValueList().
      iext = 1.
      
      DO WHILE iext <= antalextentvar:
         THIS-OBJECT:OrdningExtent[iext] = iext.
         THIS-OBJECT:FaltExtent[iext] = falT + "[" + STRING(iext) + "]".
         THIS-OBJECT:RubrikExtent[iext] = rubriK[iext].
         iext = iext + 1.
      END.   

      
   END CONSTRUCTOR.
   */
   
   METHOD PUBLIC VOID ComboAdd (varbenamning AS CHARACTER):
      comboList:ValueListItems:Add(varbenamning).      
      
   END METHOD.   
   METHOD PUBLIC VOID ComboAdd (varvalue AS INTEGER, varbenamning AS CHARACTER):
      comboList:ValueListItems:Add(varvalue, varbenamning).      
      
   END METHOD.
   METHOD PUBLIC VOID ComboAdd (comboListin  AS Infragistics.Win.ValueList):
      comboList = comboListin.      
      
   END METHOD.
   
   METHOD PUBLIC VOID RensaCombo():
      comboList:ValueListItems:Clear().      
      
   END METHOD.
   
/*   list = NEW Infragistics.Win.ValueList().*/
/*                                           */
/*                                           */
/*   list:ValueListItems:Add(2, "B").        */
/*   list:ValueListItems:Add(3, "C").        */
   
END CLASS.
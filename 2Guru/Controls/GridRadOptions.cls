 
 /*------------------------------------------------------------------------
    File        : GridRadOptions
    Purpose     : 
    Syntax      : egenskaper för en rubrik
    Description : 
    Author(s)   : elpao
    Created     : Thu May 19 16:26:53 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


/* En rubrik i en Grid */
CLASS Controls.GridRadOptions : 
    
  
   DEFINE PUBLIC PROPERTY CellCondition AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY JmfCellName AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.   
   DEFINE PUBLIC PROPERTY CellName AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY WholeRow AS LOGICAL NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY CellNameColor AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET. 
   DEFINE PUBLIC PROPERTY COLOR AS System.Drawing.Color NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY ConditionType AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY CellFont AS System.Drawing.Font NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY CellFunc AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
                                   /*grid     jämför fält VÄRDE  färg   FONT    HELA     VILKA      VAD               */
      /*grid     jämför fält VÄRDE  färg   FONT    HELA     VILKA      VAD               */
   /*  
   CONSTRUCTOR GridRadOptions(INPUT cellname AS CHARACTER,INPUT cellval AS CHARACTER,
                              INPUT cellcol AS System.Drawing.Color,INPUT fontvar AS System.Drawing.Font,
                              INPUT WholeRow AS LOGICAL,INPUT faltlista AS CHARACTER,INPUT funktion AS INTEGER,
                              INPUT jmfhbem AS INTEGER):
      /*antigen är jmfcellname eller cellvalue blanka*/
      
      
      THIS-OBJECT:CellName = cellname.     /*Vilken cell ska jmf*/
      THIS-OBJECT:CellCondition =  cellval. /*värde*/ 
      THIS-OBJECT:COLOR = cellcol.   /*färg*/      
      THIS-OBJECT:CellFont = fontvar.   /*font*/
      THIS-OBJECT:WholeRow =  WholeRow.  /*hela raden*/
      THIS-OBJECT:CellNameColor = faltlista.   /*  lista på fäly "a,b,c," ska avslutas med , */
      THIS-OBJECT:CellFunc = funktion.   /* 0 = bg  1 = fg 2 = font*/
      THIS-OBJECT:ConditionType = jmfhbem.                                  /* 0 = "=" 1 = begins 2 = ends 3= match  4 = "not ="  4 vid logiska värden
                                                                             5 =  '<' 6 = '>'  7 =  '<=' 8 = '>='  */
      
   END CONSTRUCTOR. 
        
   */
   CONSTRUCTOR GridRadOptions():
      
      
   END CONSTRUCTOR.   
END CLASS.
 
 /*------------------------------------------------------------------------
    File        : GuruColorCondition
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue Jun 19 09:44:12 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.Subclasses.GuruColorCondition INHERITS System.Object: 
   DEFINE PUBLIC VARIABLE Grid AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE CellCondition AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE CellCondition2 AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE CellName AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE WholeRow AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE CellNameColor AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE COLOR AS System.Drawing.Color NO-UNDO.
   DEFINE PUBLIC VARIABLE COLOR2 AS System.Drawing.Color NO-UNDO.
   DEFINE PUBLIC VARIABLE ConditionType AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
   DEFINE PUBLIC VARIABLE CellFunc AS INTEGER NO-UNDO.
      
   CONSTRUCTOR PUBLIC GuruColorCondition(INPUT g AS Controls.Grid, INPUT c AS System.Drawing.Color, INPUT cn AS CHARACTER, INPUT ct AS INTEGER, INPUT cc AS CHARACTER):
      THIS-OBJECT:Grid = g.        /*Vilken grid**/            
      THIS-OBJECT:CellCondition = cc.   /*vilket värde Obs! om log skicka in tvärt om!*/
      THIS-OBJECT:CellName = cn.       /*Vilken cell ska jmf*/
      THIS-OBJECT:WholeRow = TRUE.     /*hela raden*/
      THIS-OBJECT:COLOR = c.           /*färg*/
      THIS-OBJECT:ConditionType = ct.  /* 0 = "=" 1 = begins 2 = ends 3= match  4 = "not ="  4 vid logiska värden*/
      THIS-OBJECT:CellNameColor = "".   /*vilken cell om INTE hela raden*/
      
      /*
              public const  int CT_EQUALS = 0;
        public const  int CT_BEGINS = 1;
        public const  int CT_ENDS = 2;
        public const  int CT_CONTAINS = 3;
        public const int CT_NOTEQUALS = 4;
        
      THIS-OBJECT:Grid = g.
      THIS-OBJECT:CellCondition = "0600017".
      THIS-OBJECT:CellName = "enr".
      THIS-OBJECT:WholeRow = TRUE.
      THIS-OBJECT:COLOR = c.
      THIS-OBJECT:ConditionType = 3.
      THIS-OBJECT:CellNameColor = "".
      */
   END CONSTRUCTOR.
   
   CONSTRUCTOR PUBLIC GuruColorCondition(INPUT g AS Controls.Grid, INPUT c AS System.Drawing.Color, INPUT cn AS CHARACTER, INPUT cc AS CHARACTER):
      
      THIS-OBJECT:Grid = g.        /*Vilken grid**/            
      THIS-OBJECT:CellCondition = cc.   /*vilket värde !*/
      THIS-OBJECT:CellName = cn.       /*Vilken cell ska jmf*/
      THIS-OBJECT:WholeRow = TRUE.     /*hela raden*/
      THIS-OBJECT:COLOR = c.           /*färg*/
      THIS-OBJECT:ConditionType = 0.  /* 0 = "=" 1 = begins 2 = ends 3= match  4 = "not ="  4 vid logiska värden*/
      THIS-OBJECT:CellNameColor = "".   /*vilken cell om INTE hela raden*/
   
   END CONSTRUCTOR.
   
    CONSTRUCTOR PUBLIC GuruColorCondition(INPUT g AS Controls.Grid, INPUT c AS System.Drawing.Color, INPUT c2 AS System.Drawing.Color, INPUT cn AS CHARACTER, INPUT cc AS CHARACTER,INPUT cc2 AS CHARACTER):
      
      THIS-OBJECT:Grid = g.        /*Vilken grid**/            
      THIS-OBJECT:CellCondition = cc.   /*vilket värde !*/
      THIS-OBJECT:CellCondition2 = cc2.   /*vilket värde !*/
      THIS-OBJECT:CellName = cn.       /*Vilken cell ska jmf*/
      THIS-OBJECT:WholeRow = TRUE.     /*hela raden*/
      THIS-OBJECT:COLOR = c.           /*färg*/
      THIS-OBJECT:COLOR2 = c2.           /*färg*/
      THIS-OBJECT:ConditionType = 0.  /* 0 = "=" 1 = begins 2 = ends 3= match  4 = "not ="  4 vid logiska värden*/
      THIS-OBJECT:CellNameColor = "".   /*vilken cell om INTE hela raden*/
   
   END CONSTRUCTOR.
   
               
   CONSTRUCTOR PUBLIC GuruColorCondition(INPUT g AS Controls.Grid, INPUT c AS System.Drawing.Color, INPUT cn AS CHARACTER, INPUT ct AS INTEGER, INPUT cc AS CHARACTER, INPUT wr AS LOGICAL, INPUT ccc AS CHARACTER):
     
      THIS-OBJECT:Grid = g.            /*grid*/
      THIS-OBJECT:CellCondition = cc.   /*vilket värde Obs! om log skicka in tvärt om!*/
      THIS-OBJECT:CellName = cn.     /*Vilken cell ska jmf*/
      THIS-OBJECT:WholeRow = wr.
      THIS-OBJECT:COLOR = c.
      THIS-OBJECT:ConditionType = ct.
      THIS-OBJECT:CellNameColor = ccc.
      
      
      
   END CONSTRUCTOR.
   
   CONSTRUCTOR PUBLIC GuruColorCondition(INPUT g AS Controls.Grid, INPUT c AS System.Drawing.Color, INPUT cn AS CHARACTER, INPUT ct AS INTEGER, INPUT cc AS CHARACTER, INPUT wr AS LOGICAL, INPUT ccc AS CHARACTER, INPUT cf AS System.Drawing.Font, INPUT celfunk AS INTEGER):
     
      THIS-OBJECT:Grid = g.            /*grid*/
      THIS-OBJECT:CellCondition = cc.   /*!*/
      THIS-OBJECT:CellName = cn.     /*Vilken cell ska jmf*/
      THIS-OBJECT:WholeRow = wr.
      THIS-OBJECT:COLOR = c.
      THIS-OBJECT:CellNameColor = ccc.   /*  ska avslutas med , */
      THIS-OBJECT:CellFont = cf.
      THIS-OBJECT:CellFunc = celfunk.
      THIS-OBJECT:ConditionType = 0.
      
   END CONSTRUCTOR.
   
  
        
           
   CONSTRUCTOR PUBLIC GuruColorCondition(INPUT g AS Controls.Grid,INPUT cn AS CHARACTER,INPUT cc AS CHARACTER,INPUT c AS System.Drawing.Color,INPUT incellfont AS System.Drawing.Font,INPUT wr AS LOGICAL,INPUT ccc AS CHARACTER,INPUT cfunk AS INTEGER):
     
      THIS-OBJECT:Grid = g.            /*grid*/
      THIS-OBJECT:CellName = cn.       /*jmf cell*/
      THIS-OBJECT:CellCondition = cc.   /*värde*/
      THIS-OBJECT:COLOR = c.            /*färg*/
      THIS-OBJECT:CellFont = incellfont.  /*font*/
      THIS-OBJECT:WholeRow = wr.         /*hela raden*/
      THIS-OBJECT:ConditionType = 0.     /*alltid lika med*/
      THIS-OBJECT:CellNameColor = ccc.   /*lista på fält*/
      THIS-OBJECT:CellFunc = cfunk.     /* vilken funk 0 = bgc 1 = fgc 2 = font*/
   END CONSTRUCTOR.
   
   
                                                                               /*grid     jämför fält VÄRDE  färg   FONT    HELA     VILKA      VAD               */
     
   
   
   
   
   
   METHOD PUBLIC LOGICAL Compare(INPUT x AS CHARACTER):
      RETURN Elpool.Helpers:CompareString(x, THIS-OBJECT:ConditionType, THIS-OBJECT:CellCondition).
   END METHOD.
   METHOD PUBLIC LOGICAL Compare2(INPUT x AS CHARACTER):
      IF x = THIS-OBJECT:CellCondition THEN RETURN TRUE.
      IF x = THIS-OBJECT:CellCondition2 THEN  RETURN ?.
      RETURN FALSE.
      
   END METHOD.
   /*THIS-OBJECT:ConditionType = ct.  0 = "=" 1 = begins 2 = ends 3= match  4 = "not ="  */
   METHOD PUBLIC LOGICAL Compare3(INPUT x AS CHARACTER):
      IF THIS-OBJECT:ConditionType = 0 THEN DO: 
         IF x = THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      IF THIS-OBJECT:ConditionType = 1 THEN DO: 
         IF x begins THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      /*
      IF THIS-OBJECT:ConditionType = 2 THEN DO: 
         IF x ends THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      */
      IF THIS-OBJECT:ConditionType = 3 THEN DO: 
         IF x MATCHES THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      IF THIS-OBJECT:ConditionType = 4 THEN DO: 
         IF x NE THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      IF THIS-OBJECT:ConditionType = 5 THEN DO: 
         IF x < THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      IF THIS-OBJECT:ConditionType = 6 THEN DO: 
         IF x > THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      IF THIS-OBJECT:ConditionType = 7 THEN DO: 
         IF x <= THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      IF THIS-OBJECT:ConditionType = 8 THEN DO: 
         IF x >= THIS-OBJECT:CellCondition THEN RETURN TRUE.
      END.
      RETURN FALSE.
      
   END METHOD.
   
END CLASS.
 
 /*------------------------------------------------------------------------
    File        : ButtonOver
    Purpose     : Skicka över poster från en Controls.Grid till en annan Controls.Grid
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Wed Oct 26 16:34:59 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.Misc.UltraButton.



CLASS Controls.ButtonOver INHERITS UltraButton: 
   /*------------------------------------------------------------------------------
         Purpose:  		   					  										  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   DEFINE VARIABLE jmfendast AS CHARACTER NO-UNDO.
   DEFINE VARIABLE jmfejlista AS CHARACTER NO-UNDO INITIAL "TTRECID".
   DEFINE TEMP-TABLE rowidlista NO-UNDO 
      FIELD id AS ROWID.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   /*Gridden som posterna hämtas FRÅN */
   DEFINE PROPERTY GuruFromgrid AS Controls.Grid NO-UNDO
      PUBLIC GET.
      PRIVATE SET.
   /*Gridden som posterna ska TILL */
   DEFINE PROPERTY GuruTogrid AS Controls.Grid NO-UNDO
      PUBLIC GET.
      PRIVATE SET.
   /* Ta bort från grid1 */
   DEFINE PUBLIC PROPERTY GuruRemoveFrom AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   /* Skicka över samtliga poster */
   DEFINE PUBLIC PROPERTY GuruAllaover AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   /*Anger om dubletter skall tillåtas, dvs 2 eller fler av samma post */
   DEFINE PUBLIC PROPERTY GuruDublett AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   /* Anger om knappen är kopplad, sätts i constructor */
   DEFINE PRIVATE PROPERTY GuruKopplad AS LOGICAL INITIAL FALSE NO-UNDO
      PRIVATE GET.
      PRIVATE SET. 
   DEFINE PUBLIC PROPERTY GuruFanns AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.   
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
   DEFINE VARIABLE antalmarkerade AS INTEGER NO-UNDO.
	CONSTRUCTOR PUBLIC ButtonOver (  ):
		SUPER ().	
		THIS-OBJECT:InitializeTooltip().	
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

   /*Kopplar samman två grids*/
   METHOD PUBLIC VOID GuruKoppla(grid1 AS Controls.Grid, grid2 AS Controls.Grid):
      ASSIGN
      GuruFromgrid = grid1
      GuruTogrid = grid2
      GuruKopplad = TRUE.
   END METHOD.
   /*Kopplar samman två grids med ytterligare input*/
   METHOD PUBLIC VOID GuruKoppla(grid1 AS Controls.Grid, grid2 AS Controls.Grid, rem AS LOGICAL, alla AS LOGICAL):
      ASSIGN
      GuruFromgrid = grid1
      GuruTogrid = grid2
      GuruRemovefrom = rem
      GuruAllaover = alla.
      GuruKopplad = TRUE.
   END METHOD.
 
   METHOD PUBLIC VOID GuruKoppla(grid1 AS Controls.Grid, grid2 AS Controls.Grid, rem AS LOGICAL, dub AS LOGICAL, alla AS LOGICAL):
      ASSIGN
      GuruFromgrid = grid1
      GuruTogrid = grid2
      GuruRemovefrom = rem
      GuruDublett = dub
      GuruAllaover = alla.
      GuruKopplad = TRUE.
   END METHOD.
   /*SÖKP BARA PÅ DE I jmfendastin*/
   METHOD PUBLIC LOGICAL GuruOver(INPUT jmfendastin AS CHARACTER, INPUT dy AS CHARACTER):
      jmfendast = jmfendastin.
      THIS-OBJECT:GuruOver().
   END METHOD.
   /*SÖK INTE PÅ DE I jmej*/
   METHOD PUBLIC VOID GuruOver(INPUT jmej AS CHARACTER):
      jmfejlista = jmfejlista + ',' + jmej.
      THIS-OBJECT:GuruOver().
   END METHOD.
   /* Skickar över poster */
   METHOD PUBLIC VOID GuruOver():  
     
      DEFINE VARIABLE gridrowid AS ROWID NO-UNDO. 
      EMPTY TEMP-TABLE rowidlista.   
      antalmarkerade = 0.
      DEFINE VARIABLE rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
       
      /* Om man inte ska ta bort från, försök inte skapa posterna i grid där posterna inte togs bort ifrån.
         exempel: man vill inte ta bort poster ur Vallistor, då ska heller inte poster skapas i Vallistan när man "Pilar Bort" 
         i riktning mot Vallistan.*/
      IF GuruRemoveFrom = FALSE THEN DO: 
         /* Markerade poster*/
         IF GuruAllaover = FALSE THEN DO:
               {WALLMAN/foreach.i System.Object oObject in GuruFromgrid:Selected:Rows}                  
               rCurrentRow = CAST (oObject, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .      
               Findskap(rCurrentRow).
            END. /*foreach end*/
            IF oObject = ? THEN DO:                 
               THIS-OBJECT:GuruFromgrid:GuruFirstrow(OUTPUT gridrowid).                                   
               FindskapROWID(gridrowid).
            END.
         END.
         /* Alla poster */
         ELSE IF GuruAllaover = TRUE THEN DO:         
            {WALLMAN/foreach.i System.Object oObject2 in GuruFromgrid:Rows}      
               rCurrentRow = CAST (oObject2, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .  
               Findskap(rCurrentRow).    
            END. /*foreach end*/
         END.
      END.
    
      Bortfrom().
      
      FIND FIRST rowidlista  WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE rowidlista THEN DO:
         GuruTogrid:GuruAvmarkera(). /*så bara nya poster markeras*/
         GuruTogrid:GuruFiltrera().  /* Öppnar Queryn så att de nya posterna syns */   
         GuruTogrid:GuruRadvaljare(TEMP-TABLE rowidlista:DEFAULT-BUFFER-HANDLE). /* väljer tillagda poster i GuruTogrid */
      END.
      
      /*
      EMPTY TEMP-TABLE rowidlista no-error.
      */
   END METHOD.

   METHOD PUBLIC VOID GuruOverefteruppdat():
      GuruTogrid:GuruAvmarkera(). /*så bara nya poster markeras*/
      GuruTogrid:GuruFiltrera().  /* Öppnar Queryn så att de nya posterna syns */
      GuruTogrid:GuruRadvaljare(TEMP-TABLE rowidlista:DEFAULT-BUFFER-HANDLE).
      EMPTY TEMP-TABLE rowidlista NO-ERROR.
      antalmarkerade = 0.
   END METHOD.
   
   /* Kollar om rad är markerad i fromgrid 28/5-13*/
   METHOD PUBLIC LOGICAL RadFromGrid():
      IF THIS-OBJECT:GuruFromgrid:ActiveRow = ? THEN DO:   
         RETURN FALSE.   
      END.
      ELSE RETURN TRUE.
   END METHOD.
   METHOD PUBLIC INTEGER AntalMarkerade():
      RETURN antalmarkerade.
   END METHOD.
   METHOD PUBLIC VOID TomMark():
      EMPTY TEMP-TABLE rowidlista NO-ERROR.
      antalmarkerade = 0.
   END METHOD.
   METHOD PUBLIC VOID GuruRadvaljareU(rid AS ROWID): 
      CREATE rowidlista.
      ASSIGN 
      rowidlista.id = rid.
      antalmarkerade = antalmarkerade + 1.
      GuruTogrid:GuruRadvaljare(TEMP-TABLE rowidlista:DEFAULT-BUFFER-HANDLE).
   END METHOD.   
   
   /*sub till Guruover(), hittar post i GuruFromgrid, lägger till posten i GuruTogrid samt lägger till ROWID för alla tillagda poster i en lista */
   METHOD PRIVATE VOID Findskap(rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow):
      DEFINE VARIABLE rCells AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
      DEFINE VARIABLE ttr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      rCells = rCurrentRow:Cells.      
      ttr = rCells["TTRECID"]:VALUE.
      villkor = "WHERE " + "TTRECID = " + ttr.             
      GuruFromgrid:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.          
      rid = Skapa().
      IF rid = ? THEN.
      ELSE DO:
         CREATE rowidlista.
         ASSIGN 
         rowidlista.id = rid.
         antalmarkerade = antalmarkerade + 1.
      END.
   END METHOD.
   
   METHOD PRIVATE VOID FindskapROWID(gridrowid AS ROWID):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.                 
      GuruFromgrid:Guruegenskap:ttBufferHandle:FIND-BY-ROWID(gridrowid).         
      rid = Skapa().
      IF rid = ? THEN.
      ELSE DO:
         CREATE rowidlista.
         ASSIGN 
         rowidlista.id = rid.
         antalmarkerade = antalmarkerade + 1.
      END.
   END METHOD.
   /* Tar bort från GuruFromgrid */
   METHOD PRIVATE VOID Bortfrom():
      DEFINE VARIABLE safecheck AS LOGICAL NO-UNDO.
      safecheck = GuruFromGrid:canDelete.
      guruFromGrid:canDelete = TRUE.
      IF GuruRemoveFrom = TRUE AND GuruAllaover = FALSE THEN
         GuruFromgrid:GuruBortrader(FALSE).  /* Tar bort markerade rader från GuruFromgrid utan fråga om delete */             
      ELSE IF GuruRemoveFrom = TRUE AND GuruAllaover = TRUE THEN 
         GuruFromgrid:GuruBortalla(). /* Tar bort alla rader från GuruFromgrid */
         
      guruFromGrid:canDelete = safecheck.
   END METHOD.
  
   /*Skapar de nya posterna i GuruTogrid */
   METHOD PRIVATE ROWID Skapa():
      DEFINE VARIABLE compsave AS LOGICAL INITIAL FALSE.      
      IF GuruTogrid:tqH:IS-OPEN = TRUE THEN DO:
         /*Om GuruDublett är SANT -> Jämför EJ, compsave är redan false så post skapas*/
         
         IF GuruDublett = FALSE THEN DO: 
            GuruTogrid:tqH:GET-FIRST(NO-LOCK). 
            DO WHILE GuruTogrid:tqH:QUERY-OFF-END = FALSE:
               /*
               compsave =  GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-COMPARE(GuruFromgrid:Guruegenskap:ttBufferHandle,"","TTRECID, ANDRAD") NO-ERROR.
               */
               IF jmfendast NE "" THEN 
               compsave =  GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-COMPARE(GuruFromgrid:Guruegenskap:ttBufferHandle,"","",jmfendast) NO-ERROR. 
               ELSE compsave =  GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-COMPARE(GuruFromgrid:Guruegenskap:ttBufferHandle,"",jmfejlista) NO-ERROR.
               IF compsave = TRUE THEN DO: 
                  THIS-OBJECT:GuruFanns = TRUE. 
                  RETURN GuruTogrid:Guruegenskap:ttBufferHandle:ROWID.
               END.   
               GuruTogrid:tqH:GET-NEXT(NO-LOCK).                            
            END.
         END.
         IF compsave = FALSE THEN DO: /* skapa post */
            GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-CREATE().
            GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-COPY(GuruFromgrid:Guruegenskap:ttBufferHandle).
            GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = GuruTogrid:Guruegenskap:ttBufferHandle:RECID.
            Guru.GlobalaVariabler:TTRECID = GuruTogrid:Guruegenskap:ttBufferHandle:RECID.

             /*
             MESSAGE 
             GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("TTRECID"):BUFFER-VALUE
           GuruFromgrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("benamning"):BUFFER-VALUE skip
           GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("benamning"):BUFFER-VALUE 
           GuruTogrid:Guruegenskap:ttBufferHandle:table   
           
           GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("klogsubid"):BUFFER-VALUE
           GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("arbkod"):BUFFER-VALUE
           GuruTogrid:Guruegenskap:ttBufferHandle:BUFFER-FIELD("lopnr"):BUFFER-VALUE 
           VIEW-AS ALERT-BOX.
*/
            RETURN GuruTogrid:Guruegenskap:ttBufferHandle:ROWID.          
         END.         
      END.
      RETURN ?.            
   END METHOD.
   
END CLASS.
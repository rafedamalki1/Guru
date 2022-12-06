 
 /*------------------------------------------------------------------------
    File        : GridRubrikListaJoin
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Thu May 19 16:27:10 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


/* Innehåller allt som Controls.Grid behöver för att initiera en grid 
   - RubrikLista (En lista fylld av Controls.GridRubrik)
   - ttBufferHandle (till TT som man använder)
   - filter (WHERE...)
 */
CLASS Controls.GridRubrikListaJoin: 
     
     /*Lista som innehåller alla rubriker, metoden addRubrik lägger till rubriker till listan */
     
   DEFINE PRIVATE PROPERTY RubrikLista AS CLASS System.Collections.ArrayList 
      PRIVATE GET.
      PRIVATE SET.
      /*DEFAULT-BUFFER-HANDLE till TEMP-TABLE för gridden*/
   DEFINE PUBLIC PROPERTY ttBufferHandle AS HANDLE NO-UNDO 
      PUBLIC GET.
      PUBLIC SET.
      /* filter för query, kan även sättas inne i Controls.Grid */
   DEFINE PUBLIC PROPERTY filter AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.      
      /*Anger om man får välja flera rader samtidigt */
   DEFINE PUBLIC PROPERTY multiselect AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      /*Anger om man ska få sortera gridden */
   DEFINE PUBLIC PROPERTY tillatSort AS LOGICAL INITIAL TRUE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      /*Anger om man ska få deletea poster ur gridden*/
   DEFINE PUBLIC PROPERTY tillatDelete AS LOGICAL INITIAL TRUE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY NoAutoTitle AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Bands AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.   
   DEFINE VARIABLE irak AS INTEGER NO-UNDO.  
      
   /*   
   DEFINE PUBLIC PROPERTY OrginalTitle AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      */   
   CONSTRUCTOR PUBLIC GridRubrikListaJoin (  ):      
      SUPER ().            
      RubrikLista = NEW System.Collections.ArrayList().     
         
   END CONSTRUCTOR.
   
   METHOD PUBLIC INTEGER countRubrik():
       RETURN RubrikLista:COUNT. 
   END METHOD.
      /* Hämtar hela rubrik raden */
   METHOD PUBLIC VOID getRubrik(OUTPUT varindex AS System.Object {GridRubrikListaExtent.i}):
     DEFINE VARIABLE sortColumn AS     Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
     DEFINE VARIABLE rub AS Controls.GridRubrik.
     DEFINE VARIABLE i   AS INTEGER  NO-UNDO.
     DO WHILE i < countRubrik():
         sortColumn = CAST(RubrikLista[i],Infragistics.Win.UltraWinGrid.UltraGridColumn).
         varindex[i + 1] = sortColumn.
         i = i + 1.
      END. 
   END METHOD.
   
   /* Hämtar en rubrik från en position i listan*/
   METHOD PUBLIC Controls.GridRubrik getRubrik(varindex AS INTEGER):
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE temprub AS Controls.ObjectWrapper NO-UNDO.            
      temprub = CAST(RubrikLista[varindex], Controls.ObjectWrapper).
      rub = CAST(temprub:ProgressObject, Controls.GridRubrik).  
         
      RETURN rub.      
   END METHOD.
  
  /*Tar emot fältnamn och returnerar tillbaka en rubrik-klass, returnerar ? om rubriken inte finns.   */
   METHOD PUBLIC Controls.Gridrubrik getRubrik(rubb AS CHARACTER): /**/
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE i   AS INTEGER  NO-UNDO.
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:Falt = rubb THEN RETURN rub.         
         i = i + 1.
      END.
      RETURN ?.
   END METHOD.
   
   /*Tar emot FÄLT och returnerar tillbaka en rubrik-klass, returnerar ? om FÄLT inte finns.   */
   METHOD PUBLIC Controls.Gridrubrik GetGridCol(rubb AS CHARACTER): /**/
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE i   AS INTEGER  NO-UNDO.
      
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:Falt = rubb THEN RETURN rub.
         i = i + 1.
      END.
      RETURN ?.
   END METHOD.
   
   
   
   METHOD PUBLIC LOGICAL finnsRubrik(rubb AS CHARACTER): /**/
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:Rubrik = rubb THEN RETURN TRUE.
         
         i = i + 1.
      END.
      RETURN FALSE.
   END METHOD.
   METHOD PUBLIC LOGICAL finnsfalt(falt AS CHARACTER): /**/
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:falt = falt THEN RETURN TRUE.
         
         i = i + 1.
      END.
      RETURN FALSE.
   END METHOD.
  
   /*Observera att om samma rubrik läggs till flera gridrubriklistor kommer ordningen bli lika för samtliga grids
     som för den gridden där rubriken läggs till sist. Om man vill ha olika ordning så måste man separata rubriker för varje grid.*/
   METHOD PUBLIC VOID addRubrik(INPUT rub AS CLASS Controls.GridRubrik):
      rub:Ordning = countRubrik().      
      RubrikLista:Add(NEW Controls.ObjectWrapper(rub)).
   END METHOD.
   METHOD PUBLIC Controls.GridRubrikListaJoin CreateRubrik(INPUT banditvar AS INTEGER, INPUT rubrikergrid AS CLASS Controls.GridRubrik {GridRubrikListaExtent.i}, INPUT multi AS LOGICAL,  INPUT mirak AS INTEGER, INPUT tth AS HANDLE):
      
      DEFINE VARIABLE listagridrubrik    AS Controls.GridRubrikListaJoin. 
      DEFINE VARIABLE ttrecgridrubrik     AS Controls.GridRubrik.
      
      
      listagridrubrik = NEW Controls.GridRubrikListaJoin().
      ASSIGN
      listagridrubrik:multiselect = multi
      listagridrubrik:ttBufferHandle = tth.
      irak = 1.
      
      ttrecgridrubrik = NEW Controls.GridRubrik("TTRECID","TTRECID",TRUE,200,"TTRECID").
      listagridrubrik:addRubrik(ttrecgridrubrik).
      listagridrubrik:Bands = banditvar.
      DO WHILE irak <= mirak:
         IF rubrikergrid[irak] = ? THEN.
         ELSE DO:
            IF rubrikergrid[irak]:Rubrik = "" THEN DO:
               rubrikergrid[irak]:Rubrik = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):LABEL.
            END.
            IF rubrikergrid[irak]:Rubrik = "_" THEN DO:
               rubrikergrid[irak]:Rubrik = "".
            END.
            rubrikergrid[irak]:FORMDATATYP = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):FORMAT.
            rubrikergrid[irak]:DATATYP = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):DATA-TYPE.
            listagridrubrik:addRubrik(rubrikergrid[irak]).
         END.         
         irak = irak + 1.
      END.

      RETURN listagridrubrik.
   END METHOD.      
   METHOD PUBLIC VOID Sortera():
      RubrikLista:Sort().
   END METHOD.
   
END CLASS.
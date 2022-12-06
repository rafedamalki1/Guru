 
 /*------------------------------------------------------------------------
    File        : GridRubrikLista
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
CLASS Controls.GridRubrikLista: 
     
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
   DEFINE VARIABLE irak AS INTEGER NO-UNDO.  
   DEFINE PUBLIC PROPERTY ExtentAntal AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   
   DEFINE VARIABLE senasteextent AS INTEGER NO-UNDO.
      
   /*   
   DEFINE PUBLIC PROPERTY OrginalTitle AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      */   
   CONSTRUCTOR PUBLIC GridRubrikLista (  ):      
      SUPER ().            
      RubrikLista = NEW System.Collections.ArrayList().     
         
   END CONSTRUCTOR.
   
   METHOD PUBLIC INTEGER countRubrik():
      RETURN RubrikLista:COUNT. 
   END METHOD.
   
   /*
   METHOD PUBLIC Controls.GridRubrik SetRubrik(varindex AS INTEGER):
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE temprub AS Controls.ObjectWrapper NO-UNDO.            
      temprub = CAST(RubrikLista[varindex], Controls.ObjectWrapper).
      rub = CAST(temprub:ProgressObject, Controls.GridRubrik).  
         
      RETURN rub.      
   END METHOD.
   */
   
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
      DEFINE VARIABLE iext   AS INTEGER  NO-UNDO.
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         
         IF rub:Extentnumber > 0 THEN DO:
           
            IF rub:FaltExtent[rub:Extentnumber] = rubb THEN RETURN rub.
             
         END.   
         ELSE IF rub:Falt = rubb THEN RETURN rub.
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
      DEFINE VARIABLE iext   AS INTEGER  NO-UNDO.
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:Extentnumber > 0 THEN DO:
            IF rub:FaltExtent[rub:Extentnumber] = falt THEN RETURN TRUE.
         END.               
         ELSE IF rub:Falt = falt THEN RETURN TRUE.
         i = i + 1.
      END.
      RETURN FALSE.
      /*DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:Rubrik = rubb THEN RETURN TRUE.
         
         i = i + 1.
      END.
      RETURN FALSE.
      */
      
   END METHOD.
   /*FUNKAR INTE 
   METHOD PUBLIC INTEGER AntalExtent(falt AS CHARACTER):
      DEFINE VARIABLE rub AS Controls.GridRubrik.
      DEFINE VARIABLE i   AS INTEGER  NO-UNDO.
      
      DO WHILE i < countRubrik():
         rub = getRubrik(i).
         IF rub:Falt = falt THEN RETURN rub:AntalExtent.
         i = i + 1.
      END.
      RETURN 0.
      
   END METHOD.
*/
   METHOD PUBLIC INTEGER SenasteExtent():
      senasteextent = senasteextent + 1.
      RETURN senasteextent.
   END METHOD.
   /*Observera att om samma rubrik läggs till flera gridrubriklistor kommer ordningen bli lika för samtliga grids
     som för den gridden där rubriken läggs till sist. Om man vill ha olika ordning så måste man separata rubriker för varje grid.*/
   METHOD PUBLIC VOID addRubrik(INPUT rub AS CLASS Controls.GridRubrik):
      rub:Ordning = countRubrik().      
      RubrikLista:Add(NEW Controls.ObjectWrapper(rub)).
   END METHOD.
   METHOD PUBLIC Controls.GridRubrikLista createRubrik(INPUT rubrikergrid AS CLASS Controls.GridRubrik {GridRubrikListaExtent.i}, INPUT multi AS LOGICAL,  INPUT mirak AS INTEGER, INPUT tth AS HANDLE):
      DEFINE VARIABLE listagridrubrik    AS Controls.GridRubrikLista.
      DEFINE VARIABLE ttrecgridrubrik     AS Controls.GridRubrik.
      listagridrubrik = NEW Controls.GridRubrikLista().
      ASSIGN
      listagridrubrik:multiselect = multi
      listagridrubrik:ttBufferHandle = tth.
      irak = 1.
      ttrecgridrubrik = NEW Controls.GridRubrik("TTRECID","TTRECID",TRUE,200,"TTRECID").
      listagridrubrik:addRubrik(ttrecgridrubrik).
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
      listagridrubrik:tillatSort = tillatSort.
      RETURN listagridrubrik.
   END METHOD.
   /* funkar inte med extent
   METHOD PUBLIC Controls.GridRubrikLista createRubrik(INPUT rubrikergrid AS CLASS Controls.GridRubrik {GridRubrikListaExtent.i}, INPUT multi AS LOGICAL,  INPUT mirak AS INTEGER,INPUT extentvar AS LOGICAL, INPUT tth AS HANDLE):
      DEFINE VARIABLE listagridrubrik    AS Controls.GridRubrikLista.
      DEFINE VARIABLE ttrecgridrubrik     AS Controls.GridRubrik.
      DEFINE VARIABLE irakextent AS INTEGER NO-UNDO.
      listagridrubrik = NEW Controls.GridRubrikLista().
      ASSIGN
      listagridrubrik:multiselect = multi
      listagridrubrik:ttBufferHandle = tth.
      irak = 1.
      ttrecgridrubrik = NEW Controls.GridRubrik("TTRECID","TTRECID",TRUE,200,"TTRECID").
      listagridrubrik:addRubrik(ttrecgridrubrik).
      DO WHILE irak <= mirak:
         IF rubrikergrid[irak] = ? THEN.
         ELSE DO: 
            IF rubrikergrid[irak]:AntalExtent = 0 THEN DO:
               IF rubrikergrid[irak]:Rubrik = "" THEN DO:
                  rubrikergrid[irak]:Rubrik = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):LABEL.
               END.
               rubrikergrid[irak]:FORMDATATYP = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):FORMAT.
               rubrikergrid[irak]:DATATYP = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):DATA-TYPE.
               listagridrubrik:addRubrik(rubrikergrid[irak]).
            END.
            ELSE DO:
               
               irakextent = 1.
               DO WHILE irakextent <= rubrikergrid[irak]:AntalExtent:
                     
                  IF rubrikergrid[irak]:RubrikExtent[irakextent] = "" THEN DO:
                     rubrikergrid[irak]:Rubrik = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):LABEL.
                  END.
                  ELSE DO:
                     rubrikergrid[irak]:Rubrik = rubrikergrid[irak]:RubrikExtent[irakextent].
                  END.   
                  rubrikergrid[irak]:FORMDATATYP = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):FORMAT.
                  rubrikergrid[irak]:DATATYP = tth:BUFFER-FIELD(rubrikergrid[irak]:Falt):DATA-TYPE.
                  MESSAGE rubrikergrid[irak]:Rubrik
                  VIEW-AS ALERT-BOX.
                  listagridrubrik:addRubrik(rubrikergrid[irak]).
                  irakextent = irakextent + 1.
               END.
               /*
               DO WHILE irakextent <= rubrikergrid[irak]:AntalExtent:
                  rubrikergrid[mirak + irakextent] = rubrikergrid[irak].
                  IF rubrikergrid[mirak + irakextent]:RubrikExtent[irakextent] = "" THEN DO:
                     rubrikergrid[mirak + irakextent]:Rubrik = tth:BUFFER-FIELD(rubrikergrid[mirak + irakextent]:Falt):LABEL.
                  END.
                  ELSE DO:
                     rubrikergrid[mirak + irakextent]:Rubrik = rubrikergrid[mirak + irakextent]:RubrikExtent[irakextent].
                  END.   
                  rubrikergrid[mirak + irakextent]:FORMDATATYP = tth:BUFFER-FIELD(rubrikergrid[mirak + irakextent]:Falt):FORMAT.
                  rubrikergrid[mirak + irakextent]:DATATYP = tth:BUFFER-FIELD(rubrikergrid[mirak + irakextent]:Falt):DATA-TYPE.
                  MESSAGE rubrikergrid[mirak + irakextent]:Rubrik mirak + irakextent
                  VIEW-AS ALERT-BOX.
                  listagridrubrik:addRubrik(rubrikergrid[mirak + irakextent]).
                  irakextent = irakextent + 1.
               END.
               */   
            END.
              
         END. 
         irak = irak + 1.
      END.
      RETURN listagridrubrik.
   END METHOD.
   */      
   METHOD PUBLIC VOID Sortera():
      RubrikLista:Sort().
   END METHOD.
   
END CLASS.
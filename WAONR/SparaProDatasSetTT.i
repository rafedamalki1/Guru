
/*------------------------------------------------------------------------
    File        : SparaProDatasSetTT.i
    Purpose     : 

    Syntax      : ANVÄNDS INTE
används inte 
    Description : 

    Author(s)   : elpao
    Created     : Thu Jun 28 12:52:24 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/
FUNCTION detachDataSet{1} RETURNS logic (INPUT phDataSet AS HANDLE):
  DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
  DO iBuff = 1 TO DATASET {1}:NUM-BUFFERS:
    phDataSet:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
  END.

END FUNCTION. /* detachDataSet */

PROCEDURE SparaProDataSet{1}:
   DEFINE INPUT PARAMETER DATASET FOR {1}.
   DEFINE VARIABLE ChDataSet AS HANDLE NO-UNDO.
   DEFINE VARIABLE okreset AS LOGICAL NO-UNDO. 
   
   RUN attach{1}.
   ChDataSet = DATASET {1}:HANDLE.
   RUN SPARADATSET.p (INPUT ChDataSet).
   okreset = detachDataSet{1}(ChDataSet).
   
END PROCEDURE.

PROCEDURE postDataSetFill{1} :
   DEFINE INPUT PARAMETER DATASET FOR {1}.
   DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynfalth AS HANDLE NO-UNDO.
 
   DO iBuff = 1 TO DATASET {1}:NUM-BUFFERS:
      dynbuffh = DATASET {1}:GET-BUFFER-HANDLE(iBuff).
      CREATE QUERY dynqueh.
      dynqueh:SET-BUFFERS(dynbuffh).
      kommandosortquery = "FOR EACH " + dynbuffh:TABLE + ".".    
      dynok = dynqueh:QUERY-PREPARE(kommandosortquery).   
      dynok = dynqueh:QUERY-OPEN() NO-ERROR.
      REPEAT:
         dynqueh:GET-NEXT(NO-LOCK).
         IF dynqueh:QUERY-OFF-END THEN LEAVE.            
         dynfalth = dynbuffh:BUFFER-FIELD("TTRECID").
         dynfalth:BUFFER-VALUE = dynbuffh:RECID.                               
      END.     
   END.
  
END PROCEDURE.

  
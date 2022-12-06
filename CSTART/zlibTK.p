/******************************************************************************

* OBS! INFORMATIONEN NEDAN FYLLS I AV SOURCESAFE, EJ FÖR HAND! OBS! *

$Archive:: /www-helpdesk/webspeed/wrk_dir/pro/zlib.p $
$Author:: Therese                                    $
$Date:: 05-03-04 10:38                               $
$Revision:: 6                                        $

$History:: zlibTK.p                                    $

*****************  Version 6  *****************
User: Therese      Date: 05-03-04   Time: 10:38
Updated in $/www-helpdesk/webspeed/wrk_dir/pro
Ny version

*****************  Version 5  *****************
User: Patrik       Date: 04-08-26   Time: 9:12
Updated in $/www-helpdesk/webspeed/wrk_dir/pro
Lagt till automatisk sourcesafe-information.



******************************************************************************/
/******************************************************************************

  Program:      zlibTK.p

  Written By:   Gordon Campbell
  Written On:   December 2003

  Description:

  Notes:        You must include zlib.i into your program in order to use this
                procedure.

                Also, zlibbind.p must be available in your propath.

  History:

  03/09/04  G Campbell  h_zlibbind is now passed back to the calling program
                        so that we can perform proper 'clean up' of the
                        persistent procedures.

  040428    TK          Search fungerar inte för att hitta filer i WebStars-struktur.

******************************************************************************/

DEFINE INPUT  PARAMETER pdfDir     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER h_zlibbind AS HANDLE NO-UNDO.

RUN VALUE(pdfDir + "zlibbindTK.p") PERSISTENT SET h_zlibBind (INPUT "").

/* -------------------- Begin Function Definitons ------------------------- */
/* Define a stream for the file handling, too bad in progress we can't do this
   in a procedure */
DEFINE STREAM s-compress.

/* Procedures for zlib handling in Progress */
FUNCTION CompressBuffer RETURNS INTEGER
         (INPUT        InputBuffer  AS MEMPTR,
          INPUT-OUTPUT OutputBuffer AS MEMPTR,
          OUTPUT       OutputSize   AS INTEGER) :

  /* Compress a piece of memory and return a pointer to the compressed data,
     in case of failure the size of compressed data = -1
  */

  DEFINE VARIABLE InputSize  AS INTEGER NO-UNDO.
  DEFINE VARIABLE TempBuffer AS MEMPTR  NO-UNDO.

  DEFINE VARIABLE retcode AS INT NO-UNDO.

  InputSize  = GET-SIZE(InputBuffer).
  OutputSize = (InputSize * 1.01) + 12.
  SET-SIZE(TempBuffer) = OutputSize.

  RUN compress IN h_zlibbind
              (TempBuffer,
               INPUT-OUTPUT OutputSize,
               InputBuffer,
               InputSize,
               OUTPUT retcode) .

  IF retcode = 0 THEN
  DO:
    SET-SIZE(OutputBuffer) = OutputSize.
    OutputBuffer = GET-BYTES(TempBuffer, 1, OutputSize).
  END.
  ELSE
     OutputSize = -1.

  SET-SIZE(TempBuffer) = 0.

  RETURN retcode.

END FUNCTION. /* Compress Buffer */

FUNCTION DeCompressBuffer RETURNS INTEGER
        (INPUT  InputBuffer  AS MEMPTR,
         OUTPUT OutputBuffer AS MEMPTR,
         OUTPUT OutputSize   AS INTEGER):

  /* DeCompress a piece of memory and return a pointer to the decompressed data,
     in case of failure the size of decompressed data = -1
  */

  DEFINE VARIABLE InputSize  AS INTEGER NO-UNDO.
  DEFINE VARIABLE TempBuffer AS MEMPTR  NO-UNDO.

  DEFINE VARIABLE retcode AS INT NO-UNDO.

  InputSize  = GET-SIZE(InputBuffer).
  OutputSize = (InputSize * 100).
  SET-SIZE(TempBuffer) = OutputSize.

  RUN uncompress IN h_zlibbind
      (TempBuffer,
       INPUT-OUTPUT OutputSize,
       InputBuffer,
       InputSize,
       OUTPUT retcode).

  IF retcode = 0 THEN
  DO:
    SET-SIZE(OutputBuffer) = OutputSize.
    OutputBuffer = GET-BYTES(TempBuffer, 1, OutputSize).
  END.
  ELSE
     OutputSize = -1.
  SET-SIZE(TempBuffer) = 0.

  RETURN retcode.
END FUNCTION. /* DeCompress Buffer */

/**
PROCEDURE compressfile:
    DEFINE INPUT PARAMETER cInputFile AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cOutputFile AS CHAR NO-UNDO.

    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iFileSize   AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.

    FILE-INFO:FILE-NAME = cInputFile.
    iFileSize = FILE-INFO:FILE-SIZE.

    SET-SIZE(pFileBuf) = iFIleSize.

    INPUT STREAM s-compress FROM  VALUE(cInputFile) BINARY NO-CONVERT.
    IMPORT STREAM s-compress pFileBuf.
    INPUT STREAM s-compress CLOSE.

    RUN compressbuffer(pFileBuf, OUTPUT pTargetBuf, OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN
    DO:
      OUTPUT STREAM s-compress TO VALUE(cOutputFile) BINARY NO-CONVERT.
      EXPORT STREAM s-compress pTargetBuf.
      OUTPUT STREAM s-compress CLOSE.
    END.

    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END PROCEDURE. /* Compress File */
**/

FUNCTION compressfile RETURNS LOGICAL
         (INPUT cInputFile  AS CHARACTER,
          INPUT cOutputFile AS CHARACTER):

    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iFileSize   AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.

    FILE-INFO:FILE-NAME = cInputFile.
    iFileSize = FILE-INFO:FILE-SIZE.

    SET-SIZE(pFileBuf) = iFIleSize.

    INPUT STREAM s-compress FROM  VALUE(cInputFile) BINARY NO-CONVERT.
    IMPORT STREAM s-compress pFileBuf.
    INPUT STREAM s-compress CLOSE.

    compressbuffer(pFileBuf,
                   INPUT-OUTPUT pTargetBuf,
                   OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN
    DO:
      OUTPUT STREAM s-compress TO VALUE(cOutputFile) BINARY NO-CONVERT.
      EXPORT STREAM s-compress pTargetBuf.
      OUTPUT STREAM s-compress CLOSE.
    END.

    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END FUNCTION. /* Compress File */

/*
PROCEDURE decompressfile:
    DEFINE INPUT PARAMETER cInputFile AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cOutputFile AS CHAR NO-UNDO.

    DEFINE VARIABLE iFileSize   AS INT  NO-UNDO.
    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.

    FILE-INFO:FILE-NAME = cInputFile.
    iFileSize = FILE-INFO:FILE-SIZE.

    SET-SIZE(pFileBuf) = iFIleSize.

    INPUT  STREAM s-compress FROM VALUE(cInputFile) BINARY  NO-CONVERT.
    IMPORT STREAM s-compress pFileBuf  .
    INPUT STREAM s-compress CLOSE.

    RUN decompressbuffer(pFileBuf, OUTPUT pTargetBuf, OUTPUT iTargetSize).
    IF iTargetSize > 0 THEN
    DO:
      OUTPUT STREAM s-compress TO VALUE(cOutputFile) BINARY NO-CONVERT.
      EXPORT STREAM s-compress pTargetBuf.
      OUTPUT STREAM s-compress CLOSE.
    END.

    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END PROCEDURE. /* Decompress File */
*/
FUNCTION decompressfile RETURNS LOGICAL
         (INPUT cInputFile  AS CHARACTER,
          INPUT cOutputFile AS CHARACTER):

    DEFINE VARIABLE iFileSize   AS INT  NO-UNDO.
    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.

    FILE-INFO:FILE-NAME = cInputFile.
    iFileSize = FILE-INFO:FILE-SIZE.

    SET-SIZE(pFileBuf) = iFIleSize.

    INPUT  STREAM s-compress FROM VALUE(cInputFile) BINARY  NO-CONVERT.
    IMPORT STREAM s-compress pFileBuf  .
    INPUT STREAM s-compress CLOSE.

    decompressbuffer(pFileBuf,
                     OUTPUT pTargetBuf,
                     OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN
    DO:
      OUTPUT STREAM s-compress TO VALUE(cOutputFile) BINARY NO-CONVERT.
      EXPORT STREAM s-compress pTargetBuf.
      OUTPUT STREAM s-compress CLOSE.
    END.

    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END FUNCTION. /* Decompress File */

/* --------------------- End Function Definitons -------------------------- */

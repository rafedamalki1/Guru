/******************************************************************************

  Program:      zlib8.p
  
  Written By:   Gordon Campbell
  Written On:   December 2003

  Description:  
  
  Notes:        You must include zlib.i into your program in order to use this
                procedure.  
                
                Also, zlibbind.p must be available in your propath.

  History:
  
  04/01/04  M Edu       Updated to handle Progress Version 8
  
******************************************************************************/

DEFINE INPUT  PARAMETER pdfDir     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER h_zlibbind AS HANDLE NO-UNDO.

RUN VALUE(pdfDir + "zlibbind.p") PERSISTENT SET h_zlibBind.

/* -------------------- Begin Function Definitons ------------------------- */
/* Define a stream for the file handling, too bad in progress we can't do this 
   in a procedure */ 
DEFINE STREAM s-compress.

FUNCTION get-file-size RETURNS INTEGER
  (INPUT cInputFile  AS CHARACTER):

  DEFINE VARIABLE i_Size  AS INTEGER NO-UNDO.

  FILE-INFO:FILE-NAME = cInputFile.
  IF FILE-INFO:FILE-TYPE EQ ? OR INDEX(FILE-INFO:FILE-TYPE,"F") EQ 0 THEN
    RETURN 0.
    
  INPUT FROM VALUE(FILE-INFO:FILE-NAME) NO-ECHO BINARY NO-CONVERT.
    SEEK INPUT TO END.
    i_Size = SEEK(INPUT).
  INPUT CLOSE.
  
  RETURN i_Size.

END FUNCTION. /* get-file-size */

FUNCTION copy-memptr-len RETURNS MEMPTR
  (INPUT m_in_buff  AS MEMPTR,
   INPUT i_size     AS INTEGER):

   DEFINE VARIABLE i_cnt      AS INTEGER NO-UNDO.
   DEFINE VARIABLE m_out_buff AS MEMPTR  NO-UNDO.
   
   i_size = MINIMUM(i_size,GET-SIZE(m_in_buff)).
   IF i_size <= 0 THEN RETURN ?.
   
   SET-SIZE(m_out_buff) = i_size + 1.

   DO i_cnt = 1 TO i_size:
     PUT-BYTE(m_out_buff,i_cnt) = GET-BYTE(m_in_buff,i_cnt).
   END.

END FUNCTION.

PROCEDURE load-file:
  DEFINE INPUT  PARAMETER cInputFile  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER m_buff      AS MEMPTR    NO-UNDO.
  
  DEFINE VARIABLE i_size AS INTEGER NO-UNDO.
  DEFINE VARIABLE i_cnt  AS INTEGER NO-UNDO.
  
  i_size = get-file-size(cInputFile).
  IF i_size EQ 0 THEN RETURN.
  
  SET-SIZE(m_buff) = i_size.
  INPUT FROM VALUE(cInputFile) NO-ECHO BINARY NO-CONVERT.
  
  DO i_cnt = 0 TO i_size:
    READKEY.
    PUT-BYTE(m_buff,i_cnt + 1) = LASTKEY.
  END.
  
  INPUT CLOSE.

END PROCEDURE.

PROCEDURE save-file:
  DEFINE INPUT PARAMETER cOutputFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER m_buff      AS MEMPTR    NO-UNDO.
  
  DEFINE VARIABLE i_cnt  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i_size AS INTEGER   NO-UNDO.
  DEFINE VARIABLE c_str  AS CHARACTER NO-UNDO.
  
  i_size = GET-SIZE(m_buff).
  OUTPUT TO VALUE(cOutputFile) BINARY NO-CONVERT.
  REPEAT:
    ASSIGN 
      c_str = GET-STRING(m_buff,i_cnt + 1)
      i_cnt = i_cnt + LENGTH(c_str,'RAW').
    PUT UNFORMATTED c_str.
    IF i_cnt >= i_size THEN LEAVE.
    i_cnt = i_cnt + 1.
    PUT CONTROL NULL.
  END.  
  OUTPUT CLOSE.

  SET-SIZE(m_buff) = 0.
END PROCEDURE.


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
  
  IF retcode = 0 THEN DO:
    SET-SIZE(OutputBuffer) = OutputSize.
    OutputBuffer = copy-memptr-len(TempBuffer,OutputSize).
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
  
  IF retcode = 0 THEN DO:
    SET-SIZE(OutputBuffer) = OutputSize.
    OutputBuffer = copy-memptr-len(TempBuffer,OutputSize).
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
    iFileSize = GetFileSize().

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
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.

    RUN load-file(cInputFile, OUTPUT pFileBuf).
        
    compressbuffer(pFileBuf,
                   INPUT-OUTPUT pTargetBuf,
                   OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN 
      RUN save-file(cOutputFile, pTargetBuf).
    
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

    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.

    RUN load-file(cInputFile, OUTPUT pFileBuf).
    
    decompressbuffer(pFileBuf,
                     OUTPUT pTargetBuf,
                     OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN 
      RUN save-file(cOutputFile, pTargetBuf).
      
    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END FUNCTION. /* Decompress File */

/* --------------------- End Function Definitons -------------------------- */

/******************************************************************************

    Program:        zlib.i
    
    Description:    This include file predefines the zlib funtions that 
                    are created in zlib.p.

    History (M/D/Y):
    
    03/09/04  G Campbell    Don't add as a SUPER ... not required and
                            created h_zlibbind so that we could 'clean up'
                            the persistent procedure correctly.

    04/01/04  G Campbell    Use PROVERSION preprocessor to determine which
                            Zlib procedure to run.

    10/12/04  G Campbell    Do not automatically run zlib.p procedure.  Some
                            people don't use the compression option so there
                            is no need to run it (now gets called in PDFinclude
                            procedure 'pdf_close')

******************************************************************************/



&IF DEFINED( zlib ) = 0 &THEN
  DEFINE VARIABLE h_zlib        AS HANDLE NO-UNDO.
  DEFINE VARIABLE h_zlibbind    AS HANDLE NO-UNDO.

  FUNCTION CompressBuffer RETURNS INTEGER
          (INPUT        InputBuffer  AS MEMPTR,
           INPUT-OUTPUT OutputBuffer AS MEMPTR,
           OUTPUT       OutputSize   AS INTEGER) IN h_zlib.

  FUNCTION DeCompressBuffer RETURNS INTEGER
          (INPUT        InputBuffer  AS MEMPTR,
           OUTPUT       OutputBuffer AS MEMPTR,
           OUTPUT       OutputSize   AS INTEGER) IN h_zlib.


  FUNCTION CompressFile RETURNS LOGICAL
          (INPUT  InputFile  AS CHARACTER,
           INPUT  OutputFile AS CHARACTER) IN h_zlib.

  FUNCTION DeCompressFile RETURNS LOGICAL
          (INPUT  InputFile  AS CHARACTER,
           INPUT  OutputFile AS CHARACTER) IN h_zlib.

  &GLOBAL-DEFINE zlib
&ENDIF

/* end of zlib.i */

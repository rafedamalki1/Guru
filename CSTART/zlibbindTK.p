/******************************************************************************

* OBS! INFORMATIONEN NEDAN FYLLS I AV SOURCESAFE, EJ FÖR HAND! OBS! *

$Archive:: /www-helpdesk/webspeed/wrk_dir/pro/zlibbi $
$Author:: Therese                                    $
$Date:: 05-03-04 10:39                               $
$Revision:: 4                                        $

$History:: zlibbindTK.p                                $

*****************  Version 4  *****************
User: Therese      Date: 05-03-04   Time: 10:39
Updated in $/www-helpdesk/webspeed/wrk_dir/pro
Ny version

*****************  Version 3  *****************
User: Patrik       Date: 04-08-26   Time: 9:12
Updated in $/www-helpdesk/webspeed/wrk_dir/pro
Lagt till automatisk sourcesafe-information.



******************************************************************************/
/***
**** File: zlibbind.p
****
**** Author: Maurits van Rijnen (peg@vanrijnen.nl)
****         www.vanrijnen.nl
****
**** Comments: Some procedures for handling zlib.dll from progress
***/

/* History

  03/01/04  G Campbell  As per Herbert Bayer (herbert.bayer@bundrinno.de)
                        Added Preprocessor support for UNIX environments

  06/23/04  G Campbell  Added ReleaseZlib procedure.  Called from pdf_close
                        in pdf_inc.p. This is to ensure that the DLL is 
                        released from memory.

  08/19/04  G Campbell  When non-Unix make the default ZLIB processor be just
                        zlib1.dll (removed the c:\gord stuff 
*/

DEFINE INPUT  PARAMETER vDummy     AS CHARACTER NO-UNDO.

&IF OPSYS = "UNIX" &THEN
  &GLOBAL-DEFINE zlib   /lib/libz.so.1
&ELSE
  &GLOBAL-DEFINE zlib   zlib1.dll
&ENDIF

/* External procedure definitions */
PROCEDURE compress EXTERNAL "{&zlib}" CDECL PERSISTENT:
    DEFINE INPUT        PARAMETER pDestBuf    AS MEMPTR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iDestSize   AS LONG NO-UNDO.
    DEFINE INPUT        PARAMETER pSourceBuf  AS MEMPTR NO-UNDO.
    DEFINE INPUT        PARAMETER iSourceSize AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER iretcode AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE uncompress EXTERNAL "{&zlib}" CDECL PERSISTENT:
    DEFINE INPUT        PARAMETER pDestBuf    AS MEMPTR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iDestSize   AS LONG NO-UNDO.
    DEFINE INPUT        PARAMETER pSourceBuf  AS MEMPTR NO-UNDO.
    DEFINE INPUT        PARAMETER iSourceSize AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER iretcode AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ReleaseZlib:
  RELEASE EXTERNAL "{&zlib}".
END.

/* end of zlibbind.p */

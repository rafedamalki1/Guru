/******************************************************************************

* OBS! INFORMATIONEN NEDAN FYLLS I AV SOURCESAFE, EJ FÖR HAND! OBS! *

$Archive:: /www-helpdesk/webspeed/wrk_dir/include/zl $
$Author:: Patrik                                     $
$Date:: 05-03-23 9:41                                $
$Revision:: 7                                        $

$History:: zlibTK.i                                    $

*****************  Version 7  *****************
User: Patrik       Date: 05-03-23   Time: 9:41
Updated in $/www-helpdesk/webspeed/wrk_dir/include
Lagt till en saknad parameter

*****************  Version 6  *****************
User: Therese      Date: 05-03-04   Time: 11:11
Updated in $/www-helpdesk/webspeed/wrk_dir/include
sökvägar

*****************  Version 5  *****************
User: Therese      Date: 05-03-04   Time: 9:44
Updated in $/www-helpdesk/webspeed/wrk_dir/include
Ny version

*****************  Version 4  *****************
User: Patrik       Date: 04-08-26   Time: 9:13
Updated in $/www-helpdesk/webspeed/wrk_dir/include
Lagt till automatisk sourcesafe-information.



******************************************************************************/
/******************************************************************************

    Program:        zlib.i

    Description:    This include file predefines the zlib funtions that
                    are created in zlib.p.

    History:

    04/09/04  G Campbell    Don't add as a SUPER ... not required and
                            created h_zlibbind so that we could 'clean up'
                            the persistent procedure correctly.

******************************************************************************/

&IF DEFINED( zlib ) = 0 &THEN
  DEFINE VARIABLE h_zlib        AS HANDLE NO-UNDO.
  DEFINE VARIABLE h_zlibbind    AS HANDLE NO-UNDO.

  IF NOT VALID-HANDLE(h_Zlib) THEN
    RUN zlibTK.p PERSISTENT SET h_zlib ( input "", OUTPUT h_zlibbind).

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

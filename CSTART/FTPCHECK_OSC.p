/**********************************************************************
*                                                                     *
*   Delsystem   :                                                     *
*                                                                     *
*   Program     :   ftpcheck_osc.p                                        *
*                                                                     *
*                   Program som kontrollerar status efter körning av  *
*                   olika FTP-skript                                  *
*                   Indata från parametrar                            *
*                                                                     *
*   Includer    :   ftp_def.i                                         *
*                                                                     *
*   Författare  :   TKW                                               *
*                                                                     *
*   Ändrat av   :                                                     *
*                                                                     *
*   Kommentar   :                                                     *
***********************************************************************/

DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
  
DEFINE INPUT PARAMETER  vAnalysfil     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER  vl_multiple AS LOG.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.

DEF VAR vl_code AS CHAR.
DEF VAR vl_data AS CHAR.
DEFINE VARIABLE vl_status AS INTEGER NO-UNDO.

{FTP_DEF.i}


vl_status = {&NOT_CONNECTED}.

INPUT FROM VALUE(vAnalysfil).


 REPEAT:
     vl_data = "".
     vl_code = "".
     IMPORT vl_code vl_data.
     IF SUBSTRING(vl_data,1,4) = "byte" THEN
         NEXT.
     RUN process(vl_code).
     CASE vl_status.
         WHEN {&LOGIN_ERROR} THEN
         DO:
            CREATE felmeddtemp.
            felmeddtemp.FELMEDD = "Kunde inte logga in...".
            RETURN.
         END.
         WHEN {&FILE_NOT_FOUND} THEN
         DO: 
            CREATE felmeddtemp.
            felmeddtemp.FELMEDD = "Filen finns inte...".
            RETURN.
         END.
         WHEN {&NOT_OK} THEN
         DO:
            CREATE felmeddtemp.
            felmeddtemp.FELMEDD = "Något gick fel...".
            RETURN.
         END.    
         WHEN {&OK} THEN
         DO:
            CREATE felmeddtemp.
            felmeddtemp.FELMEDD = "Allt ok...".
            RETURN.
         END.
     END.
 END.
 /* Säkerhetsgrej om man fått ett okänt fel */
 vl_status =  {&NOT_OK}.
 CREATE felmeddtemp.
 felmeddtemp.FELMEDD = "Något gick fel...".


PROCEDURE process.
    DEFINE INPUT PARAMETER vl_code AS CHAR.
    CASE vl_status.

        WHEN {&NOT_CONNECTED} THEN
        DO:
            IF vl_code = {&FTP_CONNECT} THEN
            DO:
                vl_status = {&CONNECT}.
            END.
        END.

        WHEN {&CONNECT} THEN
        DO:
            IF vl_code = {&FTP_NOT_LOGIN} THEN
            DO:
                vl_status = {&LOGIN_ERROR}.
            END.
            IF vl_code = {&FTP_LOGIN} THEN
            DO:
                vl_status = {&LOGIN}.
            END.
        END.

        WHEN {&LOGIN} THEN
        DO:
            IF (vl_multiple) THEN
            DO:
                IF vl_code = {&FTP_GOODBYE} THEN
                    vl_status = {&OK}.
            END.
            IF vl_code = {&FTP_REQUEST} THEN
            DO:
                vl_status = {&REQUEST}.
            END.
            IF vl_code = {&FTP_NOT_FOUND} THEN
            DO:
                vl_status = {&NOT_FOUND}.
            END.
            IF vl_code = {&FTP_CMD_SUCCESS} THEN
            DO:
                vl_status =  {&CMD_SUCCESS}.
            END.
            IF vl_code = {&FTP_GOODBYE} THEN
            DO:
                vl_status = {&NOTHING_DONE}.
            END.
        END.

        WHEN {&REQUEST} THEN
        DO:
            IF vl_code = {&FTP_REQUEST} THEN
                RETURN.
            IF vl_code = {&FTP_SEND} THEN
            DO:
                vl_status = {&SEND}.
                RETURN.
            END.
            IF vl_code = {&FTP_SEND2} THEN
            DO:
                vl_status = {&SEND2}.
                RETURN.
            END.
            IF vl_code = {&FTP_CMD_SUCCESS} THEN
            DO:
                vl_status = {&CMD_SUCCESS}.
                RETURN.
            END.
            IF vl_code = {&FTP_NOT_FOUND} THEN
            DO:
                vl_status = {&NOT_FOUND}.
                RETURN.
            END.
        END.

        WHEN {&NOT_FOUND} THEN
        DO:
            IF vl_code = {&FTP_GOODBYE} THEN
            DO:
                vl_status =  {&FILE_NOT_FOUND}.
            END.
            IF vl_code = {&FTP_CMD_SUCCESS} THEN
            DO:
                vl_status =  {&SUCCESS}.
            END.
        END.


        WHEN {&SEND} THEN
        DO:
            IF vl_code = {&FTP_SUCCESS} THEN
            DO:
                vl_status = {&SUCCESS}.
            END.
        END.

        WHEN {&SEND2} THEN
        DO:
            IF vl_code = {&FTP_CMD_SUCCESS} THEN
            DO:
                vl_status = {&CMD_SUCCESS}.
            END.
            ELSE IF vl_code = {&FTP_SUCCESS} THEN
            DO:
                vl_status = {&SUCCESS}.
            END.
            ELSE
            DO:
                vl_status =  {&NOT_OK}.
            END.
        END.

        WHEN {&SUCCESS} THEN
        DO:
            vl_status = {&GOODBYE}.
        END.

        WHEN {&CMD_SUCCESS} THEN
        DO:
            IF (vl_multiple) THEN
            DO:
                vl_status = {&LOGIN}.
            END.
            IF vl_code = {&FTP_REQUEST} THEN
            DO:
                vl_status = {&REQUEST}.
            END.
            IF vl_code = {&FTP_NOT_FOUND} THEN
            DO:
                vl_status = {&NOT_FOUND}.
            END.
            IF vl_code = {&FTP_GOODBYE} THEN
            DO:
                vl_status = {&GOODBYE}.
            END.
        END.

    END.
    if vl_status = {&GOODBYE} THEN
    DO:
        IF vl_code = {&FTP_GOODBYE} THEN
        DO:
            vl_status = {&OK}.
        END.
    END.
END.

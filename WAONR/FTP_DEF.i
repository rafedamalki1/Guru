/**********************************************************************
*                                                                     *
*   Delsystem   :                                                     *
*                                                                     *
*   Program     :   ftp_def.i                                         *
*                                                                     *
*                   Includefil för kända resultatkoder från olika     *
*                   FTP-kommandon                                     *
*                                                                     *
*   Includer    :                                                     *
*                                                                     *
*   Författare  :   TKW                                               *
*                                                                     *
*   Ändrat av   :                                                     *
*                                                                     *
*   Kommentar   :                                                     *
***********************************************************************/

&GLOBAL-DEFINE NOT_OK          11
&GLOBAL-DEFINE NOTHING_DONE    14
&GLOBAL-DEFINE FILE_NOT_FOUND  12
&GLOBAL-DEFINE LOGIN_ERROR     31
&GLOBAL-DEFINE NOT_CONNECTED   9
&GLOBAL-DEFINE TIMEOUT         -3
&GLOBAL-DEFINE OK              10005
&GLOBAL-DEFINE MAN             999
&GLOBAL-DEFINE NO_FTP          120
&GLOBAL-DEFINE CONNECT         220
&GLOBAL-DEFINE LOGIN           230
&GLOBAL-DEFINE NOT_LOGIN       530
&GLOBAL-DEFINE REQUEST         200
&GLOBAL-DEFINE SEND            150
&GLOBAL-DEFINE SEND2           125
&GLOBAL-DEFINE SUCCESS         226
&GLOBAL-DEFINE CMD_SUCCESS     250
&GLOBAL-DEFINE NOT_FOUND       550
&GLOBAL-DEFINE GOODBYE         221
&GLOBAL-DEFINE FTP_LOGIN       "230"
&GLOBAL-DEFINE FTP_NOT_LOGIN   "530"
&GLOBAL-DEFINE FTP_CONNECT     "220"
&GLOBAL-DEFINE FTP_REQUEST     "200"
&GLOBAL-DEFINE FTP_SEND        "150"
&GLOBAL-DEFINE FTP_SEND2       "125"
&GLOBAL-DEFINE FTP_SUCCESS     "226"
&GLOBAL-DEFINE FTP_CMD_SUCCESS "250"
&GLOBAL-DEFINE FTP_NOT_FOUND   "550"
&GLOBAL-DEFINE FTP_GOODBYE     "221"
&GLOBAL-DEFINE SFTP_OK                      0
&GLOBAL-DEFINE SFTP_EOF                     1
&GLOBAL-DEFINE SFTP_NoSuchFile              2
&GLOBAL-DEFINE SFTP_PermissionDenied        3
&GLOBAL-DEFINE SFTP_Failure                 4
&GLOBAL-DEFINE SFTP_BadMessage              5
&GLOBAL-DEFINE SFTP_NoConnection            6
&GLOBAL-DEFINE SFTP_ConnectionLost          7
&GLOBAL-DEFINE SFTP_OperationUnsupported    8
&GLOBAL-DEFINE SFTP_InvalidHandle           9
&GLOBAL-DEFINE SFTP_NoSuchPath              10
&GLOBAL-DEFINE SFTP_FileAlreadyExists       11
&GLOBAL-DEFINE SFTP_WriteProtect            12
&GLOBAL-DEFINE SFTP_NoMedia                 13
&GLOBAL-DEFINE SFTP_NoSpaceOnFileSystem     14
&GLOBAL-DEFINE SFTP_QuotaExceeded           15
&GLOBAL-DEFINE SFTP_UnknownPrincipal        16
&GLOBAL-DEFINE SFTP_LockConflict            17
&GLOBAL-DEFINE SFTP_DirectoryNotEmpty       18
&GLOBAL-DEFINE SFTP_NotAdirectory           19
&GLOBAL-DEFINE STFP_InvalidFilename         20
&GLOBAL-DEFINE SFTP_LinkLoop                21
&GLOBAL-DEFINE SFTP_ConnotDelete            22
&GLOBAL-DEFINE SFTP_InvalidParameter        23
&GLOBAL-DEFINE SFTP_FileIsAdirectory        24
&GLOBAL-DEFINE SFTP_RangeLockConflict       25
&GLOBAL-DEFINE SFTP_RangeLockRefused        26
&GLOBAL-DEFINE SFTP_DeletePending           27
&GLOBAL-DEFINE SFTP_FileCorrupt             28
&GLOBAL-DEFINE SFTP_OwnerInvalid            29
&GLOBAL-DEFINE SFTP_GroupInvalid            30
&GLOBAL-DEFINE SFTP_NoMatchByteRangeLock    31


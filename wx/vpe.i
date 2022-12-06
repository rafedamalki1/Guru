/*PreviewDoc() Flag Parameters*/
&GLOB     VPE_SHOW_NORMAL  1
&GLOB     VPE_SHOW_MAXIMIZED  2
&GLOB     VPE_SHOW_HIDE  3
/*Background Modes*/
&GLOB     VBKG_SOLID  0
&GLOB     VBKG_TRANSPARENT  1
&GLOB     VBKG_GRD_LINE  2
&GLOB     VBKG_GRD_RECT  3
&GLOB     VBKG_GRD_ELLIPSE  4
/*Modes for GradientPrint*/
&GLOB     VGRD_PRINT_AUTO  0
&GLOB     VGRD_PRINT_GRADIENT  1
&GLOB     VGRD_PRINT_SOLID  2
/*Hatch Styles*/
&GLOB     hsNone  -1
&GLOB     hsHorizontal  0
&GLOB     hsVertical  1
&GLOB     hsFDiagonal  2
&GLOB     hsBDiagonal  3
&GLOB     hsCross  4
&GLOB     hsDiagCross  5
/*Pen Styles*/
&GLOB     psSolid  0
&GLOB     psDash  1
&GLOB     psDot  2
&GLOB     psDashDot  3
&GLOB     psDashDotDot  4
/*Text-Formatting Attributes*/
&GLOB     ALIGN_LEFT  0
&GLOB     ALIGN_RIGHT  1
&GLOB     ALIGN_CENTER  2
&GLOB     ALIGN_JUSTIFIED  3
&GLOB     ALIGN_PRINT  4
&GLOB     ALIGN_JUSTIFIED_AB  5
&GLOB     ALIGN_PURE  6
&GLOB     ALIGN_LEFT_CUT  7
/*Auto-Break Options*/
&GLOB     AUTO_BREAK_ON  0
&GLOB     AUTO_BREAK_OFF  1
&GLOB     AUTO_BREAK_NO_LIMITS  2
&GLOB     AUTO_BREAK_FULL  3
/*Picture Attributes*/
&GLOB     PIC_MERGE  1
&GLOB     PIC_KEEPIMAGE  2
&GLOB     PIC_DISCARD_DDB_DRAW  4
&GLOB     PIC_KEEP_DDB_PAGE  8
&GLOB     PIC_BESTFIT  16
&GLOB     PIC_IN_FILE  32
&GLOB     PIC_ALLOWLZW  64
&GLOB     PIC_X2YRESOLUTION  128
&GLOB     PIC_DXF_BW  256
&GLOB     PIC_SCALE2GRAY  512
&GLOB     PIC_SCALE2GRAY_FLOAT  1024
&GLOB     PIC_EXACT  32768
/*Flags for SetPictureType*/
&GLOB     PIC_TYPE_AUTO  255
&GLOB     PIC_TYPE_BMP  0
&GLOB     PIC_TYPE_WMF  5
&GLOB     PIC_TYPE_EMF  6
&GLOB     PIC_TYPE_DXF  7
&GLOB     PIC_TYPE_TIFF  64
&GLOB     PIC_TYPE_GIF  65
&GLOB     PIC_TYPE_PCX  66
&GLOB     PIC_TYPE_FLT  67
&GLOB     PIC_TYPE_JPEG  68
&GLOB     PIC_TYPE_PNG  69
/*Printer-/Setup-Flags*/
&GLOB     PRINTDLG_NEVER  0
&GLOB     PRINTDLG_ONFAIL  1
&GLOB     PRINTDLG_ALWAYS  2
&GLOB     PRINTDLG_FULL  4
&GLOB     PRINT_ALL  0
&GLOB     PRINT_EVEN  1
&GLOB     PRINT_ODD  2
&GLOB     PRINT_NOABORTDLG  4
&GLOB     PRINTPOS_ABSOLUTE  0
&GLOB     PRINTPOS_RELATIVE  1
/*Action Parameter of RequestPrint Event*/
&GLOB     PRINT_MSG_ABORT  0
&GLOB     PRINT_MSG_START  1
&GLOB     PRINT_MSG_END  2
&GLOB     PRINT_MSG_SETUPABORT  3
&GLOB     PRINT_MSG_SETUPSTART  4
&GLOB     PRINT_MSG_SETUPEND  5
/*Result codes for the RequestPrint and BeforePrintNewPage events*/
&GLOB     PRINT_ACTION_OK  0
&GLOB     PRINT_ACTION_ABORT  1
&GLOB     PRINT_ACTION_CHANGE  1
/*Values for IsMAPIInstalled*/
&GLOB     VMAPI_NOT_INSTALLED  0
&GLOB     VMAPI_INSTALLED  1
&GLOB     VMAPI_UNSURE  2
/*SetBarcodeParms Parameter Values*/
&GLOB     BCP_BOTTOM  0
&GLOB     BCP_TOP  1
&GLOB     BCP_HIDE  2
/*Barcode types*/
&GLOB   BCT_EAN13           1
&GLOB   BCT_EAN8            2
&GLOB   BCT_UPCA            3
&GLOB   BCT_CODABAR         5
&GLOB   BCT_CODE39          6
&GLOB   BCT_2OF5            7
&GLOB   BCT_INTERLEAVED2OF5 8
&GLOB   BCT_UPCE            9
&GLOB   BCT_EAN13_2         10
&GLOB   BCT_EAN13_5         11
&GLOB   BCT_EAN8_2          12
&GLOB   BCT_EAN8_5          13
&GLOB   BCT_UPCA_2          14
&GLOB   BCT_UPCA_5          15
&GLOB   BCT_UPCE_2          16
&GLOB   BCT_UPCE_5          17
&GLOB   BCT_EAN128A         18
&GLOB   BCT_EAN128B         19
&GLOB   BCT_EAN128C         20
&GLOB   BCT_CODE93          21
&GLOB   BCT_POSTNET         22
&GLOB   BCT_CODE128A        23
&GLOB   BCT_CODE128B        24
&GLOB   BCT_CODE128C        25
&GLOB   BCT_CODE128         26
&GLOB   BCT_EAN128          27
&GLOB   BCT_EAN2            28
&GLOB   BCT_EAN5            29
&GLOB   BCT_CODE39EXT       30
&GLOB   BCT_CODE93EXT       31
&GLOB   BCT_RM4SCC          32
&GLOB   BCT_MSI             33
&GLOB   BCT_ISBN            34
&GLOB   BCT_ISBN_5          35
&GLOB   BCT_IDENTCODE       36
&GLOB   BCT_LEITCODE        37
&GLOB   BCT_PZN             38
&GLOB   BCT_CODE11          39
&GLOB   BCT_2OF5MATRIX      40
&GLOB   BCT_TELEPENA        41
/*Positioning Codes*/
&GLOB     VFREE  -1
&GLOB     VLEFT  -2
&GLOB     VRIGHT  -3
&GLOB     VLEFTMARGIN  -4
&GLOB     VRIGHTMARGIN  -5
&GLOB     VTOP  -6
&GLOB     VBOTTOM  -7
&GLOB     VTOPMARGIN  -8
&GLOB     VBOTTOMMARGIN  -9
&GLOB     VCENTER  -10
/*Return Codes of Rendering Functions for Text and RTF*/
&GLOB     RENDER_NO_BREAK  0
&GLOB     RENDER_BREAK  1
&GLOB     RENDER_SKIP_BREAK  2
/*Error-Codes returned by VpeLastError*/
&GLOB     VERR_OK  0
&GLOB     VERR_COMMON  1
&GLOB     VERR_MEMORY  100
&GLOB     VERR_FILE_OPEN  200
&GLOB     VERR_FILE_DOCVERSION  201
&GLOB     VERR_FILE_CREATE  202
&GLOB     VERR_FILE_READ  203
&GLOB     VERR_FILE_WRITE  204
&GLOB     VERR_FILE_ACCESS  205
&GLOB     VERR_PIC_IMPORT  300
&GLOB     VERR_PIC_NOLICENSE  301
&GLOB     VERR_PIC_DXFCOORD  302
&GLOB     VERR_PIC_EXPORT  350
&GLOB     VERR_MOD_GRAPH_IMP  400
&GLOB     VERR_MOD_GRAPH_PROC  401
&GLOB     VERR_MOD_BARCODE  402
&GLOB     VERR_MOD_CHART  403
&GLOB     VERR_MAIL_LOAD_MAPI  450
&GLOB     VERR_MAIL_CREATE  451
&GLOB     VERR_MAIL_USER_ABORT  452
&GLOB     VERR_MAIL_FAILURE  453
&GLOB     VERR_MAIL_LOGON_FAILURE  454
&GLOB     VERR_MAIL_DISK_FULL  455
&GLOB     VERR_MAIL_INSUFFICIENT_MEMORY  456
&GLOB     VERR_MAIL_ACCESS_DENIED  457
&GLOB     VERR_MAIL_RESERVED  458
&GLOB     VERR_MAIL_TOO_MANY_SESSIONS  459
&GLOB     VERR_MAIL_TOO_MANY_FILES  460
&GLOB     VERR_MAIL_TOO_MANY_RECIPIENTS  461
&GLOB     VERR_MAIL_ATTACHMENT_NOT_FOUND  462
&GLOB     VERR_MAIL_ATTACHMENT_OPEN_FAILURE  463
&GLOB     VERR_MAIL_ATTACHMENT_WRITE_FAILURE  464
&GLOB     VERR_MAIL_UNKNOWN_RECIPIENT  465
&GLOB     VERR_MAIL_BAD_RECIPTYPE  466
&GLOB     VERR_MAIL_NO_MESSAGES  467
&GLOB     VERR_MAIL_INVALID_MESSAGE  468
&GLOB     VERR_MAIL_TEXT_TOO_LARGE  469
&GLOB     VERR_MAIL_INVALID_SESSION  470
&GLOB     VERR_MAIL_TYPE_NOT_SUPPORTED  471
&GLOB     VERR_MAIL_AMBIGUOUS_RECIPIENT  472
&GLOB     VERR_MAIL_MESSAGE_IN_USE  473
&GLOB     VERR_MAIL_NETWORK_FAILURE  474
&GLOB     VERR_MAIL_INVALID_EDITFIELDS  475
&GLOB     VERR_MAIL_INVALID_RECIPS  476
&GLOB     VERR_MAIL_NOT_SUPPORTED  477
&GLOB     VERR_RTF_BRACES  1000
&GLOB     VERR_RTF_OVERFLOW  1001
&GLOB     VERR_RTF_FONTTBL  1002
&GLOB     VERR_RTF_COLORTBL  1003
&GLOB     VERR_TPL_OWNERSHIP  2000
&GLOB     VERR_TPL_PAGE_ALREADY_DUMPED 2001
/*Predefined PageOrientation values*/
&GLOB     VORIENT_PORTRAIT  1
&GLOB     VORIENT_LANDSCAPE  2
/*Predefined DevPrintQuality values*/
&GLOB     VRES_DRAFT  -1
&GLOB     VRES_LOW  -2
&GLOB     VRES_MEDIUM  -3
&GLOB     VRES_HIGH  -4
/*Predefined DevColor values*/
&GLOB     VCOLOR_MONOCHROME  1
&GLOB     VCOLOR_COLOR  2
/*Predefined DevDuplex values*/
&GLOB     VDUP_SIMPLEX  1
&GLOB     VDUP_VERTICAL  2
&GLOB     VDUP_HORIZONTAL  3
/*Predefined DevTTOption values*/
&GLOB     VTT_BITMAP  1
&GLOB     VTT_DOWNLOAD  2
&GLOB     VTT_SUBDEV  3
/*Predefined PageFormat values*/
&GLOB     VPAPER_USER_DEFINED  0
&GLOB     VPAPER_A4  -1
&GLOB     VPAPER_LETTER  -2
&GLOB     VPAPER_LEGAL  -3
&GLOB     VPAPER_CSHEET  -4
&GLOB     VPAPER_DSHEET  -5
&GLOB     VPAPER_ESHEET  -6
&GLOB     VPAPER_LETTERSMALL  -7
&GLOB     VPAPER_TABLOID  -8
&GLOB     VPAPER_LEDGER  -9
&GLOB     VPAPER_STATEMENT  -10
&GLOB     VPAPER_EXECUTIVE  -11
&GLOB     VPAPER_A3  -12
&GLOB     VPAPER_A4SMALL  -13
&GLOB     VPAPER_A5  -14
&GLOB     VPAPER_B4  -15
&GLOB     VPAPER_B5  -16
&GLOB     VPAPER_FOLIO  -17
&GLOB     VPAPER_QUARTO  -18
&GLOB     VPAPER_10X14  -19
&GLOB     VPAPER_11X17  -20
&GLOB     VPAPER_NOTE  -21
&GLOB     VPAPER_ENV_9  -22
&GLOB     VPAPER_ENV_10  -23
&GLOB     VPAPER_ENV_11  -24
&GLOB     VPAPER_ENV_12  -25
&GLOB     VPAPER_ENV_14  -26
&GLOB     VPAPER_ENV_DL  -27
&GLOB     VPAPER_ENV_C5  -28
&GLOB     VPAPER_ENV_C3  -29
&GLOB     VPAPER_ENV_C4  -30
&GLOB     VPAPER_ENV_C6  -31
&GLOB     VPAPER_ENV_C65  -32
&GLOB     VPAPER_ENV_B4  -33
&GLOB     VPAPER_ENV_B5  -34
&GLOB     VPAPER_ENV_B6  -35
&GLOB     VPAPER_ENV_ITALY  -36
&GLOB     VPAPER_ENV_MONARCH  -37
&GLOB     VPAPER_ENV_PERSONAL  -38
&GLOB     VPAPER_FANFOLD_US  -39
&GLOB     VPAPER_FANFOLD_STD_GERMAN  -40
&GLOB     VPAPER_FANFOLD_LGL_GERMAN  -41
/*Paper-Bins*/
&GLOB     VBIN_UNTOUCHED  -1
&GLOB     VBIN_UPPER  1
&GLOB     VBIN_ONLYONE  1
&GLOB     VBIN_LOWER  2
&GLOB     VBIN_MIDDLE  3
&GLOB     VBIN_MANUAL  4
&GLOB     VBIN_ENVELOPE  5
&GLOB     VBIN_ENVMANUAL  6
&GLOB     VBIN_AUTO  7
&GLOB     VBIN_TRACTOR  8
&GLOB     VBIN_SMALLFMT  9
&GLOB     VBIN_LARGEFMT  10
&GLOB     VBIN_LARGECAPACITY  11
&GLOB     VBIN_CASSETTE  14
/*Mail Recipient Classifications*/
&GLOB     VMAIL_ORIG  0
&GLOB     VMAIL_TO  1
&GLOB     VMAIL_CC  2
&GLOB     VMAIL_BCC  3
/*Behavior of preview for page forward / backward action*/
&GLOB     PREVIEW_STAY  0
&GLOB     PREVIEW_JUMPTOP  1
/*Keyboard-Layout*/
&GLOB     VKEY_SCROLL_LEFT  0
&GLOB     VKEY_SCROLL_PAGE_LEFT  1
&GLOB     VKEY_SCROLL_RIGHT  2
&GLOB     VKEY_SCROLL_PAGE_RIGHT  3
&GLOB     VKEY_SCROLL_UP  4
&GLOB     VKEY_SCROLL_PAGE_UP  5
&GLOB     VKEY_SCROLL_DOWN  6
&GLOB     VKEY_SCROLL_PAGE_DOWN  7
&GLOB     VKEY_SCROLL_TOP  8
&GLOB     VKEY_SCROLL_BOTTOM  9
&GLOB     VKEY_PRINT  10
&GLOB     VKEY_MAIL  11
&GLOB     VKEY_1_1  12
&GLOB     VKEY_FULL_PAGE  13
&GLOB     VKEY_ZOOM_IN  14
&GLOB     VKEY_ZOOM_OUT  15
&GLOB     VKEY_GRID  16
&GLOB     VKEY_PAGE_FIRST  17
&GLOB     VKEY_PAGE_LEFT  18
&GLOB     VKEY_PAGE_RIGHT  19
&GLOB     VKEY_PAGE_LAST  20
&GLOB     VKEY_HELP  21
&GLOB     VKEY_INFO  22
&GLOB     VKEY_CLOSE  23
&GLOB     VKEY_GOTO_PAGE  24
/*GUI Languages*/
&GLOB     VGUI_LANGUAGE_ENGLISH  0
&GLOB     VGUI_LANGUAGE_GERMAN  1
&GLOB     VGUI_LANGUAGE_FRENCH  2
&GLOB     VGUI_LANGUAGE_DUTCH  3
&GLOB     VGUI_LANGUAGE_SPANISH  4
&GLOB     VGUI_LANGUAGE_DANISH  5
&GLOB     VGUI_LANGUAGE_SWEDISH  6
&GLOB     VGUI_LANGUAGE_FINNISH  7
&GLOB     VGUI_LANGUAGE_ITALIAN  8
&GLOB     VGUI_LANGUAGE_NORWEGIAN  9
&GLOB     VGUI_LANGUAGE_PORTUGUESE  10
/*Values for RulersMeasure*/
&GLOB     cm  0
&GLOB     inch  1
/*Values for GridMode*/
&GLOB     In_Background  0
&GLOB     In_Foreground  1
/*Old values for Orientation*/
&GLOB     Portrait  0
&GLOB     Landscape  1
/*Predefined Color Values*/
&GLOB     COLOR_BLACK  0
&GLOB     COLOR_DKGRAY  8421504
&GLOB     COLOR_GRAY  12632256
&GLOB     COLOR_LTGRAY  15132390
&GLOB     COLOR_WHITE  16777215
&GLOB     COLOR_DKRED  128
&GLOB     COLOR_RED  192
&GLOB     COLOR_LTRED  255
&GLOB     COLOR_DKORANGE  16639
&GLOB     COLOR_ORANGE  33023
&GLOB     COLOR_LTORANGE  49407
&GLOB     COLOR_DKYELLOW  57568
&GLOB     COLOR_YELLOW  62194
&GLOB     COLOR_LTYELLOW  65535
&GLOB     COLOR_DKGREEN  32768
&GLOB     COLOR_GREEN  49152
&GLOB     COLOR_LTGREEN  65280
&GLOB     COLOR_HIGREEN  8453888
&GLOB     COLOR_BLUEGREEN  8421376
&GLOB     COLOR_OLIVE  32896
&GLOB     COLOR_BROWN  20608
&GLOB     COLOR_DKBLUE  8388608
&GLOB     COLOR_BLUE  16711680
&GLOB     COLOR_LTBLUE  16744448
&GLOB     COLOR_LTLTBLUE  16752640
&GLOB     COLOR_HIBLUE  16760832
&GLOB     COLOR_CYAN  16776960
&GLOB     COLOR_DKPURPLE  8388736
&GLOB     COLOR_PURPLE  12583104
&GLOB     COLOR_MAGENTA  16711935
/*for Compatibility with older versions - do not use*/
&GLOB     DIN_A_4  0
&GLOB     US_Letter  1
&GLOB     User_Defined  -1
/*Edition Values*/
&GLOB     VEDITION_STANDARD  1000
&GLOB     VEDITION_ENHANCED  2000
&GLOB     VEDITION_PROFESSIONAL  3000
&GLOB     VEDITION_ENTERPRISE  4000
&GLOB     VEDITION_INTERACTIVE  5000
/*Values for PictureExportOptions*/
&GLOB     PICEXP_WRITE_COMPRESSED  2
&GLOB     PICEXP_TIFF_NOCOMP  32
&GLOB     PICEXP_TIFF_LZW  64
&GLOB     PICEXP_TIFF_CCITTRLE  96
&GLOB     PICEXP_TIFF_CCITTFAX3  128
&GLOB     PICEXP_TIFF_CCITTFAX4  160
&GLOB     PICEXP_TIFF_PACKBITS  192
&GLOB     PICEXP_TIFF_APPEND  2097152
&GLOB     PICEXP_ALLOWLZW  1024
&GLOB     PICEXP_JPEG_HIQUALITY  61440
&GLOB     PICEXP_JPEG_GOODQUALITY  36864
&GLOB     PICEXP_JPEG_MIDQUALITY  12288
&GLOB     PICEXP_JPEG_LOQUALITY  4096
&GLOB     PICEXP_PNG_INTERLACED  65536
/*Values for PictureExportColorDepth*/
&GLOB     PICEXP_COLOR_MONO  1
&GLOB     PICEXP_COLOR_16  4
&GLOB     PICEXP_COLOR_256  8
&GLOB     PICEXP_COLOR_HI  16
&GLOB     PICEXP_COLOR_TRUE  24
/*Values for PictureExportDither*/
&GLOB     PICEXP_DITHER_NONE  0
&GLOB     PICEXP_DITHER_MONO  1
&GLOB     PICEXP_DITHER_16  2
&GLOB     PICEXP_DITHER_256  3
/*Chart Types*/
&GLOB     VCHART_POINT  0
&GLOB     VCHART_LINE  1
&GLOB     VCHART_BAR  2
&GLOB     VCHART_STACKED_BAR_ABSOLUTE  3
&GLOB     VCHART_STACKED_BAR_PERCENT  4
&GLOB     VCHART_3D_BAR  5
&GLOB     VCHART_3D_STACKED_BAR_ABSOLUTE  6
&GLOB     VCHART_3D_STACKED_BAR_PERCENT  7
&GLOB     VCHART_PIE  8
&GLOB     VCHART_3D_PIE  9
&GLOB     VCHART_AREA_ABSOLUTE  10
&GLOB     VCHART_AREA_PERCENT  11
/*Symbol Types (used in Point-Chart)*/
&GLOB     VCHART_SYMBOL_NONE  -1
&GLOB     VCHART_SYMBOL_SQUARE  0
&GLOB     VCHART_SYMBOL_TRIANGLE  1
&GLOB     VCHART_SYMBOL_CIRCLE  2
&GLOB     VCHART_SYMBOL_CROSS  3
&GLOB     VCHART_SYMBOL_X  4
&GLOB     VCHART_SYMBOL_POINT  5
/*Legend Positions*/
&GLOB     VCHART_LEGENDPOS_NONE  -1
&GLOB     VCHART_LEGENDPOS_RIGHT  0
&GLOB     VCHART_LEGENDPOS_RIGHT_TOP  1
&GLOB     VCHART_LEGENDPOS_RIGHT_BOTTOM  2
&GLOB     VCHART_LEGENDPOS_LEFT  3
&GLOB     VCHART_LEGENDPOS_LEFT_TOP  4
&GLOB     VCHART_LEGENDPOS_LEFT_BOTTOM  5
&GLOB     VCHART_LEGENDPOS_TOP  6
&GLOB     VCHART_LEGENDPOS_BOTTOM  7
/*Label States*/
&GLOB     VCHART_LABEL_NONE  -1
&GLOB     VCHART_LABEL_USER  0
&GLOB     VCHART_LABEL_AUTO  1
/*Grid Types*/
&GLOB     VCHART_GRID_NONE  -1
&GLOB     VCHART_GRID_BOTH_AXIS  0
&GLOB     VCHART_GRID_X_AXIS  1
&GLOB     VCHART_GRID_Y_AXIS  2
/*Character sets*/
&GLOB     csAnsiCharset  0
&GLOB     csDefaultCharset  1
&GLOB     csSymbolCharset  2
&GLOB     csShiftjisCharset  128
&GLOB     csHangeulCharset  129
&GLOB     csGB2312Charset  134
&GLOB     csChineseBig5Charset  136
&GLOB     csOEMCharset  255
&GLOB     csJohabCharset  130
&GLOB     csHebrewCharset  177
&GLOB     csArabicCharset  178
&GLOB     csGreekCharset  161
&GLOB     csTurkishCharset  162
&GLOB     csThaiCharset  222
&GLOB     csEastEuropeCharset  238
&GLOB     csRussianCharset  204
&GLOB     csMacCharset  77
&GLOB     csBalticCharset  186
/* Object Type Information: */
&GLOB     VOBJID_NULL                     16
&GLOB     VOBJID_LINE                     17
&GLOB     VOBJID_POLYLINE                 18
&GLOB     VOBJID_FRAME                    19
&GLOB     VOBJID_TEXT                     20
&GLOB     VOBJID_PICTURE                  21
&GLOB     VOBJID_BARCODE                  22
&GLOB     VOBJID_ELLIPSE                  23
&GLOB     VOBJID_PIE                      24
&GLOB     VOBJID_POLYGON                  25
&GLOB     VOBJID_RTF                      26
&GLOB     VOBJID_CHART                    27
&GLOB     VOBJID_UDO                      28
&GLOB     VOBJID_FORMFIELD                29
&GLOB     VOBJID_RESERVED                 30
&GLOB     VOBJID_DOCDATA                  31
&GLOB     VOBJID_CTRL_FORMFIELD           32
&GLOB     VOBJID_CTRL_CHECKBOX            33
&GLOB     VOBJID_CTRL_RADIOBUTTON         34
&GLOB     VOBJID_CTRL_RADIOBUTTONGROUP    35
&GLOB     VOBJID_RESERVED2                36
/* Form Field: */
&GLOB     VFF_STYLE_NONE 0
&GLOB     VFF_STYLE_1_4  1
&GLOB     VFF_STYLE_1_3  2
&GLOB     VFF_STYLE_1_2  3
&GLOB     VFF_STYLE_2_3  4
&GLOB     VFF_STYLE_3_4  5
&GLOB     VFF_STYLE_1_1  6
&GLOB     VFF_FLAG_DIV_FIRST     1
&GLOB     VFF_FLAG_DIV_LAST      2
&GLOB     VFF_FLAG_ALT_FIRST     4
&GLOB     VFF_FLAG_ALT_LAST      8
&GLOB     VFF_FLAG_AUTO_FONTSIZE 16


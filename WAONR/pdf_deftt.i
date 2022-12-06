/*pdf_deftt.i*/

/* The following defines are used to determine the JPEG Image Height and Width */
&GLOBAL-DEFINE M_SOF0  "0xC0"		/* Start Of Frame N */
&GLOBAL-DEFINE M_SOF1  "0xC1"		/* N indicates which compression process */
&GLOBAL-DEFINE M_SOF2  "0xC2"       /* Only SOF0-SOF2 are now in common use */
&GLOBAL-DEFINE M_SOF3  "0xC3"
&GLOBAL-DEFINE M_SOF5  "0xC5"		/* NB: codes C4 and CC are NOT SOF markers */
&GLOBAL-DEFINE M_SOF6  "0xC6"
&GLOBAL-DEFINE M_SOF7  "0xC7"
&GLOBAL-DEFINE M_SOF9  "0xC9"
&GLOBAL-DEFINE M_SOF10 "0xCA"
&GLOBAL-DEFINE M_SOF11 "0xCB"
&GLOBAL-DEFINE M_SOF13 "0xCD"
&GLOBAL-DEFINE M_SOF14 "0xCE"
&GLOBAL-DEFINE M_SOF15 "0xCF"
&GLOBAL-DEFINE M_SOI   "0xD8"		/* Start Of Image (beginning of datastream) */
&GLOBAL-DEFINE M_EOI   "0xD9"		/* End Of Image (end of datastream) */
&GLOBAL-DEFINE M_SOS   "0xDA"		/* Start Of Scan (begins compressed data) */
&GLOBAL-DEFINE M_APP0  "0xE0"		/* Application-specific marker, type N */
&GLOBAL-DEFINE M_APP12 "0xEC"		/* (we don't bother to list all 16 APPn's) */
&GLOBAL-DEFINE M_COM   "0xFE"		/* COMment */
&GLOBAL-DEFINE M_MARK  "0xFF"   /* Marker */


&GLOBAL-DEFINE BoldOnChar    CHR(1)
&GLOBAL-DEFINE BoldOffChar   CHR(2)
&GLOBAL-DEFINE ItalicOnChar  CHR(3)
&GLOBAL-DEFINE ItalicOffChar CHR(4)
&GLOBAL-DEFINE ColorOnChar   CHR(5)
&GLOBAL-DEFINE ColorOffChar  CHR(6)

/* ---------------------------- Define TEMP-TABLES -------------------------


   The temp-tables are used to store the PDF streams and resources used when
   generating a PDF document */
DEFINE TEMP-TABLE TT_pdf_stream NO-UNDO
    FIELD obj_stream       AS CHARACTER
    FIELD obj_file         AS CHARACTER
    FIELD obj_encrypt      AS LOGICAL INIT FALSE
    FIELD obj_master       AS CHARACTER
    FIELD obj_user         AS CHARACTER
    FIELD obj_access       AS CHARACTER
    FIELD obj_key          AS INTEGER
    FIELD obj_silent       AS LOGICAL INIT FALSE
    FIELD obj_mode         AS CHARACTER
    FIELD obj_CallProc     AS HANDLE
    FIELD obj_footer       AS CHARACTER
    FIELD obj_header       AS CHARACTER
    FIELD obj_last         AS CHARACTER
    FIELD obj_id           AS CHARACTER
    FIELD obj_EncryptDict  AS INTEGER
    FIELD obj_UniqueID     AS CHARACTER
    FIELD obj_P            AS INTEGER
    FIELD obj_DoingText    AS LOGICAL
    FIELD obj_DoingGraphic AS LOGICAL
INDEX obj_stream  AS PRIMARY
      obj_stream.

/* The following temp-table is used to store/track parameters per stream */
DEFINE TEMP-TABLE TT_pdf_param NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD obj_parameter AS CHARACTER
    FIELD obj_valid     AS CHARACTER
    FIELD obj_value     AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream
      obj_parameter.

DEFINE TEMP-TABLE TT_pdf_error NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD obj_func    AS CHARACTER FORMAT "x(20)"
  FIELD obj_error   AS CHARACTER FORMAT "x(40)"
INDEX obj_stream AS PRIMARY
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_page NO-UNDO
  FIELD obj_stream    AS CHARACTER
  FIELD page_nbr      AS INTEGER
  FIELD page_rotate   AS INTEGER
  FIELD page_width    AS INTEGER
  FIELD page_height   AS INTEGER
  FIELD UseTotalPages AS LOGICAL
  FIELD UsePageNo     AS LOGICAL
  FIELD page_use      AS INTEGER
  FIELD page_crop     AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream
      page_nbr.

DEFINE TEMP-TABLE TT_pdf_tool NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD tool_name    AS CHARACTER
  FIELD tool_type    AS CHARACTER
  FIELD tool_handle  AS HANDLE
INDEX obj_stream AS PRIMARY UNIQUE
      obj_stream
      tool_name.

DEFINE TEMP-TABLE TT_pdf_tool_param NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD tool_name    AS CHARACTER
  FIELD tool_param   AS CHARACTER
  FIELD tool_col     AS INTEGER
  FIELD tool_value   AS CHARACTER
INDEX obj_stream AS PRIMARY UNIQUE
      obj_stream
      tool_name
      tool_param
      tool_col.

/* The following temp-table is used to build a list of objects that will appear
   in the PDF document */
DEFINE TEMP-TABLE TT_pdf_object NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD obj_nbr     AS INTEGER
  FIELD obj_desc    AS CHARACTER
  FIELD obj_offset  AS DECIMAL DECIMALS 0 FORMAT "9999999999"
  FIELD gen_nbr     AS INTEGER FORMAT "99999"
  FIELD obj_type    AS CHARACTER FORMAT "X"
  FIELD obj_page    AS INTEGER
INDEX obj_stream AS PRIMARY
      obj_stream
      obj_nbr.

/* The following temp-table is used to build a list of Bookmarks that will appear
   in the PDF document */
DEFINE TEMP-TABLE TT_pdf_bookmark NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD book_obj     AS INTEGER
  FIELD book_nbr     AS INTEGER
  FIELD book_title   AS CHARACTER
  FIELD book_parent  AS INTEGER
  FIELD book_expand  AS LOGICAL
  FIELD book_child   AS INTEGER
  FIELD book_first   AS INTEGER
  FIELD book_last    AS INTEGER
  FIELD book_page    AS INTEGER
  FIELD book_Y       AS INTEGER
INDEX book_nbr AS PRIMARY UNIQUE
      obj_stream
      book_nbr.

/* The following temp-table is used to track Document Information */
DEFINE TEMP-TABLE TT_pdf_info NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD info_attr     AS CHARACTER
    FIELD info_value    AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream.

/* The following temp-table is used to track Links (Annotations)
   loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_annot NO-UNDO
    FIELD obj_stream     AS CHARACTER
    FIELD annot_type     AS CHARACTER
    FIELD annot_color    AS CHARACTER
    FIELD annot_content  AS CHARACTER
    FIELD annot_page     AS INTEGER
    FIELD annot_rect     AS CHARACTER 
    FIELD annot_border   AS INTEGER
    FIELD annot_style    AS CHARACTER
    FIELD annot_obj      AS INTEGER
    FIELD annot_icon     AS CHARACTER
    FIELD annot_add      AS CHARACTER /* For Additional Information */
INDEX obj_stream  AS PRIMARY
      obj_stream.

/* The following temp-table is used to track Images loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_image NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD image_name    AS CHARACTER
    FIELD image_file    AS CHARACTER
    FIELD image_tag     AS CHARACTER
    FIELD image_obj     AS INTEGER
    FIELD image_len     AS INTEGER
    FIELD image_h       AS INTEGER
    FIELD image_w       AS INTEGER
INDEX obj_image AS PRIMARY
      obj_stream
      image_name.

DEFINE TEMP-TABLE TT_pdf_external NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD ext_tag       AS CHARACTER
    FIELD ext_obj       AS INTEGER
    FIELD ext_file      AS CHARACTER
    FIELD ext_len       AS INTEGER
    FIELD ext_Media1    AS DECIMAL DECIMALS 5
    FIELD ext_Media2    AS DECIMAL DECIMALS 5
    FIELD ext_Media3    AS DECIMAL DECIMALS 5
    FIELD ext_Media4    AS DECIMAL DECIMALS 5
    FIELD ext_Crop1     AS DECIMAL DECIMALS 5
    FIELD ext_Crop2     AS DECIMAL DECIMALS 5
    FIELD ext_Crop3     AS DECIMAL DECIMALS 5
    FIELD ext_Crop4     AS DECIMAL DECIMALS 5
    FIELD ext_rotate    AS INTEGER
    FIELD page_id       AS INTEGER    /* This is the new PDF page # */
    FIELD ext_page      AS INTEGER    /* This is the external PDF page # */
INDEX obj_image AS PRIMARY
      obj_stream
      ext_tag.

/* The following temp-table is used to track Fonts loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_font NO-UNDO
    FIELD obj_stream        AS CHARACTER
    FIELD font_name         AS CHARACTER
    FIELD font_file         AS CHARACTER
    FIELD font_afm          AS CHARACTER
    FIELD font_dif          AS CHARACTER
    FIELD font_type         AS CHARACTER
    FIELD font_width        AS CHARACTER
    FIELD font_obj          AS INTEGER
    FIELD font_encoding     AS INTEGER
    FIELD font_descr        AS INTEGER
    FIELD font_stream       AS INTEGER
    FIELD font_len          AS INTEGER
    FIELD font_tag          AS CHARACTER
    FIELD font_embed        AS LOGICAL INIT TRUE
    FIELD afm_ItalicAngle   AS INTEGER
    FIELD afm_Ascender      AS CHARACTER
    FIELD afm_Descender     AS CHARACTER
    FIELD afm_FontBBox      AS CHARACTER
    FIELD afm_FirstChar     AS CHARACTER
    FIELD afm_LastChar      AS CHARACTER
    FIELD afm_Widths        AS CHARACTER
    FIELD afm_IsFixedPitch  AS CHARACTER INIT "0"
    FIELD afm_flags         AS CHARACTER
    FIELD ext_page          AS INTEGER
INDEX font_name AS PRIMARY
      font_name
INDEX obj_stream
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_diff NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD font_name   AS CHARACTER
  FIELD char_num    AS INTEGER
  FIELD PS_name     AS CHARACTER
INDEX obj_stream  AS PRIMARY
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_ReplaceTxt /* peki */ NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD mergenr     AS INTEGER
  FIELD txt_from    AS CHARACTER
  FIELD txt_to      AS CHARACTER
INDEX obj_stream    AS PRIMARY
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_FillTxt NO-UNDO
  FIELD obj_stream    AS CHARACTER
  FIELD page_nbr      AS INTEGER
  FIELD fill_from     AS CHARACTER
  FIELD fill_to       AS CHARACTER
  FIELD fill_options  AS CHARACTER
INDEX obj_stream  AS PRIMARY
      obj_stream
      page_nbr.

DEFINE TEMP-TABLE TT-Merge-Pages NO-UNDO
  FIELD PageFrom    AS INTEGER
  FIELD PageTo      AS INTEGER
  FIELD MergeNr     AS INTEGER.

DEFINE TEMP-TABLE hexarray NO-UNDO
   FIELD hex-val   AS CHARACTER
   FIELD chr-val   AS INTEGER
INDEX hex-idx AS PRIMARY
      hex-val
INDEX chr-idx 
      chr-val.

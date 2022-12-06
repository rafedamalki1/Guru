/******************************************************************************

  Program:      PDFglobalTK.i
  
  Written By:   Gordon Campbell
  Written On:   January 2004
  
  Description:  Defines the temp-tables and other Global-type requirements
  
  06/03/04  G Campbell  Added res_len to TT_Resource table
  
  06/03/04  G Campbell  Added preprocessor PDFDIR to handle application 
                        directories.  Ensure that the proprocessor has a 
                        forward slash at the end of the directory:
                        
                        eg: GOOD -- /gord/pdfinclude/
                            BAD  -- /gord/pdfinclude
                            
                        Or you can leave it blank to run as is.
                        
  06/15/04  G Campbell  Changed PDFDIR from Scoped to Global preprocessor

  06/15/04  G Campbell  Added  TT_pdf_xml temp-table.
  
  06/16/04  G Campbell  Added fields to TT_Font that will handle the Encoding 
                        and FontFile3 elements in a Font Dictionary.

  06/17/04  G Campbell  Added pdf_id identifier to TT_object.  This allows
                        separation of the Object Pointers by the individual
                        PDF file.

  06/23/04  G Campbell  Added obj_stream index to TT_Object
  
  07/07/04  G Campbell  Added temp-table TT_Widget.  This stores the info
                        pertaining to an Adobe Form Field. 

  07/30/04  G Campbell  Added widget_page to the TT_Widget temp-table.  This 
                        allows us to track which page the widget actually 
                        belongs on.  Useful when using a multi-page PDF 
                        template.

******************************************************************************/

&GLOBAL-DEFINE PDFDIR 

/* Used to Return Font List */
DEFINE {1} TEMP-TABLE TT_pdf_ext NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD pdf_id      AS CHARACTER
  FIELD pdf_name    AS CHARACTER.

DEFINE {1} TEMP-TABLE TT_Font NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD pdf_id      AS CHARACTER
  FIELD obj_id      AS INTEGER
  FIELD gen_id      AS INTEGER
  FIELD desc_obj    AS INTEGER
  FIELD desc_gen    AS INTEGER
  FIELD enc_obj     AS INTEGER
  FIELD enc_gen     AS INTEGER
  FIELD file2_obj   AS INTEGER
  FIELD file2_gen   AS INTEGER
  FIELD file3_obj   AS INTEGER
  FIELD file3_gen   AS INTEGER
  FIELD font_name   AS CHARACTER
  FIELD font_tag    AS CHARACTER
  FIELD font_width  AS CHARACTER.

/* Used to Return Information Parameters */
DEFINE {1} TEMP-TABLE TT_Info NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD pdf_id      AS CHARACTER
  FIELD info_name   AS CHARACTER
  FIELD info_value  AS CHARACTER
  FIELD info_extra  AS CHARACTER.

DEFINE {1} TEMP-TABLE TT_Object NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD pdf_id      AS CHARACTER
  FIELD obj_ptr     AS INTEGER
  FIELD obj_id      AS INTEGER
  FIELD gen_id      AS INTEGER
  FIELD obj_seq     AS INTEGER
  FIELD obj_type    AS CHARACTER
  FIELD page_id     AS INTEGER
INDEX obj_id AS PRIMARY
      obj_id
INDEX pdf_id
      pdf_id
INDEX gen_id
      gen_id
INDEX obj_seq
      obj_seq
INDEX obj_type
      obj_type
INDEX obj_stream
      obj_stream.

DEFINE {1} TEMP-TABLE TT_Resource NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD pdf_id      AS CHARACTER
  FIELD par_obj     AS INTEGER
  FIELD par_gen     AS INTEGER
  FIELD page_id     AS INTEGER
  FIELD res_type    AS CHARACTER
  FIELD res_obj     AS INTEGER
  FIELD res_gen     AS INTEGER
  FIELD res_len     AS INTEGER
  FIELD res_text    AS CHARACTER
  FIELD new_obj     AS INTEGER
  FIELD new_gen     AS INTEGER
INDEX page_id AS PRIMARY
      page_id.

/* This following temp-table is used to store/track XML definitions per stream */
DEFINE {1} TEMP-TABLE TT_pdf_xml NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD xml_parent  AS CHARACTER 
  FIELD xml_pnode   AS INTEGER
  FIELD xml_node    AS CHARACTER
  FIELD xml_value   AS CHARACTER
  FIELD xml_seq     AS INTEGER
INDEX xml_seq AS PRIMARY
      xml_parent
      xml_seq
INDEX xml_pnode 
      xml_pnode .

DEFINE {1} TEMP-TABLE TT_Widget NO-UNDO
  FIELD obj_stream    AS CHARACTER
  FIELD pdf_id        AS CHARACTER
  FIELD widget_type   AS CHARACTER
  FIELD widget_name   AS CHARACTER
  FIELD widget_rect   AS CHARACTER
  FIELD widget_disp   AS CHARACTER
  FIELD widget_page   AS INTEGER.

*&---------------------------------------------------------------------*
*& Report  ZOLEOBJ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zoleobj.

**In table OLELOAD we can find all the Methods and Properties which can be used along with the word application.
**Link to docs
**https://wiki.scn.sap.com/wiki/display/ABAP/Download+Data+into+Word+Document+using+OLE+Automation
**https://blogs.sap.com/2014/11/26/download-ms-word-using-ole-method/
*********************************************************

*–Include for OLE-enabling definitions
INCLUDE ole2incl .
*–Global variables
*–Variables to hold OLE object and entity handles
DATA gs_word            TYPE ole2_object . "OLE object handle
DATA gs_documents       TYPE ole2_object . "Documents
DATA gs_actdoc          TYPE ole2_object . "Active document
DATA gs_application     TYPE ole2_object . "Application
DATA gs_options         TYPE ole2_object . "Application options
DATA gs_selection       TYPE ole2_object . "Selection
DATA gs_font            TYPE ole2_object . "Font
DATA gs_parformat       TYPE ole2_object . "Paragraph format
DATA gs_tables          TYPE ole2_object . "Tables
DATA gs_range           TYPE ole2_object . "Range handle for various ranges
DATA gs_table           TYPE ole2_object . "One table
DATA gs_table_border    TYPE ole2_object . "Table border
DATA gs_cell            TYPE ole2_object . "One cell of a table
DATA gv_pos(5)          TYPE n . "Position information for table
DATA gs_inlineshapes    TYPE ole2_object.
DATA gs_ins_logo        TYPE ole2_object.
DATA gt_sflight         TYPE TABLE OF sflight.

SELECTION-SCREEN BEGIN OF BLOCK b1.

PARAMETERS p_path TYPE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA lv_path_temp TYPE string.
  CALL METHOD cl_gui_frontend_services=>directory_browse
*               EXPORTING
*                 window_title         = window_title
*                 initial_folder       = initial_folder
    CHANGING
      selected_folder      = lv_path_temp
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
          .
  IF sy-subrc <> 0.
*              Implement suitable error handling here
  ENDIF.
  p_path = lv_path_temp.

START-OF-SELECTION .

  PERFORM f_get_data.
  PERFORM f_create_doc.

*–Setting header content
*–Getting handle for the selection which is here the character at the
*–cursor position
  GET PROPERTY OF gs_application 'Selection' = gs_selection .
  GET PROPERTY OF gs_selection 'Font' = gs_font .
  GET PROPERTY OF gs_selection 'ParagraphFormat' = gs_parformat .

*–Setting paragraph format attribute
  SET PROPERTY OF gs_parformat 'Alignment' = '0' . "Left-justified

  PERFORM f_attach_image.
  PERFORM f_display_header.
  PERFORM f_display_text.
  PERFORM f_skip_lines.
  PERFORM f_display_table.
  PERFORM f_skip_lines.
  PERFORM f_save_object.
  PERFORM f_close_object.
  PERFORM f_free_object.

*&———————————————————————*
*&      Form  F_GET_DATA
*&———————————————————————*
*  Get Data for displaying in Word
*———————————————————————-*
FORM f_get_data .

  SELECT *
    FROM sflight
    INTO TABLE gt_sflight
    UP TO 10 ROWS.

ENDFORM.                    " F_GET_DATA
*&———————————————————————*
*&      Form  F_CREATE_DOC
*&———————————————————————*
*  Create Word Document and Set its properties
*———————————————————————-*
FORM f_create_doc .

*–Creating OLE object handle variable
  CREATE OBJECT gs_word 'WORD.APPLICATION'.
  IF sy-subrc NE 0 .
    MESSAGE 'Error while creating OLE object!' TYPE 'E'.
    LEAVE PROGRAM .
  ENDIF .

*–Setting object's visibility property
*– MS Word runs in the Background
*– To see MS word in the Foreground, Set as '1'
  SET PROPERTY OF gs_word 'Visible' = '1' .

*–Opening a new document
  GET PROPERTY OF gs_word 'Documents' = gs_documents .
  CALL METHOD OF
      gs_documents
      'Add'.

*–Getting active document handle
  GET PROPERTY OF gs_word 'ActiveDocument' = gs_actdoc .

*–Getting applications handle
  GET PROPERTY OF gs_actdoc 'Application' = gs_application .

*–Setting the measurement unit
  GET PROPERTY OF gs_application 'Options' = gs_options .
  SET PROPERTY OF gs_options 'MeasurementUnit' = '1' .

ENDFORM.                    " F_CREATE_DOC
*&———————————————————————*
*&      Form  F_ATTACH_IMAGE
*&———————————————————————*
*  Attach an Image from Presentation server
*———————————————————————-*
FORM f_attach_image .

*–Insert Image from Presentation server
  GET PROPERTY OF gs_selection 'InlineShapes' = gs_inlineshapes.

  CALL METHOD OF
      gs_inlineshapes
      'AddPicture'    = gs_ins_logo
    EXPORTING
      #1              = 'C:\Users\antonio.garofalo\Desktop\OLEE.png'.

ENDFORM.                    " F_ATTACH_IMAGE
*&———————————————————————*
*&      Form  F_DISPLAY_HEADER
*&———————————————————————*
* Display Header text
*———————————————————————-*
FORM f_display_header .

* Alignment:
* '0' – Left Alignment
* '1' – Centered
* '2' – Right Alignment
* '3' – Justified

*–Advancing cursor to the new line
  CALL METHOD OF
      gs_selection
      'TypeParagraph'.
*–Setting font attributes
  SET PROPERTY OF gs_font 'Name' = 'Arial' .
  SET PROPERTY OF gs_font 'Size' = '16' .
  SET PROPERTY OF gs_font 'Bold' = '1' . "Bold
  SET PROPERTY OF gs_font 'Italic' = '0' . "Not Italic
  SET PROPERTY OF gs_font 'Underline' = '1' . "Underlined
*–Setting paragraph format attribute
  SET PROPERTY OF gs_parformat 'Alignment' = '1' . "Centered

  CALL METHOD OF
      gs_selection
      'TypeText'

    EXPORTING
      #1           = 'Download Word using OLE!'.

*–Advancing cursor to the new line
  CALL METHOD OF
      gs_selection
      'TypeParagraph'.

ENDFORM.                    " F_DISPLAY_HEADER
*&———————————————————————*
*&      Form  F_DISPLAY_TEXT
*&———————————————————————*
*  Display text and Set the font properties
*———————————————————————-*
FORM f_display_text .

*–Reseting font attributes for Text
*–Getting handle for the selection which is here the character at the
*–cursor position
  SET PROPERTY OF gs_font 'Name' = 'Times New Roman' .
  SET PROPERTY OF gs_font 'Size' = '10' .
  SET PROPERTY OF gs_font 'Bold' = '0' . "Not bold
  SET PROPERTY OF gs_font 'Italic' = '1' . "Italic
  SET PROPERTY OF gs_font 'Underline' = '0' . "Not underlined
*–Setting paragraph format attribute
  SET PROPERTY OF gs_parformat 'Alignment' = '1' . "Centered
  CALL METHOD OF
      gs_selection
      'TypeText'

    EXPORTING
      #1           = 'Text 1'.
*–Advancing cursor to the new line
  CALL METHOD OF
      gs_selection
      'TypeParagraph'.

*–Reseting font attributes for another Text
  SET PROPERTY OF gs_font 'Name' = 'Times New Roman' .
  SET PROPERTY OF gs_font 'Size' = '12' .
  SET PROPERTY OF gs_font 'Bold' = '0' . "Not bold
  SET PROPERTY OF gs_font 'Italic' = '0' . "Not Italic
  SET PROPERTY OF gs_font 'Underline' = '0' . "Not underlined
*–Setting paragraph format attribute
  SET PROPERTY OF gs_parformat 'Alignment' = '2' . "Right Aligned
  CALL METHOD OF
      gs_selection
      'TypeText'

    EXPORTING
      #1           = 'Text 2'.

ENDFORM.                    " F_DISPLAY_TEXT
*&———————————————————————*
*&      Form  F_SKIP_LINES
*&———————————————————————*
*  Skip some lines
*———————————————————————-*
FORM f_skip_lines .

*–Skip some lines
  DO 4 TIMES .
    CALL METHOD OF
        gs_selection
        'TypeParagraph'.
  ENDDO .

ENDFORM.                    " F_SKIP_LINES
*&———————————————————————*
*&      Form  F_DISPLAY_TABLE
*&———————————————————————*
*  Display Table using Internal table contents
*———————————————————————-*
FORM f_display_table .

  DATA: ls_sflight TYPE sflight,
        lv_tabix   TYPE sy-tabix,
        lv_lines   TYPE i.

  DESCRIBE TABLE gt_sflight LINES lv_lines.

*–Inserting a table and filling some of its cells.
*–Getting entity handles for the entities on the way
  GET PROPERTY OF gs_actdoc 'Tables' = gs_tables .
  GET PROPERTY OF gs_selection 'Range' = gs_range .
*–Adding a table with required columns and rows
  CALL METHOD OF
      gs_tables
      'Add'     = gs_table
    EXPORTING
      #1        = gs_range " Handle for range entity
      #2        = lv_lines "Number of rows
      #3        = '2'. "Number of columns
*–Setting border attribute for the table
  GET PROPERTY OF gs_table 'Borders' = gs_table_border .
  SET PROPERTY OF gs_table_border 'Enable' = '1' . "With border

*–Reseting font attributes for table content
  SET PROPERTY OF gs_font 'Name' = 'Garamond' .
  SET PROPERTY OF gs_font 'Size' = '11' .
  SET PROPERTY OF gs_font 'Bold' = '0' . "Not bold
  SET PROPERTY OF gs_font 'Italic' = '0' . "Not Italic
  SET PROPERTY OF gs_font 'Underline' = '0' . "Not underlined
  SET PROPERTY OF gs_font 'Color' = 'wdColorRed' . "Red

*–Filling the table with Internal Table data
  LOOP AT gt_sflight INTO ls_sflight.

    CLEAR lv_tabix.
    lv_tabix = sy-tabix.
*–Getting cell coordinates
    CALL METHOD OF
        gs_table
        'Cell'   = gs_cell
      EXPORTING
        #1       = lv_tabix "Row number
        #2       = '1'. "first column

*–Getting the range handle to write the text
    GET PROPERTY OF gs_cell 'Range' = gs_range .
*–Filling the cell
    SET PROPERTY OF gs_range 'Text' = ls_sflight-carrid .

*–Getting cell coordinates
    CALL METHOD OF
        gs_table
        'Cell'   = gs_cell
      EXPORTING
        #1       = lv_tabix "Row number
        #2       = '2'. "Second column

*–Getting the range handle to write the text
    GET PROPERTY OF gs_cell 'Range' = gs_range .
*–Filling the cell
    SET PROPERTY OF gs_range 'Text' = ls_sflight-connid .

  ENDLOOP.

*–Advancing the cursor to the end of the table
  GET PROPERTY OF gs_table 'Range' = gs_range .
  GET PROPERTY OF gs_range 'End' = gv_pos .
  SET PROPERTY OF gs_range 'Start' = gv_pos .
  CALL METHOD OF
      gs_range
      'Select'.

ENDFORM.                    " F_DISPLAY_TABLE
*&———————————————————————*
*&      Form  F_FREE_OBJECT
*&———————————————————————*
*  Free Object
*———————————————————————-*
FORM f_free_object .

*–Freeing object handle variable
  FREE OBJECT gs_word.

ENDFORM.                    " F_FREE_OBJECT
*&———————————————————————*
*&      Form  F_SAVE_OBJECT
*&———————————————————————*
*  Save Object in Presentation server
*———————————————————————-*
FORM f_save_object .
  DATA lv_path_temp TYPE string.
  CONCATENATE p_path
              '\'
              'OLEOBJ_'
              sy-datum
              '.docx'
  INTO lv_path_temp.

*–Save the Word Doc in Presentation Server
"FileSaveAs if allow user to save/saveAs/not save
  CALL METHOD OF
      gs_actdoc
      'SaveAs'

    EXPORTING
      #1        = lv_path_temp. "'C:\Users\antonio.garofalo\Desktop\docole.docx'.

ENDFORM.                    " F_SAVE_OBJECT
*&———————————————————————*
*&      Form  F_CLOSE_OBJECT
*&———————————————————————*
*  Close Object
*———————————————————————-*
FORM f_close_object .

*–Close the Document
"Close for close doc only
"Quit for exit by Word
  CALL METHOD OF
      gs_actdoc
      'Quit'.

ENDFORM.                    " F_CLOSE_OBJECT

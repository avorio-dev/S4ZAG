CLASS zag_cl_ole DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_char_data,
        text(1500),
      END OF ty_char_data .
    TYPES:
      tt_char_table TYPE TABLE OF ty_char_data-text .

    CONSTANTS c_xls_black TYPE i VALUE 0 ##NO_TEXT.
    CONSTANTS c_xls_blue TYPE i VALUE 15773440 ##NO_TEXT.
    CONSTANTS c_xls_green TYPE i VALUE 13496520 ##NO_TEXT.
    CONSTANTS c_xls_navy TYPE i VALUE 6562560 ##NO_TEXT.
    CONSTANTS c_xls_red TYPE i VALUE 13486335 ##NO_TEXT.
    CONSTANTS c_xls_white TYPE i VALUE 16777215 ##NO_TEXT.
    CONSTANTS c_xls_yell TYPE i VALUE 2992895 ##NO_TEXT.
    CONSTANTS c_separator_horizontal_tab TYPE abap_char1 VALUE %_horizontal_tab ##NO_TEXT.
    CONSTANTS c_separator_semicolon TYPE char1 VALUE ';' ##NO_TEXT.

    METHODS download_excel_local
      IMPORTING
        !x_filename  TYPE string
        !xt_str_data TYPE string_table
        !xt_fcat     TYPE lvc_t_fcat
      CHANGING
        !xt_sap_data TYPE STANDARD TABLE
      EXCEPTIONS
        unable_open_path .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA go_application TYPE ole2_object .
    DATA go_borders TYPE ole2_object .
    DATA go_cell TYPE ole2_object .
    DATA go_cellend TYPE ole2_object .
    DATA go_cellstart TYPE ole2_object .
    DATA go_column TYPE ole2_object .
    DATA go_font TYPE ole2_object .
    DATA go_interior TYPE ole2_object .
    DATA go_range TYPE ole2_object .
    DATA go_sheet TYPE ole2_object .
    DATA go_workbook TYPE ole2_object .
    DATA go_workbooks TYPE ole2_object .
    DATA go_worksheet TYPE ole2_object .
    DATA go_worksheets TYPE ole2_object .


    METHODS ole_add_sheet .
    METHODS ole_clipboard_copy .
    METHODS ole_clipboard_export
      IMPORTING
        !xt_str_data TYPE string_table .
    METHODS ole_clipboard_paste
      IMPORTING
        !x_start_row TYPE i DEFAULT 1
        !x_start_col TYPE i DEFAULT 1 .
    METHODS ole_clipboard_paste_special .
    METHODS ole_init_excel .
    METHODS ole_save_excel
      IMPORTING
        !x_filename TYPE string
      EXCEPTIONS
        unable_open_path .
    METHODS ole_set_active_sheet
      IMPORTING
        !x_sheet_number TYPE i DEFAULT 1 .
    METHODS ole_set_currency_format .
    METHODS ole_set_current_range
      IMPORTING
        !x_start_row TYPE i DEFAULT 1
        !x_start_col TYPE i DEFAULT 1
        !x_end_row   TYPE i DEFAULT 1
        !x_end_col   TYPE i DEFAULT 1 .
    METHODS ole_set_range_properties
      IMPORTING
        !x_background  TYPE i DEFAULT c_xls_white
        !x_font_name   TYPE string DEFAULT 'Arial'          "#EC NOTEXT
        !x_size        TYPE i DEFAULT 12
        !x_bold        TYPE i DEFAULT 0
        !x_italic      TYPE i DEFAULT 0
        !x_color       TYPE i DEFAULT c_xls_black
        !x_underline   TYPE i DEFAULT 0
        !x_set_borders TYPE i DEFAULT 0 .

ENDCLASS.



CLASS ZAG_CL_OLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_OLE->DOWNLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [--->] XT_FCAT                        TYPE        LVC_T_FCAT
* | [<-->] XT_SAP_DATA                    TYPE        STANDARD TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_excel_local.

    "Separator conversione from ';' TO horizontal_tab
    "-------------------------------------------------
    DATA lv_tabix TYPE sy-tabix.
    DATA(lv_lines_fcat) = lines( xt_fcat ).
    DATA(lv_lines_tab)  = lines( xt_str_data ).

    me->ole_init_excel( ).
    me->ole_add_sheet( ).

    me->ole_clipboard_export( xt_str_data = xt_str_data ).
    me->ole_clipboard_paste( ).


    "Set Header properties
    "-------------------------------------------------
    me->ole_set_current_range(
      EXPORTING
        x_start_row = 1
        x_start_col = 1
        x_end_row   = 1
        x_end_col   = lv_lines_fcat
    ).

    me->ole_set_range_properties(
      EXPORTING
        x_background  = c_xls_navy
        x_font_name   = 'Arial'                             "#EC NOTEXT
        x_size        = 12
        x_bold        = 1
        x_italic      = 0
        x_color       = c_xls_white
        x_underline   = 0
        x_set_borders = 0
    ).


    "Set currency format
    "-------------------------------------------------
    LOOP AT xt_fcat ASSIGNING FIELD-SYMBOL(<fcat>)
      WHERE datatype EQ 'CURR'.

      lv_tabix = sy-tabix.
      me->ole_set_current_range(
        EXPORTING
          x_start_row = 2
          x_start_col = lv_tabix
          x_end_row   = lv_lines_tab
          x_end_col   = lv_tabix
      ).

      me->ole_set_currency_format( ).

    ENDLOOP.



    me->ole_save_excel(
      EXPORTING
        x_filename       = x_filename
      EXCEPTIONS
        unable_open_path = 1
        OTHERS           = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_ADD_SHEET
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_add_sheet.

    "Insert a new sheet with input name

    DATA(lv_sheet_name) = |{ sy-repid(10) }|.

    GET PROPERTY OF go_application 'Sheets' = go_sheet.
    CALL METHOD OF go_sheet 'Add' = go_worksheet.
    SET PROPERTY OF go_worksheet 'Name' = lv_sheet_name.
    GET PROPERTY OF go_application 'ACTIVESHEET' = go_worksheet.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_CLIPBOARD_COPY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_copy.

    CALL METHOD OF go_range 'Copy'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_CLIPBOARD_EXPORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_export.

    DATA(lv_rc) = 0.

    DATA: lt_char_data TYPE tt_char_table.

    lt_char_data[] = xt_str_data[].

    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data                 = lt_char_data[]
      CHANGING
        rc                   = lv_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_CLIPBOARD_PASTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_START_ROW                    TYPE        I (default =1)
* | [--->] X_START_COL                    TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_paste.

    "Select the cell a1
    CALL METHOD OF go_worksheet 'cells' = go_cell
      EXPORTING
        #1           = x_start_row "Row
        #2           = x_start_col   ."Column

    "Paste clipboard from cell A1
    CALL METHOD OF go_cell 'select'.
    IF sy-subrc <> 0.

    ENDIF.

    CALL METHOD OF go_worksheet 'paste'.
    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_CLIPBOARD_PASTE_SPECIAL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_paste_special.

    CALL METHOD OF go_range 'PasteSpecial'.
    GET PROPERTY OF go_range 'EntireColumn' = go_column.
    CALL METHOD OF go_column 'AutoFit'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_INIT_EXCEL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_init_excel.

    CREATE OBJECT go_application 'Excel.Application'.
    SET PROPERTY OF go_application 'Visible' = 1.
    SET PROPERTY OF go_application 'SheetsInNewWorkbook' = 1. "no of sheets

    CALL METHOD OF go_application 'Workbooks' = go_workbooks.
    CALL METHOD OF go_workbooks 'Add' = go_workbook.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_SAVE_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_save_excel.

    DATA(lv_path) = CONV rlgrap-filename( x_filename ).
    DATA(lv_error) = abap_false.
    CALL METHOD OF go_workbook 'SaveAs'
      EXPORTING
        #1 = lv_path.
    IF sy-subrc <> 0.
      lv_error = abap_true.
    ENDIF.

    CALL METHOD OF go_workbook 'CLOSE'
      EXPORTING
        #1 = 'YES'.

    CALL METHOD OF go_application 'Quit'.

    IF lv_error EQ abap_true.
      RAISE unable_open_path.
    ENDIF.

* Free Excel objects
    FREE OBJECT:
      go_application,
      go_workbook   ,
      go_workbooks  ,
      go_range      ,
      go_worksheet  ,
      go_worksheets ,
      go_sheet      ,
      go_column     ,
      go_cell       ,
      go_font       ,
      go_borders    ,
      go_interior   ,
      go_cellstart  ,
      go_cellend    .


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_SET_ACTIVE_SHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SHEET_NUMBER                 TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_active_sheet.

    CALL METHOD OF go_application 'Worksheets' = go_worksheet
      EXPORTING #1 = x_sheet_number.
    CALL METHOD OF go_worksheet 'Activate'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_SET_CURRENCY_FORMAT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_currency_format.

    SET PROPERTY OF go_range 'NumberFormat' = '#,##0.00 $'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_SET_CURRENT_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_START_ROW                    TYPE        I (default =1)
* | [--->] X_START_COL                    TYPE        I (default =1)
* | [--->] X_END_ROW                      TYPE        I (default =1)
* | [--->] X_END_COL                      TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_current_range.

    " 1. SELECT starting cell
    CALL METHOD OF go_worksheet 'cells' = go_cellstart
      EXPORTING
        #1 = x_start_row
        #2 = x_start_col.

    " 2. Select ending cell
    CALL METHOD OF go_worksheet 'cells' = go_cellend
      EXPORTING
        #1 = x_end_row
        #2 = x_end_col.

    " Select the Range:
    CALL METHOD OF go_worksheet 'range' = go_range
      EXPORTING
        #1 = go_cellstart
        #2 = go_cellend.

    CALL METHOD OF go_range 'Select'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_OLE->OLE_SET_RANGE_PROPERTIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_BACKGROUND                   TYPE        I (default =C_XLS_WHITE)
* | [--->] X_FONT_NAME                    TYPE        STRING (default ='Arial')
* | [--->] X_SIZE                         TYPE        I (default =12)
* | [--->] X_BOLD                         TYPE        I (default =0)
* | [--->] X_ITALIC                       TYPE        I (default =0)
* | [--->] X_COLOR                        TYPE        I (default =C_XLS_BLACK)
* | [--->] X_UNDERLINE                    TYPE        I (default =0)
* | [--->] X_SET_BORDERS                  TYPE        I (default =0)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_range_properties.

    "OLE_METHOD - Specify the property of selected range cell
    "             x_background = Integer with excel color
    "             x_bold       = 0 No / 1 Yes
    "             x_size       = Integer Text size

    CALL METHOD OF go_range 'Interior' = go_interior.
    SET PROPERTY OF go_interior 'Color' = x_background.

    GET PROPERTY OF go_range 'Font'     = go_font.
    SET PROPERTY OF go_font 'Size'      = x_size.
    SET PROPERTY OF go_font 'Name'      = x_font_name .
    SET PROPERTY OF go_font 'Bold'      = x_bold.
    SET PROPERTY OF go_font 'Color'     = x_color.
    SET PROPERTY OF go_font 'Italic'    = x_italic.
    SET PROPERTY OF go_font 'Underline' = x_underline.

    IF x_set_borders GT 0.
      "Per questo caso, verranno inseriti i bordi considerando il range come se fosse
      "una mega cella, per cui i bordi interni non saranno visibili
      "se si desidera il bordo interno, dovrà essere usato lo_cell piuttosto che lo_range
      CALL METHOD OF go_range 'borders' = go_borders EXPORTING #1 = '7'."xledgeleft
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous

      CALL METHOD OF go_range 'borders' = go_borders EXPORTING #1 = '8'."xledgetop
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous

      CALL METHOD OF go_range 'borders' = go_borders EXPORTING #1 = '9'."xledgebottom
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous


      CALL METHOD OF go_range 'borders' = go_borders EXPORTING #1 = '10'."xledgeright
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous

** Increase the weight of the border if you want, in this case only for EdgeRight:
*  SET PROPERTY OF lo_borders 'weight' = 4. "xlthick
    ENDIF.

    GET PROPERTY OF go_range 'EntireColumn' = go_column.
    CALL METHOD OF go_column 'AutoFit'.

  ENDMETHOD.
ENDCLASS.

class ZAG_CL_CSV_XLSX definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_char_data,
        text(1500),
      END OF ty_char_data .
  types:
    tt_char_table TYPE TABLE OF ty_char_data-text .

  constants C_CR_LF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  constants C_INITIAL_DATA type DATUM value '00000000' ##NO_TEXT.
  constants C_MAX_DATA type DATUM value '99991231' ##NO_TEXT.
  constants C_SEPARATOR_HORIZONTAL_TAB type ABAP_CHAR1 value %_HORIZONTAL_TAB ##NO_TEXT.
  constants C_SEPARATOR_SEMICOLON type CHAR1 value ';' ##NO_TEXT.
  constants C_SOURCE_LOCAL type CHAR4 value 'LOCL' ##NO_TEXT.
  constants C_SOURCE_SERVER type CHAR4 value 'SERV' ##NO_TEXT.
  constants C_XLS_BLACK type I value 0 ##NO_TEXT.
  constants C_XLS_BLUE type I value 15773440 ##NO_TEXT.
  constants C_XLS_GREEN type I value 13496520 ##NO_TEXT.
  constants C_XLS_NAVY type I value 6562560 ##NO_TEXT.
  constants C_XLS_RED type I value 13486335 ##NO_TEXT.
  constants C_XLS_WHITE type I value 16777215 ##NO_TEXT.
  constants C_XLS_YELL type I value 2992895 ##NO_TEXT.

  class-methods CONV_SAP_TO_STRING
    importing
      !X_SAP_STRUCT_NAME type TABNAME
      !X_SAP_DATA type ANY
      !X_SEPARATOR type CHAR1 default C_SEPARATOR_SEMICOLON
    exporting
      !Y_STR_DATA type STRING .
  class-methods CONV_STRING_TO_SAP
    importing
      !X_STR_DATA type STRING
      !X_SAP_STRUCT_NAME type TABNAME
    exporting
      !Y_SAP_DATA type ANY .
  class-methods DOWNLOAD
    importing
      !X_FILENAME type STRING
      !X_HEADER type XFELD default 'X'
      !X_SAP_STRUCT_NAME type TABNAME
      !XT_SAP_DATA type TABLE
      !X_SOURCE type CHAR4 default 'LOCL'
    exceptions
      NOT_SUPPORTED_FILE
      UNABLE_OPEN_PATH .
  class-methods F4_HELP_DIR_INPUT
    importing
      !X_SOURCE type CHAR4 default 'LOCL'
    exporting
      !Y_PATH_INPUT type STRING .
  class-methods F4_HELP_DIR_OUTPUT
    importing
      !X_SOURCE type CHAR4 default 'LOCL'
    exporting
      !Y_PATH_OUTPUT type STRING .
  class-methods GET_DESKTOP_DIRECTORY
    returning
      value(Y_DESKTOP_DIR) type STRING .
  class-methods GET_FIELDCAT_FROM_ITAB
    importing
      !XT_ITAB type STANDARD TABLE
    exporting
      !YT_FCAT type LVC_T_FCAT .
  class-methods REMOVE_SPECIAL_CHAR
    changing
      !Y_TEXT type STRING .
  class-methods UPLOAD
    importing
      !X_FILENAME type STRING
      !X_HEADER type XFELD default 'X'
      !X_SAP_STRUCT_NAME type TABNAME
      !X_SOURCE type CHAR4 default 'LOCL'
    exporting
      !YT_SAP_DATA type TABLE
    exceptions
      NOT_SUPPORTED_FILE
      UNABLE_OPEN_PATH .
protected section.
private section.

  data GO_APPLICATION type OLE2_OBJECT .
  data GO_BORDERS type OLE2_OBJECT .
  data GO_CELL type OLE2_OBJECT .
  data GO_CELLEND type OLE2_OBJECT .
  data GO_CELLSTART type OLE2_OBJECT .
  data GO_COLUMN type OLE2_OBJECT .
  data GO_FONT type OLE2_OBJECT .
  data GO_INTERIOR type OLE2_OBJECT .
  data GO_RANGE type OLE2_OBJECT .
  data GO_SHEET type OLE2_OBJECT .
  data GO_WORKBOOK type OLE2_OBJECT .
  data GO_WORKBOOKS type OLE2_OBJECT .
  data GO_WORKSHEET type OLE2_OBJECT .
  data GO_WORKSHEETS type OLE2_OBJECT .

  class-methods CONV_DATA_TO_EXT
    importing
      !X_DATA_INT type DATS
      !X_SEPARATOR type C default '/'
    exporting
      !Y_DATA_EXT type STRING .
  class-methods CONV_DATA_TO_INT
    importing
      !X_DATA_EXT type STRING
    exporting
      !Y_DATA_INT type DATS .
  class-methods CONV_FORMAT_SAP_TO_XLSX
    importing
      !X_TYPEKIND type ABAP_TYPEKIND
    changing
      !Y_XLSX_VALUE type STRING .
  class-methods CONV_FORMAT_XLSX_TO_SAP
    importing
      !X_TYPEKIND type ABAP_TYPEKIND
    changing
      !Y_SAP_VALUE type STRING .
  class-methods CONV_TIME_TO_EXT
    importing
      !X_TIME type UZEIT
    exporting
      !Y_TIME type STRING .
  class-methods CONV_TIME_TO_INT
    importing
      !X_TIME type STRING
    exporting
      !Y_TIME type UZEIT .
  class-methods DOWNLOAD_CSV_LOCAL
    importing
      !X_FILENAME type STRING
    changing
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  class-methods DOWNLOAD_CSV_SERVER
    importing
      !X_FILENAME type STRING
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  class-methods DOWNLOAD_EXCEL_LOCAL
    importing
      !X_FILENAME type STRING
      !XT_FCAT type LVC_T_FCAT
    changing
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  class-methods GET_HEADER_FROM_DDIC
    importing
      !X_SAP_STRUCT_NAME type TABNAME
    changing
      !Y_STR_HEADER type STRING .
  class-methods GET_HEADER_FROM_ITAB
    importing
      !XT_ITAB type STANDARD TABLE
    changing
      !Y_STR_HEADER type STRING .
  class-methods UPLOAD_CSV_LOCAL
    importing
      !X_FILENAME type STRING
    exporting
      !YT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  class-methods UPLOAD_CSV_SERVER
    importing
      !X_FILENAME type STRING
    exporting
      !YT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  class-methods UPLOAD_EXCEL_LOCAL
    importing
      !X_FILENAME type STRING
      !X_SAP_STRUCT_NAME type TABNAME
    exporting
      !YT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods OLE_ADD_SHEET .
  methods OLE_CLIPBOARD_COPY .
  methods OLE_CLIPBOARD_EXPORT
    importing
      !XT_STR_DATA type STRING_TABLE .
  methods OLE_CLIPBOARD_PASTE
    importing
      !X_START_ROW type I default 1
      !X_START_COL type I default 1 .
  methods OLE_CLIPBOARD_PASTE_SPECIAL .
  methods OLE_INIT_EXCEL .
  methods OLE_SAVE_EXCEL
    importing
      !X_FILENAME type STRING
    exceptions
      UNABLE_OPEN_PATH .
  methods OLE_SET_ACTIVE_SHEET
    importing
      !X_SHEET_NUMBER type I default 1 .
  methods OLE_SET_CURRENCY_FORMAT .
  methods OLE_SET_CURRENT_RANGE
    importing
      !X_START_ROW type I default 1
      !X_START_COL type I default 1
      !X_END_ROW type I default 1
      !X_END_COL type I default 1 .
  methods OLE_SET_RANGE_PROPERTIES
    importing
      !X_BACKGROUND type I default C_XLS_WHITE
      !X_FONT_NAME type STRING default 'Arial'
      !X_SIZE type I default 12
      !X_BOLD type I default 0
      !X_ITALIC type I default 0
      !X_COLOR type I default C_XLS_BLACK
      !X_UNDERLINE type I default 0
      !X_SET_BORDERS type I default 0 .
ENDCLASS.



CLASS ZAG_CL_CSV_XLSX IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>CONV_DATA_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_INT                     TYPE        DATS
* | [--->] X_SEPARATOR                    TYPE        C (default ='/')
* | [<---] Y_DATA_EXT                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_DATA_TO_EXT.

    y_data_ext = |{ x_data_int+6(2) }{ x_separator }{ x_data_int+4(2) }{ x_separator }{ x_data_int(4) }|.
    CONDENSE y_data_ext NO-GAPS.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>CONV_DATA_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_EXT                     TYPE        STRING
* | [<---] Y_DATA_INT                     TYPE        DATS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_DATA_TO_INT.

    y_data_int = c_initial_data.

    CHECK strlen( x_data_ext ) EQ 10.
    y_data_int = |{ x_data_ext+6(4) }{ x_data_ext+3(2) }{ x_data_ext(2) }|.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>CONV_FORMAT_SAP_TO_XLSX
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TYPEKIND                     TYPE        ABAP_TYPEKIND
* | [<-->] Y_XLSX_VALUE                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_FORMAT_SAP_TO_XLSX.

    CASE x_typekind.
      WHEN cl_abap_typedescr=>typekind_float      "Numbers "-------------------------------------------------
        OR cl_abap_typedescr=>typekind_packed.

        REPLACE '.' IN y_xlsx_value WITH ','.
        FIND '-' IN y_xlsx_value.
        IF sy-subrc EQ 0.
          REPLACE '-' IN y_xlsx_value WITH ''.
          y_xlsx_value = |-{ y_xlsx_value }|.
        ENDIF.

        CONDENSE y_xlsx_value NO-GAPS.

      WHEN cl_abap_typedescr=>typekind_date.     "Date "-------------------------------------------------

        conv_data_to_ext(
          EXPORTING
            x_data_int  = CONV sy-datum( y_xlsx_value )
            x_separator = '/'
          IMPORTING
            y_data_ext  = y_xlsx_value
          ).

        CONDENSE y_xlsx_value NO-GAPS.

      WHEN cl_abap_typedescr=>typekind_time.    "Time "-------------------------------------------------

        conv_time_to_ext(
          EXPORTING
            x_time = CONV sy-uzeit( y_xlsx_value )
          IMPORTING
            y_time = y_xlsx_value
        ).

        CONDENSE y_xlsx_value NO-GAPS.

      WHEN OTHERS.
        "Normal Text -> Nothing To Do

    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>CONV_FORMAT_XLSX_TO_SAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TYPEKIND                     TYPE        ABAP_TYPEKIND
* | [<-->] Y_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_FORMAT_XLSX_TO_SAP.

    CASE x_typekind.
      WHEN cl_abap_typedescr=>typekind_float    "Numbers "-------------------------------------------------
        OR cl_abap_typedescr=>typekind_packed.

        REPLACE ALL OCCURRENCES OF '.' IN y_sap_value WITH ''.
        REPLACE ',' IN y_sap_value WITH '.'.

        FIND '-' IN y_sap_value.
        IF sy-subrc EQ 0.
          REPLACE '-' IN y_sap_value WITH ''.
          y_sap_value = |{ y_sap_value }-|.
        ENDIF.

        CONDENSE y_sap_value NO-GAPS.

      WHEN cl_abap_typedescr=>typekind_date. "Date "-------------------------------------------------

        conv_data_to_int(
         EXPORTING
           x_data_ext = y_sap_value
         IMPORTING
           y_data_int = DATA(lv_tmp_dats)
       ).

        y_sap_value = lv_tmp_dats.
        CONDENSE y_sap_value NO-GAPS.

      WHEN cl_abap_typedescr=>typekind_time. "Time "-------------------------------------------------

        conv_time_to_int(
          EXPORTING
            x_time = y_sap_value
          IMPORTING
            y_time = DATA(lv_tmp_time)
        ).

        y_sap_value = lv_tmp_time.
        CONDENSE y_sap_value NO-GAPS.

      WHEN OTHERS.
        "Normal Data -> Nothing To Do

    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_SAP_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [--->] X_SAP_DATA                     TYPE        ANY
* | [--->] X_SEPARATOR                    TYPE        CHAR1 (default =C_SEPARATOR_SEMICOLON)
* | [<---] Y_STR_DATA                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_SAP_TO_STRING.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lv_sap_ref     TYPE REF TO data,
          lv_tmp_data    TYPE string.

    "-------------------------------------------------

    y_str_data = ''.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
    CHECK lo_structdescr IS NOT INITIAL.

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<component>).

      ASSIGN COMPONENT <component>-name OF STRUCTURE x_sap_data TO FIELD-SYMBOL(<value>).

      lv_tmp_data = <value>.

      conv_format_sap_to_xlsx(
        EXPORTING
          x_typekind   = <component>-type_kind
        CHANGING
          y_xlsx_value = lv_tmp_data
      ).

      IF y_str_data IS INITIAL.
        y_str_data = lv_tmp_data.
      ELSE.
        y_str_data = |{ y_str_data }{ x_separator }{ lv_tmp_data }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_STRING_TO_SAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STR_DATA                     TYPE        STRING
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [<---] Y_SAP_DATA                     TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_STRING_TO_SAP.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lv_sap_ref     TYPE REF TO data,
          lv_tmp_dats    TYPE dats,
          lv_tmp_time    TYPE sy-uzeit.

    FIELD-SYMBOLS: <sap_data> TYPE any.

    "-------------------------------------------------

    CLEAR y_sap_data.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
    CHECK lo_structdescr IS NOT INITIAL.

    CREATE DATA lv_sap_ref TYPE (x_sap_struct_name).
    ASSIGN lv_sap_ref->* TO <sap_data>.

    "-------------------------------------------------

    DATA(lv_tmp_string) = x_str_data.
    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<component>).

      SPLIT lv_tmp_string AT ';'
       INTO DATA(lv_sx)
            DATA(lv_dx).

      ASSIGN COMPONENT <component>-name OF STRUCTURE <sap_data> TO FIELD-SYMBOL(<value>).

      remove_special_char(
        CHANGING
          y_text = lv_sx
      ).

      conv_format_xlsx_to_sap(
        EXPORTING
          x_typekind  = <component>-type_kind
        CHANGING
          y_sap_value = lv_sx
      ).

      <value>       = lv_sx.
      lv_tmp_string = lv_dx.

    ENDLOOP.

    y_sap_data = <sap_data>.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>CONV_TIME_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        UZEIT
* | [<---] Y_TIME                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_TIME_TO_EXT.

    y_time = |{ x_time(2) }:{ x_time+2(2) }:{ x_time+4(2) }|.
    CONDENSE y_time NO-GAPS.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>CONV_TIME_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        STRING
* | [<---] Y_TIME                         TYPE        UZEIT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_TIME_TO_INT.

    CHECK strlen( x_time ) EQ 8.
    y_time = |{ x_time(2) }{ x_time+3(2) }{ x_time+5(2) }|.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>DOWNLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [--->] XT_SAP_DATA                    TYPE        TABLE
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD DOWNLOAD.

    DATA: lt_str_data TYPE string_table.


    "Build header
    "-------------------------------------------------
    IF x_header EQ abap_true.

      APPEND INITIAL LINE TO lt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).

      IF x_sap_struct_name IS NOT INITIAL.
        get_header_from_ddic(
          EXPORTING
            x_sap_struct_name = x_sap_struct_name
          CHANGING
            y_str_header      = <str_data>
        ).

      ELSE.
        get_header_from_itab(
          EXPORTING
            xt_itab      = xt_sap_data
          CHANGING
            y_str_header = <str_data>
        ).

      ENDIF.

    ENDIF.


    "Convert SAP Data to String table
    "-------------------------------------------------
    LOOP AT xt_sap_data ASSIGNING FIELD-SYMBOL(<sap_data>).

      APPEND INITIAL LINE TO lt_str_data ASSIGNING <str_data>.

      conv_sap_to_string(
        EXPORTING
          x_sap_struct_name = x_sap_struct_name
          x_sap_data        = <sap_data>
        IMPORTING
          y_str_data        = <str_data>
      ).

    ENDLOOP.


    "Start Download
    "-------------------------------------------------
    IF x_source EQ c_source_local "XLSX only from local
      AND ( x_filename CP '*.xls' OR x_filename CP '*.xlsx' ).

      get_fieldcat_from_itab(
        EXPORTING
          xt_itab = xt_sap_data
        IMPORTING
          yt_fcat = DATA(lt_fcat)
      ).

      download_excel_local(
        EXPORTING
          x_filename       = x_filename
          xt_fcat          = lt_fcat
        CHANGING
          xt_str_data      = lt_str_data
        EXCEPTIONS
          unable_open_path = 1
          OTHERS           = 2
      ).

    ELSEIF x_filename CP '*.csv'.


      "Download string table in local or on server
      "-------------------------------------------------
      CASE x_source.
        WHEN c_source_local. "LOCAL Saving "-------------------------------------------------

          download_csv_local(
            EXPORTING
              x_filename       = x_filename
            CHANGING
              xt_str_data      = lt_str_data
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).

        WHEN c_source_server. "SERVER Saving "-------------------------------------------------

          download_csv_server(
            EXPORTING
              x_filename       = x_filename
              xt_str_data      = lt_str_data
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).

      ENDCASE.


    ELSE.

      RAISE not_supported_file.

    ENDIF.

    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>DOWNLOAD_CSV_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<-->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD DOWNLOAD_CSV_LOCAL.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = x_filename
      CHANGING
        data_tab                = xt_str_data[]
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>DOWNLOAD_CSV_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD DOWNLOAD_CSV_SERVER.

    OPEN DATASET x_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      CLOSE DATASET x_filename.
      RAISE unable_open_path.
    ENDIF.

    LOOP AT xt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).
      TRANSFER <str_data> TO x_filename.
    ENDLOOP.

    CLOSE DATASET x_filename.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>DOWNLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_FCAT                        TYPE        LVC_T_FCAT
* | [<-->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD DOWNLOAD_EXCEL_LOCAL.

    DATA lv_tabix TYPE sy-tabix.
    DATA(lv_lines_fcat) = lines( xt_fcat ).
    DATA(lv_lines_tab)  = lines( xt_str_data ).

    "Separator conversione from ';' TO horizontal_tab
    "-------------------------------------------------
    DATA(lt_str_data) = xt_str_data[].
    LOOP AT lt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).
      REPLACE ALL OCCURRENCES OF c_separator_semicolon IN <str_data>
        WITH c_separator_horizontal_tab.
    ENDLOOP.



    DATA(lo_ole) = NEW zag_cl_csv_xlsx( ).

    lo_ole->ole_init_excel( ).
    lo_ole->ole_add_sheet( ).

    lo_ole->ole_clipboard_export( xt_str_data = xt_str_data ).
    lo_ole->ole_clipboard_paste( ).


    "Set Header properties
    "-------------------------------------------------
    lo_ole->ole_set_current_range(
      EXPORTING
        x_start_row = 1
        x_start_col = 1
        x_end_row   = 1
        x_end_col   = lv_lines_fcat
    ).

    lo_ole->ole_set_range_properties(
      EXPORTING
        x_background  = c_xls_navy
        x_font_name   = 'Arial'
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
      lo_ole->ole_set_current_range(
        EXPORTING
          x_start_row = 2
          x_start_col = lv_tabix
          x_end_row   = lv_lines_tab
          x_end_col   = lv_tabix
      ).

      lo_ole->ole_set_currency_format( ).

    ENDLOOP.






    lo_ole->ole_save_excel(
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
* | Static Public Method ZAG_CL_CSV_XLSX=>F4_HELP_DIR_INPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [<---] Y_PATH_INPUT                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD F4_HELP_DIR_INPUT.

    DATA: lv_path TYPE string VALUE IS INITIAL.

    DATA: lv_server         TYPE msxxlist-name,
          lv_path_tmp       TYPE dxfields-longpath,
          lv_instancenumber TYPE instanz-systemnr.

    DATA: lt_filetable TYPE filetable,
          lcl_ref_itab TYPE REF TO file_table,
          lv_rc        TYPE i.

    "-------------------------------------------------

    CASE x_source.

      WHEN c_source_server.

        lv_instancenumber = ''.
        CALL FUNCTION 'GET_SYSTEM_NUMBER'
          IMPORTING
            instancenumber = lv_instancenumber.

        lv_server = |{ sy-host }_{ sy-sysid }_{ lv_instancenumber }|.
        CONDENSE lv_server NO-GAPS.

        lv_path_tmp = ''.
        CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
          EXPORTING
            i_location_flag = 'A'
            i_server        = lv_server
            filemask        = '*.*'
            fileoperation   = 'R'
          IMPORTING
            o_path          = lv_path_tmp
          EXCEPTIONS
            rfc_error       = 1
            error_with_gui  = 2
            OTHERS          = 3.
        CHECK sy-subrc EQ 0.
        lv_path = lv_path_tmp.

      WHEN c_source_local.

        lv_path = zag_cl_csv_xlsx=>get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          EXPORTING
*           default_filename  = '*.csv'
            initial_directory = lv_path
          CHANGING
            file_table        = lt_filetable
            rc                = lv_rc.
        ASSIGN lt_filetable[ 1 ] TO FIELD-SYMBOL(<filetable>).
        CHECK sy-subrc EQ 0.
        lv_path = <filetable>-filename.

    ENDCASE.

    y_path_input = lv_path.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>F4_HELP_DIR_OUTPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [<---] Y_PATH_OUTPUT                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD F4_HELP_DIR_OUTPUT.

    DATA: lv_path TYPE string VALUE IS INITIAL.

    "-------------------------------------------------

    CASE x_source.

      WHEN c_source_server.

        CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
          EXPORTING
*           directory        = ''
            filemask         = '.dummysap'
          IMPORTING
            serverfile       = lv_path
          EXCEPTIONS
            canceled_by_user = 1
            OTHERS           = 2.
        CHECK sy-subrc EQ 0.

      WHEN c_source_local.

        lv_path = get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>directory_browse
          EXPORTING
            window_title         = ''
            initial_folder       = lv_path
          CHANGING
            selected_folder      = lv_path
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.
        CHECK sy-subrc EQ 0.

    ENDCASE.

    y_path_output = lv_path.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>GET_DESKTOP_DIRECTORY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] Y_DESKTOP_DIR                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_DESKTOP_DIRECTORY.

    y_desktop_dir = ''.
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = y_desktop_dir
      EXCEPTIONS
        cntl_error        = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>update_view.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>GET_FIELDCAT_FROM_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ITAB                        TYPE        STANDARD TABLE
* | [<---] YT_FCAT                        TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_FIELDCAT_FROM_ITAB.

    DATA: lref_table TYPE REF TO data.

    "-------------------------------------------------

    CREATE DATA lref_table LIKE xt_itab.
    ASSIGN lref_table->* TO FIELD-SYMBOL(<table>).

    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(lt_salv_table)
                                CHANGING
                                  t_table        = <table>  ).
        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lt_salv_table->get_columns( ) " ALV Filter
                                                                     r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
                                                                     ) .

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_except_msg) = lx_ai_system_fault->get_text( ).
    ENDTRY.

    DELETE yt_fcat WHERE fieldname EQ 'MANDT'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>GET_HEADER_FROM_DDIC
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [<-->] Y_STR_HEADER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_HEADER_FROM_DDIC.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lt_dfies_tab   TYPE STANDARD TABLE OF dfies,
          lv_new_col     TYPE string.

    "-------------------------------------------------

    y_str_header = ''.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
    CHECK lo_structdescr IS NOT INITIAL.

    REFRESH lt_dfies_tab[].
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = CONV ddobjname( x_sap_struct_name )
      TABLES
        dfies_tab      = lt_dfies_tab[]
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    CHECK lt_dfies_tab[] IS NOT INITIAL.
    SORT lt_dfies_tab BY fieldname.

    "-------------------------------------------------

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<component>).

      CHECK <component>-name NE 'MANDT'.

      lv_new_col = ''.
      ASSIGN lt_dfies_tab[
        line_index( lt_dfies_tab[ fieldname = <component>-name ] ) ] TO FIELD-SYMBOL(<dfies>).
      IF sy-subrc EQ 0.
        lv_new_col = <dfies>-fieldtext.
      ENDIF.

      IF y_str_header IS INITIAL.
        y_str_header = lv_new_col.

      ELSE.
        y_str_header = |{ y_str_header };{ lv_new_col }|.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>GET_HEADER_FROM_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ITAB                        TYPE        STANDARD TABLE
* | [<-->] Y_STR_HEADER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_HEADER_FROM_ITAB.

    y_str_header = ''.

    get_fieldcat_from_itab(
      EXPORTING
        xt_itab = xt_itab
      IMPORTING
        yt_fcat = DATA(lt_fcat)                  " Catalogo campo per ListViewerControl
    ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

      IF y_str_header IS INITIAL.
        y_str_header = <fcat>-reptext.

      ELSE.
        y_str_header = |{ y_str_header };{ <fcat>-reptext }|.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_ADD_SHEET
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_ADD_SHEET.

    "Insert a new sheet with input name

    GET PROPERTY OF go_application 'Sheets' = go_sheet.
    CALL METHOD OF go_sheet 'Add' = go_worksheet.
    SET PROPERTY OF go_worksheet 'Name' = sy-repid.
    GET PROPERTY OF go_application 'ACTIVESHEET' = go_worksheet.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_CLIPBOARD_COPY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_CLIPBOARD_COPY.

    CALL METHOD OF go_range 'Copy'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_CLIPBOARD_EXPORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_CLIPBOARD_EXPORT.

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
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_CLIPBOARD_PASTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_START_ROW                    TYPE        I (default =1)
* | [--->] X_START_COL                    TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_CLIPBOARD_PASTE.

    "Select the cell a1
    CALL METHOD OF go_worksheet 'cells' = go_cell
      EXPORTING
        #1           = x_start_row "Row
        #2           = x_start_col   ."Column

    "Paste clipboard from cell A1
    CALL METHOD OF go_cell 'select'.
    CALL METHOD OF go_worksheet 'paste'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_CLIPBOARD_PASTE_SPECIAL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_CLIPBOARD_PASTE_SPECIAL.

    CALL METHOD OF go_range 'PasteSpecial'.
    GET PROPERTY OF go_range 'EntireColumn' = go_column.
    CALL METHOD OF go_column 'AutoFit'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_INIT_EXCEL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_INIT_EXCEL.

    CREATE OBJECT go_application 'Excel.Application'.
    SET PROPERTY OF go_application 'Visible' = 1.
    SET PROPERTY OF go_application 'SheetsInNewWorkbook' = 1. "no of sheets

    CALL METHOD OF go_application 'Workbooks' = go_workbooks.
    CALL METHOD OF go_workbooks 'Add' = go_workbook.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_SAVE_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_SAVE_EXCEL.

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
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_SET_ACTIVE_SHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SHEET_NUMBER                 TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_SET_ACTIVE_SHEET.

    CALL METHOD OF go_application 'Worksheets' = go_worksheet
      EXPORTING #1 = x_sheet_number.
    CALL METHOD OF go_worksheet 'Activate'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_SET_CURRENCY_FORMAT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_SET_CURRENCY_FORMAT.

    SET PROPERTY OF go_range 'NumberFormat' = '#,##0.00 $'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_SET_CURRENT_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_START_ROW                    TYPE        I (default =1)
* | [--->] X_START_COL                    TYPE        I (default =1)
* | [--->] X_END_ROW                      TYPE        I (default =1)
* | [--->] X_END_COL                      TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD OLE_SET_CURRENT_RANGE.

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
* | Instance Private Method ZAG_CL_CSV_XLSX->OLE_SET_RANGE_PROPERTIES
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
  METHOD OLE_SET_RANGE_PROPERTIES.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>REMOVE_SPECIAL_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] Y_TEXT                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD REMOVE_SPECIAL_CHAR.

    CONSTANTS: c_regex_lect_upper TYPE c LENGTH 255 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
               c_regex_lect_lower TYPE c LENGTH 255 VALUE 'abcdefghijklmnopqrstuvwxyz',
               c_regex_digit      TYPE c LENGTH 255 VALUE '0123456789',
               c_regex_symb       TYPE c LENGTH 255 VALUE '!"%/=?;,.:-_@&+*()[]{}<>',
               c_regex_lect_acc   TYPE c LENGTH 255 VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'.

    DATA: lv_text    TYPE string,
          lv_new_str TYPE string VALUE IS INITIAL.

    DATA(lv_length) = strlen( y_text ).
    lv_text = y_text.

    DO lv_length TIMES.
      DATA(lv_index)     = sy-index - 1.
      DATA(lv_curr_char) = lv_text+lv_index(1).

      CHECK lv_curr_char CA c_regex_lect_upper
         OR lv_curr_char CA c_regex_lect_lower
         OR lv_curr_char CA c_regex_digit
         OR lv_curr_char CA c_regex_symb
         OR lv_curr_char CA c_regex_lect_acc  .

      IF lv_new_str IS INITIAL.
        lv_new_str = lv_curr_char.
      ELSE.
        lv_new_str = |{ lv_new_str }{ lv_curr_char }|.
      ENDIF.

    ENDDO.

    y_text = lv_new_str.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [<---] YT_SAP_DATA                    TYPE        TABLE
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPLOAD.

    DATA: lt_str_data TYPE string_table.

    "-------------------------------------------------

    IF x_source EQ c_source_local "XLSX only from local
      AND ( x_filename CP '*.xls' OR x_filename CP '*.xlsx' ).

      upload_excel_local(
        EXPORTING
          x_filename        = x_filename
          x_sap_struct_name = x_sap_struct_name
        IMPORTING
          yt_str_data       = lt_str_data[]
      ).

    ELSEIF x_filename CP '*.csv'.

      CASE x_source.
        WHEN c_source_local. "LOCAL Reading "-------------------------------------------------
          upload_csv_local(
            EXPORTING
              x_filename       = x_filename
            IMPORTING
              yt_str_data      = lt_str_data[]
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).

        WHEN c_source_server. "SERVER Reading "-------------------------------------------------
          upload_csv_server(
            EXPORTING
              x_filename       = x_filename
            IMPORTING
              yt_str_data      = lt_str_data[]
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).

      ENDCASE.

    ELSE.

      RAISE not_supported_file.

    ENDIF.

    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.


    "Conversion in SAP Format
    "-------------------------------------------------
    CHECK lt_str_data IS NOT INITIAL.
    IF x_header EQ 'X'.
      DELETE lt_str_data INDEX 1.
    ENDIF.

    LOOP AT lt_str_data ASSIGNING FIELD-SYMBOL(<data_str>).

      APPEND INITIAL LINE TO yt_sap_data ASSIGNING FIELD-SYMBOL(<data_sap>).
      conv_string_to_sap(
        EXPORTING
          x_str_data        = <data_str>
          x_sap_struct_name = x_sap_struct_name
        IMPORTING
          y_sap_data        = <data_sap>
        ).

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>UPLOAD_CSV_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPLOAD_CSV_LOCAL.

    REFRESH yt_str_data[].
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = x_filename
        filetype = 'ASC'
      CHANGING
        data_tab = yt_str_data[]
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>UPLOAD_CSV_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPLOAD_CSV_SERVER.

    DATA: ls_str_data TYPE string.

    "-------------------------------------------------

    OPEN DATASET x_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      CLOSE DATASET x_filename.
      RAISE unable_open_path.
    ENDIF.

    DO.

      CLEAR ls_str_data.
      READ DATASET x_filename INTO ls_str_data.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND ls_str_data TO yt_str_data.

    ENDDO.

    CLOSE DATASET x_filename.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>UPLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPLOAD_EXCEL_LOCAL.

    DATA: lv_except_msg    TYPE string,
          lv_tmp_data      TYPE string,
          lt_bin_tab       TYPE solix_tab,
          lcl_excel_ref    TYPE REF TO cl_fdt_xl_spreadsheet,
          lcl_table_descr  TYPE REF TO cl_abap_tabledescr,
          lcl_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <t_excel_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    REFRESH: yt_str_data[].

    "Read in binary the excel
    "-------------------------------------------------
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = x_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = DATA(lv_filelen)
        header     = DATA(lv_headerx)
      CHANGING
        data_tab   = lt_bin_tab[]
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.


    "Conversion in XSTRING NB cl_bcs_convert not returns headerx
    "-------------------------------------------------
*      DATA(lv_xstr_tab) = cl_bcs_convert=>solix_to_xstring( it_solix   = lt_bin_tab
*                                                            iv_size    = lv_filelen ).
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelen
      IMPORTING
        buffer       = lv_headerx
      TABLES
        binary_tab   = lt_bin_tab[]
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      RAISE unable_open_path.
    ENDIF.


    "Reading excel content using HEADERX
    "-------------------------------------------------
    TRY.
        lcl_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = x_filename
                                                   xdocument     = lv_headerx ) .
      CATCH cx_fdt_excel_core INTO DATA(lx_excel_core).
        lv_except_msg = lx_excel_core->get_text( ).
    ENDTRY .

    IF lcl_excel_ref IS BOUND.
      lcl_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
        IMPORTING
          worksheet_names = DATA(lt_worksheets) ).


      "Takes the first worksheet as default
      ASSIGN lt_worksheets[ 1 ] TO FIELD-SYMBOL(<woksheetname>).
      DATA(lcl_data_ref) = lcl_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <woksheetname> ).
      ASSIGN lcl_data_ref->* TO <t_excel_data>.

      TRY.

          lcl_struct_descr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).

        CATCH cx_sy_move_cast_error INTO DATA(lx_move_cast_error).
          lv_except_msg = lx_move_cast_error->get_text( ).
      ENDTRY.


      "Convert excel format into string table with columns separated by ; like in CSV
      "-------------------------------------------------
      LOOP AT <t_excel_data> ASSIGNING FIELD-SYMBOL(<excel>).
        APPEND INITIAL LINE TO yt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).

        LOOP AT lcl_struct_descr->components ASSIGNING FIELD-SYMBOL(<component>).
          ASSIGN COMPONENT sy-tabix OF STRUCTURE <excel> TO FIELD-SYMBOL(<excel_value>).
          CHECK sy-subrc EQ 0.

          lv_tmp_data = <excel_value>.
          conv_format_sap_to_xlsx(
            EXPORTING
              x_typekind   = <component>-type_kind
            CHANGING
              y_xlsx_value = lv_tmp_data
          ).


          IF <str_data> IS INITIAL.
            <str_data> = lv_tmp_data.
          ELSE.
            <str_data> = |{ <str_data> };{ lv_tmp_data }|.
          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDIF.



    "OLD Version
    "*********************************************************************
    IF 1 = 2.

      DATA: lv_filename    TYPE rlgrap-filename,
            lt_intern      TYPE TABLE OF alsmex_tabline,
            lt_intern_indx TYPE TABLE OF alsmex_tabline,
            ls_intern      LIKE LINE OF lt_intern,
            lv_end_col     TYPE i,
            lv_end_row     TYPE i,
            lv_tabix       TYPE n LENGTH 4,
            lv_new_val     TYPE string.

      DATA: lo_structdescr    TYPE REF TO cl_abap_structdescr.

      "-------------------------------------------------

      lv_filename = x_filename.

      IF lv_filename CS '.xlsx'.

        lv_end_row = 1048576.
        lv_end_col = 16384 .

      ELSEIF lv_filename CS '.xls'.

        lv_end_row = 65536.
        lv_end_col = 256.

      ELSE.
        EXIT.

      ENDIF.

      REFRESH lt_intern[].
      CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
        EXPORTING
          filename                = lv_filename
          i_begin_col             = 1
          i_begin_row             = 1
          i_end_col               = lv_end_col
          i_end_row               = lv_end_row
        TABLES
          intern                  = lt_intern[]
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
      CHECK lo_structdescr IS NOT INITIAL.

      "-------------------------------------------------

      SORT lt_intern BY row col.

      lt_intern_indx = lt_intern[].
      DELETE ADJACENT DUPLICATES FROM lt_intern_indx COMPARING row.


      LOOP AT lt_intern_indx ASSIGNING FIELD-SYMBOL(<intern>).

        APPEND INITIAL LINE TO yt_str_data ASSIGNING FIELD-SYMBOL(<row_data>).

        LOOP AT lo_structdescr->components ASSIGNING <component>.
          lv_tabix = sy-tabix.

          "The Empty fields aren't included into lt_intern
          "So, you need to assign a dummy value
          ASSIGN lt_intern[
            line_index( lt_intern[ row = <intern>-row
                                   col = lv_tabix ] ) ] TO FIELD-SYMBOL(<intern_tmp>).
          IF sy-subrc EQ 0.
            lv_new_val = <intern_tmp>-value.

          ELSE.
            lv_new_val = '|DUMMY|'.

          ENDIF.

          IF <row_data> IS INITIAL.
            <row_data> = lv_new_val.

          ELSE.
            <row_data> = |{ <row_data> };{ lv_new_val }|.

          ENDIF.

          REPLACE ALL OCCURRENCES OF '|DUMMY|' IN <row_data> WITH space.
        ENDLOOP.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.
ENDCLASS.

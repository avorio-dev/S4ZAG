CLASS zag_cl_csv_xlsx_ecc DEFINITION
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

    CONSTANTS c_cr_lf TYPE abap_cr_lf VALUE %_cr_lf ##no_text.
    CONSTANTS c_initial_data TYPE datum VALUE '00000000' ##no_text.
    CONSTANTS c_max_data TYPE datum VALUE '99991231' ##no_text.
    CONSTANTS c_separator_horizontal_tab TYPE abap_char1 VALUE %_horizontal_tab ##no_text.
    CONSTANTS c_separator_semicolon TYPE char1 VALUE ';' ##no_text.
    CONSTANTS c_source_local TYPE char4 VALUE 'LOCL' ##no_text.
    CONSTANTS c_source_server TYPE char4 VALUE 'SERV' ##no_text.
    CONSTANTS c_xls_black TYPE i VALUE 0 ##no_text.
    CONSTANTS c_xls_blue TYPE i VALUE 15773440 ##no_text.
    CONSTANTS c_xls_green TYPE i VALUE 13496520 ##no_text.
    CONSTANTS c_xls_navy TYPE i VALUE 6562560 ##no_text.
    CONSTANTS c_xls_red TYPE i VALUE 13486335 ##no_text.
    CONSTANTS c_xls_white TYPE i VALUE 16777215 ##no_text.
    CONSTANTS c_xls_yell TYPE i VALUE 2992895 ##no_text.

    CLASS-METHODS conv_sap_to_string
      IMPORTING
        !x_sap_struct_name TYPE tabname
        !x_sap_data TYPE any
        !x_separator TYPE char1 DEFAULT c_separator_semicolon
      EXPORTING
        !y_str_data TYPE string .
    CLASS-METHODS conv_string_to_sap
      IMPORTING
        !x_str_data TYPE string
        !x_sap_struct_name TYPE tabname
      EXPORTING
        !y_sap_data TYPE any .
    CLASS-METHODS download
      IMPORTING
        !x_filename TYPE string
        !x_header TYPE xfeld DEFAULT 'X'
        !x_sap_struct_name TYPE tabname
        !xt_sap_data TYPE table
        !x_source TYPE char4 DEFAULT 'LOCL'
      EXCEPTIONS
        not_supported_file
        unable_open_path .
    CLASS-METHODS f4_help_dir_input
      IMPORTING
        !x_source TYPE char4 DEFAULT 'LOCL'
      EXPORTING
        !y_path_input TYPE string .
    CLASS-METHODS f4_help_dir_output
      IMPORTING
        !x_source TYPE char4 DEFAULT 'LOCL'
      EXPORTING
        !y_path_output TYPE string .
    CLASS-METHODS get_desktop_directory
      RETURNING
        value(y_desktop_dir) TYPE string .
    CLASS-METHODS get_fieldcat_from_itab
      IMPORTING
        !xt_itab TYPE STANDARD TABLE
      EXPORTING
        !yt_fcat TYPE lvc_t_fcat .
    CLASS-METHODS remove_special_char
      CHANGING
        !y_text TYPE string .
    CLASS-METHODS upload
      IMPORTING
        !x_filename TYPE string
        !x_header TYPE xfeld DEFAULT 'X'
        !x_sap_struct_name TYPE tabname
        !x_source TYPE char4 DEFAULT 'LOCL'
      EXPORTING
        !yt_sap_data TYPE table
      EXCEPTIONS
        not_supported_file
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

    CLASS-METHODS conv_data_to_ext
      IMPORTING
        !x_data_int TYPE dats
        !x_separator TYPE c DEFAULT '/'
      EXPORTING
        !y_data_ext TYPE string .
    CLASS-METHODS conv_data_to_int
      IMPORTING
        !x_data_ext TYPE string
      EXPORTING
        !y_data_int TYPE dats .
    CLASS-METHODS conv_format_sap_to_xlsx
      IMPORTING
        !x_typekind TYPE abap_typekind
      CHANGING
        !y_xlsx_value TYPE string .
    CLASS-METHODS conv_format_xlsx_to_sap
      IMPORTING
        !x_typekind TYPE abap_typekind
      CHANGING
        !y_sap_value TYPE string .
    CLASS-METHODS conv_time_to_ext
      IMPORTING
        !x_time TYPE uzeit
      EXPORTING
        !y_time TYPE string .
    CLASS-METHODS conv_time_to_int
      IMPORTING
        !x_time TYPE string
      EXPORTING
        !y_time TYPE uzeit .
    CLASS-METHODS download_csv_local
      IMPORTING
        !x_filename TYPE string
      CHANGING
        !xt_str_data TYPE string_table
      EXCEPTIONS
        unable_open_path .
    CLASS-METHODS download_csv_server
      IMPORTING
        !x_filename TYPE string
        !xt_str_data TYPE string_table
      EXCEPTIONS
        unable_open_path .
    CLASS-METHODS download_excel_local
      IMPORTING
        !x_filename TYPE string
        !xt_fcat TYPE lvc_t_fcat
      CHANGING
        !xt_str_data TYPE string_table
      EXCEPTIONS
        unable_open_path .
    CLASS-METHODS get_header_from_ddic
      IMPORTING
        !x_sap_struct_name TYPE tabname
      CHANGING
        !y_str_header TYPE string .
    CLASS-METHODS get_header_from_itab
      IMPORTING
        !xt_itab TYPE STANDARD TABLE
      CHANGING
        !y_str_header TYPE string .
    CLASS-METHODS upload_csv_local
      IMPORTING
        !x_filename TYPE string
      EXPORTING
        !yt_str_data TYPE string_table
      EXCEPTIONS
        unable_open_path .
    CLASS-METHODS upload_csv_server
      IMPORTING
        !x_filename TYPE string
      EXPORTING
        !yt_str_data TYPE string_table
      EXCEPTIONS
        unable_open_path .
    CLASS-METHODS upload_excel_local
      IMPORTING
        !x_filename TYPE string
        !x_sap_struct_name TYPE tabname
      EXPORTING
        !yt_str_data TYPE string_table
      EXCEPTIONS
        unable_open_path .
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
        !x_end_row TYPE i DEFAULT 1
        !x_end_col TYPE i DEFAULT 1 .
    METHODS ole_set_range_properties
      IMPORTING
        !x_background TYPE i DEFAULT c_xls_white
        !x_font_name TYPE string DEFAULT 'Arial'
        !x_size TYPE i DEFAULT 12
        !x_bold TYPE i DEFAULT 0
        !x_italic TYPE i DEFAULT 0
        !x_color TYPE i DEFAULT c_xls_black
        !x_underline TYPE i DEFAULT 0
        !x_set_borders TYPE i DEFAULT 0 .
ENDCLASS.



CLASS ZAG_CL_CSV_XLSX_ECC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>CONV_DATA_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_INT                     TYPE        DATS
* | [--->] X_SEPARATOR                    TYPE        C (default ='/')
* | [<---] Y_DATA_EXT                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_data_to_ext.

    y_data_ext = |{ x_data_int+6(2) }{ x_separator }{ x_data_int+4(2) }{ x_separator }{ x_data_int(4) }|.
    CONDENSE y_data_ext NO-GAPS.

  ENDMETHOD.                    "CONV_DATA_TO_EXT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>CONV_DATA_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_EXT                     TYPE        STRING
* | [<---] Y_DATA_INT                     TYPE        DATS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_data_to_int.

    y_data_int = c_initial_data.

    CHECK strlen( x_data_ext ) EQ 10.
    y_data_int = |{ x_data_ext+6(4) }{ x_data_ext+3(2) }{ x_data_ext(2) }|.

  ENDMETHOD.                    "CONV_DATA_TO_INT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>CONV_FORMAT_SAP_TO_XLSX
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TYPEKIND                     TYPE        ABAP_TYPEKIND
* | [<-->] Y_XLSX_VALUE                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_format_sap_to_xlsx.

    DATA: lv_Data_int TYPE sy-datum,
          lv_Time     TYPE sy-uzeit.

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

        lv_data_int = y_xlsx_value.
        conv_data_to_ext(
          EXPORTING
            x_data_int  = lv_data_int
            x_separator = '/'
          IMPORTING
            y_data_ext  = y_xlsx_value
          ).

        CONDENSE y_xlsx_value NO-GAPS.

      WHEN cl_abap_typedescr=>typekind_time.    "Time "-------------------------------------------------

        lv_time = y_xlsx_value.
        conv_time_to_ext(
          EXPORTING
            x_time = lv_time
          IMPORTING
            y_time = y_xlsx_value
        ).

        CONDENSE y_xlsx_value NO-GAPS.

      WHEN OTHERS.
        "Normal Text -> Nothing To Do

    ENDCASE.

  ENDMETHOD.                    "CONV_FORMAT_SAP_TO_XLSX


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>CONV_FORMAT_XLSX_TO_SAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TYPEKIND                     TYPE        ABAP_TYPEKIND
* | [<-->] Y_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_format_xlsx_to_sap.

    DATA: lv_tmp_dats TYPE sy-datum,
          lv_tmp_time TYPE sy-uzeit.

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
           y_data_int = lv_tmp_dats
       ).

        y_sap_value = lv_tmp_dats.
        CONDENSE y_sap_value NO-GAPS.

      WHEN cl_abap_typedescr=>typekind_time. "Time "-------------------------------------------------

        conv_time_to_int(
          EXPORTING
            x_time = y_sap_value
          IMPORTING
            y_time = lv_tmp_time
        ).

        y_sap_value = lv_tmp_time.
        CONDENSE y_sap_value NO-GAPS.

      WHEN OTHERS.
        "Normal Data -> Nothing To Do

    ENDCASE.

  ENDMETHOD.                    "CONV_FORMAT_XLSX_TO_SAP


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>CONV_SAP_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [--->] X_SAP_DATA                     TYPE        ANY
* | [--->] X_SEPARATOR                    TYPE        CHAR1 (default =C_SEPARATOR_SEMICOLON)
* | [<---] Y_STR_DATA                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_sap_to_string.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lv_sap_ref     TYPE REF TO data,
          lv_tmp_data    TYPE string.

    FIELD-SYMBOLS: <component> LIKE LINE OF lo_structdescr->components,
                   <value>     TYPE any.

    "-------------------------------------------------

    y_str_data = ''.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
    CHECK lo_structdescr IS NOT INITIAL.

    LOOP AT lo_structdescr->components ASSIGNING <component>.

      ASSIGN COMPONENT <component>-name OF STRUCTURE x_sap_data TO <value>.

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

  ENDMETHOD.                    "CONV_SAP_TO_STRING


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>CONV_STRING_TO_SAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STR_DATA                     TYPE        STRING
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [<---] Y_SAP_DATA                     TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_string_to_sap.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lv_sap_ref     TYPE REF TO data,
          lv_tmp_dats    TYPE dats,
          lv_tmp_time    TYPE sy-uzeit,
          lv_tmp_string  TYPE string,
          lv_sx          TYPE string,
          lv_dx          TYPE string.

    FIELD-SYMBOLS: <sap_data>  TYPE any,
                   <component> LIKE LINE OF lo_structdescr->components,
                   <value>     TYPE any.


    "-------------------------------------------------

    CLEAR y_sap_data.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
    CHECK lo_structdescr IS NOT INITIAL.

    CREATE DATA lv_sap_ref TYPE (x_sap_struct_name).
    ASSIGN lv_sap_ref->* TO <sap_data>.

    "-------------------------------------------------

    lv_tmp_string = x_str_data.
    LOOP AT lo_structdescr->components ASSIGNING <component>.

      SPLIT lv_tmp_string AT ';'
       INTO lv_sx
            lv_dx.

      ASSIGN COMPONENT <component>-name OF STRUCTURE <sap_data> TO <value>.

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

  ENDMETHOD.                    "CONV_STRING_TO_SAP


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>CONV_TIME_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        UZEIT
* | [<---] Y_TIME                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_time_to_ext.

    y_time = |{ x_time(2) }:{ x_time+2(2) }:{ x_time+4(2) }|.
    CONDENSE y_time NO-GAPS.

  ENDMETHOD.                    "CONV_TIME_TO_EXT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>CONV_TIME_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        STRING
* | [<---] Y_TIME                         TYPE        UZEIT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_time_to_int.

    CHECK strlen( x_time ) EQ 8.
    y_time = |{ x_time(2) }{ x_time+3(2) }{ x_time+5(2) }|.

  ENDMETHOD.                    "CONV_TIME_TO_INT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>DOWNLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [--->] XT_SAP_DATA                    TYPE        TABLE
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download.

    DATA: lt_str_data TYPE string_table,
          lt_fcat     TYPE lvc_t_fcat.

    FIELD-SYMBOLS: <str_data> LIKE LINE OF lt_str_data,
                   <sap_data> TYPE any.


    "Build header
    "-------------------------------------------------
    IF x_header EQ abap_true.

      APPEND INITIAL LINE TO lt_str_data ASSIGNING <str_data>.

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
    LOOP AT xt_sap_data ASSIGNING <sap_data>.

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
          yt_fcat = lt_fcat
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

  ENDMETHOD.                    "DOWNLOAD


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>DOWNLOAD_CSV_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<-->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_csv_local.

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

  ENDMETHOD.                    "DOWNLOAD_CSV_LOCAL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>DOWNLOAD_CSV_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_csv_server.

    FIELD-SYMBOLS: <str_data> LIKE LINE OF xt_str_data.

    OPEN DATASET x_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      CLOSE DATASET x_filename.
      RAISE unable_open_path.
    ENDIF.

    LOOP AT xt_str_data ASSIGNING <str_data>.
      TRANSFER <str_data> TO x_filename.
    ENDLOOP.

    CLOSE DATASET x_filename.

  ENDMETHOD.                    "DOWNLOAD_CSV_SERVER


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>DOWNLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_FCAT                        TYPE        LVC_T_FCAT
* | [<-->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_excel_local.

    DATA: lt_str_data   TYPE string_table,
          lv_tabix      TYPE sy-tabix,
          lv_lines_fcat TYPE i,
          lv_lines_tab  TYPE i,

          lo_ole        TYPE REF TO zag_cl_csv_xlsx_ecc.

    FIELD-SYMBOLS: <str_data> LIKE LINE OF lt_str_data,
                   <fcat>     LIKE LINE OF xt_fcat.

    lv_lines_fcat = lines( xt_fcat ).
    lv_lines_tab  = lines( xt_str_data ).

    "Separator conversione from ';' TO horizontal_tab
    "-------------------------------------------------
    lt_str_data = xt_str_data[].
    LOOP AT lt_str_data ASSIGNING <str_data>.
      REPLACE ALL OCCURRENCES OF c_separator_semicolon IN <str_data>
        WITH c_separator_horizontal_tab.
    ENDLOOP.

    CREATE OBJECT lo_ole.

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
    LOOP AT xt_fcat ASSIGNING <fcat>
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

  ENDMETHOD.                    "DOWNLOAD_EXCEL_LOCAL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>F4_HELP_DIR_INPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [<---] Y_PATH_INPUT                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD f4_help_dir_input.

    DATA: lv_path TYPE string VALUE IS INITIAL.

    DATA: lv_server         TYPE msxxlist-name,
          lv_path_tmp       TYPE dxfields-longpath,
          lv_instancenumber TYPE instanz-systemnr.

    DATA: lt_filetable TYPE filetable,
          lcl_ref_itab TYPE REF TO file_table,
          lv_rc        TYPE i.

    FIELD-SYMBOLS: <filetable> LIKE LINE OF lt_filetable.

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

        lv_path = zag_cl_csv_xlsx_ecc=>get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          EXPORTING
*           default_filename  = '*.csv'
            initial_directory = lv_path
          CHANGING
            file_table        = lt_filetable
            rc                = lv_rc.

        READ TABLE lt_filetable ASSIGNING <filetable> INDEX 1.
        CHECK sy-subrc EQ 0.
        lv_path = <filetable>-filename.

    ENDCASE.

    y_path_input = lv_path.

  ENDMETHOD.                    "F4_HELP_DIR_INPUT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>F4_HELP_DIR_OUTPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [<---] Y_PATH_OUTPUT                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD f4_help_dir_output.

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

  ENDMETHOD.                    "F4_HELP_DIR_OUTPUT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>GET_DESKTOP_DIRECTORY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] Y_DESKTOP_DIR                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_desktop_directory.

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

  ENDMETHOD.                    "GET_DESKTOP_DIRECTORY


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>GET_FIELDCAT_FROM_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ITAB                        TYPE        STANDARD TABLE
* | [<---] YT_FCAT                        TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_fieldcat_from_itab.

    DATA: lref_table    TYPE REF TO data,
          lt_salv_table TYPE REF TO cl_salv_table.

    DATA: lx_ai_system_fault TYPE REF TO cx_ai_system_fault,
          lv_except_msg      TYPE string.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE.

    "-------------------------------------------------

    CREATE DATA lref_table LIKE xt_itab.
    ASSIGN lref_table->* TO <table>.

    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = lt_salv_table
                                CHANGING
                                  t_table        = <table>  ).
        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lt_salv_table->get_columns( ) " ALV Filter
                                                                     r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
                                                                     ) .

      CATCH cx_ai_system_fault INTO lx_ai_system_fault.
        lv_except_msg = lx_ai_system_fault->get_text( ).
    ENDTRY.

    DELETE yt_fcat WHERE fieldname EQ 'MANDT'.

  ENDMETHOD.                    "GET_FIELDCAT_FROM_ITAB


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>GET_HEADER_FROM_DDIC
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [<-->] Y_STR_HEADER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_header_from_ddic.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lt_dfies_tab   TYPE STANDARD TABLE OF dfies,
          lv_new_col     TYPE string.

    DATA: lv_tabname     TYPE ddobjname.

    FIELD-SYMBOLS: <component> LIKE LINE OF lo_structdescr->components,
                   <dfies>     LIKE LINE OF lt_dfies_tab.

    "-------------------------------------------------

    y_str_header = ''.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).
    CHECK lo_structdescr IS NOT INITIAL.

    lv_tabname = x_sap_struct_name.

    REFRESH lt_dfies_tab[].
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = lv_tabname
      TABLES
        dfies_tab      = lt_dfies_tab[]
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    CHECK lt_dfies_tab[] IS NOT INITIAL.
    SORT lt_dfies_tab BY fieldname.

    "-------------------------------------------------

    LOOP AT lo_structdescr->components ASSIGNING <component>.

      CHECK <component>-name NE 'MANDT'.

      lv_new_col = ''.
      READ TABLE lt_dfies_tab ASSIGNING <dfies>
        WITH KEY fieldname = <component>-name
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_new_col = <dfies>-fieldtext.
      ENDIF.

      IF y_str_header IS INITIAL.
        y_str_header = lv_new_col.

      ELSE.
        y_str_header = |{ y_str_header };{ lv_new_col }|.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "GET_HEADER_FROM_DDIC


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>GET_HEADER_FROM_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ITAB                        TYPE        STANDARD TABLE
* | [<-->] Y_STR_HEADER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_header_from_itab.

    DATA: lt_fcat TYPE lvc_t_fcat.

    FIELD-SYMBOLS: <fcat> LIKE LINE OF lt_fcat.

    y_str_header = ''.

    get_fieldcat_from_itab(
      EXPORTING
        xt_itab = xt_itab
      IMPORTING
        yt_fcat = lt_fcat                  " Catalogo campo per ListViewerControl
    ).

    LOOP AT lt_fcat ASSIGNING <fcat>.

      IF y_str_header IS INITIAL.
        y_str_header = <fcat>-reptext.

      ELSE.
        y_str_header = |{ y_str_header };{ <fcat>-reptext }|.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "GET_HEADER_FROM_ITAB


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_ADD_SHEET
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_add_sheet.

    "Insert a new sheet with input name

    GET PROPERTY OF go_application 'Sheets' = go_sheet.
    CALL METHOD OF
        go_sheet
        'Add'    = go_worksheet.
    SET PROPERTY OF go_worksheet 'Name' = sy-repid.
    GET PROPERTY OF go_application 'ACTIVESHEET' = go_worksheet.

  ENDMETHOD.                    "OLE_ADD_SHEET


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_CLIPBOARD_COPY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_copy.

    CALL METHOD OF
        go_range
        'Copy'.

  ENDMETHOD.                    "OLE_CLIPBOARD_COPY


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_CLIPBOARD_EXPORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_export.

    DATA: lt_char_data TYPE tt_char_table,
          lv_rc TYPE i.

    lv_rc = 0.

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

  ENDMETHOD.                    "OLE_CLIPBOARD_EXPORT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_CLIPBOARD_PASTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_START_ROW                    TYPE        I (default =1)
* | [--->] X_START_COL                    TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_paste.

    "Select the cell a1
    CALL METHOD OF
        go_worksheet
        'cells'      = go_cell
      EXPORTING
        #1           = x_start_row "Row
        #2           = x_start_col."Column

    "Paste clipboard from cell A1
    CALL METHOD OF
        go_cell
        'select'.
    CALL METHOD OF
        go_worksheet
        'paste'.

  ENDMETHOD.                    "OLE_CLIPBOARD_PASTE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_CLIPBOARD_PASTE_SPECIAL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_clipboard_paste_special.

    CALL METHOD OF
        go_range
        'PasteSpecial'.
    GET PROPERTY OF go_range 'EntireColumn' = go_column.
    CALL METHOD OF
        go_column
        'AutoFit'.

  ENDMETHOD.                    "OLE_CLIPBOARD_PASTE_SPECIAL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_INIT_EXCEL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_init_excel.

    CREATE OBJECT go_application 'Excel.Application'.
    SET PROPERTY OF go_application 'Visible' = 1.
    SET PROPERTY OF go_application 'SheetsInNewWorkbook' = 1. "no of sheets

    CALL METHOD OF
        go_application
        'Workbooks'    = go_workbooks.
    CALL METHOD OF
        go_workbooks
        'Add'        = go_workbook.

  ENDMETHOD.                    "OLE_INIT_EXCEL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_SAVE_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_save_excel.

    DATA: lv_filename TYPE rlgrap-filename,
          lv_path     TYPE rlgrap-filename,
          lv_error    .

    lv_filename = x_filename.


    lv_path  = lv_filename .
    lv_error = abap_false.

    CALL METHOD OF go_workbook 'SaveAs'
      EXPORTING
        #1          = lv_path.
    IF sy-subrc <> 0.
      lv_error = abap_true.
    ENDIF.

    CALL METHOD OF go_workbook 'CLOSE'
      EXPORTING
        #1          = 'YES'.

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


  ENDMETHOD.                    "OLE_SAVE_EXCEL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_SET_ACTIVE_SHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SHEET_NUMBER                 TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_active_sheet.

    CALL METHOD OF
        go_application
        'Worksheets'   = go_worksheet
      EXPORTING
        #1             = x_sheet_number.
    CALL METHOD OF
        go_worksheet
        'Activate'.

  ENDMETHOD.                    "OLE_SET_ACTIVE_SHEET


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_SET_CURRENCY_FORMAT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_currency_format.

    SET PROPERTY OF go_range 'NumberFormat' = '#,##0.00 $'.

  ENDMETHOD.                    "OLE_SET_CURRENCY_FORMAT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_SET_CURRENT_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_START_ROW                    TYPE        I (default =1)
* | [--->] X_START_COL                    TYPE        I (default =1)
* | [--->] X_END_ROW                      TYPE        I (default =1)
* | [--->] X_END_COL                      TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ole_set_current_range.

    " 1. SELECT starting cell
    CALL METHOD OF
        go_worksheet
        'cells'      = go_cellstart
      EXPORTING
        #1           = x_start_row
        #2           = x_start_col.

    " 2. Select ending cell
    CALL METHOD OF
        go_worksheet
        'cells'      = go_cellend
      EXPORTING
        #1           = x_end_row
        #2           = x_end_col.

    " Select the Range:
    CALL METHOD OF
        go_worksheet
        'range'      = go_range
      EXPORTING
        #1           = go_cellstart
        #2           = go_cellend.

    CALL METHOD OF
        go_range
        'Select'.

  ENDMETHOD.                    "OLE_SET_CURRENT_RANGE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX_ECC->OLE_SET_RANGE_PROPERTIES
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

    CALL METHOD OF
        go_range
        'Interior' = go_interior.
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
      CALL METHOD OF
          go_range
          'borders' = go_borders
        EXPORTING
          #1        = '7'."xledgeleft
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous

      CALL METHOD OF
          go_range
          'borders' = go_borders
        EXPORTING
          #1        = '8'."xledgetop
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous

      CALL METHOD OF
          go_range
          'borders' = go_borders
        EXPORTING
          #1        = '9'."xledgebottom
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous


      CALL METHOD OF
          go_range
          'borders' = go_borders
        EXPORTING
          #1        = '10'."xledgeright
      SET PROPERTY OF go_borders 'linestyle' = '1'. "xlcontinuous

** Increase the weight of the border if you want, in this case only for EdgeRight:
*  SET PROPERTY OF lo_borders 'weight' = 4. "xlthick
    ENDIF.

    GET PROPERTY OF go_range 'EntireColumn' = go_column.
    CALL METHOD OF
        go_column
        'AutoFit'.

  ENDMETHOD.                    "OLE_SET_RANGE_PROPERTIES


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>REMOVE_SPECIAL_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] Y_TEXT                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_special_char.

    CONSTANTS: c_regex_lect_upper TYPE c LENGTH 255 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
               c_regex_lect_lower TYPE c LENGTH 255 VALUE 'abcdefghijklmnopqrstuvwxyz',
               c_regex_digit      TYPE c LENGTH 255 VALUE '0123456789',
               c_regex_symb       TYPE c LENGTH 255 VALUE '!"%/=?;,.:-_@&+*()[]{}<>',
               c_regex_lect_acc   TYPE c LENGTH 255 VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'.

    DATA: lv_text    TYPE string,
          lv_new_str TYPE string VALUE IS INITIAL,
          lv_length  TYPE i,
          lv_index   TYPE sy-index,
          lv_curr_char.

    lv_length = strlen( y_text ).
    lv_text   = y_text.

    DO lv_length TIMES.
      lv_index     = sy-index - 1.
      lv_curr_char = lv_text+lv_index(1).

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

  ENDMETHOD.                    "REMOVE_SPECIAL_CHAR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX_ECC=>UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [--->] X_SOURCE                       TYPE        CHAR4 (default ='LOCL')
* | [<---] YT_SAP_DATA                    TYPE        TABLE
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload.

    DATA: lt_str_data TYPE string_table.

    FIELD-SYMBOLS: <data_str> LIKE LINE OF lt_str_data,
                  <data_sap> TYPE any.

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

    LOOP AT lt_str_data ASSIGNING <data_str>.

      APPEND INITIAL LINE TO yt_sap_data ASSIGNING <data_sap>.
      conv_string_to_sap(
        EXPORTING
          x_str_data        = <data_str>
          x_sap_struct_name = x_sap_struct_name
        IMPORTING
          y_sap_data        = <data_sap>
        ).

    ENDLOOP.

  ENDMETHOD.                    "UPLOAD


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>UPLOAD_CSV_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_csv_local.

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

  ENDMETHOD.                    "UPLOAD_CSV_LOCAL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>UPLOAD_CSV_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_csv_server.

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

  ENDMETHOD.                    "UPLOAD_CSV_SERVER


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX_ECC=>UPLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_SAP_STRUCT_NAME              TYPE        TABNAME
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_excel_local.

    DATA: lv_except_msg    TYPE string,
          lv_tmp_data      TYPE string,
          lt_bin_tab       TYPE solix_tab,
          lcl_excel_ref    TYPE REF TO cl_fdt_xl_spreadsheet,
          lcl_table_descr  TYPE REF TO cl_abap_tabledescr,
          lcl_struct_descr TYPE REF TO cl_abap_structdescr,

          lv_filelen     	 TYPE i,
          lv_headerx       TYPE xstring,
          lcl_data_ref     TYPE REF TO data,

          lt_worksheets    TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.

    DATA: lx_excel_core      TYPE REF TO cx_fdt_excel_core,
          lx_move_cast_error TYPE REF TO cx_sy_move_cast_error.

    FIELD-SYMBOLS: <t_excel_data> TYPE STANDARD TABLE,
                   <woksheetname> LIKE LINE OF lt_worksheets,
                   <excel>        TYPE any,
                   <str_data>     LIKE LINE OF yt_str_data,
                   <component>    LIKE LINE OF lcl_struct_descr->components,
                   <excel_value>  TYPE any.

    "-------------------------------------------------

    REFRESH: yt_str_data[].

    "Read in binary the excel
    "-------------------------------------------------
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = x_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_filelen
        header     = lv_headerx
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
        CREATE OBJECT lcl_excel_ref
          EXPORTING
            document_name = x_filename
            xdocument     = lv_headerx.

      CATCH cx_fdt_excel_core INTO lx_excel_core.
        lv_except_msg = lx_excel_core->get_text( ).
    ENDTRY .

    IF lcl_excel_ref IS BOUND.
      lcl_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
        IMPORTING
          worksheet_names = lt_worksheets ).


      "Takes the first worksheet as default
      READ TABLE lt_worksheets ASSIGNING <woksheetname> INDEX 1.
      lcl_data_ref = lcl_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <woksheetname> ).
      ASSIGN lcl_data_ref->* TO <t_excel_data>.

      TRY.

          lcl_struct_descr ?= cl_abap_typedescr=>describe_by_name( x_sap_struct_name ).

        CATCH cx_sy_move_cast_error INTO lx_move_cast_error.
          lv_except_msg = lx_move_cast_error->get_text( ).
      ENDTRY.


      "Convert excel format into string table with columns separated by ; like in CSV
      "-------------------------------------------------
      LOOP AT <t_excel_data> ASSIGNING <excel>.
        APPEND INITIAL LINE TO yt_str_data ASSIGNING <str_data>.

        LOOP AT lcl_struct_descr->components ASSIGNING <component>.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE <excel> TO <excel_value>.
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

      FIELD-SYMBOLS: <intern>     LIKE LINE OF lt_intern_indx,
                     <row_data>   LIKE LINE OF yt_str_data,
                     <intern_tmp> LIKE LINE OF lt_intern.

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


      LOOP AT lt_intern_indx ASSIGNING <intern>.

        APPEND INITIAL LINE TO yt_str_data ASSIGNING <row_data>.

        LOOP AT lo_structdescr->components ASSIGNING <component>.
          lv_tabix = sy-tabix.

          "The Empty fields aren't included into lt_intern
          "So, you need to assign a dummy value
          READ TABLE lt_intern ASSIGNING <intern_tmp>
            WITH KEY  row = <intern>-row
                      col = lv_tabix
                      BINARY SEARCH.
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


  ENDMETHOD.                    "UPLOAD_EXCEL_LOCAL
ENDCLASS.

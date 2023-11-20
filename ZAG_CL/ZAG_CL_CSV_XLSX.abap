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
    BEGIN OF ty_conversions_errors,
        row_num TYPE i,
        field   TYPE fieldname,
        error   TYPE string,
      END OF ty_conversions_errors .
  types:
    tt_char_table TYPE TABLE OF ty_char_data-text .
  types:
    tt_conversions_errors TYPE TABLE OF ty_conversions_errors .

  constants C_CR_LF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  constants C_INITIAL_DATA type DATUM value '00000000' ##NO_TEXT.
  constants C_INITIAL_TIME type TIME value 000000 ##NO_TEXT.
  constants C_MAX_DATA type DATUM value '99991231' ##NO_TEXT.
  constants C_SEPARATOR_HORIZONTAL_TAB type ABAP_CHAR1 value %_HORIZONTAL_TAB ##NO_TEXT.
  constants C_SEPARATOR_SEMICOLON type CHAR1 value ';' ##NO_TEXT.
  constants C_SOURCE_LOCAL type CHAR1 value 'L' ##NO_TEXT.
  constants C_SOURCE_SERVER type CHAR1 value 'S' ##NO_TEXT.
  constants C_XLS_BLACK type I value 0 ##NO_TEXT.
  constants C_XLS_BLUE type I value 15773440 ##NO_TEXT.
  constants C_XLS_GREEN type I value 13496520 ##NO_TEXT.
  constants C_XLS_NAVY type I value 6562560 ##NO_TEXT.
  constants C_XLS_RED type I value 13486335 ##NO_TEXT.
  constants C_XLS_WHITE type I value 16777215 ##NO_TEXT.
  constants C_XLS_YELL type I value 2992895 ##NO_TEXT.

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
      !Y_DATA_INT type DATS
    exceptions
      FORMAT_ERROR
      PLAUSIBILITY_ERROR .
  class-methods CONV_NUMB_TO_EXT
    importing
      !X_NUMB_INT type STRING
    exporting
      !Y_NUMB_EXT type STRING .
  class-methods CONV_NUMB_TO_INT
    importing
      !X_NUMB_EXT type STRING
    exporting
      !Y_NUMB_INT type DECFLOAT
    exceptions
      FORMAT_ERROR
      PLAUSIBILITY_ERROR .
  class-methods CONV_SAP_TO_STRING
    importing
      !XO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR optional
      !X_SAP_DATA type ANY
      !X_SEPARATOR type CHAR1 default C_SEPARATOR_SEMICOLON
    exporting
      !Y_STR_DATA type STRING
      !Y_ERROR_MSG type STRING
    exceptions
      CONVERSION_ERROR .
  class-methods CONV_STRING_TO_SAP
    importing
      !X_STR_DATA type STRING
      !XO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR optional
    exporting
      !Y_SAP_DATA type ANY
      !Y_CONVERSIONS_ERRORS type TY_CONVERSIONS_ERRORS
    exceptions
      CONVERSION_ERROR .
  class-methods CONV_TIME_TO_EXT
    importing
      !X_TIME type UZEIT
    exporting
      !Y_TIME type STRING .
  class-methods CONV_TIME_TO_INT
    importing
      !X_TIME type STRING
    exporting
      !Y_TIME type UZEIT
    exceptions
      FORMAT_ERROR
      PLAUSIBILITY_ERROR .
  class-methods DOWNLOAD
    importing
      !X_FILENAME type STRING
      !X_HEADER type XFELD default 'X'
      !X_SOURCE type CHAR1 default 'L'
    changing
      !XT_SAP_DATA type TABLE
    exceptions
      NOT_SUPPORTED_FILE
      UNABLE_OPEN_PATH
      UNABLE_DEFINE_STRUCTURE .
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
  class-methods GET_COMPDESCR_FROM_DATA
    importing
      !XS_SAP_LINE type ANY optional
      !XT_SAP_TABLE type TABLE optional
    exporting
      !YO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
    exceptions
      UNABLE_DEFINE_STRUCTURE .
  class-methods GET_DESKTOP_DIRECTORY
    returning
      value(Y_DESKTOP_DIR) type STRING .
  class-methods GET_FIELDCAT_FROM_ITAB
    importing
      !XT_ITAB type STANDARD TABLE
    exporting
      !YT_FCAT type LVC_T_FCAT .
  class-methods GET_HEADER_FROM_DATA
    importing
      !XS_SAP_LINE type ANY optional
      !XT_SAP_TABLE type TABLE optional
    changing
      !Y_STR_HEADER type STRING
    exceptions
      UNABLE_DEFINE_STRUCTURE .
  class-methods REMOVE_SPECIAL_CHAR
    changing
      !Y_TEXT type STRING .
  class-methods UPLOAD
    importing
      !X_FILENAME type STRING
      !X_HEADER type XFELD default 'X'
      !X_SOURCE type CHAR1 default 'L'
    exporting
      !YT_SAP_DATA type TABLE
      !YT_CONVERSIONS_ERRORS type TT_CONVERSIONS_ERRORS
    exceptions
      NOT_SUPPORTED_FILE
      UNABLE_OPEN_PATH
      UNABLE_DEFINE_STRUCTURE
      EMPTY_FILE
      CONVERSION_ERROR .
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
      !XT_STR_DATA type STRING_TABLE
      !X_USE_CUSTOM_OLE type FLAG default SPACE
    changing
      !XT_SAP_DATA type STANDARD TABLE
    exceptions
      UNABLE_OPEN_PATH .
  class-methods DOWNLOAD_EXCEL_SERVER
    importing
      !X_FILENAME type STRING
      !XT_SAP_DATA type STANDARD TABLE
    exceptions
      UNABLE_OPEN_PATH .
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
      !X_HEADER type XFELD optional
      !X_FILENAME type STRING
      !XO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
    exporting
      !YT_SAP_DATA type TABLE
    exceptions
      UNABLE_OPEN_PATH
      EMPTY_FILE .
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
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_DATA_TO_EXT
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
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_DATA_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_EXT                     TYPE        STRING
* | [<---] Y_DATA_INT                     TYPE        DATS
* | [EXC!] FORMAT_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_data_to_int.

    y_data_int = c_initial_data.

    IF strlen( x_data_ext ) NE 10.
      RAISE format_error.
    ENDIF.

    y_data_int = |{ x_data_ext+6(4) }{ x_data_ext+3(2) }{ x_data_ext(2) }|.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = y_data_int
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE plausibility_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_NUMB_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_NUMB_INT                     TYPE        STRING
* | [<---] Y_NUMB_EXT                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_numb_to_ext.

    DATA: lv_numb TYPE string.

    y_numb_ext  = 0.

    lv_numb     = x_numb_int.

    REPLACE '.' IN lv_numb WITH ','.
    FIND '-' IN lv_numb.
    IF sy-subrc EQ 0.
      REPLACE '-' IN lv_numb WITH ''.
      lv_numb = |-{ lv_numb }|.
    ENDIF.

    CONDENSE lv_numb NO-GAPS.

    y_numb_ext = lv_numb.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_NUMB_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_NUMB_EXT                     TYPE        STRING
* | [<---] Y_NUMB_INT                     TYPE        DECFLOAT
* | [EXC!] FORMAT_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_numb_to_int.

    DATA: lv_numb  TYPE string,
          lv_count TYPE i.

    y_numb_int = 0.

    lv_numb    = x_numb_ext.

    "-------------------------------------------------

    IF lv_numb CN '01234567890.,-'.
      RAISE format_error.
    ENDIF.

    FIND '-' IN lv_numb.
    IF sy-subrc EQ 0.
      REPLACE ALL OCCURRENCES OF '-' IN lv_numb WITH ''.
      lv_numb = |{ lv_numb }-|.
    ENDIF.

    "-------------------------------------------------

    DATA(lv_counter_comma) = 0.
    FIND ALL OCCURRENCES OF ',' IN lv_numb MATCH COUNT lv_counter_comma.

    DATA(lv_counter_dot) = 0.
    FIND ALL OCCURRENCES OF '.' IN lv_numb MATCH COUNT lv_counter_dot.

    IF lv_counter_comma EQ lv_counter_dot.
      "ex. 1,000.25 / 1.000,25

      DATA(lv_offset_comma) = 0.
      FIND FIRST OCCURRENCE OF ',' IN lv_numb MATCH OFFSET lv_offset_comma.

      DATA(lv_offset_dot) = 0.
      FIND FIRST OCCURRENCE OF '.' IN lv_numb MATCH OFFSET lv_offset_dot.

      IF lv_offset_comma LT lv_offset_dot.
        "1,000.25

        REPLACE ALL OCCURRENCES OF ',' IN lv_numb WITH ''.

        FIND ALL OCCURRENCES OF '.' IN lv_numb MATCH COUNT lv_count.
        IF lv_count GT 1.
          RAISE format_error.
        ENDIF.

      ELSEIF lv_offset_dot LT lv_offset_comma.
        "1.000,25

        REPLACE ALL OCCURRENCES OF '.' IN lv_numb WITH ''.
        REPLACE ',' IN lv_numb WITH '.'.

        FIND ALL OCCURRENCES OF ',' IN lv_numb MATCH COUNT lv_count.
        IF lv_count GT 1.
          RAISE format_error.
        ENDIF.

      ENDIF.

    ELSEIF lv_counter_comma GT lv_counter_dot.
      "ex. 1,000,000.25

      REPLACE ALL OCCURRENCES OF ',' IN lv_numb WITH ''.

      FIND ALL OCCURRENCES OF '.' IN lv_numb MATCH COUNT lv_count.
      IF lv_count GT 1.
        RAISE format_error.
      ENDIF.

    ELSEIF lv_counter_dot GT lv_counter_comma.
      "ex. 1.000.000,25

      REPLACE ALL OCCURRENCES OF '.' IN lv_numb WITH ''.
      REPLACE ',' IN lv_numb WITH '.'.

      FIND ALL OCCURRENCES OF ',' IN lv_numb MATCH COUNT lv_count.
      IF lv_count GT 1.
        RAISE format_error.
      ENDIF.

    ENDIF.

    "-------------------------------------------------

    CONDENSE lv_numb NO-GAPS.

    y_numb_int = lv_numb.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_SAP_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] XO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR(optional)
* | [--->] X_SAP_DATA                     TYPE        ANY
* | [--->] X_SEPARATOR                    TYPE        CHAR1 (default =C_SEPARATOR_SEMICOLON)
* | [<---] Y_STR_DATA                     TYPE        STRING
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [EXC!] CONVERSION_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_sap_to_string.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lv_sap_ref     TYPE REF TO data,
          lv_tmp_data    TYPE string.

    "-------------------------------------------------

    y_str_data = ''.

    IF xo_structdescr IS NOT INITIAL.
      lo_structdescr = xo_structdescr.

    ELSE.
      get_compdescr_from_data(
        EXPORTING
          xs_sap_line             = x_sap_data
        IMPORTING
          yo_structdescr          = lo_structdescr
      ).

    ENDIF.


    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<component>).

      CHECK <component>-name NE 'MANDT'.

      ASSIGN COMPONENT <component>-name OF STRUCTURE x_sap_data TO FIELD-SYMBOL(<value>).

      CASE <component>-type_kind.
        WHEN cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_int8
          OR cl_abap_typedescr=>typekind_intf
          OR cl_abap_typedescr=>typekind_num
          OR cl_abap_typedescr=>typekind_numeric
          OR cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_date
          OR cl_abap_typedescr=>typekind_time
          OR cl_abap_typedescr=>typekind_char
          OR cl_abap_typedescr=>typekind_clike
          OR cl_abap_typedescr=>typekind_csequence
          OR cl_abap_typedescr=>typekind_string.

          lv_tmp_data = <value>.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      "-------------------------------------------------

      CASE <component>-type_kind.
        WHEN cl_abap_typedescr=>typekind_float      "Numbers "-------------------------------------------------
          OR cl_abap_typedescr=>typekind_decfloat
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_int8
          OR cl_abap_typedescr=>typekind_intf
          OR cl_abap_typedescr=>typekind_num
          OR cl_abap_typedescr=>typekind_numeric
          OR cl_abap_typedescr=>typekind_packed.

          conv_numb_to_ext(
            EXPORTING
              x_numb_int = lv_tmp_data
            IMPORTING
              y_numb_ext = lv_tmp_data
          ).

        WHEN cl_abap_typedescr=>typekind_date.     "Date "-------------------------------------------------

          conv_data_to_ext(
            EXPORTING
              x_data_int  = CONV sy-datum( lv_tmp_data )
              x_separator = '/'
            IMPORTING
              y_data_ext  = lv_tmp_data
            ).

          CONDENSE lv_tmp_data NO-GAPS.

        WHEN cl_abap_typedescr=>typekind_time.    "Time "-------------------------------------------------

          conv_time_to_ext(
            EXPORTING
              x_time = CONV sy-uzeit( lv_tmp_data )
            IMPORTING
              y_time = lv_tmp_data
          ).

          CONDENSE lv_tmp_data NO-GAPS.

        WHEN cl_abap_typedescr=>typekind_char   "Char like "-------------------------------------------------
          OR cl_abap_typedescr=>typekind_clike
          OR cl_abap_typedescr=>typekind_csequence
          OR cl_abap_typedescr=>typekind_string.

          "Normal Text -> Nothing To Do

      ENDCASE.


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
* | [--->] XO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR(optional)
* | [<---] Y_SAP_DATA                     TYPE        ANY
* | [<---] Y_CONVERSIONS_ERRORS           TYPE        TY_CONVERSIONS_ERRORS
* | [EXC!] CONVERSION_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_string_to_sap.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lv_sap_ref     TYPE REF TO data.

    FIELD-SYMBOLS: <sap_data> TYPE any.

    "-------------------------------------------------

    CLEAR: y_sap_data, y_conversions_errors.

    CREATE DATA lv_sap_ref LIKE y_sap_data.
    ASSIGN lv_sap_ref->* TO <sap_data>.

    IF xo_structdescr IS NOT INITIAL.
      lo_structdescr = xo_structdescr.

    ELSE.
      get_compdescr_from_data(
        EXPORTING
          xs_sap_line             = y_sap_data
        IMPORTING
          yo_structdescr          = lo_structdescr
      ).

    ENDIF.

    "-------------------------------------------------

    DATA(lv_tmp_string) = x_str_data.

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<component>).

      CHECK <component>-name NE 'MANDT'.

      ASSIGN COMPONENT <component>-name OF STRUCTURE <sap_data> TO FIELD-SYMBOL(<value>).

      CASE <component>-type_kind.
        WHEN cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_int8
          OR cl_abap_typedescr=>typekind_intf
          OR cl_abap_typedescr=>typekind_num
          OR cl_abap_typedescr=>typekind_numeric
          OR cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_date
          OR cl_abap_typedescr=>typekind_time
          OR cl_abap_typedescr=>typekind_char
          OR cl_abap_typedescr=>typekind_clike
          OR cl_abap_typedescr=>typekind_csequence
          OR cl_abap_typedescr=>typekind_string.

          SPLIT lv_tmp_string AT ';'
            INTO DATA(lv_sx)
                 DATA(lv_dx).

        WHEN OTHERS.
          y_conversions_errors-field = <component>-name.
          y_conversions_errors-error = 'Unmanaged data type'.
          RAISE conversion_error.
      ENDCASE.

      "-------------------------------------------------

      CASE <component>-type_kind.
        WHEN cl_abap_typedescr=>typekind_float      "Numbers "-------------------------------------------------
          OR cl_abap_typedescr=>typekind_decfloat
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_int8
          OR cl_abap_typedescr=>typekind_intf
          OR cl_abap_typedescr=>typekind_num
          OR cl_abap_typedescr=>typekind_numeric
          OR cl_abap_typedescr=>typekind_packed.

          conv_numb_to_int(
            EXPORTING
              x_numb_ext         = lv_sx
            IMPORTING
              y_numb_int         = DATA(lv_tmp_num)
            EXCEPTIONS
              format_error       = 1
              plausibility_error = 2
              OTHERS             = 3
          ).
          CASE sy-subrc.
            WHEN 0.
              lv_sx = lv_tmp_num.
              CONDENSE lv_sx NO-GAPS.

            WHEN 1.
              y_conversions_errors-field = <component>-name.
              y_conversions_errors-error = 'Number format muste be: 1.900,25 / 1,900.25 / 1900.25 / 1900,25 / '.
              RAISE conversion_error.

            WHEN 2.
              y_conversions_errors-field = <component>-name.
              y_conversions_errors-error = 'Implausible number, chars detected'.
              RAISE conversion_error.

          ENDCASE.

        WHEN cl_abap_typedescr=>typekind_date. "Date "-------------------------------------------------

          conv_data_to_int(
            EXPORTING
              x_data_ext         = lv_sx
            IMPORTING
              y_data_int         = DATA(lv_tmp_dats)
            EXCEPTIONS
              format_error       = 1
              plausibility_error = 2
              OTHERS             = 3
          ).
          CASE sy-subrc.
            WHEN 0.
              lv_sx = lv_tmp_dats.
              CONDENSE lv_sx NO-GAPS.

            WHEN 1.
              y_conversions_errors-field = <component>-name.
              y_conversions_errors-error = 'Data format must be DD/MM/YYYY'.
              RAISE conversion_error.

            WHEN 2.
              y_conversions_errors-field = <component>-name.
              y_conversions_errors-error = 'Implausible date'.
              RAISE conversion_error.

          ENDCASE.

        WHEN cl_abap_typedescr=>typekind_time. "Time "-------------------------------------------------

          conv_time_to_int(
            EXPORTING
              x_time = lv_sx
            IMPORTING
              y_time = DATA(lv_tmp_time)
          ).
          CASE sy-subrc.
            WHEN 0.
              lv_sx = lv_tmp_time.
              CONDENSE lv_sx NO-GAPS.

            WHEN 1.
              y_conversions_errors-field = <component>-name.
              y_conversions_errors-error = 'Time format must be HH:MM:SS'.
              RAISE conversion_error.

            WHEN 2.
              y_conversions_errors-field = <component>-name.
              y_conversions_errors-error = 'Implausible time'.
              RAISE conversion_error.

          ENDCASE.

        WHEN cl_abap_typedescr=>typekind_char
          OR cl_abap_typedescr=>typekind_clike
          OR cl_abap_typedescr=>typekind_csequence
          OR cl_abap_typedescr=>typekind_string.

          "Normal Data -> Nothing To Do

          remove_special_char(
            CHANGING
              y_text = lv_sx
          ).

      ENDCASE.

      <value>       = lv_sx.
      lv_tmp_string = lv_dx.

    ENDLOOP.

    y_sap_data = <sap_data>.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_TIME_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        UZEIT
* | [<---] Y_TIME                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_TIME_TO_EXT.

    y_time = |{ x_time(2) }:{ x_time+2(2) }:{ x_time+4(2) }|.
    CONDENSE y_time NO-GAPS.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_TIME_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        STRING
* | [<---] Y_TIME                         TYPE        UZEIT
* | [EXC!] FORMAT_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_time_to_int.

    y_time = c_initial_time.

    IF strlen( x_time ) NE 8.
      RAISE format_error.
    ENDIF.

    y_time = |{ x_time(2) }{ x_time+3(2) }{ x_time+5(2) }|.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = y_time
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE plausibility_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>DOWNLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] X_SOURCE                       TYPE        CHAR1 (default ='L')
* | [<-->] XT_SAP_DATA                    TYPE        TABLE
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* | [EXC!] UNABLE_DEFINE_STRUCTURE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download.

    DATA: lt_str_data TYPE string_table.

    "Get component from structure
    "-------------------------------------------------
    get_compdescr_from_data(
      EXPORTING
        xt_sap_table            = xt_sap_data
      IMPORTING
        yo_structdescr          = DATA(lo_structdescr)      " Runtime Type Services
      EXCEPTIONS
        unable_define_structure = 1
        OTHERS                  = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structure.
    ENDIF.


    "Build header
    "-------------------------------------------------
    IF x_header EQ abap_true.

      APPEND INITIAL LINE TO lt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).

      get_header_from_data(
        EXPORTING
          xt_sap_table            = xt_sap_data
        CHANGING
          y_str_header            = <str_data>
        EXCEPTIONS
          unable_define_structure = 1
          OTHERS                  = 2
      ).
      IF sy-subrc <> 0.
        RAISE unable_define_structure.
      ENDIF.

    ENDIF.


    "Convert SAP Data to String table
    "-------------------------------------------------
    LOOP AT xt_sap_data ASSIGNING FIELD-SYMBOL(<sap_data>).

      APPEND INITIAL LINE TO lt_str_data ASSIGNING <str_data>.

      conv_sap_to_string(
        EXPORTING
          xo_structdescr    = lo_structdescr
          x_sap_data        = <sap_data>
        IMPORTING
          y_str_data        = <str_data>
      ).

    ENDLOOP.


    "-------------------------------------------------
    "Start Download on Local / Serveer
    "-------------------------------------------------


    IF x_filename CP '*.xlsx'. "**********************************************************************

      CASE x_source.
        WHEN c_source_local.
          "-------------------------------------------------

          download_excel_local(
            EXPORTING
              x_filename       = x_filename
              xt_str_data      = lt_str_data
              x_use_custom_ole = space
            CHANGING
              xt_sap_data      = xt_sap_data
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).
          IF sy-subrc <> 0.
            RAISE unable_open_path.
          ENDIF.

        WHEN c_source_server.
          "-------------------------------------------------

          download_excel_server(
            EXPORTING
              x_filename       = x_filename
              xt_sap_data      = xt_sap_data
            EXCEPTIONS
              unable_open_path = 1
              others           = 2
          ).
          IF sy-subrc <> 0.
            RAISE unable_open_path.
          ENDIF.

      ENDCASE.


    ELSEIF x_filename CP '*.csv'. "**********************************************************************

      CASE x_source.
        WHEN c_source_local.
          "-------------------------------------------------

          download_csv_local(
            EXPORTING
              x_filename       = x_filename
            CHANGING
              xt_str_data      = lt_str_data
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).
          IF sy-subrc <> 0.
            RAISE unable_open_path.
          ENDIF.

        WHEN c_source_server.
          "-------------------------------------------------

          download_csv_server(
            EXPORTING
              x_filename       = x_filename
              xt_str_data      = lt_str_data
            EXCEPTIONS
              unable_open_path = 1
              OTHERS           = 2
          ).
          IF sy-subrc <> 0.
            RAISE unable_open_path.
          ENDIF.

      ENDCASE.


    ELSE.
      RAISE not_supported_file.
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
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [--->] X_USE_CUSTOM_OLE               TYPE        FLAG (default =SPACE)
* | [<-->] XT_SAP_DATA                    TYPE        STANDARD TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_excel_local.

    IF x_use_custom_ole EQ space.

      "Standard Version ->
      "Conversion of fields like data, numbers exc.
      "will be performed by SAP Standard
      "-------------------------------------------------

      DATA(lt_data_ref) = REF #( xt_sap_data ).

      get_fieldcat_from_itab(
        EXPORTING
          xt_itab = xt_sap_data
        IMPORTING
          yt_fcat = DATA(lt_fcat)
      ).

      cl_salv_bs_lex=>export_from_result_data_table(
        EXPORTING
          is_format            = if_salv_bs_lex_format=>mc_format_xlsx
          ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = lt_data_ref
                                                                             t_fieldcatalog  = lt_fcat )
        IMPORTING
          er_result_file       = DATA(lv_xdata) ).

      DATA(lv_xlength) = xstrlen( lv_xdata ).
      DATA(lt_bin_tab) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_xdata ).

      cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_xlength           " File length for binary files
        filename                  = x_filename           " Name of file
        filetype                  = 'BIN'                " File type (ASCII, binary ...)
      CHANGING
        data_tab                  = lt_bin_tab           " Transfer table
      EXCEPTIONS
        file_write_error          = 1                    " Cannot write to file
        no_batch                  = 2                    " Cannot execute front-end function in background
        gui_refuse_filetransfer   = 3                    " Incorrect Front End
        invalid_type              = 4                    " Invalid value for parameter FILETYPE
        no_authority              = 5                    " No Download Authorization
        unknown_error             = 6                    " Unknown error
        header_not_allowed        = 7                    " Invalid header
        separator_not_allowed     = 8                    " Invalid separator
        filesize_not_allowed      = 9                    " Invalid file size
        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create           = 11                   " Cannot create DataProvider
        dp_error_send             = 12                   " Error Sending Data with DataProvider
        dp_error_write            = 13                   " Error Writing Data with DataProvider
        unknown_dp_error          = 14                   " Error when calling data provider
        access_denied             = 15                   " Access to File Denied
        dp_out_of_memory          = 16                   " Not enough memory in data provider
        disk_full                 = 17                   " Storage medium is full.
        dp_timeout                = 18                   " Data provider timeout
        file_not_found            = 19                   " Could not find file
        dataprovider_exception    = 20                   " General Exception Error in DataProvider
        control_flush_error       = 21                   " Error in Control Framework
        not_supported_by_gui      = 22                   " GUI does not support this
        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24
    ).
      IF sy-subrc <> 0.
        RAISE unable_open_path.
      ENDIF.



      "OLE Version -> If you can, let avoid it
      "**********************************************************************
    ELSEIF x_use_custom_ole EQ 'X'.

      get_fieldcat_from_itab(
        EXPORTING
          xt_itab = xt_sap_data
        IMPORTING
          yt_fcat = lt_fcat
      ).

      DATA lv_tabix TYPE sy-tabix.
      DATA(lv_lines_fcat) = lines( lt_fcat ).
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

      lo_ole->ole_clipboard_export( xt_str_data = lt_str_data ).
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
      LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>)
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

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_CSV_XLSX=>DOWNLOAD_EXCEL_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_SAP_DATA                    TYPE        STANDARD TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_excel_server.

    DATA: lt_data_ref TYPE REF TO data.

    FIELD-SYMBOLS: <t_sap_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    CREATE DATA lt_data_ref LIKE xt_sap_data.
    ASSIGN lt_data_ref->* TO <t_sap_data>.
    <t_sap_data> = xt_sap_data.

    get_fieldcat_from_itab(
      EXPORTING
        xt_itab = <t_sap_data>
      IMPORTING
        yt_fcat = DATA(lt_fcat)
    ).

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = lt_data_ref
                                                                           t_fieldcatalog  = lt_fcat )
      IMPORTING
        er_result_file       = DATA(lv_xdata) ).

    DATA(lv_xlength) = xstrlen( lv_xdata ).

    "-------------------------------------------------

    OPEN DATASET x_filename FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      CLOSE DATASET x_filename.
      RAISE unable_open_path.
    ENDIF.

    TRANSFER lv_xdata TO x_filename.

    CLOSE DATASET x_filename.

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

        lv_path = get_desktop_directory( ).

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
* | Static Public Method ZAG_CL_CSV_XLSX=>GET_COMPDESCR_FROM_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] XS_SAP_LINE                    TYPE        ANY(optional)
* | [--->] XT_SAP_TABLE                   TYPE        TABLE(optional)
* | [<---] YO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [EXC!] UNABLE_DEFINE_STRUCTURE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_COMPDESCR_FROM_DATA.

    DATA: lref_sap_data TYPE REF TO data.

    FIELD-SYMBOLS: <sap_line> TYPE any.

    IF xs_sap_line IS SUPPLIED.
      CREATE DATA lref_sap_data LIKE xs_sap_line.
      ASSIGN lref_sap_data->* TO <sap_line>.

    ELSEIF xt_sap_table IS SUPPLIED.
      CREATE DATA lref_sap_data LIKE LINE OF xt_sap_table.
      ASSIGN lref_sap_data->* TO <sap_line>.

    ELSE.
      RAISE unable_define_structure.

    ENDIF.

    IF sy-subrc <> 0.
      RAISE unable_define_structure.
    ENDIF.

    yo_structdescr ?= cl_abap_typedescr=>describe_by_data( <sap_line> ).

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
* | Static Public Method ZAG_CL_CSV_XLSX=>GET_HEADER_FROM_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] XS_SAP_LINE                    TYPE        ANY(optional)
* | [--->] XT_SAP_TABLE                   TYPE        TABLE(optional)
* | [<-->] Y_STR_HEADER                   TYPE        STRING
* | [EXC!] UNABLE_DEFINE_STRUCTURE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_HEADER_FROM_DATA.

    DATA: lref_sap_data TYPE REF TO data,
          lt_fcat       TYPE lvc_t_fcat.

    FIELD-SYMBOLS: <sap_table> TYPE STANDARD TABLE.

    y_str_header = ''.

    IF xs_sap_line IS SUPPLIED.

      CREATE DATA lref_sap_data LIKE TABLE OF xs_sap_line.
      ASSIGN lref_sap_data->* TO <sap_table>.

      get_fieldcat_from_itab(
       EXPORTING
         xt_itab = <sap_table>
       IMPORTING
         yt_fcat = lt_fcat                  " Catalogo campo per ListViewerControl
      ).

    ELSEIF xt_sap_table IS SUPPLIED.

      get_fieldcat_from_itab(
        EXPORTING
          xt_itab = xt_sap_table
        IMPORTING
          yt_fcat = lt_fcat                  " Catalogo campo per ListViewerControl
      ).

    ELSE.
      RAISE unable_define_structure.

    ENDIF.



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

    DATA(lv_sheet_name) = |{ sy-repid(10) }|.

    GET PROPERTY OF go_application 'Sheets' = go_sheet.
    CALL METHOD OF go_sheet 'Add' = go_worksheet.
    SET PROPERTY OF go_worksheet 'Name' = lv_sheet_name.
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
    IF sy-subrc <> 0.

    ENDIF.

    CALL METHOD OF go_worksheet 'paste'.
    IF sy-subrc <> 0.

    ENDIF.

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
* | [--->] X_SOURCE                       TYPE        CHAR1 (default ='L')
* | [<---] YT_SAP_DATA                    TYPE        TABLE
* | [<---] YT_CONVERSIONS_ERRORS          TYPE        TT_CONVERSIONS_ERRORS
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* | [EXC!] UNABLE_DEFINE_STRUCTURE
* | [EXC!] EMPTY_FILE
* | [EXC!] CONVERSION_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload.

    DATA: lt_str_data TYPE string_table.

    REFRESH: yt_sap_data, yt_conversions_errors.

    "Get component from structure
    "-------------------------------------------------
    get_compdescr_from_data(
      EXPORTING
        xt_sap_table            = yt_sap_data
      IMPORTING
        yo_structdescr          = DATA(lo_structdescr)      " Runtime Type Services
      EXCEPTIONS
        unable_define_structure = 1
        OTHERS                  = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structure.
    ENDIF.



    "File upload
    "-------------------------------------------------
    IF x_source EQ c_source_local "XLSX only from local
      AND x_filename CP '*.xlsx'.

      "Upload from XLSX with in-built conversion in SAP Format
      "-------------------------------------------------
      upload_excel_local(
        EXPORTING
          x_header         = x_header
          x_filename       = x_filename
          xo_structdescr   = lo_structdescr
        IMPORTING
          yt_sap_data      = yt_sap_data[]
        EXCEPTIONS
          unable_open_path = 1
          empty_file       = 2
          OTHERS           = 3
      ).
      CASE sy-subrc.
        WHEN 0.
          "OK
        WHEN 1.
          RAISE unable_open_path.
        WHEN 2.
          RAISE empty_file.
      ENDCASE.

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
          IF sy-subrc <> 0.
            RAISE unable_open_path.
          ENDIF.

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
          IF sy-subrc <> 0.
            RAISE unable_open_path.
          ENDIF.

      ENDCASE.


      "Conversion in SAP Format for CSV
      "-------------------------------------------------
      CHECK lines( lt_str_data ) GT 0.
      IF x_header EQ 'X'.
        DELETE lt_str_data INDEX 1.
      ENDIF.

      IF lines( lt_str_data ) EQ 0.
        RAISE empty_file.
      ENDIF.

      LOOP AT lt_str_data ASSIGNING FIELD-SYMBOL(<data_str>).
        DATA(lv_tabix) = sy-tabix.

        APPEND INITIAL LINE TO yt_sap_data ASSIGNING FIELD-SYMBOL(<data_sap>).
        conv_string_to_sap(
          EXPORTING
            x_str_data           = <data_str>
            xo_structdescr       = lo_structdescr
          IMPORTING
            y_sap_data           = <data_sap>
            y_conversions_errors = DATA(ls_conv_error)
          EXCEPTIONS
            conversion_error     = 1
            OTHERS               = 2
        ).
        IF sy-subrc <> 0.
          ls_conv_error-row_num = lv_tabix.
          APPEND ls_conv_error TO yt_conversions_errors.
        ENDIF.

      ENDLOOP.

      IF yt_conversions_errors IS NOT INITIAL.
        RAISE conversion_error.
      ENDIF.

    ELSE.
      RAISE not_supported_file.
    ENDIF.



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
* | [--->] X_HEADER                       TYPE        XFELD(optional)
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [<---] YT_SAP_DATA                    TYPE        TABLE
* | [EXC!] UNABLE_OPEN_PATH
* | [EXC!] EMPTY_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_excel_local.

    DATA: lv_except_msg    TYPE string,
          lv_tmp_data      TYPE string,
          lt_bin_tab       TYPE solix_tab,
          lcl_excel_ref    TYPE REF TO cl_fdt_xl_spreadsheet,
          lcl_table_descr  TYPE REF TO cl_abap_tabledescr,
          lcl_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <t_excel_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    REFRESH: yt_sap_data[].

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

      IF x_header EQ 'X'.
        DELETE <t_excel_data> INDEX 1.
      ENDIF.

      IF <t_excel_data> IS INITIAL.
        RAISE empty_file.
      ENDIF.

      "Convert excel format into string table with columns separated by ; like in CSV
      "-------------------------------------------------
      LOOP AT <t_excel_data> ASSIGNING FIELD-SYMBOL(<excel>).

        get_compdescr_from_data(
          EXPORTING
            xs_sap_line             = <excel>
          IMPORTING
            yo_structdescr          = DATA(lo_tmp_structdescr)
          EXCEPTIONS
            unable_define_structure = 1
            others                  = 2
        ).

        lv_tmp_data = ''.
        LOOP AT lo_tmp_structdescr->components ASSIGNING FIELD-SYMBOL(<comp>).
          ASSIGN COMPONENT <comp>-name OF STRUCTURE <excel> TO FIELD-SYMBOL(<excel_col>).
          IF lv_tmp_data IS INITIAL.
            lv_tmp_data = <excel_col>.
          ELSE.
            lv_tmp_data = |{ lv_tmp_data };{ <excel_col> }|.
          ENDIF.
        ENDLOOP.

        APPEND INITIAL LINE TO yt_sap_data ASSIGNING FIELD-SYMBOL(<sap_data>).

        conv_string_to_sap(
          EXPORTING
            x_str_data     = lv_tmp_data
            xo_structdescr = xo_structdescr
          IMPORTING
            y_sap_data     = <sap_data>
        ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

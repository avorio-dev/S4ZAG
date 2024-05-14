class ZAG_CL_CSV_XLSX definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_conversions_errors,
        row_num     TYPE i,
        field       TYPE fieldname,
        field_descr TYPE string,
        value       TYPE string,
        error       TYPE string,
      END OF ts_conversions_errors .
  types:
    tt_conversions_errors TYPE TABLE OF ts_conversions_errors .
  types:
    BEGIN OF ts_exit_config,
        repid                    TYPE sy-repid,
        exit_sap_to_str          TYPE string,
        exit_str_to_sap_preconv  TYPE string,
        exit_str_to_sap_postconv TYPE string,
      END OF ts_exit_config .

  constants C_CR_LF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  constants C_INITIAL_DATA type DATUM value '00000000' ##NO_TEXT.
  constants C_INITIAL_TIME type TIME value 000000 ##NO_TEXT.
  constants C_MAX_DATA type DATUM value '99991231' ##NO_TEXT.
  constants C_SEPARATOR_HORIZONTAL_TAB type ABAP_CHAR1 value %_HORIZONTAL_TAB ##NO_TEXT.
  constants C_SEPARATOR_SEMICOLON type CHAR1 value ';' ##NO_TEXT.
  constants C_SOURCE_LOCAL type CHAR1 value 'L' ##NO_TEXT.
  constants C_SOURCE_SERVER type CHAR1 value 'S' ##NO_TEXT.
  constants C_FILETYPE_CSV type CHAR3 value 'CSV' ##NO_TEXT.
  constants C_FILETYPE_XLSX type CHAR4 value 'XLSX' ##NO_TEXT.

  class-methods CONV_DATA_TO_EXT
    importing
      !X_DATA_INT type DATS
      !X_SEPARATOR type C default '/'
    returning
      value(Y_DATA_EXT) type STRING .
  class-methods CONV_DATA_TO_INT
    importing
      !X_DATA_EXT type STRING
    returning
      value(Y_DATA_INT) type DATS
    exceptions
      FORMAT_ERROR
      PLAUSIBILITY_ERROR .
  class-methods CONV_NUMB_TO_EXT
    importing
      !X_NUMB_INT type STRING
    returning
      value(Y_NUMB_EXT) type STRING .
  class-methods CONV_NUMB_TO_INT
    importing
      !X_NUMB_EXT type STRING
    returning
      value(Y_NUMB_INT) type STRING
    exceptions
      FORMAT_ERROR
      PLAUSIBILITY_ERROR .
  class-methods CONV_SAP_TO_STRING
    importing
      !X_SAP_DATA type ANY
      !X_SEPARATOR type CHAR1 default C_SEPARATOR_SEMICOLON
      !XT_FCAT type LVC_T_FCAT
      !XO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
      !XS_EXIT_CONFIG type TS_EXIT_CONFIG optional
    exporting
      !Y_STR_DATA type STRING .
  class-methods CONV_STRING_TO_SAP
    importing
      !X_STR_DATA type STRING
      !XT_FCAT type LVC_T_FCAT
      !XO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
      !XS_EXIT_CONFIG type TS_EXIT_CONFIG optional
    exporting
      !Y_SAP_DATA type ANY
      !Y_CONVERSIONS_ERRORS type TS_CONVERSIONS_ERRORS
    exceptions
      CONVERSION_ERROR
      PLAUSIBILITY_ERROR .
  class-methods CONV_TIME_TO_EXT
    importing
      !X_TIME type UZEIT
    returning
      value(Y_TIME) type STRING .
  class-methods CONV_TIME_TO_INT
    importing
      !X_TIME type STRING
    returning
      value(Y_TIME) type UZEIT
    exceptions
      FORMAT_ERROR
      PLAUSIBILITY_ERROR .
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
  class-methods GET_FIELDCAT_FROM_DATA
    importing
      !XS_SAP_LINE type ANY optional
      !XT_SAP_TABLE type TABLE optional
    exporting
      !YO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
      !YT_FCAT type LVC_T_FCAT
    exceptions
      UNABLE_DEFINE_STRUCTDESCR .
  class-methods REMOVE_SPECIAL_CHAR
    changing
      !Y_TEXT type STRING .
  methods CONSTRUCTOR
    importing
      !XS_SAP_LINE type ANY optional
      !XT_SAP_TABLE type TABLE optional
      !XS_EXIT_CONFIG type TS_EXIT_CONFIG optional
    exceptions
      INPUT_ERROR
      UNABLE_DEFINE_STRUCTDESCR .
  methods CONV_TAB_TO_EXT
    importing
      !X_HEADER type OS_BOOLEAN default 'X'
    returning
      value(YT_STR_DATA) type STRING_TABLE .
  methods CONV_TAB_TO_INT
    importing
      !X_HEADER type XFELD default 'X'
      !XT_STR_DATA type STRING_TABLE
    exporting
      !YT_SAP_DATA type TABLE
      !YT_CONVERSIONS_ERRORS type TT_CONVERSIONS_ERRORS
    exceptions
      CONVERSION_ERROR .
  methods FILE_DOWNLOAD
    importing
      !X_FILENAME type STRING
      !X_HEADER type OS_BOOLEAN default 'X'
      !X_SOURCE type CHAR1 default 'L'
    exceptions
      NOT_SUPPORTED_FILE
      UNABLE_OPEN_PATH .
  methods FILE_UPLOAD
    importing
      !X_FILENAME type STRING
      !X_HEADER type XFELD default 'X'
      !X_SOURCE type CHAR1 default 'L'
    exporting
      !YT_SAP_DATA type TABLE
      !YT_CONVERSIONS_ERRORS type TT_CONVERSIONS_ERRORS
    exceptions
      INPUT_ERROR
      NOT_SUPPORTED_FILE
      UNABLE_OPEN_PATH
      EMPTY_FILE
      CONVERSION_ERROR .
  methods GET_HEADER_FROM_DATA
    returning
      value(Y_STR_HEADER) type STRING .
  PROTECTED SECTION.
private section.

  data GO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR .
  data GT_FCAT type LVC_T_FCAT .
  data GS_EXIT_CONFIG type TS_EXIT_CONFIG .
  data GREF_SAP_DATA type ref to DATA .
  class-data:
    gr_typekind_numbers TYPE RANGE OF abap_typekind .
  class-data:
    gr_typekind_date TYPE RANGE OF abap_typekind .
  class-data:
    gr_typekind_time TYPE RANGE OF abap_typekind .
  class-data:
    gr_typekind_charlike TYPE RANGE OF abap_typekind .

  methods DOWNLOAD_CSV_LOCAL
    importing
      !X_FILENAME type STRING
    changing
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods DOWNLOAD_CSV_SERVER
    importing
      !X_FILENAME type STRING
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods DOWNLOAD_EXCEL_LOCAL
    importing
      !X_FILENAME type STRING
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods DOWNLOAD_EXCEL_SERVER
    importing
      !X_FILENAME type STRING
      !XT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods UPLOAD_CSV_LOCAL
    importing
      !X_FILENAME type STRING
    exporting
      !YT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods UPLOAD_CSV_SERVER
    importing
      !X_FILENAME type STRING
    exporting
      !YT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
  methods UPLOAD_EXCEL_LOCAL
    importing
      !X_FILENAME type STRING
    exporting
      !YT_STR_DATA type STRING_TABLE
    exceptions
      UNABLE_OPEN_PATH .
ENDCLASS.



CLASS ZAG_CL_CSV_XLSX IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_CSV_XLSX->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] XS_SAP_LINE                    TYPE        ANY(optional)
* | [--->] XT_SAP_TABLE                   TYPE        TABLE(optional)
* | [--->] XS_EXIT_CONFIG                 TYPE        TS_EXIT_CONFIG(optional)
* | [EXC!] INPUT_ERROR
* | [EXC!] UNABLE_DEFINE_STRUCTDESCR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    FIELD-SYMBOLS: <sap_data> TYPE STANDARD TABLE.

    IF xs_sap_line IS NOT SUPPLIED
      AND xt_sap_table IS NOT SUPPLIED.
      RAISE input_error.
    ENDIF.


    "Set Attributes
    "---------------------------------------------------------------
    IF xs_sap_line IS SUPPLIED.
      CREATE DATA me->gref_sap_data LIKE TABLE OF xs_sap_line.
      ASSIGN me->gref_sap_data->* TO <sap_data>.

      APPEND xs_sap_line TO <sap_data>.

    ELSEIF xt_sap_table IS SUPPLIED.
      me->gref_sap_data = REF #( xt_sap_table ).
      ASSIGN me->gref_sap_data->* TO <sap_data>.

    ENDIF.


    me->gs_exit_config = xs_exit_config.


    "Set Fieldcat and Fields Descriptor
    "---------------------------------------------------------------
    me->get_fieldcat_from_data(
      EXPORTING
        xt_sap_table              = <sap_data>
      IMPORTING
        yo_structdescr            = me->go_structdescr
        yt_fcat                   = me->gt_fcat
      EXCEPTIONS
        unable_define_structdescr = 1
        OTHERS                    = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structdescr.
    ENDIF.


    "Init range for managed abap typekind
    "---------------------------------------------------------------

    gr_typekind_numbers = VALUE #(
      sign = 'I' option = 'EQ'
      ( low = cl_abap_typedescr=>typekind_float )
      ( low = cl_abap_typedescr=>typekind_decfloat )
      ( low = cl_abap_typedescr=>typekind_decfloat16 )
      ( low = cl_abap_typedescr=>typekind_decfloat34 )
      ( low = cl_abap_typedescr=>typekind_int )
      ( low = cl_abap_typedescr=>typekind_int1 )
      ( low = cl_abap_typedescr=>typekind_int2 )
      ( low = cl_abap_typedescr=>typekind_int8 )
      ( low = cl_abap_typedescr=>typekind_intf )
      ( low = cl_abap_typedescr=>typekind_num )
      ( low = cl_abap_typedescr=>typekind_numeric )
      ( low = cl_abap_typedescr=>typekind_packed )
    ).

    gr_typekind_date = VALUE #(
      sign = 'I' option = 'EQ'
      ( low = cl_abap_typedescr=>typekind_date )
    ).

    gr_typekind_time = VALUE #(
      sign = 'I' option = 'EQ'
      ( low = cl_abap_typedescr=>typekind_time )
    ).

    gr_typekind_charlike = VALUE #(
      sign = 'I' option = 'EQ'
      ( low = cl_abap_typedescr=>typekind_char )
      ( low = cl_abap_typedescr=>typekind_clike )
      ( low = cl_abap_typedescr=>typekind_csequence )
      ( low = cl_abap_typedescr=>typekind_string )
    ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_DATA_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_INT                     TYPE        DATS
* | [--->] X_SEPARATOR                    TYPE        C (default ='/')
* | [<-()] Y_DATA_EXT                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_data_to_ext.

*    Data conversion exit from SAP to External
*    From
*        -> 20241231
*    To
*        -> 31/12/2024

    y_data_ext = COND #(
      WHEN x_data_int EQ c_initial_data THEN ''
      ELSE |{ x_data_int+6(2) }{ x_separator }{ x_data_int+4(2) }{ x_separator }{ x_data_int(4) }|
    ).

    CONDENSE y_data_ext NO-GAPS.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_DATA_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_EXT                     TYPE        STRING
* | [<-()] Y_DATA_INT                     TYPE        DATS
* | [EXC!] FORMAT_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_data_to_int.

*    Data conversion exit from External to SAP
*    From
*        -> 25/12/2024
*        -> 2024/12/31
*        -> 20241231
*    To
*        -> 20241231

    y_data_int = c_initial_data.
    IF x_data_ext IS INITIAL.
      EXIT.
    ENDIF.

    IF strlen( x_data_ext ) NE 10
      AND strlen( x_data_ext ) NE 8.
      RAISE format_error.
    ENDIF.


    IF strlen( x_data_ext ) EQ 10.

      "Data format managed
      "25-12-2023
      "2023-12-25

      IF x_data_ext+2(1) CA '0123456789'.                   "#EC NOTEXT
        "Format like 25-12-2023
        y_data_int = |{ x_data_ext(4) }{ x_data_ext+5(2) }{ x_data_ext+8(2) }|.

      ELSEIF x_data_ext+2(1) NA '0123456789'.              "#EC NOTEXT.
        "Format like 2023-12-25
        y_data_int = |{ x_data_ext+6(4) }{ x_data_ext+3(2) }{ x_data_ext(2) }|.

      ELSE.
        RAISE format_error.

      ENDIF.

    ELSEIF strlen( x_data_ext ) EQ 8.
      "Format like 20231225
      y_data_int = x_data_ext.

    ENDIF.


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
* | [<-()] Y_NUMB_EXT                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_numb_to_ext.

*    Number conversion exit from SAP to External
*    From
*        -> 1000.25
*        -> 1000.25-
*    To
*        -> 1000,25
*        -> -1000,25

    DATA(lv_numb) = x_numb_int.

    y_numb_ext  = 0.

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
* | [<-()] Y_NUMB_INT                     TYPE        STRING
* | [EXC!] FORMAT_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_numb_to_int.

*    Number conversion exit from External to SAP
*    From
*        -> Dot / Comma separator for decimal
*        -> Dot / Comma separator for thousands
*        -> Exp notation
*    To
*        -> 1000.25
*        -> 1000.25-
*
*    WARNING
*    In case of a number like 100,257
*    It's not possible to define if comma is the separator of decimal values or thousnds
*    So, in this case, the number will be converted in SAP Format -> 100.257

    DATA: lv_count TYPE i,
          lv_sign.

    DATA(lv_numb) = x_numb_ext.

    y_numb_int = 0.

    "-------------------------------------------------

    CONDENSE lv_numb NO-GAPS.
    TRANSLATE lv_numb TO UPPER CASE.
    IF lv_numb CN '0123456789.,+-E'.                        "#EC NOTEXT
      RAISE plausibility_error.
    ENDIF.

    "Find special chars occurrences
    "-------------------------------------------------

    DATA(lv_counter_dot) = 0.
    FIND ALL OCCURRENCES OF '.' IN lv_numb MATCH COUNT lv_counter_dot.

    DATA(lv_counter_comma) = 0.
    FIND ALL OCCURRENCES OF ',' IN lv_numb MATCH COUNT lv_counter_comma.

    DATA(lv_counter_plus) = 0.
    FIND ALL OCCURRENCES OF '+' IN lv_numb MATCH COUNT lv_counter_plus.

    DATA(lv_counter_minus) = 0.
    FIND ALL OCCURRENCES OF '-' IN lv_numb MATCH COUNT lv_counter_minus.

    DATA(lv_counter_exp) = 0.
    FIND ALL OCCURRENCES OF 'E' IN lv_numb MATCH COUNT lv_counter_exp.


    "Exponential notation management
    "-------------------------------------------------
    CASE lv_counter_exp.

      WHEN 0. "-------------------------------------------------

        IF lv_counter_plus GT 0.
          RAISE format_error.
        ENDIF.

        IF lv_counter_minus GT 1.
          RAISE format_error.
        ENDIF.

        IF lv_counter_minus EQ 1.
          REPLACE ALL OCCURRENCES OF '-' IN lv_numb WITH ''.
          lv_numb = |{ lv_numb }-|.
        ENDIF.

      WHEN 1. "-------------------------------------------------

        CASE lv_counter_plus.
          WHEN 0.

            CASE lv_counter_minus.
              WHEN 0.
                RAISE format_error.

              WHEN OTHERS.
                IF lv_numb NS 'E-'.
                  RAISE format_error.
                ENDIF.

            ENDCASE.

          WHEN 1.

            IF lv_numb NS 'E+'.
              RAISE format_error.
            ENDIF.

          WHEN OTHERS.
            RAISE format_error.

        ENDCASE.


        CASE lv_counter_minus.
          WHEN 1.
            IF lv_numb NS 'E-'.
              RAISE format_error.
            ENDIF.

          WHEN 2.
            IF lv_numb NS 'E-' OR lv_numb(1) NE '-'.
              RAISE format_error.
            ENDIF.

          WHEN OTHERS.
            IF lv_counter_minus GT 2.
              RAISE format_error.
            ENDIF.

        ENDCASE.

      WHEN OTHERS.
        RAISE format_error.

    ENDCASE.


    "Convert Real number into SAP Number
    "-------------------------------------------------

    IF lv_counter_comma GT 0
      AND lv_counter_dot EQ 0.
      "100,257 / 1000,25 / 1,000,000,000

      IF lv_counter_comma EQ 1.
        "100,257 / 1000,25

        "WARNING
        "Unable to define if comma is the separator of decimal values or integer parts of number
        "So ->
        REPLACE ALL OCCURRENCES OF ',' IN lv_numb WITH '.'.

      ELSE.
        "1,000,000,000
        REPLACE ALL OCCURRENCES OF ',' IN lv_numb WITH ''.

      ENDIF.


    ELSEIF lv_counter_comma EQ 0
      AND lv_counter_dot GT 0.
      "100.257 / 100.25 / 1.000.000.000

      IF lv_counter_dot EQ 1.
        "100.257 / 100.25

        "WARNING
        "Unable to define if comma is the separator of decimal values or integer parts of number
        "So ->
        "Nothing to do

      ELSE.
        "1.000.000.000
        REPLACE ALL OCCURRENCES OF '.' IN lv_numb WITH ''.

      ENDIF.

    ELSEIF lv_counter_comma GT 0
      AND lv_counter_dot GT 0.

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

    ENDIF.

    "-------------------------------------------------

    CONDENSE lv_numb NO-GAPS.

    y_numb_int = lv_numb.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_SAP_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SAP_DATA                     TYPE        ANY
* | [--->] X_SEPARATOR                    TYPE        CHAR1 (default =C_SEPARATOR_SEMICOLON)
* | [--->] XT_FCAT                        TYPE        LVC_T_FCAT
* | [--->] XO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [--->] XS_EXIT_CONFIG                 TYPE        TS_EXIT_CONFIG(optional)
* | [<---] Y_STR_DATA                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_sap_to_string.

    y_str_data  = ''.

    LOOP AT xt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CHECK <fcat>-fieldname NE 'MANDT'.

      ASSIGN xo_structdescr->components[ name = <fcat>-fieldname ] TO FIELD-SYMBOL(<component>).
      CHECK sy-subrc EQ 0.

      CHECK <component>-type_kind IN gr_typekind_numbers[]
         OR <component>-type_kind IN gr_typekind_date[]
         OR <component>-type_kind IN gr_typekind_time[]
         OR <component>-type_kind IN gr_typekind_charlike[].


      ASSIGN COMPONENT <component>-name OF STRUCTURE x_sap_data TO FIELD-SYMBOL(<value>).
      DATA(lv_tmp_data) = CONV string( <value> ).


      "Numbers
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_numbers[].
        IF <fcat>-edit_mask CS 'TSTLC'.
          "If the fields is a timestamp ( detected like numbers ),
          "it mustn't be converted to ext format like normal numbers
          "it must be converted from conv_exit_output routine
        ELSE.

          lv_tmp_data = conv_numb_to_ext( x_numb_int = lv_tmp_data ).

        ENDIF.
      ENDIF.


      "Date
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_date[].

        lv_tmp_data = conv_data_to_ext( x_data_int  = CONV sy-datum( lv_tmp_data )
                                        x_separator = '/' ).

      ENDIF.


      "Time
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_time[].

        lv_tmp_data = conv_time_to_ext( x_time = CONV sy-uzeit( lv_tmp_data ) ).

      ENDIF.


      "Charlike
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_charlike[].

        "Normal Text -> Nothing To Do

      ENDIF.


      "Conversion exit
      "---------------------------------------------------------------
      IF <fcat>-edit_mask IS NOT INITIAL.

        DATA(lv_exit_name)    = <fcat>-edit_mask+2.
        DATA(lv_fm_conv_exit) = |CONVERSION_EXIT_{ lv_exit_name }_OUTPUT|.

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = CONV rs38l-name( lv_fm_conv_exit )
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        IF sy-subrc EQ 0.

          CASE lv_exit_name.
            WHEN 'ABPSP'.
              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
                EXPORTING
                  input     = lv_tmp_data
                IMPORTING
                  output    = lv_tmp_data
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
              IF sy-subrc <> 0.
              ENDIF.


            WHEN OTHERS.
              CALL FUNCTION lv_fm_conv_exit
                EXPORTING
                  input  = lv_tmp_data
                IMPORTING
                  output = lv_tmp_data.

          ENDCASE.

*          CONDENSE lv_tmp_data NO-GAPS.

        ENDIF.

      ENDIF.


      "User exit
      "---------------------------------------------------------------
      IF xs_exit_config-repid IS NOT INITIAL.
        PERFORM (xs_exit_config-exit_sap_to_str) IN PROGRAM (xs_exit_config-repid) IF FOUND
                                                        USING
                                                            <fcat>
                                                            x_sap_data
                                                            <value>
                                                        CHANGING
                                                            lv_tmp_data.
      ENDIF.


      y_str_data = COND #(
        WHEN y_str_data EQ '' THEN lv_tmp_data
        WHEN y_str_data NE '' THEN |{ y_str_data }{ x_separator }{ lv_tmp_data }|
      ).


    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_STRING_TO_SAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STR_DATA                     TYPE        STRING
* | [--->] XT_FCAT                        TYPE        LVC_T_FCAT
* | [--->] XO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [--->] XS_EXIT_CONFIG                 TYPE        TS_EXIT_CONFIG(optional)
* | [<---] Y_SAP_DATA                     TYPE        ANY
* | [<---] Y_CONVERSIONS_ERRORS           TYPE        TS_CONVERSIONS_ERRORS
* | [EXC!] CONVERSION_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_string_to_sap.

    DATA: lv_sap_ref     TYPE REF TO data.

    FIELD-SYMBOLS: <sap_data> TYPE any.

    "-------------------------------------------------

    CLEAR: y_sap_data, y_conversions_errors.

    CREATE DATA lv_sap_ref LIKE y_sap_data.
    ASSIGN lv_sap_ref->* TO <sap_data>.

    "-------------------------------------------------

    DATA(lv_tmp_string) = x_str_data.

    LOOP AT xt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CHECK <fcat>-fieldname NE 'MANDT'.

      ASSIGN xo_structdescr->components[ name = <fcat>-fieldname ] TO FIELD-SYMBOL(<component>).
      CHECK sy-subrc EQ 0.

      ASSIGN COMPONENT <component>-name OF STRUCTURE <sap_data> TO FIELD-SYMBOL(<value>).
      y_conversions_errors-field       = <fcat>-fieldname.
      y_conversions_errors-field_descr = <fcat>-rollname.

      IF <component>-type_kind NOT IN gr_typekind_numbers[]
        AND <component>-type_kind NOT IN gr_typekind_date[]
        AND <component>-type_kind NOT IN gr_typekind_time[]
        AND <component>-type_kind NOT IN gr_typekind_charlike[].

        y_conversions_errors-error = 'Unmanaged data type'. "#EC NOTEXT
        RAISE conversion_error.

      ENDIF.


      SPLIT lv_tmp_string AT ';'
        INTO DATA(lv_current_str)
             DATA(lv_next_str).

      DATA(lv_orignal_str) = lv_current_str.


      "User Exit - POST Conversions
      "---------------------------------------------------------------
      IF xs_exit_config-repid IS NOT INITIAL.
        PERFORM (xs_exit_config-exit_str_to_sap_preconv) IN PROGRAM (xs_exit_config-repid) IF FOUND
                                                            USING
                                                                <fcat>
                                                            CHANGING
                                                                lv_current_str.
      ENDIF.


      "Deletion special chars
      "-------------------------------------------------
      IF 1 = 2.
        remove_special_char(
          CHANGING
            y_text = lv_current_str
        ).
      ENDIF.


      "Conversion Exit
      "---------------------------------------------------------------
      IF <fcat>-edit_mask IS NOT INITIAL.

        DATA(lv_exit_name)    = <fcat>-edit_mask+2.
        DATA(lv_fm_conv_exit) = |CONVERSION_EXIT_{ lv_exit_name }_INPUT|.

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = CONV rs38l-name( lv_fm_conv_exit )
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        IF sy-subrc EQ 0.

          CASE lv_exit_name.
            WHEN 'ABPSP'.
              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
                EXPORTING
                  input     = lv_current_str
                IMPORTING
                  output    = lv_current_str
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
              IF sy-subrc <> 0.
              ENDIF.


            WHEN OTHERS.
              CALL FUNCTION lv_fm_conv_exit
                EXPORTING
                  input  = lv_current_str
                IMPORTING
                  output = lv_current_str.

          ENDCASE.

        ENDIF.

      ENDIF.


      "Numbers
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_numbers[].

        conv_numb_to_int(
          EXPORTING
            x_numb_ext         = lv_current_str
          RECEIVING
            y_numb_int         = DATA(lv_tmp_num)
          EXCEPTIONS
            format_error       = 1
            plausibility_error = 2
            OTHERS             = 3
        ).
        CASE sy-subrc.
          WHEN 0.
            lv_current_str = lv_tmp_num.
            CONDENSE lv_current_str NO-GAPS.

          WHEN 1.
            y_conversions_errors-error = 'Unmanaged number format'. "#EC NOTEXT
            y_conversions_errors-value = lv_current_str.
            RAISE conversion_error.

          WHEN 2.
            y_conversions_errors-error = 'Implausible number, chars detected'. "#EC NOTEXT
            y_conversions_errors-value = lv_current_str.
            RAISE plausibility_error.

        ENDCASE.

      ENDIF.


      "Date
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_date[].

        conv_data_to_int(
          EXPORTING
            x_data_ext         = lv_current_str
          RECEIVING
            y_data_int         = DATA(lv_tmp_dats)
          EXCEPTIONS
            format_error       = 1
            plausibility_error = 2
            OTHERS             = 3
        ).
        CASE sy-subrc.
          WHEN 0.
            lv_current_str = lv_tmp_dats.
            CONDENSE lv_current_str NO-GAPS.

          WHEN 1.
            y_conversions_errors-error = 'Unmanaged data format'. "#EC NOTEXT
            y_conversions_errors-value = lv_current_str.
            RAISE conversion_error.

          WHEN 2.
            y_conversions_errors-error = 'Implausible date'. "#EC NOTEXT
            y_conversions_errors-value = lv_current_str.
            RAISE plausibility_error.

        ENDCASE.

      ENDIF.


      "Time
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_time[].

        conv_time_to_int(
          EXPORTING
            x_time             = lv_current_str
          RECEIVING
            y_time             = DATA(lv_tmp_time)
          EXCEPTIONS
            format_error       = 1
            plausibility_error = 2
            OTHERS             = 3
        ).
        CASE sy-subrc.
          WHEN 0.
            lv_current_str = lv_tmp_time.
            CONDENSE lv_current_str NO-GAPS.

          WHEN 1.
            y_conversions_errors-error = 'Unmanaged time format'. "#EC NOTEXT
            y_conversions_errors-value = lv_current_str.
            RAISE conversion_error.

          WHEN 2.
            y_conversions_errors-error = 'Implausible time'. "#EC NOTEXT
            y_conversions_errors-value = lv_current_str.
            RAISE plausibility_error.

        ENDCASE.
      ENDIF.


      "Charlike
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_charlike[].

        "Normal Data -> Nothing To Do

      ENDIF.


      "User Exit - POST Conversions
      "---------------------------------------------------------------
      IF xs_exit_config-repid IS NOT INITIAL.
        PERFORM (xs_exit_config-exit_str_to_sap_postconv) IN PROGRAM (xs_exit_config-repid) IF FOUND
                                                            USING
                                                                <fcat>
                                                                lv_orignal_str
                                                            CHANGING
                                                                lv_current_str.
      ENDIF.


      <value>       = lv_current_str.
      lv_tmp_string = lv_next_str.

    ENDLOOP.

    y_sap_data = <sap_data>.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_CSV_XLSX->CONV_TAB_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_HEADER                       TYPE        OS_BOOLEAN (default ='X')
* | [<-()] YT_STR_DATA                    TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_tab_to_ext.

    FIELD-SYMBOLS: <gt_sap_data> TYPE STANDARD TABLE.
    ASSIGN me->gref_sap_data->* TO <gt_sap_data>.

    CLEAR yt_str_data[].


    "Build header line
    "-------------------------------------------------
    IF x_header EQ abap_true.

      APPEND INITIAL LINE TO yt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).
      <str_data> = me->get_header_from_data( ).

    ENDIF.


    "Convert SAP Data to String table
    "-------------------------------------------------
    LOOP AT <gt_sap_data> ASSIGNING FIELD-SYMBOL(<sap_data>).

      APPEND INITIAL LINE TO yt_str_data ASSIGNING <str_data>.
      conv_sap_to_string(
        EXPORTING
          x_sap_data     = <sap_data>
          x_separator    = c_separator_semicolon
          xt_fcat        = me->gt_fcat
          xo_structdescr = me->go_structdescr
          xs_exit_config = me->gs_exit_config
        IMPORTING
          y_str_data     = <str_data>
      ).

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_CSV_XLSX->CONV_TAB_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [<---] YT_SAP_DATA                    TYPE        TABLE
* | [<---] YT_CONVERSIONS_ERRORS          TYPE        TT_CONVERSIONS_ERRORS
* | [EXC!] CONVERSION_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_tab_to_int.

    CLEAR: yt_sap_data[], yt_conversions_errors[].


    "Conversion in SAP Format for CSV
    "-------------------------------------------------
    LOOP AT xt_str_data ASSIGNING FIELD-SYMBOL(<data_str>).
      DATA(lv_tabix) = sy-tabix.

      IF x_header EQ abap_true.
        CHECK lv_tabix NE 1.
      ENDIF.

      APPEND INITIAL LINE TO yt_sap_data ASSIGNING FIELD-SYMBOL(<data_sap>).
      conv_string_to_sap(
        EXPORTING
          x_str_data           = <data_str>
          xt_fcat              = me->gt_fcat
          xo_structdescr       = me->go_structdescr
          xs_exit_config       = me->gs_exit_config
        IMPORTING
          y_sap_data           = <data_sap>
          y_conversions_errors = DATA(ls_conv_error)
        EXCEPTIONS
          conversion_error     = 1
          plausibility_error   = 2
          OTHERS               = 3
      ).
      CASE sy-subrc.
        WHEN 0.
          "OK
        WHEN 1 OR 2.
          ls_conv_error-row_num = lv_tabix.
          APPEND ls_conv_error TO yt_conversions_errors.
      ENDCASE.

    ENDLOOP.

    IF yt_conversions_errors IS NOT INITIAL.
      RAISE conversion_error.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_TIME_TO_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        UZEIT
* | [<-()] Y_TIME                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_time_to_ext.

*    Time conversion exit from SAP to External
*    From
*        -> 235959
*    To
*        -> 23:59:59

    y_time = COND #(
      WHEN x_time EQ c_initial_time THEN ''
      ELSE |{ x_time(2) }:{ x_time+2(2) }:{ x_time+4(2) }|
    ).

    CONDENSE y_time NO-GAPS.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>CONV_TIME_TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TIME                         TYPE        STRING
* | [<-()] Y_TIME                         TYPE        UZEIT
* | [EXC!] FORMAT_ERROR
* | [EXC!] PLAUSIBILITY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_time_to_int.

*    Time conversion exit from External to SAP
*    From
*        -> 23:59:59
*        -> 235959
*    To
*        -> 235959

    y_time = c_initial_time.

    IF strlen( x_time ) NE 8
      AND strlen( x_time ) NE 6.
      RAISE format_error.
    ENDIF.

    y_time = COND #(
        WHEN strlen( x_time ) EQ 6 THEN x_time
        WHEN strlen( x_time ) EQ 8 THEN |{ x_time(2) }{ x_time+3(2) }{ x_time+6(2) }|
   ).

    CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
      EXPORTING
        time                      = y_time
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE plausibility_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->DOWNLOAD_CSV_LOCAL
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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->DOWNLOAD_CSV_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_csv_server.

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
* | Instance Private Method ZAG_CL_CSV_XLSX->DOWNLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_excel_local.

    "Standard Version ->
    "Conversion of fields like data, numbers exc.
    "will be performed by SAP Standard
    "-------------------------------------------------
*    cl_salv_bs_lex=>export_from_result_data_table(
*      EXPORTING
*        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
*        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = me->gref_sap_data
*                                                                           t_fieldcatalog  = me->gt_fcat )
*      IMPORTING
*        er_result_file       = DATA(lv_xdata) ).

    "TODO check if works
    DATA(lv_xdata) = CONV xstring( cl_bcs_convert=>xtab_to_xstring( it_xtab = xt_str_data[] ) ).

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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->DOWNLOAD_EXCEL_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] XT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_excel_server.

    DATA: lt_data_ref TYPE REF TO data.

    FIELD-SYMBOLS: <t_sap_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

*    cl_salv_bs_lex=>export_from_result_data_table(
*      EXPORTING
*        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
*        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = me->gref_sap_data
*                                                                           t_fieldcatalog  = me->gt_fcat )
*      IMPORTING
*        er_result_file       = DATA(lv_xdata) ).

    "TODO check if works
    TRY.
        DATA(lv_xdata) = CONV xstring( cl_bcs_convert=>xtab_to_xstring( it_xtab = xt_str_data[] ) ).

      CATCH cx_bcs INTO DATA(lx_bcs). " BCS: General Exceptions
        RAISE unable_open_path.
    ENDTRY.

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
  METHOD f4_help_dir_input.

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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_CSV_XLSX->FILE_DOWNLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        OS_BOOLEAN (default ='X')
* | [--->] X_SOURCE                       TYPE        CHAR1 (default ='L')
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD file_download.

    DATA: lv_filetype TYPE char10 VALUE IS INITIAL.

    "-------------------------------------------------

    "Check if file is supported
    "-------------------------------------------------
    IF x_filename CP '*.xlsx'.
      lv_filetype = c_filetype_xlsx.
    ELSEIF x_filename CP '*.csv'.
      lv_filetype = c_filetype_csv.
    ELSE.
      RAISE not_supported_file.
    ENDIF.

    FIELD-SYMBOLS: <gt_sap_data> TYPE STANDARD TABLE.
    ASSIGN me->gref_sap_data->* TO <gt_sap_data>.


    "Convert SAP Table data into String table Data
    "-------------------------------------------------
    DATA(lt_str_data) = conv_tab_to_ext( x_header = x_header ).


    "-------------------------------------------------
    "Start Download on Local / Serveer
    "-------------------------------------------------
    CASE lv_filetype.
      WHEN c_filetype_xlsx. "**********************************************************************

        CASE x_source.
          WHEN c_source_local.
            "-------------------------------------------------

            download_excel_local(
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

          WHEN c_source_server.
            "-------------------------------------------------

            download_excel_server(
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


      WHEN c_filetype_csv. "**********************************************************************

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

    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_CSV_XLSX->FILE_UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [--->] X_HEADER                       TYPE        XFELD (default ='X')
* | [--->] X_SOURCE                       TYPE        CHAR1 (default ='L')
* | [<---] YT_SAP_DATA                    TYPE        TABLE
* | [<---] YT_CONVERSIONS_ERRORS          TYPE        TT_CONVERSIONS_ERRORS
* | [EXC!] INPUT_ERROR
* | [EXC!] NOT_SUPPORTED_FILE
* | [EXC!] UNABLE_OPEN_PATH
* | [EXC!] EMPTY_FILE
* | [EXC!] CONVERSION_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD file_upload.

    DATA: lt_str_data TYPE string_table,
          lv_filetype TYPE char10 VALUE IS INITIAL.

    "-------------------------------------------------

    CLEAR: yt_sap_data[], yt_conversions_errors[].


    "Check if file is supported
    "-------------------------------------------------
    IF x_filename CP '*.xlsx'.
      lv_filetype = c_filetype_xlsx.
    ELSEIF x_filename CP '*.csv'.
      lv_filetype = c_filetype_csv.
    ELSE.
      RAISE not_supported_file.
    ENDIF.


    "-------------------------------------------------
    "Start Download on Local / Serveer
    "-------------------------------------------------
    CASE lv_filetype.
      WHEN c_filetype_xlsx. "**********************************************************************

        CASE x_source.
          WHEN c_source_local.
            "-------------------------------------------------

            "Upload from XLSX with in-built conversion in SAP Format
            "-------------------------------------------------
            me->upload_excel_local(
              EXPORTING
                x_filename                = x_filename
              IMPORTING
                yt_str_data               = lt_str_data[]
              EXCEPTIONS
                unable_open_path          = 1
                OTHERS                    = 2
            ).
            CASE sy-subrc.
              WHEN 0.
                "OK
              WHEN 1.
                RAISE unable_open_path.
            ENDCASE.


          WHEN c_source_server.
            "-------------------------------------------------

            RAISE input_error.

        ENDCASE.

      WHEN c_filetype_csv. "**********************************************************************

        CASE x_source.
          WHEN c_source_local.
            "-------------------------------------------------

            me->upload_csv_local(
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

          WHEN c_source_server.
            "-------------------------------------------------

            me->upload_csv_server(
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

      WHEN OTHERS.
        RAISE not_supported_file.

    ENDCASE.



    "Conversion in SAP Format for CSV
    "-------------------------------------------------
    IF lines( lt_str_data ) EQ 0.
      RAISE empty_file.
    ENDIF.

    me->conv_tab_to_int(
      EXPORTING
        x_header              = x_header
        xt_str_data           = lt_str_data
      IMPORTING
        yt_sap_data           = yt_sap_data
        yt_conversions_errors = yt_conversions_errors
      EXCEPTIONS
        conversion_error      = 1
        OTHERS                = 2
    ).
    CASE sy-subrc.
      WHEN 0.
        "OK
      WHEN 1.
        RAISE conversion_error.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>GET_DESKTOP_DIRECTORY
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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>GET_FIELDCAT_FROM_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] XS_SAP_LINE                    TYPE        ANY(optional)
* | [--->] XT_SAP_TABLE                   TYPE        TABLE(optional)
* | [<---] YO_STRUCTDESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [<---] YT_FCAT                        TYPE        LVC_T_FCAT
* | [EXC!] UNABLE_DEFINE_STRUCTDESCR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_fieldcat_from_data.

    DATA: lref_sap_data  TYPE REF TO data,
          lref_sap_table TYPE REF TO data.

    DATA: lv_except_msg TYPE string.

    FIELD-SYMBOLS: <sap_line>  TYPE any,
                   <sap_table> TYPE STANDARD TABLE.

    "-------------------------------------------------

    FREE yo_structdescr.
    CLEAR yt_fcat[].

    IF xs_sap_line IS SUPPLIED.
      CREATE DATA lref_sap_data LIKE xs_sap_line.
      ASSIGN lref_sap_data->* TO <sap_line>.

      CREATE DATA lref_sap_table LIKE TABLE OF xs_sap_line.
      ASSIGN lref_sap_table->* TO <sap_table>.

    ELSEIF xt_sap_table IS SUPPLIED.
      CREATE DATA lref_sap_data LIKE LINE OF xt_sap_table.
      ASSIGN lref_sap_data->* TO <sap_line>.

      CREATE DATA lref_sap_table LIKE xt_sap_table.
      ASSIGN lref_sap_table->* TO <sap_table>.

    ELSE.
      RAISE unable_define_structdescr.

    ENDIF.

    "-------------------------------------------------

    yo_structdescr ?= cl_abap_typedescr=>describe_by_data( <sap_line> ).


    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(lt_salv_table)
                                CHANGING
                                  t_table        = <sap_table>  ).
        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lt_salv_table->get_columns( ) " ALV Filter
                                                                     r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
                                                                     ) .

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        lv_except_msg = lx_ai_system_fault->get_text( ).
        RAISE unable_define_structdescr.

      CATCH cx_salv_msg  INTO DATA(lx_salv_msg).
        lv_except_msg = lx_salv_msg->get_text( ).
        RAISE unable_define_structdescr.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_CSV_XLSX->GET_HEADER_FROM_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] Y_STR_HEADER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_header_from_data.

    y_str_header = ''.

    LOOP AT me->gt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CHECK <fcat>-fieldname NE 'MANDT'.

      y_str_header = COND #(
        WHEN y_str_header EQ '' THEN <fcat>-reptext
        WHEN y_str_header NE '' THEN |{ y_str_header };{ <fcat>-reptext }|
      ).

    ENDLOOP.

    RETURN.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_CSV_XLSX=>REMOVE_SPECIAL_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] Y_TEXT                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_special_char.

    CONSTANTS: c_pattern_lect_upper TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', "#EC NOTEXT
               c_pattern_lect_lower TYPE string VALUE 'abcdefghijklmnopqrstuvwxyz', "#EC NOTEXT
               c_pattern_digit      TYPE string VALUE '0123456789', "#EC NOTEXT
               c_pattern_symb       TYPE string VALUE '!"%/=?;,.:-_@&+*()[]{}<>€$£', "#EC NOTEXT
               c_pattern_lect_acc   TYPE string VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'. "#EC NOTEXT

    DATA: lv_text    TYPE string,
          lv_new_str TYPE string VALUE IS INITIAL.

    "Old version - Check Char by Char
    DATA(lv_length) = strlen( y_text ).
    lv_text = y_text.

    DO lv_length TIMES.
      DATA(lv_index)     = sy-index - 1.
      DATA(lv_curr_char) = lv_text+lv_index(1).

      CHECK lv_curr_char CA c_pattern_lect_upper
         OR lv_curr_char CA c_pattern_lect_lower
         OR lv_curr_char CA c_pattern_digit
         OR lv_curr_char CA c_pattern_symb
         OR lv_curr_char CA c_pattern_lect_acc
         OR lv_curr_char EQ space.

      IF lv_new_str IS INITIAL.
        lv_new_str = lv_curr_char.
      ELSE.
        lv_new_str = |{ lv_new_str }{ lv_curr_char }|.
      ENDIF.

    ENDDO.

    y_text = lv_new_str.


    "New Version - Check with regex - TODO Work in Progress Regex

*    CONSTANTS: c_regex_lect_upper TYPE string VALUE 'A-Z',
*               c_regex_lect_lower TYPE string VALUE 'a-z',
*               c_regex_digit      TYPE string VALUE '0-9',
*               c_regex_symb       TYPE string VALUE '!"%/=?;,.:-_@&+*()[]{}<>€$£',
*               c_regex_lect_acc   TYPE string VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'.
*
*    lv_new_str = y_text.
*
*    lv_regex = ''.
*    lv_regex = |{ lv_regex }[{ c_regex_lect_upper }][{ c_regex_lect_lower }]|.
*    lv_regex = |{ lv_regex }[{ c_regex_digit }]|.
*    lv_regex = |{ lv_regex }[{ c_regex_symb }][{ c_regex_lect_acc }]|.
*
*    lv_regex = |[^{ lv_regex }]|.
*
*    REPLACE ALL OCCURRENCES OF REGEX lv_regex IN lv_new_str WITH ''.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->UPLOAD_CSV_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_csv_local.

    CLEAR yt_str_data[].
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
* | Instance Private Method ZAG_CL_CSV_XLSX->UPLOAD_CSV_SERVER
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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_CSV_XLSX->UPLOAD_EXCEL_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FILENAME                     TYPE        STRING
* | [<---] YT_STR_DATA                    TYPE        STRING_TABLE
* | [EXC!] UNABLE_OPEN_PATH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_excel_local.

    DATA: lv_except_msg    TYPE string,
          lt_bin_tab       TYPE solix_tab,
          lcl_excel_ref    TYPE REF TO cl_fdt_xl_spreadsheet,
          lcl_table_descr  TYPE REF TO cl_abap_tabledescr,
          lcl_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <t_excel_data> TYPE STANDARD TABLE,
                   <str_data>     TYPE string.

    "-------------------------------------------------

    CLEAR: yt_str_data[].

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
      "-------------------------------------------------
      ASSIGN lt_worksheets[ 1 ] TO FIELD-SYMBOL(<woksheetname>).
      DATA(lcl_data_ref) = lcl_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <woksheetname> ).
      ASSIGN lcl_data_ref->* TO <t_excel_data>.

      IF <t_excel_data> IS INITIAL.
        EXIT.
      ENDIF.


      "Convert to CSV String format
      "---------------------------------------------------------------
      LOOP AT <t_excel_data> ASSIGNING FIELD-SYMBOL(<excel>).

        APPEND INITIAL LINE TO yt_str_data ASSIGNING <str_data>.

        LOOP AT me->go_structdescr->components ASSIGNING FIELD-SYMBOL(<comp>).

          ASSIGN COMPONENT <comp>-name OF STRUCTURE <excel> TO FIELD-SYMBOL(<excel_col>).
          <str_data> = COND #(
            WHEN <str_data> EQ '' THEN <excel_col>
            WHEN <str_data> NE '' THEN |{ <str_data> };{ <excel_col> }|
          ).

        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

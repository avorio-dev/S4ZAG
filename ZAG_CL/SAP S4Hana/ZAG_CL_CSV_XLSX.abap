CLASS zag_cl_csv_xlsx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_conversions_errors,
        row_num     TYPE i,
        field       TYPE fieldname,
        field_descr TYPE string,
        value       TYPE string,
        error       TYPE string,
      END OF ts_conversions_errors ,

      BEGIN OF ts_exit_config,
        repid                    TYPE sy-repid,
        exit_sap_to_str          TYPE string,
        exit_str_to_sap_preconv  TYPE string,
        exit_str_to_sap_postconv TYPE string,
      END OF ts_exit_config .

    TYPES:
      tt_conversions_errors TYPE TABLE OF ts_conversions_errors .


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF cc_file_source,
        local  TYPE char1 VALUE 'L' ##NO_TEXT,
        server TYPE char1 VALUE 'S' ##NO_TEXT,
      END OF cc_file_source,

      BEGIN OF cc_filetype,
        csv  TYPE char3 VALUE 'CSV'  ##NO_TEXT,
        xlsx TYPE char4 VALUE 'XLSX' ##NO_TEXT,
      END OF cc_filetype,

      BEGIN OF cc_separator,
        horizontal_tab TYPE abap_char1 VALUE %_horizontal_tab ##NO_TEXT,
        semicolon      TYPE char1      VALUE ';'              ##NO_TEXT,
        cr_lf          TYPE abap_cr_lf VALUE %_cr_lf          ##NO_TEXT,
        slash          TYPE char1      VALUE '/'              ##NO_TEXT,
      END OF cc_separator,

      BEGIN OF cc_user_exit,
        pre_sap_to_string  TYPE string VALUE 'PRE_SAP_TO_STRING',
        post_sap_to_string TYPE string VALUE 'POST_SAP_TO_STRING',
        pre_string_to_sap  TYPE string VALUE 'PRE_STRING_TO_SAP',
        post_string_to_sap TYPE string VALUE 'POST_STRING_TO_SAP',
      END OF cc_user_exit.

    CONSTANTS:
      c_initial_data TYPE datum VALUE '00000000' ##NO_TEXT,
      c_initial_time TYPE time  VALUE 000000     ##NO_TEXT,
      c_max_data     TYPE datum VALUE '99991231' ##NO_TEXT.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      conv_data_to_ext
        IMPORTING
                  !xv_data_int       TYPE string
                  !xv_separator      TYPE abap_char1 DEFAULT cc_separator-slash
        RETURNING VALUE(yv_data_ext) TYPE string,

      conv_data_to_int
        IMPORTING
                   !xv_data_ext       TYPE string
        RETURNING  VALUE(yv_data_int) TYPE dats
        EXCEPTIONS
                   format_error
                   plausibility_error,

      conv_numb_to_ext
        IMPORTING
                  !xv_numb_int       TYPE string
        RETURNING VALUE(yv_numb_ext) TYPE string,

      conv_numb_to_int
        IMPORTING
                   !xv_numb_ext       TYPE string
        RETURNING  VALUE(yv_numb_int) TYPE string
        EXCEPTIONS
                   format_error
                   plausibility_error,

      conv_time_to_ext
        IMPORTING
                  !xv_time_int       TYPE string
        RETURNING VALUE(yv_time_ext) TYPE string,

      conv_time_to_int
        IMPORTING
                   !xv_time_ext       TYPE string
        RETURNING  VALUE(yv_time_int) TYPE uzeit
        EXCEPTIONS
                   format_error
                   plausibility_error,

      conv_sap_to_string
        IMPORTING
          !xs_sap_data     TYPE any
          !xt_fcat         TYPE lvc_t_fcat
          !xo_structdescr  TYPE REF TO cl_abap_structdescr
          !xv_separator    TYPE char1 DEFAULT cc_separator-semicolon
          !xo_exit_handler TYPE REF TO object OPTIONAL
        EXPORTING
          !y_str_data      TYPE string,

      conv_string_to_sap
        IMPORTING
          !xv_str_data           TYPE string
          !xt_fcat               TYPE lvc_t_fcat
          !xo_structdescr        TYPE REF TO cl_abap_structdescr
          !xv_separator          TYPE abap_char1 DEFAULT cc_separator-semicolon
          !xo_exit_handler       TYPE REF TO object OPTIONAL
          !xs_exit_config        TYPE ts_exit_config OPTIONAL
        EXPORTING
          !ys_sap_data           TYPE any
          !ys_conversions_errors TYPE ts_conversions_errors
        EXCEPTIONS
          conversion_error
          plausibility_error,

      remove_special_char
        CHANGING
          !yv_text TYPE string,

      get_fieldcat_from_data
        IMPORTING
          !xs_sap_line    TYPE any OPTIONAL
          !xt_sap_table   TYPE table OPTIONAL
        EXPORTING
          !yo_structdescr TYPE REF TO cl_abap_structdescr
          !yt_fcat        TYPE lvc_t_fcat
        RAISING
          cx_ai_system_fault.

    CLASS-METHODS:
      f4_help_dir_input
        IMPORTING
          !xv_source     TYPE abap_char1 DEFAULT cc_file_source-local
        EXPORTING
          !yv_path_input TYPE string,

      f4_help_dir_output
        IMPORTING
          !xv_source      TYPE abap_char1 DEFAULT cc_file_source-local
        EXPORTING
          !yv_path_output TYPE string,

      get_desktop_directory
        RETURNING VALUE(yv_desktop_dir) TYPE string.

    METHODS:
      init_instance
        IMPORTING
          !xt_sap_table    TYPE table
          !xv_filename     TYPE string
          !xv_header       TYPE abap_char1    DEFAULT abap_true
          !xo_exit_handler TYPE REF TO object OPTIONAL
        EXCEPTIONS
          unable_define_structdescr,

      file_download
        IMPORTING
          !xv_filename     TYPE string
          !xt_sap_table    TYPE table
          !xv_source       TYPE char1         DEFAULT cc_file_source-local
          !xv_header       TYPE os_boolean    DEFAULT abap_true
          !xo_exit_handler TYPE REF TO object OPTIONAL
        EXCEPTIONS
          not_supported_file
          unable_define_structdescr
          unable_open_path,

      file_upload
        IMPORTING
          !xv_filename           TYPE string
          !xv_source             TYPE char1         DEFAULT cc_file_source-local
          !xv_header             TYPE os_boolean    DEFAULT abap_true
          !xo_exit_handler       TYPE REF TO object OPTIONAL
        EXPORTING
          !yt_conversions_errors TYPE tt_conversions_errors
        CHANGING
          !yt_sap_data           TYPE table
        EXCEPTIONS
          not_supported_file
          unable_define_structdescr
          input_error
          unable_open_path
          empty_file,

      get_header_from_data
        RETURNING VALUE(yv_str_header) TYPE string,

      conv_tab_to_ext
        RETURNING VALUE(yt_str_data) TYPE string_table,

      conv_tab_to_int
        EXPORTING
          !yt_sap_data           TYPE table
          !yt_conversions_errors TYPE tt_conversions_errors.

  PROTECTED SECTION.


  PRIVATE SECTION.

    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF cc_symbols,
        lect_upper   TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'   ##NO_TEXT,
        lect_lower   TYPE string VALUE 'abcdefghijklmnopqrstuvwxyz'   ##NO_TEXT,
        digit        TYPE string VALUE '0123456789'                   ##NO_TEXT,
        symb         TYPE string VALUE '!"%/=?;,.:-_@&+*()[]{}<>€$£'  ##NO_TEXT,
        lect_acc     TYPE string VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'             ##NO_TEXT,
        exp_notation TYPE string VALUE '0123456789.,+-E'              ##NO_TEXT,
      END OF cc_symbols,

      BEGIN OF cc_exception_msg,
        unable_read_file  TYPE string VALUE 'Unable read file'                   ##NO_TEXT,
        unable_def_struct TYPE string VALUE 'Unable define Structure Descriptor' ##NO_TEXT,
        input_error       TYPE string VALUE 'Input error'                        ##NO_TEXT,
        internal_error    TYPE string VALUE 'Internal error occurred'            ##NO_TEXT,
        not_implemented   TYPE string VALUE 'Exit method not implemented'        ##NO_TEXT,
      END OF cc_exception_msg,

      BEGIN OF cc_conversion_msg,
        unmanaged_dtype TYPE string VALUE 'Unmanaged data type' ##NO_TEXT,
        format_number   TYPE string VALUE 'Wrong number format' ##NO_TEXT,
        format_data     TYPE string VALUE 'Wrong data format'   ##NO_TEXT,
        format_time     TYPE string VALUE 'Wrong time format'   ##NO_TEXT,
        implaus_number  TYPE string VALUE 'Implausible number'  ##NO_TEXT,
        implaus_data    TYPE string VALUE 'Implausible data'    ##NO_TEXT,
        implaus_time    TYPE string VALUE 'Implausible time'    ##NO_TEXT,
      END OF cc_conversion_msg.

    CONSTANTS:
      c_mandt TYPE fieldname VALUE 'MANDT' ##NO_TEXT.


    " Data
    "-------------------------------------------------
    CLASS-DATA:
      gr_typekind_charlike TYPE RANGE OF abap_typekind,
      gr_typekind_date     TYPE RANGE OF abap_typekind,
      gr_typekind_numbers  TYPE RANGE OF abap_typekind,
      gr_typekind_time     TYPE RANGE OF abap_typekind.

    DATA:
      gref_sap_data   TYPE REF TO data,
      gv_filename     TYPE string,
      gv_header       TYPE abap_char1,
      go_exit_handler TYPE REF TO object,

      go_structdescr  TYPE REF TO cl_abap_structdescr,
      gt_fcat         TYPE lvc_t_fcat,
      gv_separator    TYPE abap_char1,

      gt_str_data     TYPE string_table.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      init_managed_ranges,

      conv_standard_exit_input
        IMPORTING
          !xv_conv_name TYPE string
        CHANGING
          !yv_tmp_data  TYPE string,

      conv_standard_exit_output
        IMPORTING
          !xv_conv_name TYPE string
        CHANGING
          !yv_tmp_data  TYPE string.

    METHODS:
      download_csv_local
        EXCEPTIONS
          unable_open_path,

      download_csv_server
        EXCEPTIONS
          unable_open_path,

      download_excel_local
        RAISING
          cx_ai_system_fault,

      download_excel_server
        RAISING
          cx_ai_system_fault,

      upload_csv_local
        RAISING
          cx_ai_system_fault,

      upload_csv_server
        RAISING
          cx_ai_system_fault,

      upload_excel_local
        RAISING
          cx_ai_system_fault.

ENDCLASS.



CLASS zag_cl_csv_xlsx IMPLEMENTATION.


  METHOD conv_data_to_ext.

*    Data conversion from SAP to External
*    From
*        -> 20241231
*    To
*        -> 31/12/2024

    DATA(lv_data_int) = xv_data_int.

    yv_data_ext = COND #(
      WHEN lv_data_int EQ c_initial_data THEN ''
      ELSE |{ lv_data_int+6(2) }{ xv_separator }{ lv_data_int+4(2) }{ xv_separator }{ lv_data_int(4) }|
    ).

    CONDENSE yv_data_ext NO-GAPS.

  ENDMETHOD.


  METHOD conv_data_to_int.

*    Data conversion exit from External to SAP
*    From
*        -> 25/12/2024
*        -> 2024/12/31
*        -> 20241231
*    To
*        -> 20241231

    yv_data_int = c_initial_data.

    CHECK xv_data_ext IS NOT INITIAL.

    DATA(lv_data_ext) = xv_data_ext.
    DATA(lv_data_int) = c_initial_data.
    CONDENSE lv_data_ext NO-GAPS.


    CASE strlen( lv_data_ext ).
      WHEN 8.
        "Format like 20231225
        lv_data_int = lv_data_ext.

      WHEN 10.

        "Data format managed
        "25-12-2023
        "2023-12-25

        IF lv_data_ext+2(1) CA cc_symbols-digit.
          "Format like 2023-12-25
          lv_data_int = |{ lv_data_ext(4) }{ lv_data_ext+5(2) }{ lv_data_ext+8(2) }|.

        ELSEIF lv_data_ext+2(1) NA cc_symbols-digit.
          "Format like 25-12-2023
          lv_data_int = |{ lv_data_ext+6(4) }{ lv_data_ext+3(2) }{ lv_data_ext(2) }|.

        ELSE.
          RAISE format_error.

        ENDIF.

      WHEN OTHERS.
        RAISE format_error.

    ENDCASE.


    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_data_int
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE plausibility_error.
    ENDIF.

    yv_data_int = lv_data_int.

  ENDMETHOD.


  METHOD conv_numb_to_ext.

*    Number conversion exit from SAP to External
*    From
*        -> 1000.25
*        -> 1000.25-
*    To
*        -> 1000,25
*        -> -1000,25

    yv_numb_ext        = 0.
    DATA(lv_numb_int) = xv_numb_int.


    REPLACE '.' IN lv_numb_int WITH ','.

    FIND '-' IN lv_numb_int.
    IF sy-subrc EQ 0.
      REPLACE '-' IN lv_numb_int WITH ''.
      lv_numb_int = |-{ lv_numb_int }|.
    ENDIF.

    CONDENSE lv_numb_int NO-GAPS.


    yv_numb_ext = lv_numb_int.

  ENDMETHOD.


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

    "---------------------------------------------------------------


    yv_numb_int       = 0.

    CHECK xv_numb_ext IS NOT INITIAL.

    DATA(lv_numb_int) = xv_numb_ext.

    CONDENSE lv_numb_int NO-GAPS.
    TRANSLATE lv_numb_int TO UPPER CASE.


    "Find special chars occurrences
    "-------------------------------------------------
    DATA(lv_counter_dot) = 0.
    FIND ALL OCCURRENCES OF '.' IN lv_numb_int MATCH COUNT lv_counter_dot.

    DATA(lv_counter_comma) = 0.
    FIND ALL OCCURRENCES OF ',' IN lv_numb_int MATCH COUNT lv_counter_comma.

    DATA(lv_counter_plus) = 0.
    FIND ALL OCCURRENCES OF '+' IN lv_numb_int MATCH COUNT lv_counter_plus.

    DATA(lv_counter_minus) = 0.
    FIND ALL OCCURRENCES OF '-' IN lv_numb_int MATCH COUNT lv_counter_minus.

    DATA(lv_counter_exp) = 0.
    FIND ALL OCCURRENCES OF 'E' IN lv_numb_int MATCH COUNT lv_counter_exp.


    "Plausibility Pre-Check
    "---------------------------------------------------------------
    IF lv_numb_int CN cc_symbols-exp_notation.
      RAISE plausibility_error.
    ENDIF.

    IF lv_counter_plus    GT 1
      OR lv_counter_minus GT 2 "Allowed double '-' in Exp notation
      OR lv_counter_exp   GT 1.
      RAISE plausibility_error.
    ENDIF.


    "Exponential notation management
    "-------------------------------------------------
    CASE lv_counter_exp.

      WHEN 0. "Normal Notation detected

        IF lv_counter_plus EQ 1.
          REPLACE '+' IN lv_numb_int WITH ''.
        ENDIF.

        IF lv_counter_minus EQ 1.
          REPLACE ALL OCCURRENCES OF '-' IN lv_numb_int WITH ''.
          lv_numb_int = |{ lv_numb_int }-|.
        ENDIF.

      WHEN 1. "Exponential Notation Detected

        "Mandatory '+' or '-' with exp notation
        IF lv_counter_plus     EQ 0
          AND lv_counter_minus EQ 0.
          RAISE format_error.
        ENDIF.


        "Mandatory 'E+' if positive
        IF lv_counter_plus EQ 1.

          IF lv_numb_int NS 'E+'.
            RAISE format_error.
          ENDIF.

        ENDIF.


        "Mandatory 'E-' if negative
        CASE lv_counter_minus.
          WHEN 1.
            IF lv_numb_int NS 'E-'.
              RAISE format_error.
            ENDIF.

          WHEN 2.
            IF lv_numb_int NS 'E-' OR lv_numb_int(1) NE '-'.
              RAISE format_error.
            ENDIF.

        ENDCASE.

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
        "So -> It will be use like decimal separator
        REPLACE ',' IN lv_numb_int WITH '.'.

      ELSE.
        "1,000,000,000
        REPLACE ALL OCCURRENCES OF ',' IN lv_numb_int WITH ''.

      ENDIF.


    ELSEIF lv_counter_comma EQ 0
      AND lv_counter_dot GT 0.
      "100.257 / 100.25 / 1.000.000.000

      IF lv_counter_dot EQ 1.
        "100.257 / 100.25

        "WARNING
        "Unable to define if comma is the separator of decimal values or integer parts of number
        "So -> It will be use like decimal separator
        "Nothing to do

      ELSE.
        "1.000.000.000
        REPLACE ALL OCCURRENCES OF '.' IN lv_numb_int WITH ''.

      ENDIF.

    ELSEIF lv_counter_comma GT 0
       AND lv_counter_dot   GT 0.

      IF lv_counter_comma EQ lv_counter_dot.
        "1,000.25 / 1.000,25

        DATA(lv_offset_comma) = 0.
        FIND FIRST OCCURRENCE OF ',' IN lv_numb_int MATCH OFFSET lv_offset_comma.

        DATA(lv_offset_dot) = 0.
        FIND FIRST OCCURRENCE OF '.' IN lv_numb_int MATCH OFFSET lv_offset_dot.

        IF lv_offset_comma LT lv_offset_dot.
          "1,000.25

          REPLACE ALL OCCURRENCES OF ',' IN lv_numb_int WITH ''.

          FIND ALL OCCURRENCES OF '.' IN lv_numb_int MATCH COUNT lv_count.
          IF lv_count GT 1.
            RAISE format_error.
          ENDIF.

        ELSEIF lv_offset_dot LT lv_offset_comma.
          "1.000,25

          REPLACE ALL OCCURRENCES OF '.' IN lv_numb_int WITH ''.

          FIND ALL OCCURRENCES OF ',' IN lv_numb_int MATCH COUNT lv_count.
          IF lv_count GT 1.
            RAISE format_error.
          ENDIF.

          REPLACE ',' IN lv_numb_int WITH '.'.

        ENDIF.

      ELSEIF lv_counter_comma GT lv_counter_dot.
        "1,000,000.25

        REPLACE ALL OCCURRENCES OF ',' IN lv_numb_int WITH ''.

        FIND ALL OCCURRENCES OF '.' IN lv_numb_int MATCH COUNT lv_count.
        IF lv_count GT 1.
          RAISE format_error.
        ENDIF.

      ELSEIF lv_counter_dot GT lv_counter_comma.
        "1.000.000,25

        REPLACE ALL OCCURRENCES OF '.' IN lv_numb_int WITH ''.

        FIND ALL OCCURRENCES OF ',' IN lv_numb_int MATCH COUNT lv_count.
        IF lv_count GT 1.
          RAISE format_error.
        ENDIF.

        REPLACE ',' IN lv_numb_int WITH '.'.

      ENDIF.

    ENDIF.

    "-------------------------------------------------

    CONDENSE lv_numb_int NO-GAPS.

    yv_numb_int = lv_numb_int.

  ENDMETHOD.


  METHOD conv_time_to_ext.

*    Time conversion exit from SAP to External
*    From
*        -> 235959
*    To
*        -> 23:59:59

    yv_time_ext        = ''.
    DATA(lv_time_int) = xv_time_int.

    lv_time_int = COND #(
      WHEN xv_time_int EQ c_initial_time THEN ''
      ELSE |{ lv_time_int(2) }:{ lv_time_int+2(2) }:{ lv_time_int+4(2) }|
    ).

    CONDENSE lv_time_int NO-GAPS.
    yv_time_ext = lv_time_int.

  ENDMETHOD.


  METHOD conv_time_to_int.

*    Time conversion exit from External to SAP
*    From
*        -> 23:59:59
*        -> 235959
*    To
*        -> 235959

    yv_time_int = c_initial_time.

    CHECK xv_time_ext IS NOT INITIAL.

    DATA(lv_time_ext) = xv_time_ext.
    CONDENSE lv_time_ext NO-GAPS.

    CHECK lv_time_ext IS NOT INITIAL.


    IF strlen( lv_time_ext ) NE 8
      AND strlen( lv_time_ext ) NE 6.
      RAISE format_error.
    ENDIF.

    yv_time_int = COND #(
        WHEN strlen( lv_time_ext ) EQ 6 THEN lv_time_ext
        WHEN strlen( lv_time_ext ) EQ 8 THEN |{ lv_time_ext(2) }{ lv_time_ext+3(2) }{ lv_time_ext+6(2) }|
   ).

    CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
      EXPORTING
        time                      = yv_time_int
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE plausibility_error.
    ENDIF.

  ENDMETHOD.


  METHOD conv_sap_to_string.

    DATA:
      lv_cx_msg TYPE string.

    y_str_data = ''.

    init_managed_ranges( ).

    LOOP AT xt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CHECK <fcat>-fieldname NE c_mandt.

      ASSIGN xo_structdescr->components[ name = <fcat>-fieldname ] TO FIELD-SYMBOL(<component>).
      CHECK sy-subrc EQ 0.

      IF NOT ( <component>-type_kind IN gr_typekind_numbers[]
            OR <component>-type_kind IN gr_typekind_date[]
            OR <component>-type_kind IN gr_typekind_time[]
            OR <component>-type_kind IN gr_typekind_charlike[] ).

        y_str_data = COND #(
            WHEN y_str_data EQ '' THEN xv_separator
            WHEN y_str_data NE '' THEN |{ y_str_data }{ xv_separator }|
        ).

        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <component>-name OF STRUCTURE xs_sap_data TO FIELD-SYMBOL(<value>).
      DATA(lv_tmp_data) = CONV string( <value> ).


      "User exit
      "---------------------------------------------------------------
      IF xo_exit_handler IS BOUND.

        TRY.
            CALL METHOD xo_exit_handler->(cc_user_exit-pre_sap_to_string)
              EXPORTING
                xs_fcat       = <fcat>
                xs_sap_data   = xs_sap_data
                xv_value      = <value>
              CHANGING
                yv_conv_value = lv_tmp_data.

          CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_illegal_method).
            lv_cx_msg = lx_illegal_method->get_longtext( ).
        ENDTRY.

      ENDIF.


      "Numbers
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_numbers[].
        IF <fcat>-edit_mask CS 'TSTLC'.
          "If the fields is a timestamp ( detected like numbers ),
          "it mustn't be converted to ext format like normal numbers
          "it must be converted from conv_exit_output routine

        ELSE.

          IF <fcat>-edit_mask CS 'EXCRT'
            OR <fcat>-edit_mask CS 'EXCRX'.

            IF lv_tmp_data CS '123456789'.

              CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
                EXPORTING
                  input  = lv_tmp_data
                IMPORTING
                  output = lv_tmp_data.

            ENDIF.

          ENDIF.

          lv_tmp_data = conv_numb_to_ext( lv_tmp_data ).

        ENDIF.
      ENDIF.


      "Date
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_date[].

        lv_tmp_data = conv_data_to_ext( xv_data_int  = lv_tmp_data
                                        xv_separator = cc_separator-slash ).

      ENDIF.


      "Time
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_time[].

        lv_tmp_data = conv_time_to_ext( lv_tmp_data ).

      ENDIF.


      "Charlike
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_charlike[].

        "Normal Text -> Nothing To Do

      ENDIF.


      "Conversion exit
      "---------------------------------------------------------------
      IF <fcat>-edit_mask IS NOT INITIAL.

        DATA(lv_conv_name)    = <fcat>-edit_mask+2.

        conv_standard_exit_output(
          EXPORTING
            xv_conv_name = CONV #( lv_conv_name )
          CHANGING
            yv_tmp_data  = lv_tmp_data
        ).

      ENDIF.


      "User exit
      "---------------------------------------------------------------
      IF xo_exit_handler IS BOUND.

        TRY.
            CALL METHOD xo_exit_handler->(cc_user_exit-post_sap_to_string)
              EXPORTING
                xs_fcat       = <fcat>
                xs_sap_data   = xs_sap_data
                xv_value      = <value>
              CHANGING
                yv_conv_value = lv_tmp_data.

          CATCH cx_sy_dyn_call_illegal_method INTO lx_illegal_method.
            lv_cx_msg = lx_illegal_method->get_longtext( ).
        ENDTRY.

      ENDIF.


      y_str_data = COND #(
        WHEN y_str_data EQ '' THEN lv_tmp_data
        WHEN y_str_data NE '' THEN |{ y_str_data }{ xv_separator }{ lv_tmp_data }|
      ).


    ENDLOOP.

  ENDMETHOD.


  METHOD conv_string_to_sap.

    DATA:
      lv_sap_ref TYPE REF TO data,
      lv_cx_msg  TYPE string.

    FIELD-SYMBOLS:
      <sap_data> TYPE any.

    "-------------------------------------------------

    CLEAR: ys_sap_data, ys_conversions_errors.

    CREATE DATA lv_sap_ref LIKE ys_sap_data.
    ASSIGN lv_sap_ref->* TO <sap_data>.

    init_managed_ranges( ).

    "-------------------------------------------------

    DATA(lv_tmp_string) = xv_str_data.

    LOOP AT xt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CHECK <fcat>-fieldname NE c_mandt.

      ASSIGN xo_structdescr->components[ name = <fcat>-fieldname ] TO FIELD-SYMBOL(<component>).
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.

      ASSIGN COMPONENT <component>-name OF STRUCTURE <sap_data> TO FIELD-SYMBOL(<value>).
      ys_conversions_errors-field       = <fcat>-fieldname.
      ys_conversions_errors-field_descr = <fcat>-reptext.

      IF <component>-type_kind NOT IN gr_typekind_numbers[]
        AND <component>-type_kind NOT IN gr_typekind_date[]
        AND <component>-type_kind NOT IN gr_typekind_time[]
        AND <component>-type_kind NOT IN gr_typekind_charlike[].

*        y_conversions_errors-error = 'Unmanaged data type'. "#EC NOTEXT
*        RAISE conversion_error.
        CONTINUE.

      ENDIF.


      SPLIT lv_tmp_string AT xv_separator
        INTO DATA(lv_current_str)
             DATA(lv_next_str).

      DATA(lv_orignal_str) = lv_current_str.


      "User Exit
      "---------------------------------------------------------------
      IF xo_exit_handler IS BOUND.

        TRY.
            CALL METHOD xo_exit_handler->(cc_user_exit-pre_string_to_sap)
              EXPORTING
                xs_fcat       = <fcat>
              CHANGING
                yv_conv_value = lv_current_str.

          CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_illegal_method).
            lv_cx_msg = lx_illegal_method->get_longtext( ).
        ENDTRY.

      ENDIF.


      "Deletion special chars
      "-------------------------------------------------
      IF 1 = 2.
        remove_special_char(
          CHANGING
            yv_text = lv_current_str
        ).
      ENDIF.


      "Conversion Exit
      "---------------------------------------------------------------
      IF <fcat>-edit_mask IS NOT INITIAL.

        DATA(lv_conv_name) = <fcat>-edit_mask+2.

        conv_standard_exit_input(
          EXPORTING
            xv_conv_name = CONV #( lv_conv_name )
          CHANGING
            yv_tmp_data  = lv_current_str
        ).

      ENDIF.


      "Numbers
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_numbers[].

        conv_numb_to_int(
          EXPORTING
            xv_numb_ext         = lv_current_str
          RECEIVING
            yv_numb_int         = DATA(lv_tmp_num)
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
            ys_conversions_errors-error = cc_conversion_msg-format_number.
            ys_conversions_errors-value = lv_current_str.
            RAISE conversion_error.

          WHEN 2.
            ys_conversions_errors-error = cc_conversion_msg-implaus_number.
            ys_conversions_errors-value = lv_current_str.
            RAISE plausibility_error.

        ENDCASE.


        IF <fcat>-edit_mask CS 'EXCRT'
         OR <fcat>-edit_mask CS 'EXCRX'.

          IF lv_tmp_num NE 0.

            CALL FUNCTION 'CONVERSION_EXIT_EXCRT_INPUT'
              EXPORTING
                input  = lv_tmp_num
              IMPORTING
                output = lv_tmp_num.

          ENDIF.

        ENDIF.

      ENDIF.


      "Date
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_date[].

        conv_data_to_int(
          EXPORTING
            xv_data_ext         = lv_current_str
          RECEIVING
            yv_data_int         = DATA(lv_tmp_dats)
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
            ys_conversions_errors-error = cc_conversion_msg-format_data.
            ys_conversions_errors-value = lv_current_str.
            RAISE conversion_error.

          WHEN 2.
            ys_conversions_errors-error = cc_conversion_msg-implaus_data.
            ys_conversions_errors-value = lv_current_str.
            RAISE plausibility_error.

        ENDCASE.

      ENDIF.


      "Time
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_time[].

        conv_time_to_int(
          EXPORTING
            xv_time_ext         = lv_current_str
          RECEIVING
            yv_time_int         = DATA(lv_tmp_time)
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
            ys_conversions_errors-error = cc_conversion_msg-format_time.
            ys_conversions_errors-value = lv_current_str.
            RAISE conversion_error.

          WHEN 2.
            ys_conversions_errors-error = cc_conversion_msg-implaus_time.
            ys_conversions_errors-value = lv_current_str.
            RAISE plausibility_error.

        ENDCASE.
      ENDIF.


      "Charlike
      "---------------------------------------------------------------
      IF <component>-type_kind IN gr_typekind_charlike[].

        "Normal Data -> Nothing To Do

      ENDIF.


      "User Exit
      "---------------------------------------------------------------
      IF xo_exit_handler IS BOUND.

        TRY.
            CALL METHOD xo_exit_handler->(cc_user_exit-post_string_to_sap)
              EXPORTING
                xs_fcat         = <fcat>
                xv_original_str = lv_orignal_str
              CHANGING
                yv_conv_value   = lv_current_str.

          CATCH cx_sy_dyn_call_illegal_method INTO lx_illegal_method.
            lv_cx_msg = lx_illegal_method->get_longtext( ).
        ENDTRY.

      ENDIF.


      <value>       = lv_current_str.
      lv_tmp_string = lv_next_str.

    ENDLOOP.

    ys_sap_data = <sap_data>.

  ENDMETHOD.


  METHOD remove_special_char.

    DATA:
      lv_text    TYPE string,
      lv_new_str TYPE string VALUE IS INITIAL.

    "Old version - Check Char by Char
    DATA(lv_length) = strlen( yv_text ).
    lv_text = yv_text.

    DO lv_length TIMES.
      DATA(lv_index)     = sy-index - 1.
      DATA(lv_curr_char) = lv_text+lv_index(1).

      CHECK lv_curr_char CA cc_symbols-lect_upper
         OR lv_curr_char CA cc_symbols-lect_lower
         OR lv_curr_char CA cc_symbols-digit
         OR lv_curr_char CA cc_symbols-symb
         OR lv_curr_char CA cc_symbols-lect_acc
         OR lv_curr_char EQ space.

      IF lv_new_str IS INITIAL.
        lv_new_str = lv_curr_char.
      ELSE.
        lv_new_str = |{ lv_new_str }{ lv_curr_char }|.
      ENDIF.

    ENDDO.

    yv_text = lv_new_str.


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


  METHOD get_fieldcat_from_data.

    DATA:
      lref_sap_struct TYPE REF TO data,
      lref_sap_table  TYPE REF TO data,

      lv_except_msg   TYPE string.

    FIELD-SYMBOLS:
      <sap_struct> TYPE any,
      <sap_table>  TYPE STANDARD TABLE.

    "-------------------------------------------------

    FREE yo_structdescr.
    CLEAR yt_fcat[].

    IF xs_sap_line IS SUPPLIED.
      CREATE DATA lref_sap_struct LIKE xs_sap_line.
      ASSIGN lref_sap_struct->* TO <sap_struct>.

      CREATE DATA lref_sap_table LIKE TABLE OF xs_sap_line.
      ASSIGN lref_sap_table->* TO <sap_table>.

    ELSEIF xt_sap_table IS SUPPLIED.
      CREATE DATA lref_sap_struct LIKE LINE OF xt_sap_table.
      ASSIGN lref_sap_struct->* TO <sap_struct>.

      CREATE DATA lref_sap_table LIKE xt_sap_table.
      ASSIGN lref_sap_table->* TO <sap_table>.

    ELSE.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-input_error.

    ENDIF.

    "-------------------------------------------------

    yo_structdescr ?= cl_abap_typedescr=>describe_by_data( <sap_struct> ).


    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lt_salv_table)
          CHANGING
            t_table      = <sap_table>
        ).

        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = lt_salv_table->get_columns( ) " ALV Filter
          r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
        ).

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        lv_except_msg = lx_ai_system_fault->get_text( ).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = cc_exception_msg-unable_def_struct.

      CATCH cx_salv_msg  INTO DATA(lx_salv_msg).
        lv_except_msg = lx_salv_msg->get_text( ).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = cc_exception_msg-unable_def_struct.

    ENDTRY.

  ENDMETHOD.


  METHOD f4_help_dir_input.

    DATA:
      lv_path           TYPE string VALUE IS INITIAL,

      lv_server         TYPE msxxlist-name,
      lv_path_tmp       TYPE dxfields-longpath,
      lv_instancenumber TYPE instanz-systemnr VALUE IS INITIAL,

      lt_filetable      TYPE filetable,
      lcl_ref_itab      TYPE REF TO file_table,
      lv_rc             TYPE i.

    "-------------------------------------------------

    CASE xv_source.

      WHEN cc_file_source-server.

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

      WHEN cc_file_source-local.

        lv_path = get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          EXPORTING
            initial_directory = lv_path
          CHANGING
            file_table        = lt_filetable
            rc                = lv_rc.
        ASSIGN lt_filetable[ 1 ] TO FIELD-SYMBOL(<filetable>).
        CHECK sy-subrc EQ 0.
        lv_path = <filetable>-filename.

    ENDCASE.

    yv_path_input = lv_path.

  ENDMETHOD.


  METHOD f4_help_dir_output.

    DATA:
      lv_path TYPE string VALUE IS INITIAL.

    "------------------------------------------------

    CASE xv_source.

      WHEN cc_file_source-server.

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

      WHEN cc_file_source-local.

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

    yv_path_output = lv_path.

  ENDMETHOD.


  METHOD get_desktop_directory.

    yv_desktop_dir = ''.
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = yv_desktop_dir
      EXCEPTIONS
        cntl_error        = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>update_view.

  ENDMETHOD.


  METHOD init_instance.

    FIELD-SYMBOLS:
     <sap_table> TYPE STANDARD TABLE.


    "Set Data Table
    "---------------------------------------------------------------
    CREATE DATA me->gref_sap_data LIKE xt_sap_table.
    me->gv_filename    = xv_filename.
    me->gv_header      = xv_header.
    me->go_exit_handler = xo_exit_handler.

    ASSIGN me->gref_sap_data->* TO <sap_table>.
    APPEND LINES OF xt_sap_table TO <sap_table>.



    "Set Fieldcat and Fields Descriptor
    "---------------------------------------------------------------
    TRY.
        me->get_fieldcat_from_data(
          EXPORTING
            xt_sap_table              = <sap_table>
          IMPORTING
            yo_structdescr            = me->go_structdescr
            yt_fcat                   = me->gt_fcat
        ).

        DELETE me->gt_fcat WHERE fieldname EQ c_mandt.


      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_excp_msg) = lx_ai_system_fault->get_text( ).
        RAISE unable_define_structdescr.
    ENDTRY.

  ENDMETHOD.


  METHOD file_download.

    DATA:
      lv_filetype TYPE char10 VALUE IS INITIAL.

    FIELD-SYMBOLS:
      <gt_sap_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    "Check if file is supported
    "-------------------------------------------------
    IF xv_filename CP '*.xlsx'.
      lv_filetype      = cc_filetype-xlsx.
      me->gv_separator = cc_separator-horizontal_tab.

    ELSEIF xv_filename CP '*.csv'.
      lv_filetype      = cc_filetype-csv.
      me->gv_separator = cc_separator-semicolon.

    ELSE.
      RAISE not_supported_file.

    ENDIF.

    init_instance(
      EXPORTING
        xt_sap_table              = xt_sap_table
        xv_filename               = xv_filename
        xv_header                 = xv_header
        xo_exit_handler           = xo_exit_handler
      EXCEPTIONS
        unable_define_structdescr = 1
        OTHERS                    = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structdescr.
    ENDIF.


    "Convert SAP Table data into String table Data
    "-------------------------------------------------
    me->gt_str_data = conv_tab_to_ext( ).


    "-------------------------------------------------
    "Start Download on Local / Server
    "-------------------------------------------------
    TRY.
        CASE lv_filetype.

          WHEN cc_filetype-csv.

            CASE xv_source.
              WHEN cc_file_source-local.

                download_csv_local( ).

              WHEN cc_file_source-server.

                download_csv_server( ).

            ENDCASE.

          WHEN cc_filetype-xlsx.

            CASE xv_source.
              WHEN cc_file_source-local.

                download_excel_local( ).

              WHEN cc_file_source-server.

                download_excel_server( ).

            ENDCASE.
        ENDCASE.

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_except_msg) = lx_ai_system_fault->get_text( ).
        RAISE unable_open_path.
    ENDTRY.

  ENDMETHOD.


  METHOD file_upload.

    DATA:
      lv_filetype TYPE char10 VALUE IS INITIAL.

    "-------------------------------------------------

    CLEAR: yt_sap_data[], yt_conversions_errors[].


    "Check if file is supported
    "-------------------------------------------------
    IF xv_filename CP '*.xlsx'.
      lv_filetype  = cc_filetype-xlsx.

    ELSEIF xv_filename CP '*.csv'.
      lv_filetype  = cc_filetype-csv.

    ELSE.
      RAISE not_supported_file.
    ENDIF.

    init_instance(
      EXPORTING
        xt_sap_table              = yt_sap_data
        xv_filename               = xv_filename
        xv_header                 = xv_header
        xo_exit_handler           = xo_exit_handler
      EXCEPTIONS
        unable_define_structdescr = 1
        OTHERS                    = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structdescr.
    ENDIF.


    "-------------------------------------------------
    "Start Download on Local / Serveer
    "-------------------------------------------------
    TRY.
        CASE lv_filetype.

          WHEN cc_filetype-csv.

            CASE xv_source.
              WHEN cc_file_source-local.

                me->upload_csv_local( ).

              WHEN cc_file_source-server.

                me->upload_csv_server( ).

            ENDCASE.

          WHEN cc_filetype-xlsx.

            CASE xv_source.
              WHEN cc_file_source-local.

                "Upload from XLSX with in-built conversion in SAP Format
                "-------------------------------------------------
                me->upload_excel_local( ).

              WHEN cc_file_source-server.

                RAISE input_error.

            ENDCASE.

          WHEN OTHERS.
            RAISE not_supported_file.

        ENDCASE.

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_excp_msg) = lx_ai_system_fault->get_text( ).
        RAISE unable_open_path.
    ENDTRY.



    "Conversion in SAP Format for CSV
    "-------------------------------------------------
    me->conv_tab_to_int(
      IMPORTING
        yt_sap_data           = yt_sap_data
        yt_conversions_errors = yt_conversions_errors
    ).

    IF lines( yt_sap_data ) EQ 0.
      RAISE empty_file.
    ENDIF.

  ENDMETHOD.


  METHOD get_header_from_data.

    yv_str_header = ''.

    LOOP AT me->gt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CHECK <fcat>-fieldname NE c_mandt.

      yv_str_header = COND #(
        WHEN yv_str_header EQ '' THEN <fcat>-reptext
        WHEN yv_str_header NE '' THEN |{ yv_str_header }{ me->gv_separator }{ <fcat>-reptext }|
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD conv_tab_to_ext.

    FIELD-SYMBOLS:
      <gt_sap_data> TYPE STANDARD TABLE.

    "---------------------------------------------------------------

    CLEAR yt_str_data[].

    ASSIGN me->gref_sap_data->* TO <gt_sap_data>.


    "Build header line
    "-------------------------------------------------
    IF me->gv_header EQ abap_true.

      APPEND INITIAL LINE TO yt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).
      <str_data> = me->get_header_from_data( ).

    ENDIF.


    "Convert SAP Data to String table
    "-------------------------------------------------
    LOOP AT <gt_sap_data> ASSIGNING FIELD-SYMBOL(<sap_data>).

      APPEND INITIAL LINE TO yt_str_data ASSIGNING <str_data>.
      conv_sap_to_string(
        EXPORTING
          xs_sap_data     = <sap_data>
          xt_fcat         = me->gt_fcat
          xo_structdescr  = me->go_structdescr
          xv_separator    = me->gv_separator
          xo_exit_handler = me->go_exit_handler
        IMPORTING
          y_str_data     = <str_data>
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD conv_tab_to_int.

    CLEAR: yt_sap_data[], yt_conversions_errors[].


    "Conversion in SAP Format for CSV
    "-------------------------------------------------
    LOOP AT me->gt_str_data ASSIGNING FIELD-SYMBOL(<data_str>).
      DATA(lv_tabix) = sy-tabix.

      IF me->gv_header EQ abap_true.
        CHECK lv_tabix NE 1.
      ENDIF.

      APPEND INITIAL LINE TO yt_sap_data ASSIGNING FIELD-SYMBOL(<data_sap>).
      conv_string_to_sap(
        EXPORTING
          xv_str_data           = <data_str>
          xt_fcat               = me->gt_fcat
          xo_structdescr        = me->go_structdescr
          xv_separator          = me->gv_separator
          xo_exit_handler       = me->go_exit_handler
        IMPORTING
          ys_sap_data           = <data_sap>
          ys_conversions_errors = DATA(ls_conv_error)
        EXCEPTIONS
          conversion_error      = 1
          plausibility_error    = 2
          OTHERS                = 3
      ).
      IF sy-subrc <> 0.
        ls_conv_error-row_num = lv_tabix.
        APPEND ls_conv_error TO yt_conversions_errors.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD init_managed_ranges.

    "Init range for managed abap typekind
    "---------------------------------------------------------------
    CHECK gr_typekind_numbers[] IS INITIAL
       OR gr_typekind_date[] IS INITIAL
       OR gr_typekind_time[] IS INITIAL
       OR gr_typekind_charlike[] IS INITIAL.


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


  METHOD conv_standard_exit_input.

    DATA(lv_fm_conv_exit) = |CONVERSION_EXIT_{ xv_conv_name }_INPUT|.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = CONV rs38l-name( lv_fm_conv_exit )
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.

      CASE xv_conv_name.

        WHEN 'EXCRT'
          OR 'EXCRX'.

          " Rate numbers mustn't be converted from standard flow
          " If needed, it will be use the user-exit

        WHEN OTHERS.

          CALL FUNCTION lv_fm_conv_exit
            EXPORTING
              input  = yv_tmp_data
            IMPORTING
              output = yv_tmp_data
            EXCEPTIONS
              OTHERS = 1.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD conv_standard_exit_output.

    DATA(lv_fm_conv_exit) = |CONVERSION_EXIT_{ xv_conv_name }_OUTPUT|.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = CONV rs38l-name( lv_fm_conv_exit )
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.

      CASE xv_conv_name.
        WHEN 'EXCRT'
          OR 'EXCRX'.

        WHEN OTHERS.

          CALL FUNCTION lv_fm_conv_exit
            EXPORTING
              input  = yv_tmp_data
            IMPORTING
              output = yv_tmp_data
            EXCEPTIONS
              OTHERS = 1.

      ENDCASE.

    ENDIF.


  ENDMETHOD.


  METHOD download_csv_local.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = me->gv_filename
      CHANGING
        data_tab                = me->gt_str_data[]
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


  METHOD download_csv_server.

    OPEN DATASET me->gv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      CLOSE DATASET me->gv_filename.
      RAISE unable_open_path.
    ENDIF.

    LOOP AT me->gt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).
      TRANSFER <str_data> TO me->gv_filename.
    ENDLOOP.

    CLOSE DATASET me->gv_filename.

  ENDMETHOD.


  METHOD download_excel_local.

*    TYPES: BEGIN OF ts_string,
*             string TYPE string,
*           END OF ts_string.
*
*    DATA lt_str_data TYPE TABLE OF ts_string.
*
*    get_fieldcat_from_data(
*      EXPORTING
*        xt_sap_table              = lt_str_data
*      IMPORTING
*        yt_fcat                   = DATA(lt_fcat)
*      EXCEPTIONS
*        unable_define_structdescr = 1
*        OTHERS                    = 2
*    ).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    lt_str_data = VALUE #( FOR <str> IN xt_str_data ( string = <str> ) ).
*    DATA(lref_str_data) = REF #( lt_str_data ).
*
*
*    cl_salv_bs_lex=>export_from_result_data_table(
*      EXPORTING
*        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
*        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = lref_str_data
*                                                                           t_fieldcatalog  = lt_fcat )
*      IMPORTING
*        er_result_file       = DATA(lv_xdata) ).



    "Standard Version ->
    "Conversion of fields like data, numbers exc.
    "will be performed by SAP Standard
    "-------------------------------------------------
    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = me->gref_sap_data
                                                                           t_fieldcatalog  = me->gt_fcat )
      IMPORTING
        er_result_file       = DATA(lv_xdata) ).


    DATA(lv_xlength) = xstrlen( lv_xdata ).
    DATA(lt_bin_tab) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_xdata ).

    cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize              = lv_xlength           " File length for binary files
          filename                  = me->gv_filename      " Name of file
          filetype                  = 'BIN'                " File type (ASCII, binary ...)
        CHANGING
          data_tab                  = lt_bin_tab          " Transfer table
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

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-unable_read_file.

    ENDIF.

  ENDMETHOD.


  METHOD download_excel_server.

    DATA: lt_data_ref TYPE REF TO data.

    FIELD-SYMBOLS: <t_sap_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = me->gref_sap_data
                                                                           t_fieldcatalog  = me->gt_fcat )
      IMPORTING
        er_result_file       = DATA(lv_xdata) ).

    DATA(lv_xlength) = xstrlen( lv_xdata ).

    "-------------------------------------------------

    OPEN DATASET me->gv_filename FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.

      CLOSE DATASET me->gv_filename.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-unable_read_file.

    ENDIF.

    TRANSFER lv_xdata TO me->gv_filename.

    CLOSE DATASET me->gv_filename.

  ENDMETHOD.


  METHOD upload_csv_local.

    CLEAR me->gt_str_data[].
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = me->gv_filename
        filetype = 'ASC'
      CHANGING
        data_tab = me->gt_str_data[]
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-unable_read_file.

    ENDIF.

  ENDMETHOD.


  METHOD upload_csv_server.

    DATA:
      ls_str_data TYPE string.

    "-------------------------------------------------

    OPEN DATASET me->gv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.

      CLOSE DATASET me->gv_filename.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-unable_read_file.

    ENDIF.

    DO.

      CLEAR ls_str_data.
      READ DATASET me->gv_filename INTO ls_str_data.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND ls_str_data TO me->gt_str_data.

    ENDDO.

    CLOSE DATASET me->gv_filename.

  ENDMETHOD.


  METHOD upload_excel_local.

    DATA:
      lv_except_msg    TYPE string,
      lt_bin_tab       TYPE solix_tab,
      lcl_excel_ref    TYPE REF TO cl_fdt_xl_spreadsheet,
      lcl_table_descr  TYPE REF TO cl_abap_tabledescr,
      lcl_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <t_excel_data> TYPE STANDARD TABLE,
      <str_data>     TYPE string.

    "-------------------------------------------------

    CLEAR: me->gt_str_data[].

    "Read in binary the excel
    "-------------------------------------------------
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = me->gv_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = DATA(lv_filelen)
        header     = DATA(lv_headerx)
      CHANGING
        data_tab   = lt_bin_tab[]
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-unable_read_file.

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

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = cc_exception_msg-internal_error.

    ENDIF.


    "Reading excel content using HEADERX
    "-------------------------------------------------
    TRY.
        lcl_excel_ref = NEW cl_fdt_xl_spreadsheet(
          document_name = me->gv_filename
          xdocument     = lv_headerx
        ).


      CATCH cx_fdt_excel_core INTO DATA(lx_excel_core).
        lv_except_msg = lx_excel_core->get_text( ).
    ENDTRY .

    CHECK lcl_excel_ref IS BOUND.

    lcl_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheets) ).


    "Takes the first worksheet as default
    "-------------------------------------------------
    ASSIGN lt_worksheets[ 1 ] TO FIELD-SYMBOL(<woksheetname>).
    DATA(lcl_data_ref) = lcl_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <woksheetname> ).
    ASSIGN lcl_data_ref->* TO <t_excel_data>.


    "Convert to CSV String format
    "---------------------------------------------------------------
    LOOP AT <t_excel_data> ASSIGNING FIELD-SYMBOL(<excel>).

      APPEND INITIAL LINE TO me->gt_str_data ASSIGNING <str_data>.

      LOOP AT me->go_structdescr->components ASSIGNING FIELD-SYMBOL(<comp>).

        ASSIGN COMPONENT sy-tabix OF STRUCTURE <excel> TO FIELD-SYMBOL(<excel_col>).
        CHECK sy-subrc EQ 0.
        <str_data> = COND #(
          WHEN <str_data> EQ '' THEN <excel_col>
          WHEN <str_data> NE '' THEN |{ <str_data> }{ me->gv_separator }{ <excel_col> }|
        ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS zag_cl_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_stdtxt_replace,
        varname TYPE string,
        value   TYPE string,
      END OF ts_stdtxt_replace,

      BEGIN OF ts_standard_text,
        std_txt_name TYPE thead-tdname,
        replacement  TYPE TABLE OF ts_stdtxt_replace WITH NON-UNIQUE KEY varname,
      END OF ts_standard_text.

    TYPES:
      tt_standard_text TYPE TABLE OF ts_standard_text WITH DEFAULT KEY,
      tt_hrrange       TYPE TABLE OF hrrange WITH DEFAULT KEY.


    "Methods
    "-------------------------------------------------
    CLASS-METHODS:
      alm_buffer_refresh,

      obj_status_change_extern
        IMPORTING
                  !xv_objnr       TYPE j_objnr
                  !xv_new_status  TYPE j_txt04
        RETURNING VALUE(yv_subrc) TYPE sy-subrc,

      f4_help
        IMPORTING
                  !xv_tabname     TYPE string
                  !xv_fieldname   TYPE string
        RETURNING VALUE(yv_value) TYPE string,

      format_message
        IMPORTING
                  !xv_msgid     TYPE sy-msgid OPTIONAL
                  !xv_msgno     TYPE sy-msgno OPTIONAL
                  !xv_msgv1     TYPE sy-msgv1 OPTIONAL
                  !xv_msgv2     TYPE sy-msgv2 OPTIONAL
                  !xv_msgv3     TYPE sy-msgv3 OPTIONAL
                  !xv_msgv4     TYPE sy-msgv4 OPTIONAL
        RETURNING VALUE(yv_msg) TYPE string ,

      get_stdtxt
        IMPORTING
                  !xs_std_text    TYPE ts_standard_text
        RETURNING VALUE(yt_lines) TYPE bcsy_text
        RAISING   cx_ai_system_fault,

      get_value_from_set
        IMPORTING
          !xv_setname   TYPE string
        CHANGING
          yr_set_values TYPE any,

      exe_unix_comm
        IMPORTING
          !x_unixcom TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zag_cl_utils IMPLEMENTATION.


  METHOD alm_buffer_refresh.

    CALL FUNCTION 'ISU_ORDER_RESET_CREATE_STATUS'.

    CALL FUNCTION 'IM_SM_DATA_RESET'.

    CALL FUNCTION 'CO_ZF_DATA_RESET_COMPLETE'.

    CALL FUNCTION 'IBAPI_Z_SET_BAPI_FLAG'
      EXPORTING
        iv_flag = space.

  ENDMETHOD.


  METHOD obj_status_change_extern.

    DATA: lv_stsma TYPE jsto-stsma.


    yv_subrc = 4.

    "Get Status Schema
    "-------------------------------------------------
    CLEAR lv_stsma.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        client           = sy-mandt
        flg_user_stat    = 'X'
        objnr            = xv_objnr
        only_active      = 'X'
        spras            = sy-langu
      IMPORTING
        e_stsma          = lv_stsma
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    CHECK sy-subrc EQ 0.


    "Get Status CODE
    "-------------------------------------------------
    SELECT SINGLE estat
      FROM tj30t
      INTO @DATA(lv_user_status)
      WHERE stsma EQ @lv_stsma
        AND spras EQ @sy-langu
        AND txt04 EQ @xv_new_status.
    CHECK sy-subrc EQ 0.


    "Change Status
    "-------------------------------------------------
    CALL FUNCTION 'STATUS_CHANGE_EXTERN'
      EXPORTING
        client              = sy-mandt
        objnr               = xv_objnr
        user_status         = lv_user_status
      EXCEPTIONS
        object_not_found    = 1
        status_inconsistent = 2
        status_not_allowed  = 3
        OTHERS              = 4.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.


    yv_subrc = 0.

  ENDMETHOD.



  METHOD f4_help.

    DATA:
      lv_retfield    TYPE dfies-fieldname,
      lt_return_tab  TYPE TABLE OF ddshretval,
      lt_datatab_ref TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_datatab> TYPE STANDARD TABLE.



    "Get DB Data
    "-------------------------------------------------
    CREATE DATA lt_datatab_ref TYPE TABLE OF (xv_tabname).
    ASSIGN lt_datatab_ref->* TO <lt_datatab>.

    SELECT *
      FROM (xv_tabname)
      INTO TABLE @<lt_datatab>.


    "Show POPUP with DB Values
    "-------------------------------------------------
    lv_retfield = xv_fieldname.

    CLEAR: lt_return_tab[].
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = lv_retfield
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        return_tab      = lt_return_tab
        value_tab       = <lt_datatab>
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    "Export selected value
    "-------------------------------------------------
    ASSIGN lt_return_tab[ 1 ] TO FIELD-SYMBOL(<return>).
    CHECK sy-subrc EQ 0.

    yv_value = <return>-fieldval.


    "Use only when you call this code into a program in routine
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '=ENT'
*      EXCEPTIONS
*        function_not_supported = 1
*        OTHERS                 = 2.


  ENDMETHOD.



  METHOD format_message.

    DATA:
      lv_msgid TYPE sy-msgid VALUE 'DB',
      lv_msgno TYPE sy-msgno VALUE '646',
      lv_msgv1 TYPE sy-msgv1,
      lv_msgv2 TYPE sy-msgv2,
      lv_msgv3 TYPE sy-msgv3,
      lv_msgv4 TYPE sy-msgv4.


    IF xv_msgid IS NOT INITIAL.

      lv_msgid = xv_msgid.
      lv_msgno = xv_msgno.
      lv_msgv1 = xv_msgv1.
      lv_msgv1 = xv_msgv2.
      lv_msgv1 = xv_msgv3.
      lv_msgv1 = xv_msgv4.

    ELSE.

      lv_msgid = sy-msgid.
      lv_msgno = sy-msgno.
      lv_msgv1 = sy-msgv1.
      lv_msgv1 = sy-msgv2.
      lv_msgv1 = sy-msgv3.
      lv_msgv1 = sy-msgv4.

    ENDIF.

    CLEAR yv_msg.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = lv_msgid
        lang      = '-D'
        no        = lv_msgno
        v1        = lv_msgv1
        v2        = lv_msgv2
        v3        = lv_msgv3
        v4        = lv_msgv4
      IMPORTING
        msg       = yv_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

  ENDMETHOD.


  METHOD get_stdtxt.

    DATA:
      lt_lines     TYPE TABLE OF tline,
      lv_error_msg TYPE string.


    CLEAR: lt_lines[].
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = xs_std_text-std_txt_name
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.

      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id   = sy-msgid
          lang = '-D'
          no   = sy-msgno
          v1   = sy-msgv1
          v2   = sy-msgv2
          v3   = sy-msgv3
          v4   = sy-msgv4
        IMPORTING
          msg  = lv_error_msg.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = lv_error_msg.

    ENDIF.


    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lines>).

      LOOP AT xs_std_text-replacement ASSIGNING FIELD-SYMBOL(<replace>).
        REPLACE ALL OCCURRENCES OF <replace>-varname IN <lines>-tdline WITH <replace>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_value_from_set.

    DATA:
      lv_setid   TYPE sethier-setid,
      lt_values  TYPE STANDARD TABLE OF rgsbv,
      lv_setname TYPE c LENGTH 24.


*    CLEAR yt_set_values[].
*
*    lv_setname = xv_setname.
*    CONDENSE lv_setname NO-GAPS.
*
*    CLEAR lv_setid.
*    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
*      EXPORTING
*        shortname = lv_setname       "Set Name
*      IMPORTING
*        new_setid = lv_setid
*      EXCEPTIONS
*        OTHERS    = 1.
*
*    IF sy-subrc EQ 0.
*
*      REFRESH lt_values.
*      CALL FUNCTION 'G_SET_FETCH'
*        EXPORTING
*          setnr           = lv_setid
*        TABLES
*          set_lines_basic = lt_values
*        EXCEPTIONS
*          OTHERS          = 1.
*
*      CHECK lt_values[] IS NOT INITIAL.
*
*      yt_set_values = VALUE #( FOR <value> IN lt_values (
*                                sign = 'I'          opti = 'EQ'
*                                low  = <value>-from high = <value>-to ) ).
*
*    ENDIF.

  ENDMETHOD.



  METHOD exe_unix_comm.

    TYPES: BEGIN OF ty_result,
             line(400),
           END OF ty_result.

    DATA: result  TYPE TABLE OF ty_result,
          unixcom TYPE rlgrap-filename.
    DATA: lines TYPE i.

    "Create directory
    "unixcom = 'mkdir /tmp/zag_test/'.

    "Show Calendar
    "unixcom = 'cal'.

    "Show file list
    "unixcom = 'ls'.

    unixcom = x_unixcom.

    REFRESH result[].
    CALL 'SYSTEM' ID 'COMMAND' FIELD unixcom
                  ID 'TAB'     FIELD result[].


    IF lines( result ) EQ 0.
      DATA(msg) = 'NO Occurances were found'.

    ELSE.



    ENDIF.

  ENDMETHOD.

ENDCLASS.
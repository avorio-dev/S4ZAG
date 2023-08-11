class ZAG_CL_UTILS definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_stdtxt_subs,
        varname TYPE string,
        value   TYPE string,
      END OF ty_stdtxt_subs .
  types:
    tt_hrrange    TYPE TABLE OF hrrange .
  types:
    tt_stdtxt_subs TYPE TABLE OF ty_stdtxt_subs .

  constants C_LOCAL type CHAR4 value 'LOCL' ##NO_TEXT.
  constants C_SERVER type CHAR4 value 'SERV' ##NO_TEXT.
  constants C_ATTCH_CSV type SOODK-OBJTP value 'CSV' ##NO_TEXT.
  constants C_ATTCH_RAW type SOODK-OBJTP value 'RAW' ##NO_TEXT.
  constants C_ATTCH_PDF type SOODK-OBJTP value 'PDF' ##NO_TEXT.

  class-methods ALM_BUFFER_REFRESH .
  class-methods ALM_STATUS_CHANGE_EXTERN
    importing
      !X_AUFNR type AUFNR
      !X_NEW_STATUS type J_TXT04
    exporting
      !Y_OK type BOOLEAN .
  class-methods F4_HELP
    importing
      !X_FIELDNAME type STRING
      !X_TABNAME type STRING
    exporting
      !Y_VALUE type STRING .
  class-methods F4_HELP_DIR_INPUT
    importing
      !X_SOURCE type CHAR4
    exporting
      !Y_PATH_INPUT type STRING .
  class-methods F4_HELP_DIR_OUTPUT
    importing
      !X_SOURCE type CHAR4
    exporting
      !Y_PATH_OUTPUT type STRING .
  class-methods FORMAT_CUST_MESSAGE
    importing
      !X_ID type SY-MSGID
      !X_NUMBER type SY-MSGNO
      !X_MSGV1 type SY-MSGV1
      !X_MSGV2 type SY-MSGV2
      !X_MSGV3 type SY-MSGV3
      !X_MSGV4 type SY-MSGV4
    exporting
      !Y_MSG type STRING .
  class-methods FORMAT_SYST_MESSAGE
    exporting
      value(Y_MSG) type STRING .
  class-methods GET_DESKTOP_DIRECTORY
    returning
      value(Y_DESKTOP_DIR) type STRING .
  class-methods GET_FIELDCAT_FROM_ITAB
    importing
      !XT_ITAB type STANDARD TABLE
    exporting
      !YT_FCAT type LVC_T_FCAT .
  class-methods GET_NEXT_NUMBER
    importing
      !XS_INRI type INRI
    exporting
      !Y_NEXT_NUMBER type STRING
      !Y_MSG type STRING .
  class-methods GET_STDTXT
    importing
      !X_STDTXT_NAME type THEAD-TDNAME optional
      !XT_STDTXT_SUBS type TT_STDTXT_SUBS optional
    exporting
      !YT_LINES type BCSY_TEXT .
  class-methods GET_VALUE_FROM_SET
    importing
      !X_SETNAME type STRING
    exporting
      !YT_RANGE_VALUES type TT_HRRANGE .
  class-methods MKDIR .
  class-methods REMOVE_SPECIAL_CHAR
    changing
      !Y_TEXT type TEXT255 .
  class-methods REST_GENERATE_TOKEN_OAUTH2
    importing
      !X_CLIENT_OPENID type STRING
      !X_CLIENT_SECRET type STRING
      !X_USER type STRING
      !X_PASSWORD type STRING
      !X_URI type STRING
    exporting
      !Y_TOKEN type STRING
      !Y_CODE type I
      !Y_REASON type STRING .
  class-methods REST_GENERATE_TOKEN_SAS
    importing
      !X_POST_URL type STRING
      !X_KEY_NAME type STRING
      !X_SHARED_KEY type STRING
      !X_URI type STRING
    returning
      value(Y_SAS_TOKEN) type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZAG_CL_UTILS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>ALM_BUFFER_REFRESH
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alm_buffer_refresh.

    CALL FUNCTION 'ISU_ORDER_RESET_CREATE_STATUS'.

    CALL FUNCTION 'IM_SM_DATA_RESET'.

    CALL FUNCTION 'CO_ZF_DATA_RESET_COMPLETE'.

    CALL FUNCTION 'IBAPI_Z_SET_BAPI_FLAG'
      EXPORTING
        iv_flag = space.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>ALM_STATUS_CHANGE_EXTERN
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_AUFNR                        TYPE        AUFNR
* | [--->] X_NEW_STATUS                   TYPE        J_TXT04
* | [<---] Y_OK                           TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alm_status_change_extern.

    DATA: lv_stsma       TYPE jsto-stsma.

    "-------------------------------------------------

    y_ok = abap_false.

    SELECT SINGLE objnr
      FROM aufk
      INTO @DATA(lv_objnr)
      WHERE aufnr EQ @x_aufnr.

    CHECK sy-subrc EQ 0.

    CLEAR lv_stsma.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        client           = sy-mandt
        flg_user_stat    = 'X'
        objnr            = lv_objnr
        only_active      = 'X'
        spras            = sy-langu
      IMPORTING
        e_stsma          = lv_stsma
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE estat
      FROM tj30t
      INTO @DATA(lv_user_status)
      WHERE stsma EQ @lv_stsma
        AND spras EQ @sy-langu
        AND txt04 EQ @x_new_status.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'STATUS_CHANGE_EXTERN'
      EXPORTING
        client              = sy-mandt
        objnr               = lv_objnr
        user_status         = lv_user_status
      EXCEPTIONS
        object_not_found    = 1
        status_inconsistent = 2
        status_not_allowed  = 3
        OTHERS              = 4.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    y_ok = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>F4_HELP
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FIELDNAME                    TYPE        STRING
* | [--->] X_TABNAME                      TYPE        STRING
* | [<---] Y_VALUE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD f4_help.

    DATA: lv_retfield    TYPE dfies-fieldname,
          lt_return_tab  TYPE TABLE OF ddshretval,
          lt_datatab_ref TYPE REF TO data.

    FIELD-SYMBOLS: <lt_datatab> TYPE STANDARD TABLE.

    "-------------------------------------------------

    CREATE DATA lt_datatab_ref TYPE TABLE OF (x_tabname).
    ASSIGN lt_datatab_ref->* TO <lt_datatab>.

    SELECT *
      FROM (x_tabname)
      INTO TABLE @<lt_datatab>.

    lv_retfield = x_fieldname.

    REFRESH: lt_return_tab[].
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = lv_retfield
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        return_tab      = lt_return_tab
        value_tab       = <lt_datatab> "TEMPLATE da definire
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_return_tab ASSIGNING FIELD-SYMBOL(<return>) INDEX 1.
    IF sy-subrc EQ 0.
      y_value = <return>-fieldval.
    ENDIF.

    "Use only when you call this code into a program in routine
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '=ENT'
*      EXCEPTIONS
*        function_not_supported = 1
*        OTHERS                 = 2.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>F4_HELP_DIR_INPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SOURCE                       TYPE        CHAR4
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

      WHEN c_server.

        lv_instancenumber = ''.
        CALL FUNCTION 'GET_SYSTEM_NUMBER'
          IMPORTING
            instancenumber = lv_instancenumber.

        CONCATENATE sy-host '_' sy-sysid '_' lv_instancenumber INTO lv_server.
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

      WHEN c_local.

        lv_path = zag_cl_utils=>get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          EXPORTING
*           default_filename  = '*.csv'
            initial_directory = lv_path
          CHANGING
            file_table        = lt_filetable
            rc                = lv_rc.
        READ TABLE lt_filetable ASSIGNING FIELD-SYMBOL(<lcl_ref_itab>) INDEX 1.
        CHECK sy-subrc EQ 0.
        lv_path = <lcl_ref_itab>-filename.


    ENDCASE.

    y_path_input = lv_path.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>F4_HELP_DIR_OUTPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SOURCE                       TYPE        CHAR4
* | [<---] Y_PATH_OUTPUT                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD f4_help_dir_output.

    DATA: lv_path TYPE string VALUE IS INITIAL.

    "-------------------------------------------------

    CASE x_source.

      WHEN c_server.

        CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
          EXPORTING
*           directory        = ''
           filemask          = '.dummysap'
          IMPORTING
           serverfile        = lv_path
          EXCEPTIONS
           canceled_by_user  = 1
           OTHERS            = 2.
        CHECK sy-subrc EQ 0.

      WHEN c_local.

        lv_path = zag_cl_utils=>get_desktop_directory( ).

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
* | Static Public Method ZAG_CL_UTILS=>FORMAT_CUST_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_ID                           TYPE        SY-MSGID
* | [--->] X_NUMBER                       TYPE        SY-MSGNO
* | [--->] X_MSGV1                        TYPE        SY-MSGV1
* | [--->] X_MSGV2                        TYPE        SY-MSGV2
* | [--->] X_MSGV3                        TYPE        SY-MSGV3
* | [--->] X_MSGV4                        TYPE        SY-MSGV4
* | [<---] Y_MSG                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_cust_message.

*--> Declaration
    "-------------------------------------------------
*    DATA: lv_msg TYPE string.
*    CONSTANTS: c_my_msg TYPE bapiret2-id VALUE 'ZMY_MSG'.
    "-------------------------------------------------

    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = x_id
        lang      = '-D'
        no        = x_number
        v1        = x_msgv1
        v2        = x_msgv2
        v3        = x_msgv3
        v4        = x_msgv4
      IMPORTING
        msg       = y_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>FORMAT_SYST_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [<---] Y_MSG                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_syst_message.

*--> Declaration
    "-------------------------------------------------
*    DATA: lv_msg TYPE string.
    "-------------------------------------------------

    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = sy-msgid
        lang      = '-D'
        no        = sy-msgno
        v1        = sy-msgv1
        v2        = sy-msgv2
        v3        = sy-msgv3
        v4        = sy-msgv4
      IMPORTING
        msg       = y_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>GET_DESKTOP_DIRECTORY
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
* | Static Public Method ZAG_CL_UTILS=>GET_FIELDCAT_FROM_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ITAB                        TYPE        STANDARD TABLE
* | [<---] YT_FCAT                        TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_fieldcat_from_itab.

    DATA: table TYPE REF TO data.

    "-------------------------------------------------

    REFRESH yt_fcat.

    CREATE DATA table LIKE xt_itab.
    ASSIGN table->* TO FIELD-SYMBOL(<table>).
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(lt_salv_table)
                                CHANGING
                                  t_table        = <table>  ).
        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lt_salv_table->get_columns( ) " ALV Filter
            r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
    ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>GET_NEXT_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] XS_INRI                        TYPE        INRI
* | [<---] Y_NEXT_NUMBER                  TYPE        STRING
* | [<---] Y_MSG                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_next_number.

*--> Declaration
    "-------------------------------------------------
*    DATA: ls_inri        TYPE inri,
*          lv_next_number TYPE string,
*          lv_msg         TYPE string.
*  CONSTANTS: c_range_name TYPE nrobj VALUE 'ZRANGE_NAME'.
*  ls_inri-object    = c_range_name.
*  ls_inri-nrrangenr = '01'.
*  ls_inri-toyear    = sy-datum(4).
*  ls_inri-quantity  = '1'.
    "-------------------------------------------------

    CLEAR: y_next_number, y_msg.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = xs_inri-nrrangenr
        object                  = xs_inri-object
        quantity                = xs_inri-quantity
        toyear                  = xs_inri-toyear
      IMPORTING
        number                  = y_next_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.

      zag_cl_utils=>format_syst_message( IMPORTING y_msg = y_msg ).

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>GET_STDTXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STDTXT_NAME                  TYPE        THEAD-TDNAME(optional)
* | [--->] XT_STDTXT_SUBS                 TYPE        TT_STDTXT_SUBS(optional)
* | [<---] YT_LINES                       TYPE        BCSY_TEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_stdtxt.

    DATA: lt_lines         TYPE TABLE OF tline.

    "-------------------------------------------------

    CLEAR yt_lines[].
    CHECK x_stdtxt_name IS NOT INITIAL.

    CLEAR: lt_lines[].
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = x_stdtxt_name
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

    CHECK sy-subrc EQ 0.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lines>).

      LOOP AT xt_stdtxt_subs ASSIGNING FIELD-SYMBOL(<subs>).
        REPLACE ALL OCCURRENCES OF <subs>-varname IN <lines>-tdline WITH <subs>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>GET_VALUE_FROM_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SETNAME                      TYPE        STRING
* | [<---] YT_RANGE_VALUES                TYPE        TT_HRRANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_value_from_set.

*--> Declaration
    "-------------------------------------------------
*  TYPES: tt_hrrange TYPE TABLE OF hrrange.
*  DATA: lr_my_range TYPE tt_hrrange.
*  CONSTANTS: c_my_set_name TYPE string VALUE 'ZMY_SET_NAME'.
    "-------------------------------------------------

    DATA: lv_setid   TYPE sethier-setid,
          lt_values  TYPE STANDARD TABLE OF rgsbv,
          lv_setname TYPE c LENGTH 24.

    REFRESH yt_range_values[].

    lv_setname = x_setname.
    CONDENSE lv_setname NO-GAPS.

    CLEAR lv_setid.
    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname = lv_setname       "Set Name
      IMPORTING
        new_setid = lv_setid
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc EQ 0.

      REFRESH lt_values.
      CALL FUNCTION 'G_SET_FETCH'
        EXPORTING
          setnr           = lv_setid
        TABLES
          set_lines_basic = lt_values
        EXCEPTIONS
          OTHERS          = 1.

      CHECK lt_values[] IS NOT INITIAL.

      yt_range_values = VALUE #( FOR <value> IN lt_values (
                                sign = 'I'          opti = 'EQ'
                                low  = <value>-from high = <value>-to ) ).

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>MKDIR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mkdir.

    TYPES: BEGIN OF ty_result,
             line(400),
           END OF ty_result.

    DATA: result  TYPE TABLE OF ty_result,
          unixcom TYPE rlgrap-filename.
    DATA: lines TYPE i.


    unixcom = 'mkdir /tmp/zag_test/'.


    REFRESH result[].
    CALL 'SYSTEM' ID 'COMMAND' FIELD unixcom
                  ID 'TAB'     FIELD result[].


    lines = lines( result ).
    IF lines = 0.
      DATA(msg) = 'NO Occurances were found'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>REMOVE_SPECIAL_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] Y_TEXT                         TYPE        TEXT255
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_special_char.

    CONSTANTS: c_regex_lect_upper TYPE c LENGTH 255 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
               c_regex_lect_lower TYPE c LENGTH 255 VALUE 'abcdefghijklmnopqrstuvwxyz',
               c_regex_digit      TYPE c LENGTH 255 VALUE '0123456789',
               c_regex_symb       TYPE c LENGTH 255 VALUE '!"%/=?;,.:-_@&+*()[]{}<>',
               c_regex_lect_acc   TYPE c LENGTH 255 VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'.

    DATA(lv_length) = strlen( y_text ).

    DO lv_length TIMES.
      DATA(lv_index) = sy-index - 1.

      IF   y_text+lv_index(1) CA c_regex_lect_upper
        OR y_text+lv_index(1) CA c_regex_lect_lower
        OR y_text+lv_index(1) CA c_regex_digit
        OR y_text+lv_index(1) CA c_regex_symb
        OR y_text+lv_index(1) CA c_regex_lect_acc  .

        CONTINUE.

      ELSE.

        y_text+lv_index(1) = ''.

      ENDIF.

    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>REST_GENERATE_TOKEN_OAUTH2
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_CLIENT_OPENID                TYPE        STRING
* | [--->] X_CLIENT_SECRET                TYPE        STRING
* | [--->] X_USER                         TYPE        STRING
* | [--->] X_PASSWORD                     TYPE        STRING
* | [--->] X_URI                          TYPE        STRING
* | [<---] Y_TOKEN                        TYPE        STRING
* | [<---] Y_CODE                         TYPE        I
* | [<---] Y_REASON                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD rest_generate_token_oauth2.

    TYPES: BEGIN OF ty_oauth2_token_resp,
             access_token       TYPE string,
             expires_in         TYPE i,
             refresh_expires_in TYPE i,
             refresh_token      TYPE string,
             token_type         TYPE string,
             not_before_policy  TYPE i,
             session_state      TYPE string,
             scope              TYPE string,
           END OF ty_oauth2_token_resp.

    DATA: lv_post_url      TYPE string,
          lv_auth          TYPE string.

    "-------------------------------------------------

    lv_post_url = x_uri.

    "HTTP Client Abstraction
    DATA  lo_client TYPE REF TO if_http_client.

    "Data variables for storing response in xstring and string
    DATA  : lv_xstring   TYPE xstring,
            lv_string    TYPE string,
            lv_node_name TYPE string.

    CLEAR : lv_xstring, lv_string, lv_node_name.

    "Creation of New IF_HTTP_Client Object
    cl_http_client=>create_by_url(
    EXPORTING
      url                = lv_post_url
      ssl_id             = 'ANONYM'
    IMPORTING
      client             = lo_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      ).
    IF sy-subrc IS NOT INITIAL.
      "Handle errors
    ENDIF.

    lo_client->propertytype_logon_popup = lo_client->co_disabled.
    lo_client->request->set_method( if_http_entity=>co_request_method_post ).
    lo_client->request->set_content_type( if_rest_media_type=>gc_appl_www_form_url_encoded ).




    DATA lv_strdata TYPE string.
    lv_strdata = |{ lv_strdata }client_id={ x_client_openid }|.
    lv_strdata = |{ lv_strdata }&client_secret={ x_client_secret }|.
    lv_strdata = |{ lv_strdata }&username={ x_user }|.
    lv_strdata = |{ lv_strdata }&password={ x_password }|.
    lv_strdata = |{ lv_strdata }&grant_type=password|.

    DATA(lv_strlen) = strlen( lv_strdata ).

    lo_client->request->set_cdata( data   = lv_strdata " Required
                                   length = lv_strlen " Optional
                                   offset = 0 ). " Optional


    "-------------------------------------------------

    "Structure of HTTP Connection and Dispatch of Data
    lo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.

    ENDIF.


    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.

    ENDIF.


    DATA lt_header TYPE tihttpnvp.

    lo_client->response->get_header_fields(
    CHANGING
        fields = lt_header
        ).

    DATA: ls_oauth2_token_resp TYPE ty_oauth2_token_resp.
    DATA(lv_json_response) = lo_client->response->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_json_response
      CHANGING
        data        = ls_oauth2_token_resp
    ).

    y_token = ls_oauth2_token_resp-access_token.

    lo_client->response->get_status(
    IMPORTING
        code   = y_code
        reason = y_reason
        ).

    lo_client->close(
    EXCEPTIONS
        http_invalid_state = 1
    OTHERS             = 2
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_UTILS=>REST_GENERATE_TOKEN_SAS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_POST_URL                     TYPE        STRING
* | [--->] X_KEY_NAME                     TYPE        STRING
* | [--->] X_SHARED_KEY                   TYPE        STRING
* | [--->] X_URI                          TYPE        STRING
* | [<-()] Y_SAS_TOKEN                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD REST_GENERATE_TOKEN_SAS.

    DATA : lv_current_timestamp TYPE timestampl,
           lv_zone              TYPE sy-zonlo,
           lv_date              TYPE datum,
           lv_time              TYPE uzeit,

           lv_seconds           TYPE p,
           lv_week_in_sec       TYPE p,
           lv_expiry_time       TYPE string.

    DATA : lv_format           TYPE i,
           lv_utf8_uri         TYPE string,
           lv_data_to_sign_str TYPE string,
           lv_data_to_sign_bin TYPE xstring,

           lv_shared_key       TYPE string,
           lv_shared_key_bin   TYPE xstring,

           lv_signature        TYPE string,
           lv_sas_token        TYPE string,
           lo_conv             TYPE REF TO cl_abap_conv_out_ce.

    "-------------------------------------------------

    lv_format     = 18.
    lv_shared_key = x_shared_key.



    "Calculate Expiry Time
    "-------------------------------------------------

    "Get the current timestamp
    GET TIME STAMP FIELD lv_current_timestamp.

    "Get the time difference
    CONVERT TIME STAMP lv_current_timestamp
      TIME ZONE lv_zone INTO
        DATE lv_date
        TIME lv_time.

    CALL METHOD cl_abap_tstmp=>td_subtract
      EXPORTING
        date1    = lv_date
        time1    = lv_time
        date2    = '19700101'
        time2    = '000000'
      IMPORTING
        res_secs = lv_seconds.

    "Add expiry time in seconds
    lv_week_in_sec = 60 * 60 * 24 * 7.
    lv_seconds     = lv_seconds + lv_week_in_sec.
    lv_expiry_time = lv_seconds.
    CONDENSE lv_expiry_time.


    "Calculate Encrypted Key
    "-------------------------------------------------

    "Build string to Sign
    lv_utf8_uri = escape( val = x_uri   format = lv_format ).
    CONCATENATE lv_utf8_uri
                cl_abap_char_utilities=>newline
                lv_expiry_time
                INTO lv_data_to_sign_str.

    "Build Binary data to Sign
    lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert( EXPORTING
                     data   = lv_data_to_sign_str
                   IMPORTING
                     buffer = lv_data_to_sign_bin ).

    "Encoding Shared Key
    lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert( EXPORTING
                     data   = lv_shared_key
                   IMPORTING
                     buffer = lv_shared_key_bin ).


    CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
      EXPORTING
        if_algorithm     = 'sha-256'
        if_key           = lv_shared_key_bin
        if_data          = lv_data_to_sign_bin
        if_length        = 0
      IMPORTING
        ef_hmacb64string = lv_signature.


    lv_signature = escape( val = lv_signature format = lv_format ).
    lv_sas_token = |SharedAccessSignature sr={ lv_utf8_uri }&sig={ lv_signature }&se={ lv_expiry_time }&skn={ x_key_name }|.
    y_sas_token  = lv_sas_token.


  ENDMETHOD.
ENDCLASS.

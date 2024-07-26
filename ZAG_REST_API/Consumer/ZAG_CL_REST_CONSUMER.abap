CLASS zag_cl_rest_consumer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_token_sas_params,
        uri        TYPE string,
        url        TYPE string,
        key_name   TYPE string,
        shared_key TYPE string,
      END OF ts_token_sas_params,

      BEGIN OF ts_token_oauth2_params,
        client_openid TYPE string,
        client_secret TYPE string,
        user          TYPE string,
        password      TYPE string,
        uri           TYPE string,
      END OF ts_token_oauth2_params,

      BEGIN OF ts_rest_response,
        http_code   TYPE i,
        reason      TYPE string,
        resp_header TYPE tihttpnvp,
        resp_body   TYPE string,
      END OF ts_rest_response.


    "Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF tc_request_method,
        get  TYPE string VALUE 'GET' ##NO_TEXT,
        post TYPE string VALUE 'POST' ##NO_TEXT,
      END OF tc_request_method.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      call_rest_api
        IMPORTING
                  !xv_url                 TYPE string
                  !xv_method              TYPE string DEFAULT tc_request_method-get
        RETURNING VALUE(ys_rest_response) TYPE ts_rest_response
        RAISING   cx_ai_system_fault,

      generate_token_sas
        IMPORTING
                  !xs_token_sas_params TYPE ts_token_sas_params
        RETURNING VALUE(yv_token_sas)  TYPE string,

      generate_token_oauth2
        IMPORTING
                  !xs_token_oauth2_params TYPE ts_token_oauth2_params
        RETURNING VALUE(yv_token_oauth2)  TYPE string
        RAISING   cx_ai_system_fault.


  PROTECTED SECTION.

    "Methods
    "-------------------------------------------------
    CLASS-METHODS:
      set_authentication
        CHANGING
                 !yo_client TYPE REF TO if_http_client
        RAISING  cx_ai_system_fault,

      set_header_request
        CHANGING
          !yo_client TYPE REF TO if_http_client,

      set_body_request
        CHANGING
          !yo_client TYPE REF TO if_http_client.


  PRIVATE SECTION.

    "Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_token_oauth2_resp,
        access_token       TYPE string,
        expires_in         TYPE i,
        refresh_expires_in TYPE i,
        refresh_token      TYPE string,
        token_type         TYPE string,
        not_before_policy  TYPE i,
        session_state      TYPE string,
        scope              TYPE string,
      END OF ts_token_oauth2_resp.


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      c_http_499          TYPE i      VALUE 499 ##NO_TEXT,
      c_content_type_json TYPE string VALUE 'application/json; charset=utf-8' ##NO_TEXT,
      c_content_type_xml  TYPE string VALUE 'application/xml; charset=utf-8' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF tc_exception_msg,
        unable_determine_http_obj TYPE string VALUE 'Unable to determine HTTP Client Object' ##NO_TEXT,
        missing_auth_params       TYPE string VALUE 'Missing Authentication Params'       ##NO_TEXT,
        unable_determine_token    TYPE string VALUE 'Unable to determine Access Token'    ##NO_TEXT,
        unable_send_request       TYPE string VALUE 'Unable to send Request'              ##NO_TEXT,
        unable_receive_response   TYPE string VALUE 'Unable to receive Response'          ##NO_TEXT,
        unable_close_connection   TYPE string VALUE 'Unable to Close Connection'          ##NO_TEXT,
      END OF tc_exception_msg.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      set_http_client
        IMPORTING
                  !xv_url          TYPE string
                  !xv_method       TYPE string DEFAULT tc_request_method-get
        RETURNING VALUE(yo_client) TYPE REF TO if_http_client
        RAISING   cx_ai_system_fault,

      send_request
        IMPORTING
                  !xo_client TYPE REF TO if_http_client
        RAISING   cx_ai_system_fault,

      get_response
        IMPORTING
                  !xo_client              TYPE REF TO if_http_client
        RETURNING VALUE(ys_rest_response) TYPE ts_rest_response
        RAISING   cx_ai_system_fault,

      close_connection
        CHANGING
                 !yo_client TYPE REF TO if_http_client
        RAISING  cx_ai_system_fault,

      format_syst_message
        RETURNING VALUE(y_msg) TYPE string.


ENDCLASS.



CLASS zag_cl_rest_consumer IMPLEMENTATION.

  METHOD call_rest_api.


    TRY.
        "Create and Config HTTP Client
        "-------------------------------------------------
        DATA(lo_client) = set_http_client( xv_url ).


        "Set Authentication data
        "-------------------------------------------------
        set_authentication(
          CHANGING
            yo_client = lo_client
        ).


        "Set Request Header data
        "-------------------------------------------------
        set_header_request(
          CHANGING
            yo_client = lo_client
        ).


        "Set Request Body data
        "-------------------------------------------------
        set_body_request(
          CHANGING
            yo_client = lo_client
        ).


        "Call HTTP Client
        "-------------------------------------------------
        send_request( lo_client ).


        "Get Response data
        "-------------------------------------------------
        ys_rest_response = get_response( lo_client ).


        "Close client connection
        "-------------------------------------------------
        close_connection(
          CHANGING
            yo_client = lo_client
        ).


      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        RAISE EXCEPTION lx_ai_system_fault.

    ENDTRY.


  ENDMETHOD.


  METHOD set_http_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = xv_url
      IMPORTING
        client             = yo_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_determine_http_obj.
    ENDIF.

    yo_client->propertytype_logon_popup = yo_client->co_disabled.
    yo_client->request->set_method( xv_method ).
    yo_client->request->set_content_type( c_content_type_json ).


  ENDMETHOD.


  METHOD set_authentication.

    TRY.

        "Authentication with Username/Password
        "-------------------------------------------------
        IF 1 = 2.

          DATA:
            lv_username TYPE string,
            lv_password TYPE string.

          yo_client->request->set_header_field(
            name  = 'Username'
            value = lv_username
          ).

          yo_client->request->set_header_field(
            name  = 'Password'
            value = lv_password
          ).

        ENDIF.


        " Authentication with SAS Token
        "-------------------------------------------------
        IF 1 = 2.

          DATA: ls_token_sas_param TYPE ts_token_sas_params.

          DATA(lv_token_sas) = generate_token_sas( ls_token_sas_param ).

          yo_client->request->set_header_field(
              name  = 'Authorization'
              value = lv_token_sas
          ).

        ENDIF.


        " Authentication with OAUTH2 Token
        "-------------------------------------------------
        IF 1 = 2.

          DATA: ls_token_oauth2_param TYPE ts_token_oauth2_params.

          DATA(lv_token_oauth2) = generate_token_oauth2( ls_token_oauth2_param ).

          yo_client->request->set_header_field(
            name  = 'Authorization'
            value = lv_token_oauth2
          ).

        ENDIF.


      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_xmsg) = lx_ai_system_fault->get_text( ).
        RAISE EXCEPTION lx_ai_system_fault.

    ENDTRY.


  ENDMETHOD.


  METHOD set_header_request.

    CHECK 1 = 2.

    yo_client->request->set_header_field(
      name  = 'x-entity'
      value = 'Customer'
    ).

    yo_client->request->set_header_field(
      name  = 'x-action'
      value = 'Created'
    ).

  ENDMETHOD.


  METHOD set_body_request.

    CHECK 1 = 2.

    DATA: ls_lfa1 TYPE lfa1.
    SELECT SINGLE * FROM lfa1 INTO ls_lfa1.

    DATA(lv_json)  = /ui2/cl_json=>serialize( ls_lfa1 ).
    yo_client->request->set_cdata( lv_json ).

  ENDMETHOD.


  METHOD send_request.

    DATA: lv_xmsg TYPE string.

    xo_client->send(
      EXCEPTIONS
        http_communication_failure = 1 " Communication Error
        http_invalid_state         = 2 " Invalid state
        http_processing_failed     = 3 " Error When Processing Method
        http_invalid_timeout       = 4 " Invalid Time Entry
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
      lv_xmsg = format_syst_message( ).
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_send_request.
    ENDIF.


    xo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1 " Communication Error
        http_invalid_state         = 2 " Invalid state
        http_processing_failed     = 3 " Error when processing method
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.
      lv_xmsg = format_syst_message( ).
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_receive_response.
    ENDIF.

  ENDMETHOD.


  METHOD get_response.

    CLEAR ys_rest_response.


    xo_client->response->get_header_fields(
      CHANGING
        fields = ys_rest_response-resp_header " Header fields
    ).

    ys_rest_response-resp_body = xo_client->response->get_cdata( ).

    xo_client->response->get_status(
      IMPORTING
        code   = ys_rest_response-http_code " HTTP Status Code
        reason = ys_rest_response-reason    " HTTP status description
    ).


  ENDMETHOD.


  METHOD close_connection.

    yo_client->close(
      EXCEPTIONS
        http_invalid_state = 1 " Invalid state
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
      DATA(lv_xmsg) = format_syst_message( ).
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_close_connection.
    ENDIF.

  ENDMETHOD.


  METHOD generate_token_sas.

    DATA:
      lv_seconds_to_now   TYPE p,
      lv_seconds_in_week  TYPE p,
      lv_expiry_time      TYPE string,

      lv_utf8_url         TYPE string,
      lv_data_to_sign_str TYPE string,
      lv_data_to_sign_bin TYPE xstring,
      lv_shared_key_bin   TYPE xstring.


    "Alghorithm reference from Microsoft Documentation
    "https://learn.microsoft.com/en-us/rest/api/eventhub/generate-sas-token
    "-------------------------------------------------


    "Calculate Expiry Time
    "-------------------------------------------------

    "Get the current timestamp
    GET TIME STAMP FIELD DATA(lv_current_timestamp).

    CONVERT TIME STAMP lv_current_timestamp
      TIME ZONE sy-zonlo INTO DATE DATA(lv_date)
                              TIME DATA(lv_time).

    "Get the difference from 01.01.1970 to now in seconds
    cl_abap_tstmp=>td_subtract(
      EXPORTING
        date1    = lv_date
        time1    = lv_time
        date2    = '19700101'
        time2    = '000000'
      IMPORTING
        res_secs = lv_seconds_to_now
    ).

    "Add expiry time in seconds ( seconds to now + one week in seconds )
    lv_seconds_in_week = 60 * 60 * 24 * 7.
    lv_expiry_time     = lv_seconds_to_now + lv_seconds_in_week.
    CONDENSE lv_expiry_time.


    "Calculate Encrypted Key
    "-------------------------------------------------

    "Build string to Sign
    lv_utf8_url = escape(
      val    = xs_token_sas_params-url
      format = cl_abap_format=>e_uri_full
    ).

    lv_data_to_sign_str = |{ lv_utf8_url }{ cl_abap_char_utilities=>newline }{ lv_expiry_time }|.

    "Build Binary data to Sign
    DATA(lo_conv_data) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv_data->convert(
      EXPORTING
        data   = lv_data_to_sign_str
      IMPORTING
        buffer = lv_data_to_sign_bin
    ).

    "Encoding Shared Key
    DATA(lo_conv_key) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv_key->convert(
      EXPORTING
        data   = xs_token_sas_params-shared_key
      IMPORTING
        buffer = lv_shared_key_bin
    ).


    "Build Signature
    "-------------------------------------------------
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_algorithm     = 'sha-256'
            if_key           = lv_shared_key_bin
            if_data          = lv_data_to_sign_bin
            if_length        = 0
          IMPORTING
            ef_hmacb64string = DATA(lv_signature)
        ).

      CATCH cx_abap_message_digest INTO DATA(lx_abap_message_digest).

    ENDTRY.

    lv_signature = escape(
      val    = lv_signature
      format = cl_abap_format=>e_uri_full
    ).


    "Build SAS Token
    "-------------------------------------------------
    yv_token_sas = |SharedAccessSignature sr={ lv_utf8_url }&sig={ lv_signature }&se={ lv_expiry_time }&skn={ xs_token_sas_params-key_name }|.


  ENDMETHOD.


  METHOD generate_token_oauth2.

    DATA:
      lv_req_params        TYPE string,
      ls_token_oauth2_resp TYPE ts_token_oauth2_resp,
      lt_header            TYPE tihttpnvp.


    "Creation of New IF_HTTP_Client Object
    "-------------------------------------------------
    cl_http_client=>create_by_url(
      EXPORTING
        url                = xs_token_oauth2_params-uri
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = DATA(lo_client)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_determine_token.
    ENDIF.

    lo_client->propertytype_logon_popup = lo_client->co_disabled.
    lo_client->request->set_method( if_http_entity=>co_request_method_post ).
    lo_client->request->set_content_type( if_rest_media_type=>gc_appl_www_form_url_encoded ).


    "Set Request Data
    "-------------------------------------------------
    lv_req_params = ''.
    lv_req_params = |{ lv_req_params }client_id={ xs_token_oauth2_params-client_openid }|.
    lv_req_params = |{ lv_req_params }&client_secret={ xs_token_oauth2_params-client_secret }|.
    lv_req_params = |{ lv_req_params }&username={ xs_token_oauth2_params-user }|.
    lv_req_params = |{ lv_req_params }&password={ xs_token_oauth2_params-password }|.
    lv_req_params = |{ lv_req_params }&grant_type=password|.

    DATA(lv_strlen) = strlen( lv_req_params ).
    lo_client->request->set_cdata(
      data   = lv_req_params
      length = lv_strlen
      offset = 0
    ).


    "Send HTTP Request
    "-------------------------------------------------
    lo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_determine_token.
    ENDIF.


    "Check Response
    "-------------------------------------------------
    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_determine_token.
    ENDIF.


    "Get data from response
    "-------------------------------------------------
    DATA(lv_json) = lo_client->response->get_cdata( ).

    CLEAR ls_token_oauth2_resp.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_json
      CHANGING
        data        = ls_token_oauth2_resp
    ).

    yv_token_oauth2 = |Bearer { ls_token_oauth2_resp-access_token }|.


    "Close connection
    "-------------------------------------------------
    CLEAR lt_header[].
    lo_client->response->get_header_fields(
      CHANGING
        fields = lt_header
    ).

    lo_client->response->get_status(
      IMPORTING
        code   = DATA(lv_code)
        reason = DATA(lv_reason)
    ).

    lo_client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_determine_token.
    ENDIF.


  ENDMETHOD.


  METHOD format_syst_message.

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

ENDCLASS.
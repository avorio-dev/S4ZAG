CLASS zag_cl_rest_consumer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------


    " Aliases
    "-------------------------------------------------


    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_sas_token_params,
        uri        TYPE string,
        url        TYPE string,
        key_name   TYPE string,
        shared_key TYPE string,
      END OF ts_sas_token_params,

      BEGIN OF ts_oauth2_token_params,
        par TYPE string,
      END OF ts_oauth2_token_params,

      BEGIN OF ts_rest_response,
        http_code TYPE string,
        reason    TYPE string,
      END OF ts_rest_response.


    " Constants
    "-------------------------------------------------


    " Data
    "-------------------------------------------------


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      consume_rest
        IMPORTING
          !xv_url                TYPE string
          !xv_username           TYPE string
          !xv_password           TYPE string
          !xref_data             TYPE REF TO data
          !xs_sas_token_param    TYPE ts_sas_token_params OPTIONAL
          !xs_oauth2_token_param TYPE ts_oauth2_token_params OPTIONAL
        RETURNING VALUE(ys_rest_response) TYPE ts_rest_response
        RAISING cx_ai_system_fault.


  PROTECTED SECTION.

  PRIVATE SECTION.

    " Interfaces
    "-------------------------------------------------


    " Aliases
    "-------------------------------------------------


    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_oauth2_token_resp,
        access_token       TYPE string,
        expires_in         TYPE i,
        refresh_expires_in TYPE i,
        refresh_token      TYPE string,
        token_type         TYPE string,
        not_before_policy  TYPE i,
        session_state      TYPE string,
        scope              TYPE string,
      END OF ts_oauth2_token_resp .


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      c_http_499            TYPE i      VALUE 499 ##NO_TEXT,
      c_content_type_json   TYPE string VALUE 'application/json; charset=utf-8' ##NO_TEXT,
      c_content_type_xml    TYPE string VALUE 'application/xml; charset=utf-8' ##NO_TEXT,
      c_request_method_get  TYPE string VALUE 'GET' ##NO_TEXT,
      c_request_method_post TYPE string VALUE 'POST' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF tc_exception_msg,
        unable_det_client_obj TYPE string VALUE 'Unable determine Client Object'     ##NO_TEXT,
      END OF tc_exception_msg.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      generate_token_sas
        IMPORTING
                  !xv_token_post_url  TYPE string
                  !xv_key_name        TYPE string
                  !xv_shared_key      TYPE string
        RETURNING VALUE(yv_sas_token) TYPE string,

      generate_token_oauth2
        IMPORTING
          !xv_client_openid TYPE string
          !xv_client_secret TYPE string
          !xv_user          TYPE string
          !xv_password      TYPE string
          !xv_uri           TYPE string
        EXPORTING
          !yv_token         TYPE string
          !yv_code          TYPE i
          !yv_reason        TYPE string
        EXCEPTIONS
          http_client_error ,

      format_syst_message
        RETURNING VALUE(y_msg) TYPE string.


ENDCLASS.



CLASS zag_cl_rest_consumer IMPLEMENTATION.

  METHOD consume_rest.




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

    "-------------------------------------------------

    "Alghorithm reference from Microsoft Documentation
    "https://learn.microsoft.com/en-us/rest/api/eventhub/generate-sas-token

    "HOW TO USE
    "-------------------------------------------------

*    cl_http_client=>create_by_url(
*    EXPORTING
*      url                = post_url
*    IMPORTING
*      client             = lo_client
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3
*      ).
*
*    IF sy-subrc IS NOT INITIAL.
*      "Handle errors
*    ENDIF.
*
*    lo_client->propertytype_logon_popup = lo_client->co_disabled.
*    lo_client->request->set_method( 'POST' ).
*
*    "-------------------------------------------------
*
*    lo_client->request->set_header_field(
*    EXPORTING
*       name  = 'Content-Type'
*       value = 'application/json'
*    ).
*
*    zag_cl_rest=>generate_sas_token(
*      EXPORTING
*        X_TOKEN_POST_URL = token_url
*        x_key_name       = key_name
*        x_shared_key     = shared_key
*      RECEIVING
*        y_sas_token  = lv_auth
*    ).
*
*    lo_client->request->set_header_field(
*    EXPORTING
*       name  = 'Authorization'
*       value = lv_auth ).

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
      val    = xv_token_post_url
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
        data   = xv_shared_key
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
    yv_sas_token = |SharedAccessSignature sr={ lv_utf8_url }&sig={ lv_signature }&se={ lv_expiry_time }&skn={ xv_key_name }|.


  ENDMETHOD.


  METHOD generate_token_oauth2.

    DATA:
      lv_req_params        TYPE string,
      ls_oauth2_token_resp TYPE ts_oauth2_token_resp,
      lt_header            TYPE tihttpnvp.

    "HOW TO USE
    "Once token will be generated, it will need to be put in your service as follow
    "-------------------------------------------------
*    generate_oauth2_token(
*      EXPORTING
*        x_client_openid = lv_client_openid
*        x_client_secret = lv_client_secret
*        x_user          = lv_user
*        x_password      = lv_password
*        x_uri           = lv_uri_token
*      IMPORTING
*        y_token         = DATA(lv_token)
*        y_code          = DATA(lv_code)
*        y_reason        = DATA(lv_reason)
*    ).
*
*    lv_auth     = |Bearer { lv_token }|.
*
*    -------- Other code to config http client --------
*
*    lo_client->request->set_header_field(
*    EXPORTING
*       name  = 'Authorization'
*       value = lv_auth ).

    "-------------------------------------------------


    "Creation of New IF_HTTP_Client Object
    "-------------------------------------------------
    cl_http_client=>create_by_url(
      EXPORTING
        url                = xv_uri
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
      yv_code   = c_http_499.
      yv_reason = tc_exception_msg-unable_det_client_obj.
      RAISE http_client_error.
    ENDIF.


    lo_client->propertytype_logon_popup = lo_client->co_disabled.
    lo_client->request->set_method( if_http_entity=>co_request_method_post ).
    lo_client->request->set_content_type( if_rest_media_type=>gc_appl_www_form_url_encoded ).


    "Set Data
    "-------------------------------------------------
    lv_req_params = ''.
    lv_req_params = |{ lv_req_params }client_id={ xv_client_openid }|.
    lv_req_params = |{ lv_req_params }&client_secret={ xv_client_secret }|.
    lv_req_params = |{ lv_req_params }&username={ xv_user }|.
    lv_req_params = |{ lv_req_params }&password={ xv_password }|.
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
      yv_code   = c_http_499.
      yv_reason = format_syst_message( ).
      RAISE http_client_error.
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
      yv_code   = c_http_499.
      yv_reason = format_syst_message( ).
      RAISE http_client_error.
    ENDIF.


    "Get data from response
    "-------------------------------------------------
    DATA(lv_json_response) = lo_client->response->get_cdata( ).

    CLEAR ls_oauth2_token_resp.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_json_response
      CHANGING
        data        = ls_oauth2_token_resp
    ).

    yv_token = ls_oauth2_token_resp-access_token.


    "Close connection
    "-------------------------------------------------
    CLEAR lt_header[].
    lo_client->response->get_header_fields(
      CHANGING
        fields = lt_header
    ).

    lo_client->response->get_status(
      IMPORTING
        code   = yv_code
        reason = yv_reason
    ).

    lo_client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
      yv_code   = c_http_499.
      yv_reason = format_syst_message( ).
      RAISE http_client_error.
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
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
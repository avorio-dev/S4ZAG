class ZAG_CL_REST definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_oauth2_token_resp,
             access_token       TYPE string,
             expires_in         TYPE i,
             refresh_expires_in TYPE i,
             refresh_token      TYPE string,
             token_type         TYPE string,
             not_before_policy  TYPE i,
             session_state      TYPE string,
             scope              TYPE string,
           END OF ty_oauth2_token_resp .

  constants C_HTTP_499 type I value 499 ##NO_TEXT.

  class-methods FORMAT_SYST_MESSAGE
    returning
      value(Y_MSG) type STRING .
  class-methods GENERATE_SAS_TOKEN
    importing
      !X_TOKEN_POST_URL type STRING
      !X_KEY_NAME type STRING
      !X_SHARED_KEY type STRING
    exporting
      !Y_SAS_TOKEN type STRING .
  class-methods GENERATE_TOKEN_OAUTH2
    importing
      !X_CLIENT_OPENID type STRING
      !X_CLIENT_SECRET type STRING
      !X_USER type STRING
      !X_PASSWORD type STRING
      !X_URI type STRING
    exporting
      !Y_TOKEN type STRING
      !Y_CODE type I
      !Y_REASON type STRING
    exceptions
      HTTP_CLIENT_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZAG_CL_REST IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_REST=>FORMAT_SYST_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] Y_MSG                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_REST=>GENERATE_SAS_TOKEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_TOKEN_POST_URL               TYPE        STRING
* | [--->] X_KEY_NAME                     TYPE        STRING
* | [--->] X_SHARED_KEY                   TYPE        STRING
* | [<---] Y_SAS_TOKEN                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_sas_token.

    DATA: lv_seconds_to_now   TYPE p,
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
    CALL METHOD cl_abap_tstmp=>td_subtract
      EXPORTING
        date1    = lv_date
        time1    = lv_time
        date2    = '19700101'
        time2    = '000000'
      IMPORTING
        res_secs = lv_seconds_to_now.

    "Add expiry time in seconds ( seconds to now + one week in seconds )
    lv_seconds_in_week = 60 * 60 * 24 * 7.
    lv_expiry_time     = lv_seconds_to_now + lv_seconds_in_week.
    CONDENSE lv_expiry_time.


    "Calculate Encrypted Key
    "-------------------------------------------------

    "Build string to Sign
    lv_utf8_url         = escape( val    = x_token_post_url
                                  format = cl_abap_format=>e_uri_full ).
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
        data   = x_shared_key
      IMPORTING
        buffer = lv_shared_key_bin
    ).


    "Build Signature
    "-------------------------------------------------
    CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
      EXPORTING
        if_algorithm     = 'sha-256'
        if_key           = lv_shared_key_bin
        if_data          = lv_data_to_sign_bin
        if_length        = 0
      IMPORTING
        ef_hmacb64string = DATA(lv_signature).


    lv_signature = escape( val    = lv_signature
                           format = cl_abap_format=>e_uri_full ).

    "Build SAS Token
    "-------------------------------------------------
    y_sas_token = |SharedAccessSignature sr={ lv_utf8_url }&sig={ lv_signature }&se={ lv_expiry_time }&skn={ x_key_name }|.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_REST=>GENERATE_TOKEN_OAUTH2
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_CLIENT_OPENID                TYPE        STRING
* | [--->] X_CLIENT_SECRET                TYPE        STRING
* | [--->] X_USER                         TYPE        STRING
* | [--->] X_PASSWORD                     TYPE        STRING
* | [--->] X_URI                          TYPE        STRING
* | [<---] Y_TOKEN                        TYPE        STRING
* | [<---] Y_CODE                         TYPE        I
* | [<---] Y_REASON                       TYPE        STRING
* | [EXC!] HTTP_CLIENT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_token_oauth2.

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
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = x_uri
*       proxy_host         =
*       proxy_service      =
        ssl_id             = 'ANONYM'
*       sap_username       =
*       sap_client         =
*       proxy_user         =
*       proxy_passwd       =
      IMPORTING
        client             = DATA(lo_client)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      y_code   = c_http_499.
      y_reason = 'Unable determine Client Object'.
      RAISE http_client_error.
    ENDIF.


    lo_client->propertytype_logon_popup = lo_client->co_disabled.
    lo_client->request->set_method( if_http_entity=>co_request_method_post ).
    lo_client->request->set_content_type( if_rest_media_type=>gc_appl_www_form_url_encoded ).


    "Set Data
    "-------------------------------------------------
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
      y_code   = c_http_499.
      y_reason = format_syst_message( ).
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
      y_code   = c_http_499.
      y_reason = format_syst_message( ).
      RAISE http_client_error.
    ENDIF.


    "Get data from response
    "-------------------------------------------------
    DATA: ls_oauth2_token_resp TYPE ty_oauth2_token_resp.
    DATA(lv_json_response) = lo_client->response->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_json_response
      CHANGING
        data        = ls_oauth2_token_resp
    ).

    y_token = ls_oauth2_token_resp-access_token.


    "Close connection
    "-------------------------------------------------
    DATA lt_header TYPE tihttpnvp.
    lo_client->response->get_header_fields(
      CHANGING
        fields = lt_header
    ).

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
    IF sy-subrc <> 0.
      y_code   = c_http_499.
      y_reason = format_syst_message( ).
      RAISE http_client_error.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

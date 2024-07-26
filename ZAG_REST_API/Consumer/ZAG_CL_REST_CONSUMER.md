# ZAG_CL_REST_CONSUMER <a name="zag_cl_rest_consumer"></a>
 - CALL_REST_API
    - Main method to use to call your API
 
 - GENERATE_TOKEN_SAS / GENERATE_TOKEN_OAUTH2
    - Generate your authentication token using SAS ( Create a token which expires after a certain time) or OAUTH2 ( Create a token using a service provided by your BSN )

 - SET_AUTHENTICATION / SET_HEADER_REQUEST / SET_BODY
    - Redefine this methods to impelment your custom logic based on your need
 ---

```abap

  "Example 1 -> Call a Public API without particular settings like auth or body
  "-------------------------------------------------
  TRY.
      DATA(lv_url) = CONV string( 'http://jsonplaceholder.typicode.com/posts' ).

      DATA(lo_rest_api) = NEW zag_cl_rest_consumer( ).
      DATA(ls_rest_response) = lo_rest_api->call_rest_api(
          xv_url    = lv_url
          xv_method = zag_cl_rest_consumer=>tc_request_method-get
      ).

      cl_demo_output=>display_json( ls_rest_response-resp_body ).


    CATCH cx_ai_system_fault INTO DATA(lx_system_fault). " Application Integration: Technical Error
      cl_demo_output=>display_text( text = lx_system_fault->get_text( ) ).

  ENDTRY.

```

---

```abap

"Example 2 -> Call a Public API using your redefined settings like auth or body
"-------------------------------------------------
CLASS lcl_custom_rest_consumer DEFINITION
  INHERITING FROM zag_cl_rest_consumer.

  PROTECTED SECTION.
    METHODS:
      set_authentication REDEFINITION,
      set_header_request REDEFINITION,
      set_body_request  REDEFINITION.

ENDCLASS.

CLASS lcl_custom_rest_consumer IMPLEMENTATION.

  METHOD set_authentication.

    TRY.

        "Authentication with Username/Password
        "-------------------------------------------------
        IF 1 = 2.

          DATA:
            lv_username TYPE string VALUE 'ZAGUSR',
            lv_password TYPE string VALUE 'P@ssw0rd'.

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
        DATA(lv_cx_msg) = lx_ai_system_fault->get_text( ).
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

ENDCLASS.



START-OF-SELECTION.


  TRY.
      DATA(lv_url) = CONV string( 'http://jsonplaceholder.typicode.com/posts' ).

      DATA(lo_rest_api) = NEW lcl_custom_rest_consumer( ).
      DATA(ls_rest_response) = lo_rest_api->call_rest_api(
          xv_url    = lv_url
          xv_method = zag_cl_rest_consumer=>tc_request_method-get
      ).

      cl_demo_output=>display_json( ls_rest_response-resp_body ).


    CATCH cx_ai_system_fault INTO DATA(lx_system_fault). " Application Integration: Technical Error
      cl_demo_output=>display_text( text = lx_system_fault->get_text( ) ).

  ENDTRY.
```
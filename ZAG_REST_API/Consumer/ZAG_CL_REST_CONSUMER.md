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

      DATA(ls_rest_response) = zag_cl_rest_consumer=>call_rest_api(
          xv_url    = lv_url
          xv_method = zag_cl_rest_consumer=>tc_request_method-get
      ).

      cl_demo_output=>display_json( ls_rest_response-resp_body ).


    CATCH cx_ai_system_fault INTO DATA(lx_system_fault). " Application Integration: Technical Error
      cl_demo_output=>display_text( text = lx_system_fault->get_text( ) ).

  ENDTRY.

```
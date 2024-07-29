CLASS zag_cl_rest_handler DEFINITION
  INHERITING FROM cl_rest_http_handler
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Methods
    "-------------------------------------------------
    METHODS:
      if_rest_application~get_root_handler REDEFINITION.


  PROTECTED SECTION.

    " Methods
    "-------------------------------------------------
    METHODS:
      handle_csrf_token REDEFINITION.


  PRIVATE SECTION.


ENDCLASS.



CLASS ZAG_CL_REST_HANDLER IMPLEMENTATION.


  METHOD handle_csrf_token.

*    super->handle_csrf_token(
*      EXPORTING
*        io_csrf_handler =                  " REST CSRF Handler
*        io_request      =                  " REST Request
*        io_response     =                  " REST Response
*    ).

    "If you want to disable, keep this method redefined. Just as an empty method.


  ENDMETHOD.


  METHOD if_rest_application~get_root_handler.

    DATA:
      lv_rest_uri      TYPE string,
      lv_handler_class TYPE string.

    DATA(lo_router) = NEW cl_rest_router( ).

    "/default_host/sap/bc/ZAG_REST_PROV
    lv_rest_uri      = '/default_host/sap/bc/ZAG_REST_PROV'.
    lv_handler_class = 'ZAG_CL_REST_PROVIDER'.

    lo_router->attach(
      EXPORTING
        iv_template      = lv_rest_uri                " Unified Name for Resources
        iv_handler_class = CONV #( lv_handler_class ) " Object Type Name
*        it_parameter     =                            " Resource contructor parameters
    ).


    "If needed, you can set multiple Provider
*    DATA(lv_rest_uri)      = '/ZAG_REST2'.
*    DATA(lv_handler_class) = 'ZAG_CL_REST_PROVIDER2'.
*
*    lo_router->attach(
*      EXPORTING
*        iv_template      = CONV #( lv_rest_uri )      " Unified Name for Resources
*        iv_handler_class = CONV #( lv_handler_class ) " Object Type Name
**        it_parameter     =                            " Resource contructor parameters
*    ).


    ro_root_handler = lo_router.

  ENDMETHOD.
ENDCLASS.
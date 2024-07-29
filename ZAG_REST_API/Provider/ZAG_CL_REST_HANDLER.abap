CLASS zag_cl_rest_handler DEFINITION
  INHERITING FROM cl_rest_http_handler
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_application~get_root_handler REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      handle_csrf_token REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zag_cl_rest_handler IMPLEMENTATION.
  METHOD if_rest_application~get_root_handler.

    DATA(lo_router) = NEW cl_rest_router( ).

    DATA(lv_handler_class) = 'ZAG_CL_REST_PROVIDER'.
    lo_router->attach(
      EXPORTING
        iv_template      = '/rest'                    " Unified Name for Resources
        iv_handler_class = CONV #( lv_handler_class ) " Object Type Name
*        it_parameter     =                            " Resource contructor parameters
    ).

  ENDMETHOD.

  METHOD handle_csrf_token.

  ENDMETHOD.

ENDCLASS.
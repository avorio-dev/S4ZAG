CLASS zag_cl_odatav4_vendor_data DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_v4_abs_data_provider
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zag_if_odatav4_vendor.

    METHODS:
      /iwbep/if_v4_dp_basic~read_entity REDEFINITION,
      /iwbep/if_v4_dp_basic~read_entity_list REDEFINITION,
      /iwbep/if_v4_dp_basic~read_ref_target_key_data_list REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
          ts_cds_views         FOR zag_if_odatav4_vendor~ts_cds_views,
          cc_entity_set_names  FOR zag_if_odatav4_vendor~cc_entity_set_names,
          cc_entity_type_names FOR zag_if_odatav4_vendor~cc_entity_type_names,
          cc_nav_prop_names    FOR zag_if_odatav4_vendor~cc_nav_prop_names.


    " Vendor related methods
    "---------------------------------------------------------------
    METHODS:
      read_list_vendor
        IMPORTING
          io_request        TYPE REF TO /iwbep/if_v4_requ_basic_list
          io_response       TYPE REF TO /iwbep/if_v4_resp_basic_list
          iv_orderby_string TYPE string
          iv_where_clause   TYPE string
          iv_select_string  TYPE string
          iv_skip           TYPE i
          iv_top            TYPE i
          is_done_list      TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list
        RAISING
          /iwbep/cx_gateway,

      read_entity_vendor
        IMPORTING
          io_request  TYPE REF TO /iwbep/if_v4_requ_basic_read
          io_response TYPE REF TO /iwbep/if_v4_resp_basic_read
        RAISING
          /iwbep/cx_gateway,

      read_ref_key_list_vendor
        IMPORTING
          io_request  TYPE REF TO /iwbep/if_v4_requ_basic_ref_l
          io_response TYPE REF TO /iwbep/if_v4_resp_basic_ref_l
        RAISING
          /iwbep/cx_gateway.


    " Company related methods
    "---------------------------------------------------------------
    METHODS:
      read_list_company
        IMPORTING
          io_request        TYPE REF TO /iwbep/if_v4_requ_basic_list
          io_response       TYPE REF TO /iwbep/if_v4_resp_basic_list
          iv_orderby_string TYPE string
          iv_where_clause   TYPE string
          iv_select_string  TYPE string
          iv_skip           TYPE i
          iv_top            TYPE i
          is_done_list      TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list
        RAISING
          /iwbep/cx_gateway,

      read_entity_company
        IMPORTING
          io_request  TYPE REF TO /iwbep/if_v4_requ_basic_read
          io_response TYPE REF TO /iwbep/if_v4_resp_basic_read
        RAISING
          /iwbep/cx_gateway.


    " Purch. Org. related methods
    "---------------------------------------------------------------
    METHODS:
      read_list_purchorg
        IMPORTING
          io_request        TYPE REF TO /iwbep/if_v4_requ_basic_list
          io_response       TYPE REF TO /iwbep/if_v4_resp_basic_list
          iv_orderby_string TYPE string
          iv_where_clause   TYPE string
          iv_select_string  TYPE string
          iv_skip           TYPE i
          iv_top            TYPE i
          is_done_list      TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list
        RAISING
          /iwbep/cx_gateway,

      read_entity_purchorg
        IMPORTING
          io_request  TYPE REF TO /iwbep/if_v4_requ_basic_read
          io_response TYPE REF TO /iwbep/if_v4_resp_basic_read
        RAISING
          /iwbep/cx_gateway.

ENDCLASS.


CLASS zag_cl_odatav4_vendor_data IMPLEMENTATION.

  METHOD /iwbep/if_v4_dp_basic~read_entity.

    io_request->get_entity_set(
      IMPORTING
        ev_entity_set_name = DATA(lv_entityset_name)
    ).

    CASE lv_entityset_name.
      WHEN cc_entity_set_names-internal-vendor.

        read_entity_vendor(
          EXPORTING
            io_request  = io_request
            io_response = io_response
        ).


      WHEN cc_entity_set_names-internal-company.

        read_entity_company(
          EXPORTING
            io_request  = io_request
            io_response = io_response
        ).


      WHEN cc_entity_set_names-internal-purchorg.

        read_entity_purchorg(
          EXPORTING
            io_request  = io_request
            io_response = io_response
        ).

    ENDCASE.

  ENDMETHOD.

  METHOD /iwbep/if_v4_dp_basic~read_entity_list.

    DATA: ls_done_list      TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list,
          lv_orderby_string TYPE string,
          lv_skip           TYPE i,
          lv_top            TYPE i,
          lv_select_string  TYPE string,
          lv_where_clause   TYPE string.


    CLEAR ls_done_list.
    io_request->get_todos(
      IMPORTING
        es_todo_list = DATA(ls_todo_list)
    ).


    "Sort settings
    "---------------------------------------------------------------
    lv_orderby_string = 'PRIMARY KEY'.

    IF ls_todo_list-process-orderby = abap_true.

      ls_done_list-orderby = abap_true.

      io_request->get_orderby(
        IMPORTING
            et_orderby_property = DATA(lt_orderby_property)
      ).

      CLEAR lv_orderby_string.
      LOOP AT lt_orderby_property ASSIGNING FIELD-SYMBOL(<ls_orderby_property>).

        lv_orderby_string = COND #(
            WHEN <ls_orderby_property>-descending EQ abap_false
                THEN |{ lv_orderby_string } { <ls_orderby_property>-name } ASCENDING|
            WHEN <ls_orderby_property>-descending EQ abap_true
                THEN |{ lv_orderby_string } { <ls_orderby_property>-name } DESCENDING|
        ).

      ENDLOOP.

    ENDIF.


    " $skip / $top handling
    "---------------------------------------------------------------
    lv_skip = 0. lv_top = 0.

    IF ls_todo_list-process-skip = abap_true.

      ls_done_list-skip = abap_true.
      io_request->get_skip(
        IMPORTING
            ev_skip = lv_skip
      ).

    ENDIF.

    IF ls_todo_list-process-top = abap_true.

      ls_done_list-top = abap_true.
      io_request->get_top(
        IMPORTING
            ev_top = lv_top
      ).

    ENDIF.


    " $select handling
    "---------------------------------------------------------------
    lv_select_string = '*'.

    IF ls_todo_list-process-select = abap_true.

      ls_done_list-select = abap_true.
      io_request->get_selected_properties(
        IMPORTING
            et_selected_property = DATA(lt_selected_property)
      ).

      CONCATENATE LINES OF lt_selected_property INTO lv_select_string SEPARATED BY ','.

    ENDIF.


    "$ filter handling
    "---------------------------------------------------------------
    lv_where_clause = ''.

    IF ls_todo_list-process-filter = abap_true.

      ls_done_list-filter = abap_true.
      io_request->get_filter_osql_where_clause(
        IMPORTING
            ev_osql_where_clause = lv_where_clause
      ).

    ENDIF.


    "Read List Dispatcher
    "---------------------------------------------------------------
    io_request->get_entity_set(
       IMPORTING
         ev_entity_set_name = DATA(lv_entityset_name)
     ).

    CASE lv_entityset_name.
      WHEN cc_entity_set_names-internal-vendor.

        read_list_vendor(
          EXPORTING
            io_request        = io_request
            io_response       = io_response
            iv_orderby_string = lv_orderby_string
            iv_select_string  = lv_select_string
            iv_where_clause   = lv_where_clause
            iv_skip           = lv_skip
            iv_top            = lv_top
            is_done_list      = ls_done_list
        ).


      WHEN cc_entity_set_names-internal-company.

        read_list_company(
          EXPORTING
            io_request        = io_request
            io_response       = io_response
            iv_orderby_string = lv_orderby_string
            iv_select_string  = lv_select_string
            iv_where_clause   = lv_where_clause
            iv_skip           = lv_skip
            iv_top            = lv_top
            is_done_list      = ls_done_list
        ).


      WHEN cc_entity_set_names-internal-purchorg.

        read_list_purchorg(
          EXPORTING
            io_request        = io_request
            io_response       = io_response
            iv_orderby_string = lv_orderby_string
            iv_select_string  = lv_select_string
            iv_where_clause   = lv_where_clause
            iv_skip           = lv_skip
            iv_top            = lv_top
            is_done_list      = ls_done_list
        ).

      WHEN OTHERS.

        super->/iwbep/if_v4_dp_basic~read_entity_list( io_request  = io_request
                                                       io_response = io_response ).

    ENDCASE.


  ENDMETHOD.

  METHOD /iwbep/if_v4_dp_basic~read_ref_target_key_data_list.

    io_request->get_source_entity_type(
        IMPORTING
            ev_source_entity_type_name = DATA(lv_source_entity_name)
    ).

    CASE lv_source_entity_name.

      WHEN cc_entity_type_names-internal-vendor.

        read_ref_key_list_vendor(
           EXPORTING
            io_request  = io_request
            io_response = io_response ).


      WHEN OTHERS.

        super->/iwbep/if_v4_dp_basic~read_ref_target_key_data_list(
          EXPORTING
            io_request  = io_request
            io_response = io_response ).

    ENDCASE.

  ENDMETHOD.

  METHOD read_entity_company.

  ENDMETHOD.

  METHOD read_entity_purchorg.

  ENDMETHOD.

  METHOD read_entity_vendor.

  ENDMETHOD.

  METHOD read_list_company.

  ENDMETHOD.

  METHOD read_list_purchorg.

  ENDMETHOD.

  METHOD read_list_vendor.

  ENDMETHOD.

  METHOD read_ref_key_list_vendor.

  ENDMETHOD.

ENDCLASS.

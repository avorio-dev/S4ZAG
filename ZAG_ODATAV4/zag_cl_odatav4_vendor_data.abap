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

    DATA: ls_key_data TYPE ts_cds_views-company,
          ls_cds_lfb1 TYPE ts_cds_views-company.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).


    " Read the key data
    "---------------------------------------------------------------
    io_request->get_key_data(
        IMPORTING
            es_key_data = ls_key_data
    ).
    ls_done_list-key_data = abap_true.


    "Perform the extraction
    "---------------------------------------------------------------
    CLEAR ls_cds_lfb1.
    SELECT *
        FROM zag_cds_lfb1
        INTO @ls_cds_lfb1
        WHERE lifnr EQ @ls_key_data-lifnr
          AND bukrs EQ @ls_key_data-bukrs.
    ENDSELECT.
    IF sy-subrc EQ 0.

      io_response->set_busi_data( is_busi_data = ls_cds_lfb1 ).

    ENDIF.

    io_response->set_is_done( ls_done_list ).


  ENDMETHOD.

  METHOD read_entity_purchorg.

    DATA: ls_key_data TYPE ts_cds_views-purchorg,
          ls_cds_lfm1 TYPE ts_cds_views-purchorg.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    " Read the key data
    "---------------------------------------------------------------
    io_request->get_key_data(
        IMPORTING
            es_key_data = ls_key_data
    ).
    ls_done_list-key_data = abap_true.


    "Perform the extraction
    "---------------------------------------------------------------
    CLEAR ls_cds_lfm1.
    SELECT *
        FROM zag_cds_lfm1
        INTO @ls_cds_lfm1
        WHERE lifnr EQ @ls_key_data-lifnr
          AND ekorg EQ @ls_key_data-ekorg.
    ENDSELECT.
    IF sy-subrc EQ 0.

      io_response->set_busi_data( is_busi_data = ls_cds_lfm1 ).

    ENDIF.

    io_response->set_is_done( ls_done_list ).

  ENDMETHOD.

  METHOD read_entity_vendor.

    DATA: ls_key_data TYPE ts_cds_views-vendor,
          ls_cds_lfa1 TYPE ts_cds_views-vendor.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).


    " Read the key data
    "---------------------------------------------------------------
    io_request->get_key_data(
        IMPORTING
            es_key_data = ls_key_data
    ).
    ls_done_list-key_data = abap_true.


    "Perform the extraction
    "---------------------------------------------------------------
    CLEAR ls_cds_lfa1.
    SELECT * UP TO 1 ROWS
        FROM zag_cds_lfa1
        INTO @ls_cds_lfa1
        WHERE lifnr EQ @ls_key_data-lifnr.
    ENDSELECT.
    IF sy-subrc EQ 0.

      io_response->set_busi_data( is_busi_data = ls_cds_lfa1 ).

    ENDIF.

    io_response->set_is_done( ls_done_list ).

  ENDMETHOD.

  METHOD read_list_company.

    DATA: lt_key_company TYPE STANDARD TABLE OF ts_cds_views-company,
          lr_key_company TYPE zag_if_odatav4_vendor=>ts_key_range-bukrs,
          lv_max_index   TYPE i,
          lt_company     TYPE STANDARD TABLE OF ts_cds_views-company.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    " Get the request options the application has already handled
    ls_done_list = is_done_list.


    " Build key range
    "---------------------------------------------------------------
    IF ls_todo_list-process-key_data = abap_true.

      io_request->get_key_data(
        IMPORTING
            et_key_data = lt_key_company
      ).

      CLEAR lr_key_company[].
      lr_key_company = VALUE #( FOR <key> IN lt_key_company
          ( sign = 'I' option = 'EQ' low = <key>-lifnr )
      ).
      SORT lr_key_company.
      DELETE ADJACENT DUPLICATES FROM lr_key_company.

      ls_done_list-key_data = abap_true.

    ENDIF.


    "Data Extraction
    "---------------------------------------------------------------
    CASE ls_todo_list-return-busi_data.
      WHEN abap_true.

        " Read data from the CDS view
        " value for max_index must only be calculated if the request also contains a $top
        lv_max_index = COND #(
            WHEN iv_top IS NOT INITIAL THEN iv_top + iv_skip
            ELSE 0
        ).

        CLEAR lt_company[].
        SELECT (iv_select_string) UP TO @lv_max_index ROWS
         FROM zag_cds_lfb1
         INTO CORRESPONDING FIELDS OF TABLE @lt_company
          WHERE (iv_where_clause)
            AND lifnr IN @lr_key_company[]
        ORDER BY (iv_orderby_string).

        " Skipping entries specified by $skip
        " not needed as of NW751 where OFFSET is supported in Open SQL
        IF iv_skip IS NOT INITIAL.
          DELETE lt_company TO iv_skip.
        ENDIF.

        io_response->set_busi_data( it_busi_data = lt_company ).

      WHEN abap_false.

        " If business data is requested count will be calculated by the framework
        IF ls_todo_list-return-count = abap_true.

          SELECT COUNT( * )
              FROM zag_cds_lfb1
              WHERE (iv_where_clause)
                AND lifnr IN @lr_key_company[].

          io_response->set_count( sy-dbcnt ).

        ENDIF.

    ENDCASE.


    " Report list of request options handled by application
    "---------------------------------------------------------------
    io_response->set_is_done( ls_done_list ).


  ENDMETHOD.

  METHOD read_list_purchorg.

    DATA: lt_key_purchorg TYPE STANDARD TABLE OF ts_cds_views-purchorg,
          lr_key_purchorg TYPE zag_if_odatav4_vendor=>ts_key_range-ekorg,
          lv_max_index    TYPE i,
          lt_purchorg     TYPE STANDARD TABLE OF ts_cds_views-purchorg.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    " Get the request options the application has already handled
    ls_done_list = is_done_list.


    " Build key range
    "---------------------------------------------------------------
    IF ls_todo_list-process-key_data = abap_true.

      io_request->get_key_data(
        IMPORTING
            et_key_data = lt_key_purchorg
      ).

      CLEAR lr_key_purchorg[].
      lr_key_purchorg = VALUE #( FOR <key> IN lt_key_purchorg
          ( sign = 'I' option = 'EQ' low = <key>-lifnr )
      ).
      SORT lr_key_purchorg.
      DELETE ADJACENT DUPLICATES FROM lr_key_purchorg.

      ls_done_list-key_data = abap_true.

    ENDIF.


    "Data Extraction
    "---------------------------------------------------------------
    CASE ls_todo_list-return-busi_data.
      WHEN abap_true.

        " Read data from the CDS view
        " value for max_index must only be calculated if the request also contains a $top
        lv_max_index = COND #(
            WHEN iv_top IS NOT INITIAL THEN iv_top + iv_skip
            ELSE 0
        ).

        CLEAR lt_key_purchorg[].
        SELECT (iv_select_string) UP TO @lv_max_index ROWS
         FROM zag_cds_lfm1
         INTO CORRESPONDING FIELDS OF TABLE @lt_key_purchorg
          WHERE (iv_where_clause)
            AND lifnr IN @lr_key_purchorg[]
        ORDER BY (iv_orderby_string).

        " Skipping entries specified by $skip
        " not needed as of NW751 where OFFSET is supported in Open SQL
        IF iv_skip IS NOT INITIAL.
          DELETE lt_key_purchorg TO iv_skip.
        ENDIF.

        io_response->set_busi_data( it_busi_data = lt_key_purchorg ).

      WHEN abap_false.

        " If business data is requested count will be calculated by the framework
        IF ls_todo_list-return-count = abap_true.

          SELECT COUNT( * )
              FROM zag_cds_lfm1
              WHERE (iv_where_clause)
                AND lifnr IN @lr_key_purchorg[].

          io_response->set_count( sy-dbcnt ).

        ENDIF.

    ENDCASE.


    " Report list of request options handled by application
    "---------------------------------------------------------------
    io_response->set_is_done( ls_done_list ).


  ENDMETHOD.

  METHOD read_list_vendor.

    DATA: lt_key_vendor TYPE STANDARD TABLE OF ts_cds_views-vendor,
          lr_key_vendor TYPE zag_if_odatav4_vendor=>ts_key_range-lifnr,
          lv_max_index  TYPE i,
          lt_vendor     TYPE STANDARD TABLE OF ts_cds_views-vendor.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    " Get the request options the application has already handled
    ls_done_list = is_done_list.


    " Build key range
    "---------------------------------------------------------------
    IF ls_todo_list-process-key_data = abap_true.

      io_request->get_key_data(
        IMPORTING
            et_key_data = lt_key_vendor
      ).

      CLEAR lr_key_vendor[].
      lr_key_vendor = VALUE #( FOR <key> IN lt_key_vendor
          ( sign = 'I' option = 'EQ' low = <key>-lifnr )
      ).
      ls_done_list-key_data = abap_true.

    ENDIF.


    "Data Extraction
    "---------------------------------------------------------------
    CASE ls_todo_list-return-busi_data.
      WHEN abap_true.

        " Read data from the CDS view
        " value for max_index must only be calculated if the request also contains a $top
        lv_max_index = COND #(
            WHEN iv_top IS NOT INITIAL THEN iv_top + iv_skip
            ELSE 0
        ).

        CLEAR lt_vendor[].
        SELECT (iv_select_string) UP TO @lv_max_index ROWS
         FROM zag_cds_lfa1
         INTO CORRESPONDING FIELDS OF TABLE @lt_vendor
          WHERE (iv_where_clause)
            AND lifnr IN @lr_key_vendor[]
        ORDER BY (iv_orderby_string).

        " Skipping entries specified by $skip
        " not needed as of NW751 where OFFSET is supported in Open SQL
        IF iv_skip IS NOT INITIAL.
          DELETE lt_vendor TO iv_skip.
        ENDIF.

        io_response->set_busi_data( it_busi_data = lt_vendor ).

      WHEN abap_false.

        " If business data is requested count will be calculated by the framework
        IF ls_todo_list-return-count = abap_true.

          SELECT COUNT( * )
              FROM zag_cds_lfa1
              WHERE (iv_where_clause)
                AND lifnr IN @lr_key_vendor[].

          io_response->set_count( sy-dbcnt ).

        ENDIF.

    ENDCASE.


    " Report list of request options handled by application
    "---------------------------------------------------------------
    io_response->set_is_done( ls_done_list ).


  ENDMETHOD.

  METHOD read_ref_key_list_vendor.

    DATA: ls_key_data          TYPE ts_cds_views-vendor,
          lt_company_key_data  TYPE STANDARD TABLE OF ts_cds_views-company,
          lt_purchorg_key_data TYPE STANDARD TABLE OF ts_cds_views-purchorg,
          lv_nav_property_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.


    " Get the request options the application should/must handle
    "---------------------------------------------------------------
    DATA: ls_todo_list TYPE /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_list,
          ls_done_list TYPE /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_process_list.

    io_request->get_todos(
        IMPORTING
            es_todo_list = ls_todo_list
    ).

    IF ls_todo_list-process-source_key_data = abap_true.
      io_request->get_source_key_data(
        IMPORTING
            es_source_key_data =  ls_key_data
      ).
      ls_done_list-source_key_data = abap_true.
    ENDIF.


    io_request->get_navigation_prop(
        IMPORTING
            ev_navigation_prop_name = lv_nav_property_name
    ).

    CASE lv_nav_property_name.
      WHEN cc_nav_prop_names-internal-vendor_to_company.

        CLEAR lt_company_key_data[].
        SELECT lifnr, bukrs
            FROM zag_cds_lfb1
            INTO TABLE @lt_company_key_data
            WHERE lifnr EQ @ls_key_data-lifnr.

        io_response->set_target_key_data( lt_company_key_data ).

      WHEN cc_nav_prop_names-internal-vendor_to_purchorg.

        CLEAR lt_purchorg_key_data[].
        SELECT lifnr, ekorg
            FROM zag_cds_lfm1
            INTO TABLE @lt_purchorg_key_data
            WHERE lifnr EQ @ls_key_data-lifnr.

        io_response->set_target_key_data( lt_purchorg_key_data ).

      WHEN OTHERS.

    ENDCASE.


    io_response->set_is_done( ls_done_list ).


  ENDMETHOD.

ENDCLASS.
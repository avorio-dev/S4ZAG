CLASS zag_cl_salv_ida DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_field_label,
        fieldname TYPE string,
        label     TYPE string,
      END OF ts_field_label,

      BEGIN OF ts_double_click,
        repid   TYPE sy-repid,
        perform TYPE string,
      END OF ts_double_click.

    TYPES:
      tt_selopt      TYPE TABLE OF asint_frange,
      tt_field_list  TYPE if_salv_gui_types_ida=>yts_field_name,
      tt_field_label TYPE TABLE OF ts_field_label,
      tt_sort_group  TYPE if_salv_gui_types_ida=>yt_sort_rule.


    " Methods
    "-------------------------------------------------
    METHODS:
      constructor
        IMPORTING
          !xv_ddic_tabname  TYPE dbtabl
          !xt_selopt        TYPE tt_selopt OPTIONAL
          !xt_field_list    TYPE tt_field_list OPTIONAL
          !xt_field_label   TYPE tt_field_label OPTIONAL
          !xt_sort_group    TYPE tt_sort_group OPTIONAL
          !xo_event_handler TYPE REF TO object OPTIONAL
          !xv_max_rows      TYPE i OPTIONAL
        EXCEPTIONS
          table_not_supported,

      display .


  PROTECTED SECTION.

  PRIVATE SECTION.

    " Data
    "-------------------------------------------------
    DATA:
      gv_ddic_tabname  TYPE dbtabl,
      go_ida           TYPE REF TO if_salv_gui_table_ida,
      go_event_handler TYPE REF TO object.



    " Methods
    "-------------------------------------------------
    METHODS:
      set_selopt
        IMPORTING
          !xt_selopt TYPE tt_selopt,

      set_field_list
        IMPORTING
          !xt_field_list TYPE tt_field_list,

      set_field_label
        IMPORTING
          !xt_field_label TYPE tt_field_label,

      set_sort_group
        IMPORTING
          !xt_sort_group TYPE tt_sort_group,

      set_handler
        IMPORTING
          !xo_event_handler TYPE REF TO object,

      handle_dbclick
        FOR EVENT double_click OF if_salv_gui_table_display_opt.

ENDCLASS.



CLASS zag_cl_salv_ida IMPLEMENTATION.


  METHOD constructor.

    "Check if table is supported by IDA
    "-------------------------------------------------
    IF NOT cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = xv_ddic_tabname ).
      RAISE table_not_supported.
    ENDIF.

    "Init Instance
    me->gv_ddic_tabname = xv_ddic_tabname.
    me->go_ida          = cl_salv_gui_table_ida=>create( gv_ddic_tabname ).

    IF xv_max_rows NE 0.
      me->go_ida->set_maximum_number_of_rows( iv_number_of_rows = xv_max_rows ).
    ENDIF.

    IF cl_salv_gui_table_ida=>db_capabilities( )->is_max_rows_recommended( )
      AND xv_max_rows EQ 0 OR xv_max_rows GT 10000.
      me->go_ida->set_maximum_number_of_rows( iv_number_of_rows = 10000 ).
    ENDIF.


    "Select options
    "-------------------------------------------------
    IF xt_selopt[] IS NOT INITIAL.
      me->set_selopt( xt_selopt[] ).
    ENDIF.


    "Field List
    "-------------------------------------------------
    IF xt_field_list[] IS NOT INITIAL.
      me->set_field_list( xt_field_list[] ).
    ENDIF.


    "Field Label
    "-------------------------------------------------
    IF xt_field_label[] IS NOT INITIAL.
      me->set_field_label( xt_field_label[] ).
    ENDIF.


    "Sorting and Grouping
    "-------------------------------------------------
    IF xt_sort_group[] IS NOT INITIAL.
      me->set_sort_group( xt_sort_group[] ).
    ENDIF.


    "Set Handler
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF xo_event_handler IS BOUND.
      me->set_handler( xo_event_handler ).
    ENDIF.

  ENDMETHOD.


  METHOD display.

    me->go_ida->display_options( )->enable_alternating_row_pattern( ).
    me->go_ida->fullscreen( )->display( ).

  ENDMETHOD.


  METHOD set_selopt.

    DATA(lo_sel) = NEW cl_salv_range_tab_collector( ).

    LOOP AT xt_selopt ASSIGNING FIELD-SYMBOL(<named_range>).

      lo_sel->add_ranges_for_name(
        EXPORTING
          iv_name   = CONV #( <named_range>-fieldname )
          it_ranges = <named_range>-selopt_t[]
      ).

    ENDLOOP.

    lo_sel->get_collected_ranges(
      IMPORTING
        et_named_ranges = DATA(lt_named_ranges)
    ).

    me->go_ida->set_select_options( it_ranges = lt_named_ranges[] ).

  ENDMETHOD.


  METHOD set_field_list.

    me->go_ida->field_catalog( )->set_available_fields( its_field_names = xt_field_list[] ).

  ENDMETHOD.


  METHOD set_field_label.

    LOOP AT xt_field_label ASSIGNING FIELD-SYMBOL(<field_label>).

      me->go_ida->field_catalog( )->set_field_header_texts(
        iv_field_name  = CONV #( <field_label>-fieldname )
        iv_header_text = CONV #( <field_label>-label )
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD set_sort_group.

    me->go_ida->default_layout( )->set_sort_order( it_sort_order = xt_sort_group[] ).

  ENDMETHOD.


  METHOD set_handler.

    FREE me->go_event_handler.
    me->go_event_handler = xo_event_handler.

    me->go_ida->display_options( )->enable_double_click( ).
    me->go_ida->selection( )->set_selection_mode( iv_mode = 'SINGLE' ).
    SET HANDLER me->handle_dbclick FOR me->go_ida->display_options( ).

  ENDMETHOD.


  METHOD handle_dbclick.

    DATA:
      lref_selected_row TYPE REF TO data.

    CHECK me->go_ida->selection( )->is_row_selected( ).

    CREATE DATA lref_selected_row TYPE (me->gv_ddic_tabname).
    ASSIGN lref_selected_row->* TO FIELD-SYMBOL(<selected_row>).

    me->go_ida->selection( )->get_selected_row(
      IMPORTING
        es_row = <selected_row>
    ).

    TRY.
        CALL METHOD me->go_event_handler->('ON_DOUBLE_CLICK')
          EXPORTING
            xv_tabname      = me->gv_ddic_tabname
            xs_selected_row = <selected_row>.

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_illegal_method).
        DATA(lv_cx_msg) = lx_illegal_method->get_longtext( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

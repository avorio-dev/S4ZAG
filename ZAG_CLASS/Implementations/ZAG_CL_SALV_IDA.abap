class ZAG_CL_SALV_IDA definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_field_label,
        fieldname TYPE string,
        label     TYPE string,
      END OF ts_field_label .
  types:
    BEGIN OF ts_double_click,
        repid   TYPE sy-repid,
        perform TYPE string,
      END OF ts_double_click .
  types:
    tt_field_label TYPE TABLE OF ts_field_label .
  types:
    tt_selopt TYPE TABLE OF asint_frange .             "if_salv_service_types=>yt_named_ranges,
  types TT_FIELD_LIST type IF_SALV_GUI_TYPES_IDA=>YTS_FIELD_NAME .
  types TT_SORT_GROUP type IF_SALV_GUI_TYPES_IDA=>YT_SORT_RULE .

  methods CONSTRUCTOR
    importing
      !XV_DDIC_TABNAME type DBTABL
      !XT_SELOPT type TT_SELOPT optional
      !XT_FIELD_LIST type TT_FIELD_LIST optional
      !XT_FIELD_LABEL type TT_FIELD_LABEL optional
      !XT_SORT_GROUP type TT_SORT_GROUP optional
      !XS_DOUBLE_CLICK type TS_DOUBLE_CLICK optional
    exceptions
      TABLE_NOT_SUPPORTED .
  methods DISPLAY .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA go_ida TYPE REF TO if_salv_gui_table_ida .
    DATA gs_double_click TYPE ts_double_click.
    DATA gv_ddic_tabname TYPE dbtabl.

    METHODS set_selopt
      IMPORTING
        !xt_selopt TYPE tt_selopt .
    METHODS set_field_list
      IMPORTING
        !xt_field_list TYPE tt_field_list .
    METHODS set_field_label
      IMPORTING
        !xt_field_label TYPE tt_field_label .
    METHODS set_sort_group
      IMPORTING
        !xt_sort_group TYPE tt_sort_group .
    METHODS set_handler
      IMPORTING
        !xs_double_click TYPE ts_double_click.
    METHODS handle_dbclick
        FOR EVENT double_click OF if_salv_gui_table_display_opt.

ENDCLASS.



CLASS ZAG_CL_SALV_IDA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    "Check if table is supported by IDA
    "-------------------------------------------------
    IF NOT cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = xv_ddic_tabname ).
      RAISE table_not_supported.
    ENDIF.

    gv_ddic_tabname = xv_ddic_tabname.
    go_ida = cl_salv_gui_table_ida=>create( iv_table_name = gv_ddic_tabname ).
    IF cl_salv_gui_table_ida=>db_capabilities( )->is_max_rows_recommended( ).
      go_ida->set_maximum_number_of_rows(  iv_number_of_rows = 10000 ).
    ENDIF.


    "Select options
    "-------------------------------------------------
    IF xt_selopt IS NOT INITIAL.
      me->set_selopt( xt_selopt = xt_selopt[] ).
    ENDIF.


    "Field List
    "-------------------------------------------------
    IF xt_field_list IS NOT INITIAL.
      me->set_field_list( xt_field_list = xt_field_list[] ).
    ENDIF.


    "Field Label
    "-------------------------------------------------
    IF xt_field_label IS NOT INITIAL.
      me->set_field_label( xt_field_label = xt_field_label[] ).
    ENDIF.


    "Sorting and Grouping
    "-------------------------------------------------
    IF xt_sort_group IS NOT INITIAL.
      me->set_sort_group( xt_sort_group = xt_sort_group[] ).
    ENDIF.


    "Set Handler
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF xs_double_click IS NOT INITIAL.
      me->set_handler( xs_double_click = xs_double_click ).
    ENDIF.

  ENDMETHOD.


  METHOD DISPLAY.

    go_ida->display_options( )->enable_alternating_row_pattern( ).
    go_ida->fullscreen( )->display( ).

  ENDMETHOD.


  METHOD HANDLE_DBCLICK.

    DATA: lref_selected_row TYPE REF TO data.

    CHECK go_ida->selection( )->is_row_selected( ).


    CREATE DATA lref_selected_row TYPE (gv_ddic_tabname).
    ASSIGN lref_selected_row->* TO FIELD-SYMBOL(<selected_row>).

    go_ida->selection( )->get_selected_row(
      IMPORTING
        es_row = <selected_row>
    ).

    PERFORM (gs_double_click-perform) IN PROGRAM (gs_double_click-repid) IF FOUND
        USING gv_ddic_tabname
              <selected_row>.

  ENDMETHOD.


  METHOD SET_FIELD_LABEL.

    LOOP AT xt_field_label ASSIGNING FIELD-SYMBOL(<field_label>).

      go_ida->field_catalog( )->set_field_header_texts(
        iv_field_name  = CONV #( <field_label>-fieldname )
        iv_header_text = CONV #( <field_label>-label )
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD SET_FIELD_LIST.

    go_ida->field_catalog( )->set_available_fields( its_field_names = xt_field_list ).

  ENDMETHOD.


  METHOD SET_HANDLER.

    CLEAR gs_double_click.
    gs_double_click = xs_double_click.

    go_ida->display_options( )->enable_double_click( ).
    go_ida->selection( )->set_selection_mode( iv_mode = 'SINGLE' ).
    SET HANDLER me->handle_dbclick FOR go_ida->display_options( ).

  ENDMETHOD.


  METHOD SET_SELOPT.

    DATA(lo_sel) = NEW cl_salv_range_tab_collector( ).

    LOOP AT xt_selopt ASSIGNING FIELD-SYMBOL(<named_range>).

      lo_sel->add_ranges_for_name(
        EXPORTING
          iv_name   = CONV #( <named_range>-fieldname )
          it_ranges = <named_range>-selopt_t
      ).

    ENDLOOP.

    lo_sel->get_collected_ranges(
      IMPORTING
        et_named_ranges = DATA(lt_named_ranges)
    ).

    go_ida->set_select_options( it_ranges = lt_named_ranges ).

  ENDMETHOD.


  METHOD SET_SORT_GROUP.

    go_ida->default_layout( )->set_sort_order( it_sort_order = xt_sort_group ).

  ENDMETHOD.
ENDCLASS.

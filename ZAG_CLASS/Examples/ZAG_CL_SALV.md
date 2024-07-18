# ZAG_CL_SALV <a name="zag_cl_salv"></a>
 - DISPLAY_GENERIC_ALV
    - It allows to Display whatever table you want, providing only the table itself.
    - You can also pass additional parameters like
       - Display in popup
       - Column Settings in which you can give provide your labels or hide fields

 - GET_FIELDCAT_FROM_DATA
    - It allows to extract Fieldcat/Struct Descr from both table or simple row which you provide

 - SET_COLOR_CELL / SET_COLOR_ROW
    - It allows to set Colors which will be printed.
    - The only constraint is that you will need to have a component in your types named T_COL TYPE lvc_t_scol
   
 - Set Handler For Event ( Double Click, Hotspot Click )    
    - It allows to implement custom code for event Double Click / Hotspot
     declaring your class with specific method name.
   
---

```abap
  "Example 1 -> Display a generic ALV
  "-------------------------------------------------

  SELECT * FROM mara UP TO 10 ROWS INTO TABLE @DATA(lt_mara).

  DATA(lo_salv) = NEW zag_cl_salv( ).
  lo_salv->display_generic_alv( lt_mara ).
```

---

```abap
  "Example 2 -> Set colors for cells and / or rows
  "          -> Set Labels / Hide fields
  "-------------------------------------------------

  TYPES: 
    BEGIN OF ty_alv,
        icon  TYPE icon_d,
        matnr TYPE mara-matnr,
        ersda TYPE mara-ersda,
        ernam TYPE mara-ernam,
        laeda TYPE mara-laeda,
        aenam TYPE mara-aenam,
        zeinr TYPE mara-zeinr,

        t_col TYPE lvc_t_scol, "Use this if you want colors in output
    END OF ty_alv.

  DATA: 
    gt_alv       TYPE TABLE OF ty_alv.

  SELECT * FROM mara UP TO 10 ROWS INTO TABLE @DATA(lt_mara).
  LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<mara>).

    "Set Data
    APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<alv>).
    MOVE-CORRESPONDING <mara> TO <alv>.


    "Set Icon
    DATA(lv_diff) = sy-datum - <mara>-ersda.
    <alv>-icon = COND #(
      WHEN lv_diff MOD 2 EQ 0 THEN zag_cl_salv=>tc_icon-green
      ELSE zag_cl_salv=>tc_icon-red
    ).


    lv_diff = sy-datum - <mara>-ersda.
    IF lv_diff MOD 2 EQ 0.

      "Set Single Cell Color
      zag_cl_salv=>set_color_cell(
        EXPORTING
          xs_color            = VALUE #( col = zag_cl_salv=>tc_cell_col-green
                                         int = 1
                                         inv = 1 )
          xt_fieldname        = VALUE #( ( 'MATNR' ) )
        CHANGING
          y_row               = <alv>
        EXCEPTIONS
          col_tab_not_found   = 1
          fieldname_not_found = 2
          OTHERS              = 3
      ).

    ELSE.

      "Set Row Color
      zag_cl_salv=>set_color_row(
        EXPORTING
          xs_color          = VALUE #( col = zag_cl_salv=>tc_cell_col-red
                                       int = 0
                                       inv = 0 )
        CHANGING
          ys_row            = <alv>
        EXCEPTIONS
          col_tab_not_found = 1
          fcat_not_found    = 2
          OTHERS            = 3
      ).

    ENDIF.
  ENDLOOP.


  DATA: lt_col_settings TYPE zag_cl_salv=>tt_col_settings.
  lt_col_settings = VALUE #(
    ( fieldname = 'MATNR'
      label     = 'My Material'
      no_out    = '' )

    ( fieldname = 'ERSDA'
      label     = 'Data Creation' )

    ( fieldname = 'ZEINR'
      no_out    = 'X' )

   ).

  DATA(lo_salv) = NEW zag_cl_salv( ).
  lo_salv->display_generic_alv(
    EXPORTING
      xv_popup            = abap_true
      xt_col_settings     = lt_col_settings
      xt_output           = gt_alv[]
  ).
```

---

```abap
  "Example 3 -> Set Event Handler
  "-------------------------------------------------

  "Class Event Hanlder which you will implement
  CLASS lcl_event_handler DEFINITION.
  
    PUBLIC SECTION.
      METHODS:
        on_link_click
          IMPORTING
            VALUE(xv_row)    TYPE salv_de_row
            VALUE(xv_column) TYPE salv_de_column,
  
        on_double_click
          IMPORTING
            VALUE(xv_row)    TYPE salv_de_row
            VALUE(xv_column) TYPE salv_de_column.
  
  ENDCLASS.
  CLASS lcl_event_handler IMPLEMENTATION.
    METHOD on_link_click.
  
      MESSAGE i646(db) WITH 'Row:' xv_row 'Column:' xv_column.
  
    ENDMETHOD.
    METHOD on_double_click.
  
      MESSAGE i646(db) WITH 'Row:' xv_row 'Column:' xv_column.
  
    ENDMETHOD.
  ENDCLASS.

  START-OF-SELECTION.

  SELECT * FROM mara UP TO 10 ROWS INTO TABLE @lt_mara.

  lt_col_settings = VALUE #(
    ( fieldname = 'MATNR'
      hotspot   = 'X' )
  ).

  DATA(lo_salv)          = NEW zag_cl_salv( ).
  DATA(lo_event_handler) = NEW lcl_event_handler( ). 

  lo_salv->display_generic_alv(
    EXPORTING
      xt_output        = lt_mara[]
      xt_col_settings  = lt_col_settings[]
      xo_event_handler = lo_event_handler 
      xv_popup         = abap_true
  ).
```
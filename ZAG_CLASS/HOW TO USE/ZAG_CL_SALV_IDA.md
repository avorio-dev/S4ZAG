# ZAG_CL_ALV_IDA <a name="zag_cl_salv_ida"></a>

- Display
  - It allows to execute a query directly without select statement into provided table
  - You can specify the fields list to extract, labels and set event handler providing your instance class with specific method ( ON_DOUBLE_CLICK )  
```abap
  "Example 1 -> Display a generic ALV
  "-------------------------------------------------

  DATA:
    lo_ida     TYPE REF TO zag_cl_salv_ida,
    lv_tabname TYPE string.

  lv_tabname = 'EKPO'.

  CREATE OBJECT lo_ida
    EXPORTING
      xv_ddic_tabname     = CONV #( lv_tabname )
      xv_max_rows         = 100
    EXCEPTIONS
      table_not_supported = 1
      OTHERS              = 2.

  lo_ida->display( ).
```

---

```abap
"Example 2 -> Display ALV with custom settings
  "-------------------------------------------------

  DATA:
    lo_ida         TYPE REF TO zag_cl_salv_ida,
    lv_tabname     TYPE string,
    lt_selopt      TYPE zag_cl_salv_ida=>tt_selopt,
    lt_field_list  TYPE zag_cl_salv_ida=>tt_field_list,
    lt_field_label TYPE zag_cl_salv_ida=>tt_field_label,
    lt_sort_group  TYPE zag_cl_salv_ida=>tt_sort_group.


  "DDIC Tabname or CDS Name
  "-------------------------------------------------
  lv_tabname = 'EKPO'.


  "Select Options
  "-------------------------------------------------
  lt_selopt = VALUE #(
     ( fieldname = 'BUKRS'
       selopt_t  = VALUE #(
        ( sign = 'I' option = 'EQ'
          low  = 'ITA1'
          high = 'ITA1' )
       )
     )

     ( fieldname = 'MATNR'
       selopt_t  = VALUE #(
         ( sign = 'I' option = 'NE'
           low  = ''
           high = '' )
        )
     )
  ).


  "List of fields to extract
  "-------------------------------------------------
  lt_field_list = VALUE zag_cl_salv_ida=>tt_field_list(
    ( CONV string('EBELN') )
    ( CONV string('EBELP') )
    ( CONV string('AEDAT') )
    ( CONV string('MATNR') )
    ( CONV string('NETWR') )
  ).


  "Field Labels
  "-------------------------------------------------
  lt_field_label = VALUE #(
    ( fieldname = 'EBELN' label = 'PO Order' )
    ( fieldname = 'EBELP' label = 'PO Item' )
    ( fieldname = 'MATNR' label = 'Mat.' )
    ( fieldname = 'NETWR' label = 'Amount' )
  ).


  "Sort and Grouping
  "-------------------------------------------------
  lt_sort_group = VALUE #(
    ( field_name = 'MATNR'
      descending = abap_false
      is_grouped = abap_true )

    ( field_name = 'AEDAT'
      descending = abap_true
      is_grouped = abap_false )

    ( field_name = 'EBELN'
      descending = abap_false
      is_grouped = abap_false )

    ( field_name = 'EBELP'
      descending = abap_false
      is_grouped = abap_false )
  ).


  "ALV IDA
  "-------------------------------------------------
  CREATE OBJECT lo_ida
    EXPORTING
      xv_ddic_tabname     = CONV #( lv_tabname )
      xt_selopt           = lt_selopt[]
      xt_field_list       = lt_field_list[]
      xt_field_label      = lt_field_label[]
      xt_sort_group       = lt_sort_group[]
      xv_max_rows         = 100
    EXCEPTIONS
      table_not_supported = 1
      OTHERS              = 2.

  lo_ida->display( ).
```

---

```abap
  "Example 3 -> Display ALV event handler
  "-------------------------------------------------

  CLASS lcl_event_hanlder DEFINITION.
    PUBLIC SECTION.
      METHODS on_double_click
        IMPORTING
          xv_tabname      TYPE dbtabl
          xs_selected_row TYPE any.
  
  ENDCLASS.
  
  CLASS lcl_event_hanlder IMPLEMENTATION.
  
    METHOD on_double_click.
  
  
  
    ENDMETHOD.
  ENDCLASS.
  
  
  START-OF-SELECTION.
  
    DATA:
      lo_ida           TYPE REF TO zag_cl_salv_ida,
      lv_tabname       TYPE string,
      lo_event_hanlder TYPE REF TO lcl_event_hanlder.
  
  
    lv_tabname = 'EKPO'.
  
    lo_event_hanlder = NEW lcl_event_hanlder( ).
    CREATE OBJECT lo_ida
      EXPORTING
        xv_ddic_tabname     = CONV #( lv_tabname )
        xo_event_handler    = lo_event_hanlder
        xv_max_rows         = 100
      EXCEPTIONS
        table_not_supported = 1
        OTHERS              = 2.
  
    lo_ida->display( ).
```
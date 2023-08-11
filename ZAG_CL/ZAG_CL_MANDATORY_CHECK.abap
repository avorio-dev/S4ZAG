CLASS zag_cl_mandatory_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_fieldlist,
        tabname    TYPE dd03l-tabname,
        fieldname  TYPE dd03l-fieldname,
        position   TYPE dd03l-position,
        datatype   TYPE dd03l-datatype,
        depth      TYPE dd03l-depth,
        comptype   TYPE dd03l-comptype,
        rollname   TYPE dd03l-rollname,
        path_field TYPE string,
        ddtext     TYPE dd04t-ddtext,
      END OF ty_fieldlist .
    TYPES:
      tt_fieldlist TYPE TABLE OF ty_fieldlist .
    TYPES:
      BEGIN OF ty_mand_cust_struct,
        tabname TYPE dd03l-tabname,
      END OF ty_mand_cust_struct .
    TYPES:
      tt_mand_cust_struct TYPE TABLE OF ty_mand_cust_struct .

    METHODS constructor
      IMPORTING
        !x_struct_name         TYPE string OPTIONAL
        !x_customizing_tabname TYPE string OPTIONAL
        !x_data_to_check       TYPE any OPTIONAL
      EXCEPTIONS
        ddic_error
        customizing_error
        internal_error .
    METHODS get_field_list
      IMPORTING
        !x_show_alv         TYPE xfeld OPTIONAL
      EXPORTING
        VALUE(yt_fieldlist) TYPE tt_fieldlist .
  PROTECTED SECTION.
private section.

  data GT_FIELDLIST type TT_FIELDLIST .
  data GS_STRUCT_NAME type STRING .
  data GT_MAND_CUST_STRUCT type TT_MAND_CUST_STRUCT .

  methods BUILD_FIELD_LIST_RECURS
    importing
      !X_STRUCT_NAME type STRING
      !X_PARENT type TY_FIELDLIST .
  methods CHECK_STRUCT
    importing
      !XT_CUSTOMIZING type STANDARD TABLE
      !X_DATA_TO_CHECK type ANY
    exporting
      !Y_MISSING_FIELD type XFELD
      !Y_MESSAGE type STRING .
  methods CHECK_STRUCT_RECURS
    importing
      !X_DATA_TO_CHECK type ANY
      !X_PATH_PROCESSED type STRING
      !X_PATH_TO_PROCESS type STRING
    exporting
      !Y_MISSING_FIELD type XFELD
      !Y_MESSAGE type STRING .
  methods EXTR_FIELD_LIST
    importing
      !X_STRUCT_NAME type STRING
    changing
      !YT_FIELDLIST type TT_FIELDLIST .
  methods PRINT_FIELD_LIST .
  methods INIT_FIELD_LIST
    importing
      !X_STRUCT_NAME type STRING
    exceptions
      DDIC_ERROR .
  methods INIT_CUSTOMIZING
    importing
      !X_CUSTOMIZING_TABNAME type STRING
    exporting
      !YT_CUSTOMIZING type ANY
    exceptions
      CUSTOMIZING_ERROR .
  methods INIT_MAND_CUST_STRUCT .
ENDCLASS.



CLASS ZAG_CL_MANDATORY_CHECK IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->BUILD_FIELD_LIST_RECURS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STRUCT_NAME                  TYPE        STRING
* | [--->] X_PARENT                       TYPE        TY_FIELDLIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_field_list_recurs.

    DATA: lt_fieldlist  TYPE tt_fieldlist,
          ls_parent     LIKE LINE OF lt_fieldlist,

          lv_to_replace TYPE string,
          lv_str_tmp    TYPE string.


    me->extr_field_list(
      EXPORTING
        x_struct_name = x_struct_name
      CHANGING
        yt_fieldlist  = lt_fieldlist
    ).


    CLEAR: ls_parent.
    IF x_parent IS NOT INITIAL.
      ls_parent       = x_parent.
    ENDIF.

    "-------------------------------------------------

    LOOP AT lt_fieldlist ASSIGNING FIELD-SYMBOL(<fieldlist>).

      IF x_parent IS NOT INITIAL.
        <fieldlist>-depth = <fieldlist>-depth + x_parent-depth + 1.
      ENDIF.


      "Same Level -> It's reading a field of the same structure
      "-------------------------------------------------
      IF <fieldlist>-depth EQ ls_parent-depth.

        <fieldlist>-path_field = ls_parent-path_field.
        IF <fieldlist>-comptype NE 'E'.
          ls_parent = <fieldlist>.
        ENDIF.

      ENDIF.


      "Getting into a new inner level
      "-------------------------------------------------
      IF <fieldlist>-depth GT ls_parent-depth.

        <fieldlist>-path_field = ls_parent-path_field && '-' && ls_parent-fieldname.
        IF <fieldlist>-comptype NE 'E'.
          ls_parent = <fieldlist>.
        ENDIF.

      ENDIF.


      "Returning to the parent level
      "-------------------------------------------------
      IF <fieldlist>-depth LT ls_parent-depth.

        DATA lv_diff TYPE i.
        lv_diff = ls_parent-depth - <fieldlist>-depth.

        "Delete the final piece of the path N-Times equal to the
        "difference in return values
        DO lv_diff TIMES.

          lv_to_replace = ls_parent-path_field.

          DO .

            SPLIT lv_to_replace AT '-'
              INTO DATA(lv_sx)
                   DATA(lv_dx).

            IF lv_dx IS INITIAL.
              lv_to_replace = '-' && lv_sx.
              EXIT.
            ENDIF.

            IF lv_str_tmp IS INITIAL.
              lv_str_tmp = lv_sx.
            ELSE.
              lv_str_tmp = lv_str_tmp && '-' && lv_sx.
            ENDIF.

            lv_to_replace = lv_dx.

          ENDDO.

          REPLACE lv_to_replace IN ls_parent-path_field WITH ''.
        ENDDO.

        <fieldlist>-path_field = ls_parent-path_field.
        ls_parent              = <fieldlist>.

      ENDIF.


      "Insert into the fields list global table
      "-------------------------------------------------
      APPEND INITIAL LINE TO gt_fieldlist ASSIGNING FIELD-SYMBOL(<global_fieldlist>).
      MOVE-CORRESPONDING <fieldlist> TO <global_fieldlist>.
      <global_fieldlist>-position = sy-tabix.
      REPLACE '-' IN <global_fieldlist>-path_field WITH ''.


      "If checking type table structure, it will be execute the recursion
      "because the sub-fields of the type table are missing into the first extraction
      "-------------------------------------------------
      IF <fieldlist>-comptype EQ 'L'. "Type Table

        DATA lv_fieldname  TYPE string.
        lv_fieldname = <fieldlist>-rollname.
        ls_parent    = <fieldlist>.
        me->build_field_list_recurs( x_struct_name = lv_fieldname
                                     x_parent      = ls_parent ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->CHECK_STRUCT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_CUSTOMIZING                 TYPE        STANDARD TABLE
* | [--->] X_DATA_TO_CHECK                TYPE        ANY
* | [<---] Y_MISSING_FIELD                TYPE        XFELD
* | [<---] Y_MESSAGE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_struct.

    DATA: lo_data_to_check TYPE REF TO data.

    SORT gt_fieldlist BY fieldname
                         path_field .

    CREATE DATA lo_data_to_check TYPE (gs_struct_name).
    ASSIGN lo_data_to_check->* TO FIELD-SYMBOL(<data_to_check>).
    <data_to_check> = x_data_to_check.

    LOOP AT xt_customizing ASSIGNING FIELD-SYMBOL(<customizing>).

      ASSIGN COMPONENT 'PARENT_PATH' OF STRUCTURE <customizing> TO FIELD-SYMBOL(<parent_path>).
      CHECK sy-subrc EQ 0.

      ASSIGN COMPONENT 'SAP_FIELD' OF STRUCTURE <customizing> TO FIELD-SYMBOL(<sap_field>).
      CHECK sy-subrc EQ 0.

      CONCATENATE <parent_path>
                  <sap_field>
                  INTO DATA(lv_path_to_process)
                  SEPARATED BY '-'.

      "Start recursion
      me->check_struct_recurs(
        EXPORTING
          x_data_to_check   = <data_to_check>
          x_path_processed  = ''
          x_path_to_process = lv_path_to_process
        IMPORTING
          y_missing_field   = y_missing_field
          y_message         = y_message
      ).

      CHECK y_missing_field EQ 'X'.
      EXIT.


    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->CHECK_STRUCT_RECURS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_DATA_TO_CHECK                TYPE        ANY
* | [--->] X_PATH_PROCESSED               TYPE        STRING
* | [--->] X_PATH_TO_PROCESS              TYPE        STRING
* | [<---] Y_MISSING_FIELD                TYPE        XFELD
* | [<---] Y_MESSAGE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_struct_recurs.

    CHECK y_missing_field IS INITIAL.

    SPLIT x_path_to_process AT '-'
      INTO DATA(lv_field_to_assign)
           DATA(lv_rest).

    READ TABLE gt_fieldlist ASSIGNING FIELD-SYMBOL(<campo>)
      WITH KEY fieldname  = lv_field_to_assign
               path_field = x_path_processed
               BINARY SEARCH.


    IF x_path_processed IS NOT INITIAL.

      CONCATENATE x_path_processed
                  lv_field_to_assign
                  INTO lv_field_to_assign SEPARATED BY '-'.

    ENDIF.


    ASSIGN COMPONENT lv_field_to_assign OF STRUCTURE x_data_to_check TO FIELD-SYMBOL(<valore_campo>).
    CHECK sy-subrc EQ 0.

    CASE <campo>-comptype.
      WHEN 'S'.

        me->check_struct_recurs(
        EXPORTING
          x_data_to_check   = x_data_to_check
          x_path_processed  = lv_field_to_assign
          x_path_to_process = lv_rest
        IMPORTING
          y_missing_field   = y_missing_field
          y_message         = y_message
      ).


      WHEN 'L'.

        FIELD-SYMBOLS: <t_tab> TYPE STANDARD TABLE.

        ASSIGN <valore_campo> TO <t_tab>.

        IF <t_tab> IS INITIAL.
          y_message = 'Empty Table:' && lv_field_to_assign.
          y_missing_field = 'X'.
          EXIT.
        ENDIF.
        LOOP AT <t_tab> ASSIGNING FIELD-SYMBOL(<s_row>).

          me->check_struct_recurs(
                  EXPORTING
                    x_data_to_check   = <s_row>
                    x_path_processed  = ''
                    x_path_to_process = lv_rest
                  IMPORTING
                    y_missing_field   = y_missing_field                  " Casella di spunta
                    y_message         = y_message
                ).

        ENDLOOP.

      WHEN OTHERS.
        IF <valore_campo> IS INITIAL.
          y_message = 'Empty Field:' && lv_field_to_assign.
          y_missing_field = 'X'.
          EXIT.
        ENDIF.

    ENDCASE.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_MANDATORY_CHECK->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STRUCT_NAME                  TYPE        STRING(optional)
* | [--->] X_CUSTOMIZING_TABNAME          TYPE        STRING(optional)
* | [--->] X_DATA_TO_CHECK                TYPE        ANY(optional)
* | [EXC!] DDIC_ERROR
* | [EXC!] CUSTOMIZING_ERROR
* | [EXC!] INTERNAL_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

**********************************************************************
*--> HOW TO USE

* This class can be used in two different ways:
*  1 -> You can build an instance with only the structure name to check in input.
*       In this way, you can use the method 'Get_field_list' to get the list of fields builded by class
*       for which, it will be assigned the hierarchy of the inner structures to each field.
*       You can also print the result into a simple ALV.
*       Example of structure 'CVIS_EI_EXTERN'
*
*  2 -> If you need to check if in a structure all requested fields are filled,
*       you can give in input the structure name and the structure data.
*       The class will provide an error message if some filed is empty.
*       You will need to have a Custom DDIC table with folllowing structure
*       ( You can also add other fields, but this are mandatory )

        "Example -> ZAG_MAND_FIELDS
*       PROGNAME    REPID    -> The program to check
*       COUNTER     INT4     -> Simple Counter
*       PARENT_PATH STRING   -> It will contains the path of the parent structure
*       SAP_FIELD   STRING   -> The filed to check

**********************************************************************


    DATA: lo_customizing TYPE REF TO data.

    FIELD-SYMBOLS: <lt_customizing> TYPE STANDARD TABLE.


    "Initialize and builds the fields list
    "-------------------------------------------------
    me->init_field_list(
      EXPORTING
        x_struct_name = x_struct_name
      EXCEPTIONS
        ddic_error    = 1
        OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.



    "Start the field-checks based on the input structure
    "-------------------------------------------------
    IF x_customizing_tabname IS NOT INITIAL.

      IF x_data_to_check IS INITIAL.
        MESSAGE e646(db) WITH 'Data to check' 'must be filled!'
          RAISING internal_error.
      ENDIF.


      me->init_customizing(
        EXPORTING
          x_customizing_tabname = x_customizing_tabname
        IMPORTING
          yt_customizing        = lo_customizing
        EXCEPTIONS
          customizing_error     = 1
          OTHERS                = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      ASSIGN lo_customizing->* TO <lt_customizing>.


      me->check_struct(
        EXPORTING
          xt_customizing  = <lt_customizing>[]
          x_data_to_check = x_data_to_check
        IMPORTING
          y_missing_field = DATA(lv_missing)
          y_message       = DATA(lv_message)
      ).


    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->EXTR_FIELD_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STRUCT_NAME                  TYPE        STRING
* | [<-->] YT_FIELDLIST                   TYPE        TT_FIELDLIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD extr_field_list.

    REFRESH: yt_fieldlist.

    SELECT tabname,
         fieldname,
         position,
         datatype,
         depth,
         comptype,
         rollname
    FROM dd03l
    INTO TABLE @yt_fieldlist
    WHERE tabname   EQ @x_struct_name
    ORDER BY tabname, position.
    IF sy-subrc <> 0.

      SELECT SINGLE rowtype
        FROM dd40l
          INTO @DATA(lv_struct_name)
        WHERE typename EQ @x_struct_name.
      CHECK sy-subrc EQ 0.

      SELECT tabname,
             fieldname,
             position,
             datatype,
             depth,
             comptype,
             rollname
        FROM dd03l
        INTO TABLE @yt_fieldlist
        WHERE tabname   EQ @lv_struct_name
          ORDER BY tabname, position.
      CHECK sy-subrc EQ 0.

    ENDIF.

    DELETE yt_fieldlist WHERE fieldname CS '.'. "Exclusion of INCLUDE/APPEND Record


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZAG_CL_MANDATORY_CHECK->GET_FIELD_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SHOW_ALV                     TYPE        XFELD(optional)
* | [<---] YT_FIELDLIST                   TYPE        TT_FIELDLIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_field_list.

    yt_fieldlist[] = gt_fieldlist[].

    CHECK x_show_alv EQ 'X'.

    me->print_field_list( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->INIT_CUSTOMIZING
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_CUSTOMIZING_TABNAME          TYPE        STRING
* | [<---] YT_CUSTOMIZING                 TYPE        ANY
* | [EXC!] CUSTOMIZING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_customizing.

    DATA: lo_customizing TYPE REF TO data.

    FIELD-SYMBOLS: <lt_customizing> TYPE ANY TABLE,
                   <yt_customizing> TYPE ANY TABLE.

    "-------------------------------------------------


    "Check if customizing table exists
    "-------------------------------------------------
    SELECT COUNT(*)
      FROM dd02l
      WHERE tabname EQ x_customizing_tabname.
    IF sy-dbcnt EQ 0.
      MESSAGE e646(db) WITH 'Table' x_customizing_tabname 'not exists!'
        RAISING customizing_error.
    ENDIF.



    "Check if mandatory fields of the customizing table exists
    "-------------------------------------------------
    SELECT *
      FROM dd03l
      INTO TABLE @DATA(lt_dd03l)
      WHERE tabname EQ @x_customizing_tabname
      ORDER BY fieldname.

    me->init_mand_cust_struct( ).
    LOOP AT gt_mand_cust_struct ASSIGNING FIELD-SYMBOL(<mand>).

      READ TABLE lt_dd03l WITH KEY fieldname = <mand>-tabname
        BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        MESSAGE e646(db) WITH 'Missing field' <mand>-tabname 'in customizing table!'
          RAISING customizing_error.
      ENDIF.

    ENDLOOP.



    "Extracts Customizing records
    "-------------------------------------------------
    CREATE DATA lo_customizing TYPE TABLE OF (x_customizing_tabname).
    ASSIGN lo_customizing->* TO <lt_customizing>.

    SELECT *
      FROM (x_customizing_tabname)
      INTO TABLE @<lt_customizing>
      WHERE progname EQ @sy-repid
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      MESSAGE e646(db) WITH 'Table' x_customizing_tabname 'is empty!'
          RAISING customizing_error.
    ENDIF.



    "Export customizing
    "-------------------------------------------------
    CREATE DATA yt_customizing TYPE TABLE OF (x_customizing_tabname).
    ASSIGN yt_customizing->* TO <yt_customizing>.
    <yt_customizing> = <lt_customizing>.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->INIT_FIELD_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STRUCT_NAME                  TYPE        STRING
* | [EXC!] DDIC_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD init_field_list.

      DATA: ls_dummy       TYPE ty_fieldlist.

      "-------------------------------------------------

      REFRESH gt_fieldlist.

      SELECT COUNT(*)
        FROM dd02l
        WHERE tabname EQ x_struct_name.
      IF sy-dbcnt EQ 0.

        SELECT COUNT(*)
          FROM dd40l
          WHERE typename EQ x_struct_name.
        IF sy-dbcnt EQ 0.
          MESSAGE e646(db) WITH 'Structure' x_struct_name 'not exists!'
            RAISING ddic_error.
        ENDIF.
      ENDIF.

      gs_struct_name = x_struct_name.

      me->build_field_list_recurs( x_struct_name = x_struct_name
                                   x_parent      = ls_dummy ).

      IF gt_fieldlist IS NOT INITIAL.

        SELECT rollname, ddtext
          FROM dd04t
          INTO TABLE @DATA(lt_dd04t)
          FOR ALL ENTRIES IN @gt_fieldlist
          WHERE rollname   EQ @gt_fieldlist-rollname
            AND ddlanguage EQ @sy-langu.
        IF sy-subrc EQ 0.

          SORT lt_dd04t BY rollname.

          LOOP AT gt_fieldlist ASSIGNING FIELD-SYMBOL(<fieldlist>).
            READ TABLE lt_dd04t ASSIGNING FIELD-SYMBOL(<dd04t>)
              WITH KEY rollname = <fieldlist>-rollname
              BINARY SEARCH.
            CHECK sy-subrc EQ 0.
            <fieldlist>-ddtext = <dd04t>-ddtext.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->INIT_MAND_CUST_STRUCT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
        METHOD init_mand_cust_struct.

          APPEND INITIAL LINE TO gt_mand_cust_struct ASSIGNING FIELD-SYMBOL(<mand>).
          <mand>-tabname = 'PROGNAME'.

          APPEND INITIAL LINE TO gt_mand_cust_struct ASSIGNING <mand>.
          <mand>-tabname = 'COUNTER'.

          APPEND INITIAL LINE TO gt_mand_cust_struct ASSIGNING <mand>.
          <mand>-tabname = 'PARENT_PATH'.

          APPEND INITIAL LINE TO gt_mand_cust_struct ASSIGNING <mand>.
          <mand>-tabname = 'SAP_FIELD'.

        ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_MANDATORY_CHECK->PRINT_FIELD_LIST
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
        METHOD print_field_list.

          DATA: go_alv   TYPE REF TO cl_salv_table,
                lv_title TYPE lvc_title,
                lv_lines TYPE i.

          DATA: lr_columns    TYPE REF TO cl_salv_columns_table,
                lr_column     TYPE REF TO cl_salv_column_table,
                lr_functions  TYPE REF TO cl_salv_functions_list,
                lr_display    TYPE REF TO cl_salv_display_settings,
                lr_selections TYPE REF TO cl_salv_selections.

          "-------------------------------------------------

          lv_lines = lines( gt_fieldlist[] ).
          lv_title = gs_struct_name && ' (' && lv_lines && ' Record' && ')'.

          TRY.
              cl_salv_table=>factory(
                IMPORTING
                  r_salv_table = go_alv
                CHANGING
                  t_table      = gt_fieldlist[] ).

            CATCH cx_salv_msg.
          ENDTRY.

          lr_columns = go_alv->get_columns( ).
          lr_columns->set_optimize( 'X' ). "--> Optimise all columns

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'TABNAME'
              position   = '1'
          ).

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'POSITION'
              position   = '2'
          ).

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'DDTEXT'
              position   = '3'
          ).

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'FIELDNAME'
              position   = '4'
          ).

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'PATH_FIELD'
              position   = '5'
          ).

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'COMPTYPE'
              position   = '6'
          ).

          lr_columns->set_column_position(
            EXPORTING
              columnname = 'DEPTH'
              position   = '7'
          ).

*    LOOP AT lr_columns->get( ) ASSIGNING FIELD-SYMBOL(<column>).
**      <column>-r_column->set_alignment( if_salv_c_alignment=>centered ).
*      CASE <column>-columnname.
*        WHEN 'TABNAME'.
*
*      ENDCASE.
*    ENDLOOP.

          lr_functions = go_alv->get_functions( ).
          lr_functions->set_all( 'X' ).

          lr_display = go_alv->get_display_settings( ).
          lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
          lr_display->set_list_header( lv_title ).

          lr_selections = go_alv->get_selections( ).
          lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).


          go_alv->display( ).


        ENDMETHOD.
ENDCLASS.

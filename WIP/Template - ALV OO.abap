*&---------------------------------------------------------------------*
*& Include          ZAG_INCLUDE_DYNAMIC
*&---------------------------------------------------------------------*
**********************************************************************
***************************  DYNAMIC ALV TEMPLATE  ******************

" Inizializzazione (una volta sola)
"PERFORM initialize_dynamic_alv.

" Per chiamare un ALV
"PERFORM call_alv_screen USING '0100'.

" Per aggiungere nuovi ALV
"lcl_config_manager=>register_alv( 
"  iv_dynnr = '0400'
"  iv_container = 'CONTAINER_0400' 
"  iv_structure = 'MARA'
"  iv_enable_edit = abap_true
").
**********************************************************************

"TYPES - Configurazione Dinamica
*--------------------------------------------------------------------*
TYPES: BEGIN OF ts_alv_config,
         dynnr           TYPE sy-dynnr,
         container_name  TYPE scrfname,
         structure_name  TYPE dd02l-tabname,
         title_key       TYPE string,
         pf_status       TYPE string,
         data_table_ref  TYPE REF TO data,
         alv_grid_ref    TYPE REF TO cl_gui_alv_grid,
         container_ref   TYPE REF TO cl_gui_custom_container,
         enable_edit     TYPE abap_bool,
         enable_toolbar  TYPE abap_bool,
         toolbar_config  TYPE string,
       END OF ts_alv_config,
       tt_alv_config TYPE TABLE OF ts_alv_config WITH KEY dynnr.

TYPES: BEGIN OF ts_field_config,
         dynnr       TYPE sy-dynnr,
         fieldname   TYPE fieldname,
         col_pos     TYPE i,
         scrtext_s   TYPE scrtext_s,
         scrtext_m   TYPE scrtext_m, 
         scrtext_l   TYPE scrtext_l,
         outputlen   TYPE i,
         key_field   TYPE abap_bool,
         edit_field  TYPE abap_bool,
         hotspot     TYPE abap_bool,
         icon_field  TYPE abap_bool,
         hide_field  TYPE abap_bool,
       END OF ts_field_config,
       tt_field_config TYPE TABLE OF ts_field_config WITH KEY dynnr fieldname.

TYPES: BEGIN OF ts_validation_rule,
         dynnr       TYPE sy-dynnr,
         fieldname   TYPE fieldname,
         rule_type   TYPE string, "MANDATORY, NUMERIC, RANGE, CUSTOM
         rule_params TYPE string,
         error_msg   TYPE string,
       END OF ts_validation_rule,
       tt_validation_rule TYPE TABLE OF ts_validation_rule WITH KEY dynnr fieldname.

"Global Data
*--------------------------------------------------------------------*
DATA: gt_alv_config      TYPE tt_alv_config,
      gt_field_config    TYPE tt_field_config,
      gt_validation_rule TYPE tt_validation_rule,
      go_current_alv     TYPE REF TO cl_gui_alv_grid,
      go_event_handler   TYPE REF TO lcl_alv_event_dynamic.

"Dynamic ALV Event Handler Class
*--------------------------------------------------------------------*
CLASS lcl_alv_event_dynamic DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.
        
  PRIVATE SECTION.
    CLASS-METHODS:
      get_current_config RETURNING VALUE(rs_config) TYPE ts_alv_config,
      validate_field_data IMPORTING iv_fieldname TYPE fieldname
                                   iv_value     TYPE any
                                   iv_dynnr     TYPE sy-dynnr
                         RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

"ALV Factory Class
*--------------------------------------------------------------------*
CLASS lcl_alv_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_alv IMPORTING iv_dynnr TYPE sy-dynnr
                 RETURNING VALUE(ro_alv) TYPE REF TO cl_gui_alv_grid,
      
      build_fieldcat IMPORTING iv_dynnr TYPE sy-dynnr
                               iv_structure TYPE dd02l-tabname
                     RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat,
      
      setup_layout IMPORTING ir_data_table TYPE REF TO data
                   RETURNING VALUE(rs_layout) TYPE lvc_s_layo,
      
      register_events IMPORTING io_alv TYPE REF TO cl_gui_alv_grid.
      
  PRIVATE SECTION.
    CLASS-METHODS:
      apply_field_config IMPORTING iv_dynnr TYPE sy-dynnr
                        CHANGING ct_fcat TYPE lvc_t_fcat.
ENDCLASS.

"Configuration Manager Class
*--------------------------------------------------------------------*
CLASS lcl_config_manager DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      initialize_config,
      
      register_alv IMPORTING iv_dynnr TYPE sy-dynnr
                            iv_container TYPE scrfname
                            iv_structure TYPE dd02l-tabname
                            iv_title TYPE string OPTIONAL
                            iv_enable_edit TYPE abap_bool DEFAULT abap_false,
      
      add_field_config IMPORTING is_field_config TYPE ts_field_config,
      
      add_validation_rule IMPORTING is_validation TYPE ts_validation_rule,
      
      get_alv_config IMPORTING iv_dynnr TYPE sy-dynnr
                     RETURNING VALUE(rs_config) TYPE ts_alv_config.
ENDCLASS.

*--------------------------------------------------------------------*
* IMPLEMENTATIONS
*--------------------------------------------------------------------*

"Configuration Manager Implementation
*--------------------------------------------------------------------*
CLASS lcl_config_manager IMPLEMENTATION.
  
  METHOD initialize_config.
    CLEAR: gt_alv_config, gt_field_config, gt_validation_rule.
    
    " Esempio di configurazione default
    PERFORM setup_default_configurations.
  ENDMETHOD.
  
  METHOD register_alv.
    DATA: ls_config TYPE ts_alv_config.
    
    ls_config-dynnr = iv_dynnr.
    ls_config-container_name = iv_container.
    ls_config-structure_name = iv_structure.
    ls_config-title_key = iv_title.
    ls_config-enable_edit = iv_enable_edit.
    ls_config-pf_status = 'ZPF_GENERIC'.
    
    APPEND ls_config TO gt_alv_config.
  ENDMETHOD.
  
  METHOD add_field_config.
    APPEND is_field_config TO gt_field_config.
  ENDMETHOD.
  
  METHOD add_validation_rule.
    APPEND is_validation TO gt_validation_rule.
  ENDMETHOD.
  
  METHOD get_alv_config.
    READ TABLE gt_alv_config INTO rs_config WITH KEY dynnr = iv_dynnr.
  ENDMETHOD.
  
ENDCLASS.

"ALV Factory Implementation
*--------------------------------------------------------------------*
CLASS lcl_alv_factory IMPLEMENTATION.
  
  METHOD create_alv.
    DATA: ls_config    TYPE ts_alv_config,
          lo_container TYPE REF TO cl_gui_custom_container,
          lt_fcat      TYPE lvc_t_fcat,
          ls_layout    TYPE lvc_s_layo.
    
    " Get configuration
    ls_config = lcl_config_manager=>get_alv_config( iv_dynnr ).
    
    " Create container and ALV
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
      CREATE OBJECT lo_container
        EXPORTING container_name = ls_config-container_name.
      CREATE OBJECT ro_alv
        EXPORTING i_parent = lo_container.
    ELSE.
      CREATE OBJECT ro_alv
        EXPORTING i_parent = cl_gui_alv_grid=>default_screen.
    ENDIF.
    
    " Build fieldcat and layout
    lt_fcat = build_fieldcat( iv_dynnr = iv_dynnr 
                              iv_structure = ls_config-structure_name ).
    ls_layout = setup_layout( ls_config-data_table_ref ).
    
    " Display ALV
    CALL METHOD ro_alv->set_table_for_first_display
      EXPORTING
        i_buffer_active = 'X'
        i_save = 'A'
        is_layout = ls_layout
      CHANGING
        it_outtab = ls_config-data_table_ref
        it_fieldcatalog = lt_fcat.
    
    " Register events if needed
    IF ls_config-enable_edit = abap_true OR 
       ls_config-enable_toolbar = abap_true.
      register_events( ro_alv ).
    ENDIF.
    
  ENDMETHOD.
  
  METHOD build_fieldcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = iv_structure
      CHANGING
        ct_fieldcat = rt_fcat.
    
    " Apply custom field configurations
    apply_field_config( EXPORTING iv_dynnr = iv_dynnr 
                       CHANGING ct_fcat = rt_fcat ).
  ENDMETHOD.
  
  METHOD apply_field_config.
    FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
    
    LOOP AT ct_fcat ASSIGNING <ls_fcat>.
      READ TABLE gt_field_config INTO DATA(ls_field_config)
        WITH KEY dynnr = iv_dynnr fieldname = <ls_fcat>-fieldname.
      
      IF sy-subrc = 0.
        IF ls_field_config-col_pos > 0.
          <ls_fcat>-col_pos = ls_field_config-col_pos.
        ENDIF.
        IF ls_field_config-scrtext_s IS NOT INITIAL.
          <ls_fcat>-scrtext_s = ls_field_config-scrtext_s.
        ENDIF.
        " Altri campi...
        <ls_fcat>-edit = ls_field_config-edit_field.
        <ls_fcat>-hotspot = ls_field_config-hotspot.
        <ls_fcat>-no_out = ls_field_config-hide_field.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD setup_layout.
    rs_layout-zebra = 'X'.
    rs_layout-sel_mode = 'A'.
    rs_layout-cwidth_opt = 'X'.
    
    " Check if data structure has color/style fields
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_row> TYPE any.
    
    ASSIGN ir_data_table->* TO <lt_table>.
    IF sy-subrc = 0 AND lines( <lt_table> ) > 0.
      ASSIGN <lt_table>[ 1 ] TO <ls_row>.
      
      ASSIGN COMPONENT 'C_COL' OF STRUCTURE <ls_row> 
             TO FIELD-SYMBOL(<color>).
      IF sy-subrc = 0.
        rs_layout-ctab_fname = 'C_COL'.
      ENDIF.
      
      ASSIGN COMPONENT 'C_STY' OF STRUCTURE <ls_row> 
             TO FIELD-SYMBOL(<style>).
      IF sy-subrc = 0.
        rs_layout-stylefname = 'C_STY'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  
  METHOD register_events.
    " Register edit events if editing is enabled
    io_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
    io_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    
    " Create and set event handlers
    IF go_event_handler IS INITIAL.
      CREATE OBJECT go_event_handler.
    ENDIF.
    
    SET HANDLER go_event_handler->handle_toolbar FOR io_alv.
    SET HANDLER go_event_handler->handle_user_command FOR io_alv.
    SET HANDLER go_event_handler->handle_hotspot_click FOR io_alv.
    SET HANDLER go_event_handler->handle_data_changed FOR io_alv.
  ENDMETHOD.
  
ENDCLASS.

"Event Handler Implementation
*--------------------------------------------------------------------*
CLASS lcl_alv_event_dynamic IMPLEMENTATION.
  
  METHOD handle_data_changed.
    DATA: ls_config TYPE ts_alv_config.
    
    ls_config = get_current_config( ).
    
    " Validate each changed cell
    LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_cell).
      IF validate_field_data( iv_fieldname = ls_cell-fieldname
                             iv_value = ls_cell-value
                             iv_dynnr = ls_config-dynnr ) = abap_false.
        
        er_data_changed->add_protocol_entry(
          i_msgid = 'ZZ'
          i_msgno = '001'
          i_msgty = 'E'
          i_msgv1 = |Invalid value for { ls_cell-fieldname }|
          i_fieldname = ls_cell-fieldname
        ).
      ENDIF.
    ENDLOOP.
    
    " Display protocol if errors exist
    IF er_data_changed->mt_protocol IS NOT INITIAL.
      er_data_changed->display_protocol( ).
    ENDIF.
  ENDMETHOD.
  
  METHOD handle_user_command.
    " Generic command handling based on configuration
    CASE e_ucomm.
      WHEN '&SAVE'.
        PERFORM save_alv_data.
      WHEN '&REFRESH'.
        PERFORM refresh_alv_data.
      WHEN OTHERS.
        " Custom command handling
        PERFORM handle_custom_command USING e_ucomm.
    ENDCASE.
  ENDMETHOD.
  
  METHOD handle_toolbar.
    DATA: ls_config TYPE ts_alv_config.
    
    ls_config = get_current_config( ).
    
    " Add dynamic toolbar buttons based on configuration
    PERFORM build_dynamic_toolbar CHANGING e_object ls_config.
  ENDMETHOD.
  
  METHOD handle_hotspot_click.
    PERFORM handle_dynamic_hotspot USING e_row_id e_column_id es_row_no.
  ENDMETHOD.
  
  METHOD get_current_config.
    READ TABLE gt_alv_config INTO rs_config WITH KEY dynnr = sy-dynnr.
  ENDMETHOD.
  
  METHOD validate_field_data.
    READ TABLE gt_validation_rule INTO DATA(ls_rule)
      WITH KEY dynnr = iv_dynnr fieldname = iv_fieldname.
    
    IF sy-subrc = 0.
      CASE ls_rule-rule_type.
        WHEN 'MANDATORY'.
          rv_valid = COND #( WHEN iv_value IS NOT INITIAL THEN abap_true
                            ELSE abap_false ).
        WHEN 'NUMERIC'.
          PERFORM check_numeric USING iv_value CHANGING rv_valid.
        WHEN 'CUSTOM'.
          PERFORM validate_custom_rule USING ls_rule iv_value 
                                      CHANGING rv_valid.
        WHEN OTHERS.
          rv_valid = abap_true.
      ENDCASE.
    ELSE.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.
  
ENDCLASS.

*--------------------------------------------------------------------*
* FORMS - Interfaccia semplificata
*--------------------------------------------------------------------*

"Inizializzazione del sistema
FORM initialize_dynamic_alv.
  lcl_config_manager=>initialize_config( ).
ENDFORM.

"Chiamata screen semplificata
FORM call_alv_screen USING iv_dynnr TYPE sy-dynnr.
  DATA: lo_alv TYPE REF TO cl_gui_alv_grid.
  
  " Create ALV using factory
  lo_alv = lcl_alv_factory=>create_alv( iv_dynnr ).
  go_current_alv = lo_alv.
  
  CALL SCREEN iv_dynnr.
ENDFORM.

"Configurazione default degli ALV
FORM setup_default_configurations.
  
  " Registra ALV per screen 100
  lcl_config_manager=>register_alv( 
    iv_dynnr = '0100'
    iv_container = 'CONTAINER_0100'
    iv_structure = 'T001'
    iv_title = 'Company Codes'
    iv_enable_edit = abap_true
  ).
  
  " Aggiungi configurazione campi per screen 100
  lcl_config_manager=>add_field_config( VALUE #(
    dynnr = '0100'
    fieldname = 'BUKRS'
    col_pos = 1
    scrtext_s = 'Co.Code'
    key_field = abap_true
    hotspot = abap_true
  ) ).
  
  " Aggiungi regola validazione
  lcl_config_manager=>add_validation_rule( VALUE #(
    dynnr = '0100'
    fieldname = 'BUKRS'
    rule_type = 'MANDATORY'
    error_msg = 'Company Code is mandatory'
  ) ).
  
ENDFORM.

*--------------------------------------------------------------------*
* FORM aggiuntive per gestione dinamica
*--------------------------------------------------------------------*

FORM save_alv_data.
  " Implementazione salvataggio generica
  MESSAGE 'Data saved successfully' TYPE 'S'.
ENDFORM.

FORM refresh_alv_data.
  " Implementazione refresh generica
  go_current_alv->refresh_table_display( ).
ENDFORM.

FORM handle_custom_command USING iv_command TYPE sy-ucomm.
  " Gestione comandi personalizzati basata su configurazione
ENDFORM.

FORM build_dynamic_toolbar CHANGING co_object TYPE REF TO cl_alv_event_toolbar_set
                                   cs_config TYPE ts_alv_config.
  " Costruzione toolbar dinamica basata su configurazione
ENDFORM.

FORM handle_dynamic_hotspot USING is_row_id TYPE lvc_s_row
                                  is_column_id TYPE lvc_s_col  
                                  is_row_no TYPE lvc_s_roid.
  " Gestione hotspot dinamica
ENDFORM.

FORM check_numeric USING iv_value TYPE any
                   CHANGING cv_valid TYPE abap_bool.
  " Validazione numerica
  cv_valid = abap_true. "Implementare logica
ENDFORM.

FORM validate_custom_rule USING is_rule TYPE ts_validation_rule
                                iv_value TYPE any
                          CHANGING cv_valid TYPE abap_bool.
  " Validazione regole custom
  cv_valid = abap_true. "Implementare logica
ENDFORM.
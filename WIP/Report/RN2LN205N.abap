*&---------------------------------------------------------------------*
*& Report  rn2ln205n                                                   *
*&---------------------------------------------------------------------*
*& siehe Programmdokumentation                                rswatch0 *
*&---------------------------------------------------------------------*
*
REPORT rn2ln205n MESSAGE-ID n2pc.
*
TYPE-POOLS icon.
TYPE-POOLS n2pqv.
*
CONSTANTS c_separator(1)     TYPE c VALUE '/'.              "#EC NOTEXT
CONSTANTS c_backslash(1)     TYPE c VALUE '\'.              "#EC NOTEXT
CONSTANTS c_start_dir(9)     TYPE c VALUE '/usr/sap/'.      "#EC NOTEXT
CONSTANTS c_ftp_user(64)     TYPE c VALUE 'ftp'.            "#EC NOTEXT
CONSTANTS c_ftp_password(64) TYPE c VALUE 'ftp'.            "#EC NOTEXT
CONSTANTS c_ftp_host(64)     TYPE c
                             VALUE 'sapserv3.wdf.sap.corp'. "#EC NOTEXT
* Define reference variables
DATA g_alv_tree              TYPE REF TO cl_gui_alv_tree.
DATA g_custom_container      TYPE REF TO cl_gui_custom_container.
DATA g_toolbar               TYPE REF TO cl_gui_toolbar.
*
DATA g_return_code           TYPE i.
DATA ok_code                 TYPE sy-ucomm.
DATA save_ok                 TYPE sy-ucomm.                   "OK-Code
DATA g_repid                 TYPE sy-repid.
*
*                            "for Windows position default values
DATA gs_top                  TYPE i VALUE 100. "view start
DATA gs_left                 TYPE i VALUE 100.
DATA gs_count                TYPE i VALUE   0.
*
TYPES:
  BEGIN OF  t_file_list,          "Globaler Tree mit allen Information
    g_node                   TYPE lvc_nkey,
    g_parent                 TYPE lvc_nkey,
    type(1)                  TYPE c, "Typ D=directory, F=file, S=special
    name                     TYPE rlgrap-filename,  "without dir
    len(8)                   TYPE c,    "length in bytes max. or > 10 MB
    owner(8)                 TYPE c,    "owner of the entry.
    mode(9)                  TYPE c,    "rwx-r-x--x":protection mode
    date                     TYPE d,
    time(8)                  TYPE c,    "hh:mm:ss
    full_name                TYPE string,
  END   OF  t_file_list.
DATA:
   gt_t_file_list            TYPE TABLE OF t_file_list,
   gw_t_file_list            TYPE          t_file_list.
*
DATA gd_dirname              TYPE rlgrap-filename.
DATA g_edit_filename         TYPE rlgrap-filename.          "Dynpro 200
DATA g_command(80)           TYPE c.                        "Dynpro 210
DATA g_current_file          TYPE rlgrap-filename.          "delete file
*
DATA g_sys_code_page         TYPE cpcodepage.
DATA conv                    TYPE REF TO cl_abap_conv_uc_number.
*
DATA g_exit_buttons          TYPE TABLE OF gui_code.
*
DATA g_ftp_handle            TYPE i.
DATA g_ftp_user(64)          TYPE c VALUE 'ftp'.
DATA g_ftp_user_test(64)     TYPE c.      "check set/get params
DATA g_ftp_password(64)      TYPE c VALUE 'ftp'.
DATA g_ftp_host(64)          TYPE c
                             VALUE 'sapserv3.wdf.sap.corp'. "#EC NOTEXT
*
*###############################################################
* LOCAL CLASSES
*###############################################################
*Local class for event handling
*
CLASS lcl_tree_event_receiver DEFINITION FINAL.
*
  PUBLIC SECTION.
*Event handler method for each event to react to.
    METHODS handle_node_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key sender.
    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING node_key fieldname sender.
*
* 'sender' is an implicit event parameter that is provided by
* ABAP Objects runtime system. It contains a reference to the
* object that fired the event. You may directly use it to
* call methods of this instance.
    METHODS handle_button_click
      FOR EVENT button_click OF cl_gui_alv_tree
      IMPORTING node_key sender.
*
    METHODS handle_link_click
      FOR EVENT link_click OF cl_gui_alv_tree
      IMPORTING node_key sender.
*
    METHODS handle_expand_nc
      FOR EVENT expand_nc OF cl_gui_alv_tree
      IMPORTING node_key sender.                            "#EC NEEDED
*
    METHODS on_function_selected
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode.
*
ENDCLASS.                      "lcl_tree_event_receiver DEFINITION.
*##################################################################
*******************************************************************
**
CLASS lcl_tree_event_receiver IMPLEMENTATION.
*Implement event handler methods.
**
  METHOD handle_node_double_click.
*
    DATA: lt_children TYPE lvc_t_nkey.
*first check if the node is a leaf, i.e. can not be expanded
*
    READ TABLE gt_t_file_list
      INTO     gw_t_file_list
      WITH KEY g_node = node_key.
    IF gw_t_file_list-type <> 'D'.     "only files
      PERFORM handle_double_click USING node_key.
      RETURN.
    ENDIF.
*
    CALL METHOD sender->get_children
      EXPORTING
        i_node_key  = node_key
      IMPORTING
        et_children = lt_children.
*
    IF NOT lt_children IS INITIAL.
      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ELSE.
      PERFORM handle_double_click USING node_key.
      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ENDIF.
*
  ENDMETHOD.                    "handle_node_double_click
**
**
**
  METHOD handle_item_double_click.
*
    IF fieldname = '&Hierarchy'.
      CALL METHOD me->handle_node_double_click
        EXPORTING
          node_key = node_key
          sender   = sender.
    ENDIF.
*
  ENDMETHOD.                    "handle_item_double_click
**
**
**
  METHOD handle_button_click.
    DATA: lt_children TYPE lvc_t_nkey.
*
    CALL METHOD sender->get_children
      EXPORTING
        i_node_key  = node_key
      IMPORTING
        et_children = lt_children.
    IF NOT lt_children IS INITIAL.
      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ENDIF.
*
    PERFORM handle_double_click USING node_key.
  ENDMETHOD.                    "handle_button_click
**
**
**
  METHOD handle_link_click.
    DATA: lt_children TYPE lvc_t_nkey.
*
    CALL METHOD sender->get_children
      EXPORTING
        i_node_key  = node_key
      IMPORTING
        et_children = lt_children.
    IF NOT lt_children IS INITIAL.
      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ENDIF.
*
    PERFORM handle_double_click USING node_key.
  ENDMETHOD.                    "handle_link_click
**
**
**
  METHOD handle_expand_nc.
*
    PERFORM handle_double_click USING node_key.
    PERFORM dynpro_0100_write.
*
  ENDMETHOD.                    "handle_expand_nc
**
**
**
  METHOD on_function_selected.
*
*  Query the function codes of the toolbar, definded in change_toolbar
    CASE fcode.
      WHEN 'SHOW'.                     "like double click
        PERFORM show_file_view.
*
      WHEN 'HEXS'.
        PERFORM show_hex_view.
*
      WHEN 'NEWFILE'.
        PERFORM write_newfile.
*
      WHEN 'DELETE'.
        PERFORM delete_file.
*
      WHEN 'GETFILE'.
        PERFORM get_files.
*
      WHEN 'PUTFILE'.
        PERFORM put_files.
*
      WHEN 'EDITFILE'.
        PERFORM edit_file.
*
      WHEN OTHERS.
        RETURN.
*
    ENDCASE.
*
  ENDMETHOD.                    "on_function_selected
**
ENDCLASS.                  "lcl_tree_event_receiver IMPLEMENTATION.
*##################################################################
**
**
TYPES:
  BEGIN OF tt_file_list,
    type(1)               TYPE c, "Typ D=directory, F=file, S=special
    name(70)              TYPE c, "name of entry. (possibly truncated)
    len(8)                TYPE c, "length in bytes max. or > 10 MB
    owner(8)              TYPE c, "owner of the entry.
    mode(9)               TYPE c, "like "rwx-r-x--x": protection mode.
    date                  TYPE d, "jjjjmmdd
    time(8)               TYPE c, "hh:mm:ss
    full_name             TYPE string,
  END OF tt_file_list.
DATA gt_file_list         TYPE TABLE OF tt_file_list.
DATA gw_file_list         TYPE tt_file_list.
*
TYPES:
  BEGIN OF s_file_list,                "Show Tree
    mode(9)               TYPE c,
    owner(8)              TYPE c,
    date                  TYPE d,
    time(8)               TYPE c,      " hh:mm:ss
    len(8)                TYPE c,
  END   OF s_file_list.
DATA:
   gt_s_file_list         TYPE TABLE OF s_file_list,
   wa_s_file_list         TYPE          s_file_list.
*
DATA g_timezone_sec(5)    TYPE p.      "seconds local time
"is later than GMT
************************************************************************
*
END-OF-SELECTION.
************************************************************************
INITIALIZATION.
  PERFORM get_codepage.
  CLASS cl_abap_conv_uc_number DEFINITION LOAD.
  IF conv IS INITIAL.
    CREATE OBJECT conv
      EXPORTING
        im_source_codepage = g_sys_code_page.    "'4102' => Hex.
  ENDIF.
  GET PARAMETER ID 'G_FTP_USER'        FIELD g_ftp_user_test."#EC EXISTS
  IF g_ftp_user_test IS INITIAL.
    SET PARAMETER ID 'G_FTP_USER'      FIELD c_ftp_user.    "#EC EXISTS
    SET PARAMETER ID 'G_FTP_PASSWORD'  FIELD c_ftp_password."#EC EXISTS
    SET PARAMETER ID 'G_FTP_HOST'      FIELD c_ftp_host.    "#EC EXISTS
    MOVE c_ftp_user                    TO g_ftp_user.
    MOVE c_ftp_password                TO g_ftp_password.
    MOVE c_ftp_host                    TO g_ftp_host.
  ENDIF.
  IF gd_dirname IS INITIAL.
    GET PARAMETER ID 'GD_DIRNAME'      FIELD gd_dirname.    "#EC EXISTS
  ENDIF.
*
************************************************************************
START-OF-SELECTION.
  PERFORM check_ta.
  CALL SCREEN 100.
  LEAVE PROGRAM.
*
************************************************************************
* Es wird eine Liste von Dateinamen in die Tabelle FILE_LIST gelesen.
************************************************************************
FORM get_file_list.
*
  CONSTANTS c_generic_name(1)              TYPE c VALUE '*'.
*
  DATA:
    l_sum_error                            TYPE i,
    l_errno(3)                             TYPE c,
    l_errmsg(40)                           TYPE c.
*
  DATA:
    BEGIN OF file,
      name(128)   TYPE c, " name of entry. (possibly truncated.)
      type(10)    TYPE c,              " type of entry.
      len(8)      TYPE p,              " length in bytes.
      owner(8)    TYPE c,              " owner of the entry.
      mtime(6)    TYPE p, " last modification date, seconds since 1970
      mode(9)     TYPE c, " like "rwx-r-x--x": protection mode.
      mod_date    TYPE d,
      mod_time(8) TYPE c,              " hh:mm:ss
    END OF file.
*
  PERFORM set_gd_dirname.
  CALL 'C_DIR_READ_FINISH'             "just to be sure    "#EC CI_CCALL
      ID 'ERRNO'  FIELD l_errno
      ID 'ERRMSG' FIELD l_errmsg.
  IF sy-subrc = 0 OR sy-subrc = 3.
  ELSE.
    MESSAGE ID 'NG' TYPE 'I' NUMBER '799'
      WITH sy-subrc text-e01 gd_dirname l_errmsg.
  ENDIF.
  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD gd_dirname    "#EC CI_CCALL
                          ID 'FILE'   FIELD c_generic_name
                          ID 'ERRNO'  FIELD l_errno
                          ID 'ERRMSG' FIELD l_errmsg.
  IF sy-subrc <> 0.
    MESSAGE ID 'NG' TYPE 'I' NUMBER '799'
    WITH sy-subrc text-e02 gd_dirname l_errmsg.
  ENDIF.
  CLEAR l_sum_error.
  CLEAR gt_file_list.
  REFRESH gt_file_list.
  DO.
    IF l_sum_error > 10.
      MOVE text-m01          TO gw_file_list-name.
      APPEND gw_file_list    TO gt_file_list.
      EXIT.
    ENDIF.
    CLEAR file.
    CALL 'C_DIR_READ_NEXT'                                "#EC CI_CCALL
      ID 'TYPE'   FIELD file-type
      ID 'NAME'   FIELD file-name
      ID 'LEN'    FIELD file-len
      ID 'OWNER'  FIELD file-owner
      ID 'MTIME'  FIELD file-mtime
      ID 'MODE'   FIELD file-mode
      ID 'ERRNO'  FIELD l_errno
      ID 'ERRMSG' FIELD l_errmsg.
    CASE sy-subrc.
      WHEN 0.                                               " or 4.
        CLEAR: l_errno, l_errmsg.
        CASE file-type(1).
          WHEN 'F'.                      " normal file.
            MOVE 'F'                   TO gw_file_list-type.
          WHEN 'f'.                      " normal file.
            MOVE 'F'                   TO gw_file_list-type.
          WHEN 'D'.                      " directory
            MOVE 'D'                   TO gw_file_list-type.
          WHEN 'd'.                      " directory
            MOVE 'D'                   TO gw_file_list-type.
          WHEN OTHERS.                   " device, fifo, socket,...
            MOVE 'S'                   TO gw_file_list-type.
        ENDCASE.
        IF ( file-name = '.' OR file-name = '..' ).
        ELSE.
          PERFORM p6_to_date_time IN PROGRAM rstr0400
            USING file-mtime g_timezone_sec
                  file-mod_time
                  file-mod_date.
          MOVE file-mode               TO gw_file_list-mode.
          MOVE file-owner              TO gw_file_list-owner.
          MOVE file-mod_date           TO gw_file_list-date.
          MOVE file-mod_time           TO gw_file_list-time.
          IF file-len > 9999999.
            MOVE '>10 MB'              TO gw_file_list-len.
          ELSE.
            WRITE file-len             TO gw_file_list-len.
          ENDIF.
          MOVE file-name               TO gw_file_list-name.
          CONCATENATE gd_dirname
                      file-name
                 INTO gw_file_list-full_name.
          APPEND gw_file_list TO gt_file_list.
        ENDIF.
      WHEN 1.                          "End of List
        EXIT.                          "exit do loop
      WHEN OTHERS.
        ADD 1                          TO l_sum_error.
    ENDCASE.
  ENDDO.
  CALL 'C_DIR_READ_FINISH'                                "#EC CI_CCALL
      ID 'ERRNO'  FIELD l_errno
      ID 'ERRMSG' FIELD l_errmsg.
  IF sy-subrc = 0 OR sy-subrc = 4.
  ELSE.
    MESSAGE ID 'NG' TYPE 'I' NUMBER '799'
    WITH sy-subrc text-e03 gd_dirname l_errmsg.
  ENDIF.
  SORT gt_file_list BY type ASCENDING name ASCENDING time DESCENDING.
*
ENDFORM.                               "get_file_list
*
************************************************************************
*           prepare time zone correction.
************************************************************************
FORM prepare_time_zone.                "g_timezone_sec ist global
*
  DATA timezone_name(7)                  TYPE c.
  CALL 'C_GET_TIMEZONE' ID 'NAME' FIELD timezone_name     "#EC CI_CCALL
                        ID 'SEC'  FIELD g_timezone_sec.
  g_timezone_sec = 0 - sy-tzone.
  IF sy-dayst = 'X'.
    SUBTRACT 3600 FROM g_timezone_sec.
  ENDIF.
*
ENDFORM.                               "prepare_time_zone
*
*&---------------------------------------------------------------------*
*&      Module  GET_FILENAME  OUTPUT
*&---------------------------------------------------------------------*
MODULE get_filename OUTPUT.
*
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR  'TITLE_0200'.
  IF g_edit_filename IS INITIAL.
    GET PARAMETER ID 'G_EDIT_FILENAME'
               FIELD g_edit_filename.                       "#EC EXISTS
  ENDIF.
  MODIFY SCREEN.
*
ENDMODULE.                             " GET_FILENAME  OUTPUT
*
*&---------------------------------------------------------------------*
*&      Module  GET_FILENAME  INPUT
*&---------------------------------------------------------------------*
MODULE get_filename INPUT.
*
  SET PARAMETER ID 'G_EDIT_FILENAME' FIELD g_edit_filename. "#EC EXISTS
  CASE sy-ucomm.
    WHEN 'ENTE'.
      MOVE 1 TO g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'BACK'.
      CLEAR g_edit_filename.
      CLEAR g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'ALLE'.
      MOVE 2 TO g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.
      CLEAR g_edit_filename.
      CLEAR g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.
*
ENDMODULE.                             " GET_FILENAME  INPUT
*
*&---------------------------------------------------------------------*
*&      Module  GET_COMMAND  OUTPUT
*&---------------------------------------------------------------------*
MODULE get_command OUTPUT.
*
  SET PF-STATUS 'STATUS_0210'.
  SET TITLEBAR  'TITLE_0210'.
  GET PARAMETER ID 'G_COMMAND' FIELD g_command.             "#EC EXISTS
  MODIFY SCREEN.
*
ENDMODULE.                             " GET_Command  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_COMMAND   INPUT
*&---------------------------------------------------------------------*
MODULE get_command INPUT.
*
  SET PARAMETER ID 'G_COMMAND' FIELD g_command.             "#EC EXISTS
  CASE sy-ucomm.
    WHEN 'ENTE'.
      MOVE 1 TO g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'BACK'.
      CLEAR g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.
      CLEAR g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.
*
ENDMODULE.                             " GET_command  INPUT
*
*&---------------------------------------------------------------------*
*&      Module  pbo_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*
  SET TITLEBAR  'TITLE_0100'.
  SET PF-STATUS 'STATUS_0100' EXCLUDING g_exit_buttons.
  GET PARAMETER ID 'GD_DIRNAME' FIELD gd_dirname.           "#EC EXISTS
*
  IF g_alv_tree IS INITIAL.
    PERFORM init_tree.
*
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Fehler in: Automation Queue'(801)
          txt1  = 'Interner Fehler'(802)
          txt2  = 'Eine Methode der Automation Queue'(803)
          txt3  = 'verursachte einen Fehler.'(804).
    ENDIF.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
*
ENDMODULE.                             " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  pai_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
*
  save_ok = ok_code.
  CLEAR ok_code.
*
  CASE save_ok.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.
      LEAVE PROGRAM.
    WHEN 'SET_GD_DIRNAME'.
      RETURN.
*
    WHEN 'ENTE'.
      PERFORM build_new_tree.
      SET PARAMETER ID 'GD_DIRNAME' FIELD gd_dirname.       "#EC EXISTS
*
    WHEN 'SHOW'.                       "like double click
      PERFORM show_file_view.
*
    WHEN 'HEXS'.
      PERFORM show_hex_view.
*
    WHEN 'NEWFILE'.
      PERFORM write_newfile.
*
    WHEN 'COMMAND'.
      PERFORM call_user_command.
*
    WHEN 'DELETE'.
      PERFORM delete_file.
*
    WHEN 'GETFILE'.
      PERFORM get_files.
*
    WHEN 'PUTFILE'.
      PERFORM put_files.
*
    WHEN 'EDITFILE'.
      PERFORM edit_file.
*
    WHEN 'FTPOPEN'.
      PERFORM ftp_open.
    WHEN 'FTPCMD'.
      PERFORM ftp_cmd.
    WHEN 'FTPCLOSE'.
      PERFORM ftp_close.
    WHEN 'FTPDIR'.
      PERFORM ftp_dir.
    WHEN 'FTPGET'.
      PERFORM ftp_get.
    WHEN 'FTPPUT'.
      PERFORM ftp_put.
*
    WHEN OTHERS.
*  Call dispatch to process toolbar functions
      CALL METHOD cl_gui_cfw=>dispatch
        IMPORTING
          return_code = g_return_code.
      IF g_return_code EQ cl_gui_cfw=>rc_noevent.
        RETURN.
      ENDIF.
*
  ENDCASE.
*
  CALL METHOD cl_gui_cfw=>dispatch
    IMPORTING
      return_code = g_return_code.
  IF g_return_code EQ cl_gui_cfw=>rc_noevent.
    RETURN.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
*
ENDMODULE.                             " USER_COMMAND_0100  INPUT
*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0220 OUTPUT.
*
  SET PF-STATUS 'STATUS_0220'.
  SET TITLEBAR  'TITLE_0220'.
  GET PARAMETER ID 'G_FTP_USER'     FIELD g_ftp_user.       "#EC EXISTS
  GET PARAMETER ID 'G_FTP_PASSWORD' FIELD g_ftp_password.   "#EC EXISTS
  GET PARAMETER ID 'G_FTP_HOST'     FIELD g_ftp_host.       "#EC EXISTS
*
ENDMODULE.                 " STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.
*
  SET PARAMETER ID 'G_FTP_USER'     FIELD g_ftp_user.       "#EC EXISTS
  SET PARAMETER ID 'G_FTP_PASSWORD' FIELD g_ftp_password.   "#EC EXISTS
  SET PARAMETER ID 'G_FTP_HOST'     FIELD g_ftp_host.       "#EC EXISTS
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'OK'.
      MOVE 1 TO g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.
      CLEAR g_return_code.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.
*
ENDMODULE.                 " USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_program.
*
  FREE g_alv_tree.
  CALL METHOD g_custom_container->free.
  FREE g_custom_container.
  IF g_ftp_handle IS NOT INITIAL.
    PERFORM ftp_close.
  ENDIF.
*
ENDFORM.                               " exit_program
*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.
*
  DATA:
    l_tree_container_name(30) TYPE c.
*
  PERFORM check_ta.
* Create ALV Tree Control and corresponding Container.
*
* create container for alv-tree
  l_tree_container_name = 'G_CONTAINER'.
*
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = l_tree_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc <> 0.       "Dump
    MESSAGE ID 'N2PC' TYPE 'X' NUMBER '102'
       WITH 'Fehler beim Erzeugen des Containers'(100) sy-subrc.
  ENDIF.
*
  CREATE OBJECT g_alv_tree
    EXPORTING
*      LIFETIME               =
      parent                  = g_custom_container
*      SHELLSTYLE             =
      node_selection_mode     =
                             cl_gui_column_tree=>node_sel_mode_multiple
*      HIDE_SELECTION         =
      item_selection          = 'X'
      no_toolbar              = ' '
      no_html_header          = 'X'
      i_print                 = ' '     "initial Print
    EXCEPTIONS
      cntl_error              = 1
      cntl_system_error       = 2
      create_error            = 3
      lifetime_error          = 4
      illegal_node_selection_mode = 5
      failed                  = 6
      illegal_column_name     = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.                             "Dump
    MESSAGE ID 'N2PC' TYPE 'X' NUMBER '102'
      WITH 'Fehler beim Erzeugen des Baums'(101) sy-subrc.
  ENDIF.
*
* Extend funtions of standard toolbar
* befor the generic Funktion call for first display
  PERFORM change_toolbar USING 'X'.    "Add user buttons
*
  PERFORM create_empty_tree USING 'X'.
  CLEAR gt_t_file_list. REFRESH gt_t_file_list.
  CLEAR gt_s_file_list. REFRESH gt_s_file_list.
* Create hierarchy (nodes and leaves)
  PERFORM create_hierarchy USING ' '.  "Add first
*
  PERFORM register_events.
*
  PERFORM change_toolbar USING space.  "Delete buttons standart
*
* Send data to frontend.
  CALL METHOD g_alv_tree->frontend_update.
* and wait for automatic flush at end of pbo
*
ENDFORM.                               " init_tree
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM build_hierarchy_header
     CHANGING p_hierarchy_header TYPE treev_hhdr.
*
  p_hierarchy_header-heading   = 'Verzeichnis/Dateien'(300).
  p_hierarchy_header-tooltip   = 'Aktuelle Ansicht'(400).
  p_hierarchy_header-width     = 80.              "length node Dir/File
  p_hierarchy_header-width_pix = ' '.
*
ENDFORM.                               " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM build_field_catalog
     CHANGING p_fieldcat TYPE lvc_t_fcat.
*
  DATA ls_fcat           TYPE lvc_s_fcat.
*
  ls_fcat-fieldname   = 'MODE'.
  ls_fcat-inttype     =  'C'.
  ls_fcat-outputlen   =  10.
  ls_fcat-coltext     =  'Mode'(f01).
  ls_fcat-seltext     =  'Berechtigung'(f02).
  ls_fcat-col_pos     = 1.
  ls_fcat-fix_column  = 'X'.
  INSERT ls_fcat INTO p_fieldcat INDEX 1.
*
  ls_fcat-fieldname   = 'OWNER'.
  ls_fcat-inttype     =  'C'.
  ls_fcat-outputlen   =  15.
  ls_fcat-coltext     =  'User'(f03).
  ls_fcat-seltext     =  'Benutzer'(f04).
  ls_fcat-col_pos     = 2.
  INSERT ls_fcat INTO p_fieldcat INDEX 2.
*
  ls_fcat-fieldname   = 'DATE'.
  ls_fcat-inttype     =  'D'.
  ls_fcat-outputlen   =  16.
  ls_fcat-coltext     =  'Datum'(f05).
  ls_fcat-seltext     =  'Datum'(f05).
  ls_fcat-col_pos = 3.
  INSERT ls_fcat INTO p_fieldcat INDEX 3.
*
  ls_fcat-fieldname   = 'TIME'.
  ls_fcat-inttype     =  'T'.
  ls_fcat-outputlen   =  12.
  ls_fcat-coltext     =  'Zeit'(f06).
  ls_fcat-seltext     =  'Uhrzeit'(f07).
  ls_fcat-col_pos     = 4.
  INSERT ls_fcat INTO p_fieldcat INDEX 4.
*
  ls_fcat-fieldname   = 'LEN'.
  ls_fcat-inttype     =  'P'.
  ls_fcat-outputlen   =  15.
  ls_fcat-coltext     =  'Bytes  '(f08).
  ls_fcat-seltext     =  'Datei Größe'(f09).
  ls_fcat-no_zero     = 'X'.
  ls_fcat-col_pos     =  5.
  INSERT ls_fcat INTO p_fieldcat INDEX 5.
*
ENDFORM.                               " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events.
* Event registration: tell ALV Tree which events shall be passed
* from frontend to backend.
  DATA lt_events             TYPE cntl_simple_events.
  DATA l_event               TYPE cntl_simple_event.
  DATA l_event_receiver      TYPE REF TO lcl_tree_event_receiver.
*
* Frontend registration(i):  get already registered tree events.
*................................................................
* The following four tree events registers ALV Tree in the constructor
* method itself.
*    - cl_gui_column_tree=>eventid_expand_no_children
* (needed to load data to frontend when a user expands a node)
*    - cl_gui_column_tree=>eventid_header_context_men_req
* (needed for header context menu)
*    - cl_gui_column_tree=>eventid_header_click
* (allows selection of columns (only when item selection activated))
*    - cl_gui_column_tree=>eventid_item_keypress
* (needed for F1-Help (only when item selection activated))
*
* Nevertheless you have to provide their IDs again if you register
* additional events with SET_REGISTERED_EVENTS (see below).
* To do so, call first method  GET_REGISTERED_EVENTS (this way,
* all already registered events remain registered, even your own):
  CALL METHOD g_alv_tree->get_registered_events
    IMPORTING
      events = lt_events.
* (If you do not these events will be deregistered!!!).
* You do not have to register events of the toolbar again.
*
* Toolbar events:
* Note that the instance of your ALV Tree Control registers the toolbar
* events on frontend as application events (within the constructor).

* Frontend registration(ii): add additional event ids
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND l_event TO lt_events.
**
*  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
*  append l_event to lt_events.
**
*  l_event-eventid = cl_gui_column_tree=>eventid_button_click.
*  append l_event to lt_events.
***
*  l_event-eventid = cl_gui_column_tree=>eventid_link_click.
*   append l_event to lt_events.
*
* Frontend registration. provide new event table to alv tree
  CALL METHOD g_alv_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.       "Dump
    MESSAGE ID 'N2PC' TYPE 'X' NUMBER '102'
      WITH 'Fehler beim Erzeugen des Baums'(101) sy-subrc.
  ENDIF.
* Register events on backend (ABAP Objects event handling)
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->handle_node_double_click FOR g_alv_tree.
  SET HANDLER l_event_receiver->handle_item_double_click FOR g_alv_tree.
  SET HANDLER l_event_receiver->handle_button_click      FOR g_alv_tree.
  SET HANDLER l_event_receiver->handle_link_click        FOR g_alv_tree.
  SET HANDLER l_event_receiver->handle_expand_nc         FOR g_alv_tree.
  SET HANDLER l_event_receiver->on_function_selected     FOR g_toolbar.
*
ENDFORM.                               " register_events
*&---------------------------------------------------------------------*
*&      Form  create_hierarchy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy USING p_relat_key TYPE lvc_nkey.
*
  DATA l_node_key                TYPE lvc_nkey.
  DATA l_note_layout             TYPE lvc_s_layn.
*
  PERFORM get_file_list.
*
  LOOP AT gt_file_list INTO gw_file_list.
    MOVE gw_file_list-type        TO gw_t_file_list-type.
    MOVE gw_file_list-full_name   TO gw_t_file_list-full_name.
    MOVE gw_file_list-name        TO gw_t_file_list-name.
    MOVE gw_file_list-mode        TO gw_t_file_list-mode.
    MOVE gw_file_list-owner       TO gw_t_file_list-owner.
    MOVE gw_file_list-date        TO gw_t_file_list-date.
    MOVE gw_file_list-time        TO gw_t_file_list-time.
    MOVE gw_file_list-len         TO gw_t_file_list-len.
*
    MOVE gw_file_list-mode        TO wa_s_file_list-mode.
    MOVE gw_file_list-owner       TO wa_s_file_list-owner.
    MOVE gw_file_list-date        TO wa_s_file_list-date.
    MOVE gw_file_list-time        TO wa_s_file_list-time.
    MOVE gw_file_list-len         TO wa_s_file_list-len.
    APPEND wa_s_file_list         TO gt_s_file_list.
*
    IF gw_file_list-type = 'D'.           "Directory
      l_note_layout-isfolder = 'X'.
      l_note_layout-expander = 'X'.
      PERFORM add_node USING p_relat_key
                             l_note_layout
                             l_node_key
                             wa_s_file_list.
    ELSEIF gw_file_list-type = 'F'.       "File
      l_note_layout-isfolder = ' '.
      l_note_layout-expander = ' '.
      PERFORM add_node USING p_relat_key
                             l_note_layout
                             l_node_key
                             wa_s_file_list.
*   ELSE.
*       special don't show
    ENDIF.
    MOVE l_node_key          TO gw_t_file_list-g_node.
    MOVE p_relat_key         TO gw_t_file_list-g_parent.
    APPEND gw_t_file_list    TO gt_t_file_list.
*
  ENDLOOP.
*
ENDFORM.                               " create_hierarchy
*&---------------------------------------------------------------------*
*&      Form  add_node
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_node
  USING p_relat_key    TYPE lvc_nkey
        p_note_layout  TYPE lvc_s_layn
        p_node_key     TYPE lvc_nkey
        p_file_list    TYPE s_file_list.
*
  CALL METHOD g_alv_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = gw_t_file_list-name
      is_node_layout   = p_note_layout
      is_outtab_line   = p_file_list
    IMPORTING
      e_new_node_key   = p_node_key.
*
ENDFORM.                               " add_node
*&---------------------------------------------------------------------*
*&      Form  create_empty_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_empty_tree USING p_new TYPE c.
* Create Hierarchy-header
* The simple ALV Tree uses the text of the fields which were used
* for sorting to define this header. When you use
* the 'normal' ALV Tree the hierarchy is build up freely
* by the programmer this is not possible, so he has to define it
* himself.
* Create empty Tree Control
* by calling methods of CL_GUI_ALV_TREE.
* Furthermore, the output table 'it_outtab' must be global and can
* only be used for one ALV Tree Control.
*
  DATA l_hierarchy_header    TYPE treev_hhdr.
  DATA l_fieldcatalog        TYPE lvc_t_fcat.
*
  CLEAR gt_s_file_list. REFRESH gt_s_file_list.
*
  IF p_new = abap_true.
    PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
    PERFORM build_field_catalog    CHANGING l_fieldcatalog.
    CALL METHOD g_alv_tree->set_table_for_first_display
      EXPORTING
*        I_STRUCTURE_NAME     =
*        IS_VARIANT           =
*        I_SAVE               =
*        I_DEFAULT            = 'X'
         is_hierarchy_header  = l_hierarchy_header
*        IS_EXCEPTION_FIELD   =
*        IT_SPECIAL_GROUPS    =
*        IT_LIST_COMMENTARY   =
*        I_LOGO               =
*        I_BACKGROUND_ID      =
*        IT_TOOLBAR_EXCLUDING =
*        IT_EXCEPT_QINFO      =
      CHANGING
        it_outtab             = gt_s_file_list "first empty table
*        IT_FILTER            =
        it_fieldcatalog       = l_fieldcatalog.
  ELSE.
*egal, wie neuer aufbau geht nicht! global, ohne field usw.
    PERFORM build_field_catalog CHANGING l_fieldcatalog.
    CALL METHOD g_alv_tree->set_table_for_first_display
      CHANGING
        it_outtab       = gt_s_file_list "empty table
        it_fieldcatalog = l_fieldcatalog.
  ENDIF.
*
ENDFORM.                               " create_empty_tree
*&---------------------------------------------------------------------*
*&      Form  clear_nodes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_nodes.
*
  DATA l_node_key            TYPE lvc_nkey.
*
  CLEAR gt_s_file_list. REFRESH gt_s_file_list.
*
  LOOP AT gt_t_file_list INTO gw_t_file_list.
    l_node_key = gw_t_file_list-g_node.
    CALL METHOD g_alv_tree->delete_subtree
      EXPORTING
        i_node_key                = l_node_key
        i_update_parents_expander = 'X'
        i_update_parents_folder   = 'X'
      EXCEPTIONS
        node_key_not_in_model     = 1
        OTHERS                    = 2.
    IF sy-subrc > 1.               "don't check node_key not in model
      MESSAGE ID 'N2PC' TYPE 'E' NUMBER '798'
        WITH 'alv_tree->delete_subtree'(e10) sy-subrc
             space space.
    ENDIF.
  ENDLOOP.
*
ENDFORM.                               " clear_nodes
*&---------------------------------------------------------------------*
*&      Form  handle_double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*----------------------------------------------------------------------*
FORM handle_double_click
  USING    p_node_key TYPE lvc_nkey.
*
  READ TABLE gt_t_file_list INTO gw_t_file_list
    WITH KEY g_node = p_node_key.
  IF gw_t_file_list-type = 'F'.
    MOVE gw_t_file_list-full_name TO g_current_file.
    PERFORM show_this_window
      USING gw_t_file_list-full_name.
  ELSEIF gw_t_file_list-type = 'D'.
    MOVE gw_t_file_list-full_name TO gd_dirname.
    PERFORM create_hierarchy USING p_node_key.
  ENDIF.
*
  SORT gt_t_file_list.
  CALL METHOD g_alv_tree->frontend_update.
*
ENDFORM.                               " handle_double_click
*&---------------------------------------------------------------------*
*&      Form  show_this_window
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_this_window
       USING p_filename TYPE string.
*
  CONSTANTS:
    c_max_out_line_length   TYPE i VALUE 80,
    c_max_input_length      TYPE i VALUE 32000.
*
  FIELD-SYMBOLS <ff>        TYPE ANY.
*
  DATA:
    l_dumpline_t            TYPE n2pqv_dumplin_t,
    l_dumpline_wa           TYPE rn2dumplin,
    l_eof                   TYPE sy-subrc,
    l_bytes_read            TYPE i,
    l_string_length         TYPE i,
    l_input_string(32000)   TYPE c,
    lo_max_input_length(6)  TYPE n,
    lo_bytes_read(6)        TYPE n,
    lo_string_length(6)     TYPE n.
  DATA l_header_caption(80) TYPE c.
*
  CLEAR l_eof.
  OPEN DATASET p_filename
    FOR INPUT IN TEXT MODE ENCODING DEFAULT
    IGNORING CONVERSION ERRORS
    REPLACEMENT CHARACTER '#'.
  IF sy-subrc <> 0.
    IF  p_filename = 'stdout.command'.
      MESSAGE ID 'NG' TYPE 'S' NUMBER '758'.
      RETURN.                         "no msg command
    ELSE.
      MESSAGE ID 'NG' TYPE 'I' NUMBER '743'
         WITH p_filename sy-subrc.
* Öffnen der Datei <&> fehlgeschlagen mit sy-subrc=&
      RETURN.
    ENDIF.
  ENDIF.
  WHILE l_eof <> 4.
    READ DATASET p_filename
            INTO l_input_string
          LENGTH l_bytes_read.
    IF sy-subrc = 4.                   " eof erreicht
      MOVE sy-subrc                TO l_eof.
      CLOSE DATASET p_filename.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE ID 'NG' TYPE 'I' NUMBER '744'
         WITH p_filename sy-subrc.
* Lesen der Datei <&> fehlgeschlagen mit sy-subrc=&
      RETURN.
    ENDIF.
    IF l_bytes_read > c_max_input_length.
*add warning line befor text block, only 32000 Bytes are viewed
      l_string_length = l_bytes_read - c_max_input_length.
      MOVE l_bytes_read       TO lo_bytes_read.
      MOVE c_max_input_length TO lo_max_input_length.
      MOVE l_string_length    TO lo_string_length.
      CONCATENATE '>'
                  text-e04
                  lo_bytes_read
                  text-e05
                  lo_max_input_length
                  text-e06
                  lo_string_length
                  text-e07
        INTO      l_dumpline_wa SEPARATED BY space.
      APPEND l_dumpline_wa TO l_dumpline_t.
    ENDIF.
    l_string_length = 0.
    WHILE l_bytes_read > 0 AND c_max_input_length > l_string_length.
      ASSIGN l_input_string+l_string_length(c_max_out_line_length)
          TO <ff>.
      l_bytes_read = l_bytes_read - c_max_out_line_length.
      IF l_string_length = 0.          "mormel line
        CLEAR l_dumpline_wa.
        MOVE <ff> TO l_dumpline_wa+1.  "pos 1 empty
        APPEND l_dumpline_wa TO l_dumpline_t.
      ELSE.                            "coninues line
        MOVE '>'     TO l_dumpline_wa.
        MOVE <ff>    TO l_dumpline_wa+1(c_max_out_line_length).
        APPEND l_dumpline_wa TO l_dumpline_t.
      ENDIF.
      ADD c_max_out_line_length TO l_string_length.
    ENDWHILE.
  ENDWHILE.
  CLOSE DATASET p_filename.
*
  MOVE p_filename            TO l_header_caption.
  PERFORM show_this_q_window USING l_header_caption l_dumpline_t.
*
ENDFORM.                               " show_this_window
*&---------------------------------------------------------------------*
*&      Form  show_hex_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_hex_view.
*
  DATA lt_selected_nodes     TYPE lvc_t_nkey.
  DATA lv_selected_node      TYPE lvc_nkey.
  DATA l_fieldname           TYPE lvc_fname.                "#EC NEEDED
  DATA l_flag_showed         TYPE abap_bool.
*
*get Global wa_t_file_list
  CALL METHOD g_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_selected_nodes.
*
  IF lt_selected_nodes IS INITIAL.
    CALL METHOD g_alv_tree->get_selected_item
      IMPORTING
        e_selected_node = lv_selected_node
        e_fieldname     = l_fieldname.
    IF lv_selected_node IS INITIAL.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
        WITH 'Kein Blatt oder Knoten ausgewählt'(s01).
    ELSE.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
* no line selected
  ELSE.
    LOOP AT lt_selected_nodes INTO lv_selected_node.
      IF NOT lv_selected_node IS INITIAL.
        READ TABLE gt_t_file_list INTO gw_t_file_list
          WITH KEY g_node = lv_selected_node.
      ENDIF.
      IF gw_t_file_list-type = 'F'.
        PERFORM show_this_hex_window
          USING gw_t_file_list-full_name.
        MOVE abap_true TO l_flag_showed.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
  IF l_flag_showed EQ abap_false.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
      WITH 'Keine Datei ausgewählt'(s04).
  ENDIF.
*
ENDFORM.                               " show_hex_view
*
*&---------------------------------------------------------------------*
*&      Form  show_this_hex_window
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_this_hex_window
       USING p_filename TYPE string.
*
  CONSTANTS c_24                       TYPE i VALUE 24.
*
  FIELD-SYMBOLS <ff>                   TYPE ANY.
*
  DATA:
   l_dumpline_t                        TYPE n2pqv_dumplin_t,
   l_dumpline_wa                       TYPE rn2dumplin,
   l_eof                               TYPE sy-subrc,
   l_bytes_read                        TYPE i,
   l_string_length                     TYPE i,
   l_lng                               TYPE i, "for unicode
   l_read_hex                          TYPE i,
   l_message(80)                       TYPE c,
   l_inp_line_c(24)                    TYPE c,
   l_inp_line_cc(54)                   TYPE c,  "48 + 6 space
   l_inp_line_x(27)                    TYPE x.  "24 + 6 space in mask
  DATA l_header_caption(80)            TYPE c.
*
  CLEAR l_eof.
  CLEAR l_dumpline_t. REFRESH l_dumpline_t.
*Write Header line
  CONCATENATE
    '1...5...10...14...20..24'
    sy-vline
    ' 1.... 4  5.....8  9....12 13....16 17....20 21....24'
    sy-vline
    INTO l_dumpline_wa.
  APPEND l_dumpline_wa    TO l_dumpline_t.
*
  OPEN DATASET p_filename FOR INPUT IN BINARY MODE
    MESSAGE l_message.
  IF sy-subrc <> 0.
    MESSAGE ID 'NG' TYPE 'I' NUMBER '743'
      WITH p_filename l_message.
* Öffnen der Datei <&> fehlgeschlagen mit sy-subrc=&
    RETURN.
  ENDIF.
  WHILE l_eof <> 4.
    CLEAR l_dumpline_wa.
    READ DATASET p_filename INTO l_inp_line_c LENGTH l_bytes_read.
    IF sy-subrc = 4.                   " eof erreicht
      MOVE sy-subrc                TO l_eof.
      CLOSE DATASET p_filename.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE ID 'NG' TYPE 'I' NUMBER '744'
        WITH p_filename sy-subrc.
* Lesen der Datei <&> fehlgeschlagen mit sy-subrc=&
      RETURN.
    ENDIF.
    IF l_eof = 4.                      "= eof
      IF  l_bytes_read > 0.            "last piece of string
        MOVE l_bytes_read              TO l_read_hex.
        l_lng = STRLEN( l_inp_line_c ). "GETWA_NOT_ASSIGNED_RANGE
        IF l_bytes_read GT l_lng.
          MOVE l_lng                   TO l_bytes_read.
        ENDIF.
        ASSIGN l_inp_line_c+0(l_bytes_read)
                                       TO <ff>.
        MOVE  <ff>                     TO l_inp_line_cc.
        PERFORM set_hex_val
          USING l_inp_line_cc l_inp_line_x.
        CLEAR l_inp_line_c.
        MOVE l_inp_line_cc             TO l_inp_line_c.
        ADD l_bytes_read               TO l_bytes_read.
        WRITE l_inp_line_x
          TO l_inp_line_cc USING EDIT MASK
          '________ ________ ________ ________ ________ ________'.

        l_string_length = 48 - l_bytes_read.
        IF l_bytes_read < 9.           "set correct position
          ADD 5 TO l_string_length.
        ELSEIF l_bytes_read < 17.
          ADD 4 TO l_string_length.
          ADD 1 TO l_bytes_read.
        ELSEIF l_bytes_read < 25.
          ADD 3 TO l_string_length.
          ADD 2 TO l_bytes_read.
        ELSEIF l_bytes_read < 33.
          ADD 2 TO l_string_length.
          ADD 3 TO l_bytes_read.
        ELSEIF l_bytes_read < 41.
          ADD 1 TO l_string_length.
          ADD 4 TO l_bytes_read.
        ELSEIF l_bytes_read < 49.
          ADD 5 TO l_bytes_read.
        ENDIF.
        DO l_string_length TIMES.      "clear empty hex
          IF l_bytes_read > 53.        "check 54 char in line_cc
            EXIT.
          ENDIF.
          WRITE space           TO l_inp_line_cc+l_bytes_read(1).
          ADD 1 TO l_bytes_read.
        ENDDO.
        PERFORM set_white_space
          USING l_inp_line_c
                l_inp_line_x
                l_read_hex.
        MOVE l_inp_line_c       TO l_dumpline_wa.
        MOVE sy-vline           TO l_dumpline_wa+24(1).
        MOVE l_inp_line_cc      TO l_dumpline_wa+25(54).
        MOVE sy-vline           TO l_dumpline_wa+78(1).
        APPEND l_dumpline_wa    TO l_dumpline_t.
      ENDIF.
    ELSE.                              "not eof
      ASSIGN l_inp_line_c              TO <ff>.
      PERFORM set_hex_val
        USING <ff> l_inp_line_x.
      WRITE l_inp_line_x
         TO l_inp_line_cc USING EDIT MASK
         '________ ________ ________ ________ ________ ________'.
      PERFORM set_white_space
        USING l_inp_line_c
              l_inp_line_x
              c_24.
      MOVE l_inp_line_c       TO l_dumpline_wa.
      MOVE sy-vline           TO l_dumpline_wa+24(1).
      MOVE l_inp_line_cc      TO l_dumpline_wa+25(54).
      MOVE sy-vline           TO l_dumpline_wa+78(1).
      APPEND l_dumpline_wa    TO l_dumpline_t.
    ENDIF.
  ENDWHILE.
*
  MOVE p_filename             TO l_header_caption.
  PERFORM show_this_q_window USING l_header_caption l_dumpline_t.
*
ENDFORM.                               " show_this_hex_window
*&---------------------------------------------------------------------*
*&      Form  show_file_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_file_view.
*
  DATA lt_selected_nodes      TYPE lvc_t_nkey.
  DATA lv_selected_node       TYPE lvc_nkey.
  DATA l_fieldname            TYPE lvc_fname.               "#EC NEEDED
  DATA l_flag_showed          TYPE abap_bool.
*
*get Global wa_t_file_list
  CALL METHOD g_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_selected_nodes.
*
  IF lt_selected_nodes IS INITIAL.
    CALL METHOD g_alv_tree->get_selected_item
      IMPORTING
        e_selected_node = lv_selected_node
        e_fieldname     = l_fieldname.
    IF lv_selected_node IS INITIAL.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
        WITH 'Kein Blatt oder Knoten ausgewählt'(s01).
    ELSE.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
* no line selected
  ELSE.
    LOOP AT lt_selected_nodes INTO lv_selected_node.
      IF NOT lv_selected_node IS INITIAL.
        READ TABLE gt_t_file_list INTO gw_t_file_list
          WITH KEY g_node = lv_selected_node.
      ENDIF.
      IF gw_t_file_list-type = 'F'.
        PERFORM show_this_window
          USING gw_t_file_list-full_name.
        MOVE abap_true TO l_flag_showed.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
  IF l_flag_showed EQ abap_false.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
      WITH 'Keine Datei ausgewählt'(s04).
  ENDIF.
*
ENDFORM.                               " show_file_view

*&---------------------------------------------------------------------*
*&      Form  write_newfile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_newfile.
*
  TYPES l_lines(72)          TYPE c.
*
  DATA l_string_length       TYPE i.
  DATA l_full_filename       TYPE string.
  DATA l_abab_title(26)      TYPE c.   "max 26 statt 30 byte
  DATA l_edit_table          TYPE TABLE OF l_lines.
  DATA l_edit_line           TYPE l_lines.
*
  CALL SCREEN 200
       STARTING AT 10 10
       ENDING AT   75 11.
  IF g_return_code IS INITIAL. RETURN. ENDIF.
*
  IF g_edit_filename = space.          "Abbruch keine Eingabe
    RETURN.
  ENDIF.
*
  l_string_length = STRLEN( gd_dirname ).
  CLEAR l_full_filename.
  MOVE gd_dirname                   TO l_full_filename.
  SUBTRACT 1 FROM l_string_length.
  IF l_full_filename+l_string_length(1) = c_separator.
  ELSE.
    CONCATENATE l_full_filename
                c_separator
           INTO l_full_filename.
    ADD 1 TO l_string_length.
  ENDIF.
  ADD 1 TO l_string_length.
  CONCATENATE l_full_filename
              g_edit_filename
         INTO l_full_filename.
*
  MOVE g_edit_filename            TO l_abab_title.   "max 26 Bytes Title
*
  EDITOR-CALL FOR l_edit_table TITLE l_abab_title.   "#EC CI_EDITORCALL
  IF l_edit_table IS INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH
    'Leere Dateien können nicht angelegt werden.'(m10).
    RETURN.
  ENDIF.
  IF sy-subrc = 0.                     "Im Editor wurde gespeichert!!!
    OPEN DATASET l_full_filename
      FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
      IGNORING CONVERSION ERRORS
      REPLACEMENT CHARACTER '#'.
    IF sy-subrc <> 0.
      MESSAGE ID 'NG' TYPE 'I' NUMBER '743'
         WITH l_full_filename sy-subrc.
* Öffnen der Datei <&> fehlgeschlagen mit sy-subrc=&
      RETURN.
    ENDIF.
    LOOP AT l_edit_table INTO l_edit_line.
      l_string_length = STRLEN( l_edit_line ).
      TRANSFER l_edit_line TO l_full_filename LENGTH l_string_length.
    ENDLOOP.
    CLOSE DATASET l_full_filename.
  ENDIF.
  MESSAGE ID 'N2PC' TYPE 'S' NUMBER '103' WITH
    'Datei:'(m02) l_full_filename 'erfolgreich erstellt'(m08).
*
  PERFORM build_new_tree.
*
ENDFORM.                               " write_newfile
*&---------------------------------------------------------------------*
*&      Form  call_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_user_command.
*
  DATA l_command_file        TYPE string
                             VALUE 'stdout.command'.        "#EC NOTEXT
  DATA l_command_line        TYPE rlgrap-filename.
  DATA l_err                 TYPE symsgv.
*
  CALL SCREEN 210
       STARTING AT 10  10
       ENDING AT  100  9.
  IF g_return_code IS INITIAL. RETURN. ENDIF.
  MOVE g_command                       TO l_command_line.
  IF l_command_line = space. RETURN. ENDIF.
*
  TRY.
      OPEN DATASET l_command_file
        FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
        IGNORING CONVERSION ERRORS
        REPLACEMENT CHARACTER '#'
        FILTER     l_command_line
        MESSAGE    l_err.
    CATCH cx_sy_file_open.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
    CATCH cx_sy_codepage_converter_init.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
    CATCH cx_sy_conversion_codepage.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
    CATCH cx_sy_file_authority.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
    CATCH cx_sy_pipes_not_supported.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
    CATCH cx_sy_too_many_files.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
    CATCH cx_root.                                       "#EC CATCH_ALL
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH l_err.
      RETURN.
  ENDTRY.
  WAIT UP TO 3 SECONDS. "to finish asynchron call
  MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
    WITH l_command_line 'ausgeführt'(908).
*
  TRY.
      CLOSE DATASET l_command_file.
    CATCH cx_sy_file_close.
      RETURN.
    CATCH cx_root.                                       "#EC CATCH_ALL
      RETURN.
  ENDTRY.
*
  PERFORM show_this_window
    USING l_command_file.
*
  TRY.
      DELETE DATASET l_command_file.
    CATCH cx_sy_file_authority.
      RETURN.
    CATCH cx_sy_file_open.
      RETURN.
    CATCH cx_root.                                       "#EC CATCH_ALL
      RETURN.
  ENDTRY.
*
ENDFORM.                               " call_user_command
*&---------------------------------------------------------------------*
*&      Form  delete_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_file.
*
  DATA lt_selected_nodes     TYPE lvc_t_nkey.
  DATA lv_selected_node      TYPE lvc_nkey.
  DATA l_fieldname           TYPE lvc_fname.                "#EC NEEDED
  DATA l_flag_showed         TYPE abap_bool.
  DATA l_sel_lines(5)        TYPE n.
  DATA l_answer              TYPE c.
*
*get Global wa_t_file_list
  CALL METHOD g_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_selected_nodes.
*
  IF lt_selected_nodes IS INITIAL.
    CALL METHOD g_alv_tree->get_selected_item
      IMPORTING
        e_selected_node = lv_selected_node
        e_fieldname     = l_fieldname.
    IF lv_selected_node IS INITIAL.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
        WITH 'Kein Blatt oder Knoten ausgewählt'(s01).
    ELSE.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
* no line selected
  ELSE.
    CLEAR l_answer.
    l_sel_lines = LINES( lt_selected_nodes ).
    LOOP AT lt_selected_nodes INTO lv_selected_node.
      IF NOT lv_selected_node IS INITIAL.
        READ TABLE gt_t_file_list INTO gw_t_file_list
          WITH KEY g_node = lv_selected_node.
      ENDIF.
      IF gw_t_file_list-type = 'F'.
        PERFORM delete_this_file USING l_sel_lines l_answer.
        MOVE abap_true TO l_flag_showed.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
  IF l_flag_showed EQ abap_false.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
      WITH 'Keine Datei ausgewählt'(s04).
  ENDIF.
*
  PERFORM build_new_tree.
*
ENDFORM.                               " delete_file
*&---------------------------------------------------------------------*
*&      Form  delete_this_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_this_file
      USING p_sel_lines TYPE n
            p_answer    TYPE c.
*
  DATA l_titlebar(50)        TYPE c.
*
  MOVE gw_t_file_list-full_name TO g_current_file.
*
  IF p_answer = '1'.
    DELETE DATASET g_current_file.
    IF sy-subrc <> 0.
      MESSAGE ID 'NG' TYPE 'I' NUMBER '732' WITH g_current_file.
    ENDIF.
    RETURN.
  ELSE.
    IF p_answer <> space.
      RETURN.
    ENDIF.
  ENDIF.
*
  IF p_sel_lines GT 1.
    CONCATENATE p_sel_lines
                'Dateien sind zum löschen markiert!'(q05)
           INTO l_titlebar SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            titlebar              = l_titlebar
*           DIAGNOSE_OBJECT       = ' '
            text_question         =
     'Sollen alle markierten Dateien gelöscht werden?'(q04)
            text_button_1         = 'Ja, löschen'(q01)
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = 'Nein'(q02)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
*           USERDEFINED_F1_HELP   = ' '
            start_column          = 25
            start_row             = 6
            popup_type            = 'ICON_MESSAGE_QUESTION'
       IMPORTING
            answer                = p_answer.
  ELSE.
*
    CALL FUNCTION 'POPUP_TO_CONFIRM'
         EXPORTING
              titlebar              = g_current_file
*           DIAGNOSE_OBJECT       = ' '
              text_question         =
       'Soll die Datei wirklich gelöscht werden?'(q03)
              text_button_1         = 'Ja, löschen'(q01)
              icon_button_1         = 'ICON_OKAY'
              text_button_2         = 'Nein'(q02)
              icon_button_2         = 'ICON_CANCEL'
              default_button        = '1'
              display_cancel_button = ' '
*           USERDEFINED_F1_HELP   = ' '
              start_column          = 25
              start_row             = 6
              popup_type            = 'ICON_MESSAGE_QUESTION'
         IMPORTING
              answer                = p_answer.
  ENDIF.
  IF p_answer <> '1'.             "Yes, delete
    RETURN.                       "'2'= No, 'A' = Abort
  ENDIF.
*
  DELETE DATASET g_current_file.
  IF sy-subrc <> 0.
    MESSAGE ID 'NG' TYPE 'I' NUMBER '732' WITH g_current_file.
  ENDIF.
*
ENDFORM.                               " delete_this_file
*
*&---------------------------------------------------------------------*
*&      Form  build_new_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_new_tree.
*
  CLEAR gt_s_file_list. REFRESH gt_s_file_list.
  PERFORM clear_nodes.
  CALL METHOD g_alv_tree->frontend_update.
  PERFORM create_hierarchy USING  ' '.
  CALL METHOD g_alv_tree->frontend_update.
*
ENDFORM.                               " build_new_tree
*&---------------------------------------------------------------------*
*&      Form  get_node_or_leave
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_node_or_leave.
*
  DATA lt_selected_nodes      TYPE lvc_t_nkey.
  DATA lv_selected_node       TYPE lvc_nkey.
  DATA l_fieldname            TYPE lvc_fname.               "#EC NEEDED
*
*set Global wa_t_file_list
  CALL METHOD g_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_selected_nodes.
*
  IF lt_selected_nodes IS INITIAL.
    CALL METHOD g_alv_tree->get_selected_item
      IMPORTING
        e_selected_node = lv_selected_node
        e_fieldname     = l_fieldname.
    IF lv_selected_node IS INITIAL.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
        WITH 'Kein Blatt oder Knoten ausgewählt'(s01).
    ELSE.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
*
* no line selected
  ELSE.
    READ TABLE lt_selected_nodes INTO lv_selected_node INDEX 1.
    IF NOT lv_selected_node IS INITIAL.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
  ENDIF.
*
ENDFORM.                               " get_node_or_leave
*&---------------------------------------------------------------------*
*&      Form  change_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_toolbar USING p_init TYPE c.
* p_init = 'X' for Initial, create first own,
* P_init = space, delete standart entrys
*
* Get toolbar instance of your ALV Tree.
* When you instantiate an instance of CL_GUI_ALV_TREE the constructor
* of the base class (CL_ALV_TREE_BASE) creates a toolbar.
* Fetch its reference with the following method if you want to
* modify it:
  IF p_init = 'X'.                     "add own Buttons
    CALL METHOD g_alv_tree->get_toolbar_object
      IMPORTING
        er_toolbar = g_toolbar.
*
    CHECK NOT g_toolbar IS INITIAL. "could happen if you do not use the
*                                   "standard toolbar
*
* Modify toolbar with methods of CL_GUI_TOOLBAR:
* add separator  to toolbar
    CALL METHOD g_toolbar->add_button
      EXPORTING
        fcode     = 'SHOW'
        icon      = icon_display_note                       "'@0L@'
        butn_type = cntb_btype_button
        text      = space
        quickinfo = text-901.      "Show File as is it
*
    CALL METHOD g_toolbar->add_button
      EXPORTING
        fcode     = 'HEXS'
        icon      = icon_display_text                       "'@0P@'
        butn_type = cntb_btype_button
        text      = text-t02
        quickinfo = text-902.      "Show File as Hex
*
    READ TABLE g_exit_buttons
      WITH KEY table_line = 'NEWFILE' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CALL METHOD g_toolbar->add_button
        EXPORTING
          fcode     = 'NEWFILE'
          icon      = icon_create                           "@0Y@'
          butn_type = cntb_btype_button
          text      = text-t03
          quickinfo = text-903.      "Write New File
    ENDIF.
*
    READ TABLE g_exit_buttons
      WITH KEY table_line = 'EDITFILE' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CALL METHOD g_toolbar->add_button
        EXPORTING
          fcode     = 'EDITFILE'
          icon      = icon_create_note                      "@0K@'
          butn_type = cntb_btype_button
          text      = text-t07
          quickinfo = text-907.      "Write New File
    ENDIF.
*
    READ TABLE g_exit_buttons
      WITH KEY table_line = 'DELETE' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.

      CALL METHOD g_toolbar->add_button
        EXPORTING
          fcode     = 'DELETE'
          icon      = icon_delete                           "'@11@'
          butn_type = cntb_btype_button
          text      = space
          quickinfo = text-904.    "Delete File
    ENDIF.
*
    READ TABLE g_exit_buttons
      WITH KEY table_line = 'GETFILE' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CALL METHOD g_toolbar->add_button
        EXPORTING
          fcode     = 'GETFILE'
          icon      = icon_export                           "'@49@'
          butn_type = cntb_btype_button
          text      = space
          quickinfo = text-905.    "Get File from workstation
    ENDIF.
*
    READ TABLE g_exit_buttons
      WITH KEY table_line = 'PUTFILE' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CALL METHOD g_toolbar->add_button
        EXPORTING
          fcode     = 'PUTFILE'
          icon      = icon_import                           "'@48@'
          butn_type = cntb_btype_button
          text      = space
          quickinfo = text-906.    "Send File from workstation
    ENDIF.
*remove default
  ELSE.
    CALL METHOD g_toolbar->delete_button    "Sum Symbol
      EXPORTING fcode = '&CALC'.
    CALL METHOD g_toolbar->delete_button    "User define
      EXPORTING fcode = '&COL0'.
    CALL METHOD g_toolbar->delete_button    "expand all nodes
      EXPORTING fcode = '&EXPAND'.
    CALL METHOD g_toolbar->delete_button    "collaps all nodes
      EXPORTING fcode = '&COLLAPSE'.
  ENDIF.
*
ENDFORM.                               " change_toolbar
*&---------------------------------------------------------------------*
*&      Form  get_files
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_files.
*
  DATA lt_selected_nodes      TYPE lvc_t_nkey.
  DATA lv_selected_node       TYPE lvc_nkey.
  DATA l_fieldname            TYPE lvc_fname.               "#EC NEEDED
*
*get Global wa_t_file_list
  CALL METHOD g_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_selected_nodes.
*
  IF lt_selected_nodes IS INITIAL.
    CALL METHOD g_alv_tree->get_selected_item
      IMPORTING
        e_selected_node = lv_selected_node
        e_fieldname     = l_fieldname.
    IF lv_selected_node IS INITIAL.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
        WITH 'Kein Blatt oder Knoten ausgewählt'(s01).
    ELSE.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
* no line selected
  ELSE.
    LOOP AT lt_selected_nodes INTO lv_selected_node.
      IF NOT lv_selected_node IS INITIAL.
        READ TABLE gt_t_file_list INTO gw_t_file_list
          WITH KEY g_node = lv_selected_node.
      ENDIF.
      IF gw_t_file_list-type = 'F'.
        PERFORM get_file
          USING gw_t_file_list-full_name.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
ENDFORM.                               " get_files
*&---------------------------------------------------------------------*
*&      Form  get_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file USING p_file_name TYPE string.
*
  DATA l_file_size           TYPE i.
  DATA l_filename            TYPE rlgrap-filename.
  DATA l_file                TYPE string.
  DATA l_rn2pcraw255         TYPE TABLE OF rn2pcraw255.
*
  CALL FUNCTION 'ISH_N2_READ_FILE_APP_SERVER'
    EXPORTING
      ss_file      = p_file_name
    IMPORTING
      ss_file_size = l_file_size
    TABLES
      ss_bin_file  = l_rn2pcraw255
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*
  PERFORM get_file_name USING p_file_name l_file.
*
  MOVE l_file                TO l_filename.
  CALL FUNCTION 'ISH_N2_GUI_DOWNLOAD'
    EXPORTING
      ss_filename     = l_filename
      ss_filesize     = l_file_size
      ss_popup        = space
    IMPORTING
      ss_new_filename = l_filename
    TABLES
      ss_data_tab     = l_rn2pcraw255
    EXCEPTIONS
      cancel          = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID 'NG' TYPE 'E' NUMBER '722' WITH
      l_filename sy-subrc.
  ELSE.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102' WITH
      l_filename ' wurde erfolgreich gespeichert'(s03).
  ENDIF.
*
ENDFORM.                               " get_file
*&---------------------------------------------------------------------*
*&      Form  put_files
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_files.
*
  DATA l_window_title        TYPE string.
  DATA l_file_table          TYPE filetable.
  DATA l_file_name           TYPE file_table.
  DATA l_rc                  TYPE i.
  DATA l_copy_all            TYPE abap_bool.
  DATA lt_excltab            TYPE TABLE OF gui_code.
*
  MOVE text-q10              TO l_window_title.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
       window_title            = l_window_title
*      DEFAULT_EXTENSION       =
*      DEFAULT_FILENAME        =
*      FILE_FILTER             =
*      WITH_ENCODING           =
*      INITIAL_DIRECTORY       =
       multiselection          = abap_true
    CHANGING
       file_table              = l_file_table
       rc                      = l_rc
*      USER_ACTION             =
*      FILE_ENCODING           =
    EXCEPTIONS
      file_open_dialog_failed  = 1
      cntl_error               = 2
      error_no_gui             = 3
      not_supported_by_gui     = 4
      OTHERS                   = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF l_rc EQ 0.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH
      'Keine Datei ausgewählt'(s04).
    RETURN.
  ENDIF.
*
  IF l_rc = 1.           "only one file selected
    APPEND 'ALLE' TO lt_excltab.
  ENDIF.
  SET PF-STATUS 'STATUS_0200'  EXCLUDING lt_excltab.
  MOVE abap_false TO l_copy_all.
  LOOP AT l_file_table INTO l_file_name.
    PERFORM put_file USING l_file_name l_copy_all.
  ENDLOOP.
  PERFORM build_new_tree.
*
ENDFORM.                               " put_files
*&---------------------------------------------------------------------*
*&      Form  put_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_file
  USING p_file_name          TYPE file_table
        p_copy_all           TYPE abap_bool.
*
  DATA l_file_size           TYPE i.
  DATA l_filename            TYPE rlgrap-filename.
  DATA l_file_full           TYPE string.
  DATA l_file_short          TYPE string.
  DATA l_file_table          TYPE TABLE OF rn2pcraw255.
*
  MOVE p_file_name TO l_filename.
  CALL FUNCTION 'ISH_N2_GUI_UPLOAD'
    EXPORTING
      ss_filename     = l_filename
    IMPORTING
      ss_filelength   = l_file_size
      ss_new_filename = l_filename
    TABLES
      ss_data_tab     = l_file_table
    EXCEPTIONS
      cancel          = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID 'NG' TYPE 'E' NUMBER '722' WITH
      l_filename sy-subrc.
  ENDIF.
*
  MOVE l_filename            TO l_file_full.
  PERFORM get_file_name USING l_file_full l_file_short.
  MOVE l_file_short          TO g_edit_filename.
*
  IF p_copy_all EQ abap_false.
    CALL SCREEN 200                      "Set host name
         STARTING AT 10 10
         ENDING AT   75 11.
    IF g_return_code = 2.
      MOVE abap_true TO p_copy_all.
    ENDIF.
    IF g_edit_filename = space           "Cancle no file
    OR l_file_size     = 0               "Cancle empty file
    OR gd_dirname IS INITIAL.            "no default dir
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '703'.
      RETURN.
    ENDIF.
  ENDIF.
*
  PERFORM set_gd_dirname.
  CONCATENATE gd_dirname g_edit_filename
    INTO l_file_full.
*
  CALL FUNCTION 'ISH_N2_WRITE_FILE_APP_SERVER'
    EXPORTING
      ss_file     = l_file_full
      ss_filesize = l_file_size
    TABLES
      ss_bin_file = l_file_table
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*
  MESSAGE ID 'N2PC' TYPE 'S' NUMBER '103'
    WITH 'Datei:'(m02) g_edit_filename 'übertragen'(m06).
*
ENDFORM.                               " put_file
*&---------------------------------------------------------------------*
*&      Form  edit_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_file.
*
  TYPES l_lines(72)          TYPE c.
*
  DATA l_string_length       TYPE i.
  DATA l_input_string(72)    TYPE c.
  DATA l_abab_title(26)      TYPE c.   "max 26 statt 30 byte
  DATA l_edit_table          TYPE TABLE OF l_lines.
  DATA l_edit_line           TYPE l_lines.
*
  PERFORM get_node_or_leave.
  OPEN DATASET gw_t_file_list-full_name
    FOR INPUT IN TEXT MODE ENCODING DEFAULT
    IGNORING CONVERSION ERRORS
    REPLACEMENT CHARACTER '#'.
  IF sy-subrc <> 0.
    MESSAGE ID 'NG' TYPE 'I' NUMBER '743'
       WITH gw_t_file_list-full_name sy-subrc.
    RETURN.
  ENDIF.
  WHILE sy-subrc = 0.
    READ DATASET gw_t_file_list-full_name
    INTO l_input_string LENGTH l_string_length.
    APPEND l_input_string TO l_edit_table.
  ENDWHILE.
  APPEND l_input_string TO l_edit_table.
  CLOSE DATASET gw_t_file_list-full_name.
*
  MOVE gw_t_file_list-name        TO l_abab_title.  "max 26 Bytes Title
  EDITOR-CALL FOR l_edit_table TITLE l_abab_title.   "#EC CI_EDITORCALL
  IF l_edit_table IS INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101' WITH
    'Leere Dateien können nicht angelegt werden.'(m10).
    RETURN.
  ENDIF.
  IF sy-subrc = 0.                     "Im Editor wurde gespeichert!!!
    OPEN DATASET gw_t_file_list-full_name
      FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
      IGNORING CONVERSION ERRORS
      REPLACEMENT CHARACTER '#'.
    IF sy-subrc <> 0.
      MESSAGE ID 'NG' TYPE 'I' NUMBER '743'
         WITH gw_t_file_list-full_name sy-subrc.
* Öffnen der Datei <&> fehlgeschlagen mit sy-subrc=&
      RETURN.
    ENDIF.
    LOOP AT l_edit_table INTO l_edit_line.
      l_string_length = STRLEN( l_edit_line ).
      TRANSFER l_edit_line
        TO gw_t_file_list-full_name LENGTH l_string_length.
    ENDLOOP.
    CLOSE DATASET gw_t_file_list-full_name.
  ENDIF.
  MESSAGE ID 'N2PC' TYPE 'S' NUMBER '103' WITH
    'Datei:'(m02)
    gw_t_file_list-full_name 'erfolgreich erstellt'(m08).
*
  PERFORM build_new_tree.
*
ENDFORM.                               " edit_file
*&---------------------------------------------------------------------*
*&      Form  get_file_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_T_FILE_LIST_FULL_NAME  text
*      -->P_L_FILENAME  text
*----------------------------------------------------------------------*
FORM get_file_name
  USING p_wa_t_file_list_full_name  TYPE string
        p_l_filename                TYPE string.
*
  TYPES:
    BEGIN OF split_type,
           line              TYPE rlgrap-filename,
    END   OF split_type.
*
  DATA lt_split              TYPE STANDARD TABLE OF split_type
                                           INITIAL SIZE 5.
  DATA l_index               TYPE i.
*
  SPLIT p_wa_t_file_list_full_name
     AT  '\'
   INTO TABLE lt_split.
  DESCRIBE TABLE lt_split LINES l_index.
  IF l_index EQ 1.  "no split
    CLEAR lt_split.
    SPLIT p_wa_t_file_list_full_name
       AT  '/'
     INTO TABLE lt_split.
  ENDIF.
  DESCRIBE TABLE lt_split LINES l_index.
  READ TABLE lt_split INDEX l_index INTO p_l_filename.
*
ENDFORM.                    " get_file_name
*&---------------------------------------------------------------------*
*&      Form  check_ta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ta.
*
  DATA l_auth                TYPE c VALUE '1'.
  DATA l_exit_func           TYPE gui_func.
*
  MOVE sy-repid TO g_repid.
  PERFORM check_authority_report
    IN PROGRAM sapmnpa0
    USING g_repid
         'N_0_UTIL'
          l_auth.
  IF l_auth NE '1'.
    MESSAGE ID 'N2PC' TYPE 'E' NUMBER '102'
      WITH 'Sie haben keine Berechtigung für:'(e11) 'N_0_UTIL'.
    LEAVE PROGRAM.
  ENDIF.
  IF sy-batch = 'X'.
    LEAVE PROGRAM.
  ENDIF.
*
  AUTHORITY-CHECK OBJECT 'N_2UX'
           ID 'ACTVT' FIELD '02'.
  IF sy-subrc <> 0.
    MOVE   'EDITFILE'      TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'N_2UX'
           ID 'ACTVT' FIELD '06'.
  IF sy-subrc <> 0.
    MOVE   'DELETE'        TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'N_2UX'
           ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MOVE   'COMMAND'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
*only when execute then FTP Commands
    MOVE   'FTPOPEN'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
    MOVE   'FTPCMD'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
    MOVE   'FTPCLOSE'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
    MOVE   'FTPDIR'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
    MOVE   'FTPGET'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'N_2UX'
           ID 'ACTVT' FIELD '34'.
  IF sy-subrc <> 0.
    MOVE   'NEWFILE'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'N_2UX'
           ID 'ACTVT' FIELD 'DL'.
  IF sy-subrc <> 0.
    MOVE   'PUTFILE'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'N_2UX'
           ID 'ACTVT' FIELD 'UL'.
  IF sy-subrc <> 0.
    MOVE   'GETFILE'       TO l_exit_func.
    APPEND l_exit_func     TO g_exit_buttons.
  ENDIF.
*
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
           ID 'S_ADMI_FCD' FIELD 'ST0R'.
  IF sy-subrc <> 0.
    MESSAGE ID 'N2PC' TYPE 'E' NUMBER '102'
      WITH 'Sie haben keine Berechtigung für:'(e11) 'S_ADMI_FCD'.
    LEAVE PROGRAM.
  ENDIF.
*
  CALL FUNCTION 'GUI_IS_ITS'
    IMPORTING
      return = l_auth.
  IF l_auth = 'X'.
    MESSAGE ID 'N2PC' TYPE 'I' NUMBER '007'.
    LEAVE PROGRAM.
  ENDIF.
*set initiale Directory
  IF gd_dirname IS INITIAL.
    MOVE c_start_dir                   TO gd_dirname.
    SET PARAMETER ID 'GD_DIRNAME'      FIELD gd_dirname.    "#EC EXISTS
  ENDIF.
  PERFORM prepare_time_zone.           "init timezone_sec as global
*
ENDFORM.                    " check_ta

************************************************************************
*
FORM get_codepage.
*
  DATA c4(4)                   TYPE c.
  DATA charsize                TYPE i.
*
  IF cl_abap_char_utilities=>charsize > 1. " system is Unicode
    MESSAGE i109.
*   Diese Programm ist nicht generell für Unicode Systeme benutzbar
  ENDIF.
  CALL 'CUR_LCL' ID 'UC_LN'    FIELD charsize             "#EC CI_CCALL
                 ID 'CODEPAGE' FIELD c4. "sys_code_page.
  MOVE c4                    TO g_sys_code_page.
*
ENDFORM.                   "get_codepage
*
************************************************************************
*
FORM set_hex_val
  USING p_char  TYPE c
        p_hex   TYPE x.
*
  FIELD-SYMBOLS <f>          TYPE x.
*
  ASSIGN p_char TO <f> CASTING.
  MOVE <f>      TO p_hex.
*
ENDFORM.                  "set_hex_val
*
**********************************************************************
*
FORM set_white_space
  USING p_char TYPE c    "24
        p_hex  TYPE x    "48 + 6 space = 54
        p_length TYPE i.
*
  CONSTANTS c_x1f TYPE x VALUE '1F'.
  CONSTANTS c_x7f TYPE x VALUE '7F'.
  CONSTANTS c_x8b TYPE x VALUE '8B'.
  CONSTANTS c_x8e TYPE x VALUE '8E'.
  CONSTANTS c_x9d TYPE x VALUE '9D'.
  CONSTANTS c_xad TYPE x VALUE 'AD'.
  CONSTANTS c_c23 TYPE c VALUE  '#'.   "x_23 replace char!
  DATA l_pos      TYPE i.
*
*  DATA:
*  BEGIN OF xxx_to_asc,
*       x00(32) TYPE x VALUE
*    '00230123022303230423052306230723082309230A230B230C230D230E230F23'
*       x01(32) TYPE x VALUE
*    '10231123122313231423152316231723182319231A231B231C231D231E231F23'
*        x02(32) TYPE x VALUE
*     '20202121222223232424252526262727282829292A2A2B2B2C2C2D2D2E2E2F2F'
*        x03(32) TYPE x VALUE
*    '30303131323233333434353536363737383839393A3A3B3B3C3C3D3D3E3E3F3F'
*        x04(32) TYPE x VALUE
*    '40404141424243434444454546464747484849494A4A4B4B4C4C4D4D4E4E4F4F'
*        x05(32) TYPE x VALUE
*    '50505151525253535454555556565757585859595A5A5B5B5C5C5D5D5E5E5F5F'
*        x06(32) TYPE x VALUE
*    '60606161626263636464656566666767686869696A6A6B6B6C6C6D6D6E6E6F6F'
*        x07(32) TYPE x VALUE
*    '70707171727273737474757576767777787879797A7A7B7B7C7C7D7D7E7E7F23'
*        x08(32) TYPE x VALUE
*    '80808181828283838484858586868787888889898A8A8B238C8C8D8D8E238F8F'
*        x09(32) TYPE x VALUE
*    '90909191929293939494959596969797989899999A9A9B9B9C9C9D239E9E9F9F'
*        x10(32) TYPE x VALUE
*    'A0A0A1A1A2A2A3A3A4A4A5A5A6A6A7A7A8A8A9A9AAAAABABACACAD23AEAEAFAF'
*        x11(32) TYPE x VALUE
*    'B0B0B1B1B2B2B3B3B4B4B5B5B6B6B7B7B8B8B9B9BABABBBBBCBCBDBDBEBEBFBF'
*        x12(32) TYPE x VALUE
*    'C0C0C1C1C2C2C3C3C4C4C5C5C6C6C7C7C8C8C9C9CACACBCBCCCCCDCDCECECFCF'
*        x13(32) TYPE x VALUE
*    'D0D0D1D1D2D2D3D3D4D4D5D5D6D6D7D7D8D8D9D9DADADBDBDCDCDDDDDEDEDFDF'
*        x14(32) TYPE x VALUE
*    'E0E0E1E1E2E2E3E3E4E4E5E5E6E6E7E7E8E8E9E9EAEAEBEBECECEDEDEEEEEFEF'
*        x15(32) TYPE x VALUE
*    'F0F0F1F1F2F2F3F3F4F4F5F5F6F6F7F7F8F8F9F9FAFAFBFBFCFCFDFDFEFEFFFF'
*    END OF xxx_to_asc.
*less then 1F --> mapped to # x'23'
*special char's --> 7f, 8b, 8e, 9d and ad
*mapped to # x'23'
*
  CLEAR l_pos.
  DO p_length TIMES.
    IF l_pos > 23.   "no more then 24 positions aviable
      RETURN.
    ENDIF.
    IF p_hex+l_pos(1) LE c_x1f.
      MOVE c_c23 TO p_char+l_pos(1).
    ELSE.
      CASE p_hex+l_pos(1).
        WHEN c_x7f.
          MOVE c_c23 TO p_char+l_pos(1).
        WHEN c_x8b.
          MOVE c_c23 TO p_char+l_pos(1).
        WHEN c_x8e.
          MOVE c_c23 TO p_char+l_pos(1).
        WHEN c_x9d.
          MOVE c_c23 TO p_char+l_pos(1).
        WHEN c_xad.
          MOVE c_c23 TO p_char+l_pos(1).
      ENDCASE.
    ENDIF.
    ADD 1 TO l_pos.
  ENDDO.
*
ENDFORM.             " set_white_space
*
**********************************************************************
*
FORM ftp_open.
*
  CONSTANTS c_key            TYPE i VALUE 26101957.
  DATA l_slen                TYPE i.

  IF g_ftp_handle IS NOT INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
      WITH 'Sie sind bereits angemeldet.'(u01)
           'Bitte vorher abmelden.'(u02).
    RETURN.
  ENDIF.
*
  CALL SCREEN 220
     STARTING AT 10 10
     ENDING AT   90 13.
  IF g_return_code IS INITIAL.
    RETURN.              "User Hit Exit-Buuton
  ENDIF.
*
  l_slen = STRLEN( g_ftp_password ).
  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      SOURCE      = g_ftp_password
      sourcelen   = l_slen
      key         = c_key
    IMPORTING
      destination = g_ftp_password.
*
  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      user                   = g_ftp_user
      password               = g_ftp_password
*     ACCOUNT                =
      host                   = g_ftp_host
      rfc_destination        = 'SAPFTPA'
*     GATEWAY_USER           =
*     GATEWAY_PASSWORD       =
*     GATEWAY_HOST           =
    IMPORTING
      handle                 = g_ftp_handle
    EXCEPTIONS
      not_connected          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
      WITH 'Anmeldung am FTP Server erfolgreich'(u03).
  ENDIF.
*
ENDFORM.             " ftp_open
*
**********************************************************************
*
FORM ftp_close.
*
  IF g_ftp_handle IS NOT INITIAL.
    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = g_ftp_handle.
    CLEAR g_ftp_handle.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
      WITH 'Abmeldung am FTP Server ist erfolgt.'(u04).
  ELSE.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
      WITH 'Sie haben keine Verbindung zum FTP.'(u05)
           'Bitte vorher anmelden.'(u06).
  ENDIF.
*
ENDFORM.             " ftp_close
*
**********************************************************************
*
FORM ftp_cmd.
*
  DATA l_header              TYPE text80.
  DATA lt_result             TYPE n2pqv_dumplin_t.
*
  IF g_ftp_handle IS INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
      WITH 'Sie haben keine Verbindung zum FTP.'(u05)
           'Bitte vorher anmelden.'(u06).
    RETURN.
  ENDIF.
*
  CALL SCREEN 210
       STARTING AT  5  5
       ENDING AT  100 10.
  IF g_return_code IS INITIAL. RETURN. ENDIF.
*
  CALL FUNCTION 'FTP_COMMAND'
  EXPORTING
    handle                = g_ftp_handle
    command               = g_command
*     COMPRESS              =
*     RFC_DESTINATION       =
*     VERIFY                =
*   IMPORTING
*     FILESIZE              =
*     FILEDATE              =
*     FILETIME              =
  TABLES
    data                  = lt_result
  EXCEPTIONS
    tcpip_error           = 1
    command_error         = 2
    data_error            = 3
    OTHERS                = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.
  IF g_command+0(2) EQ 'cd'.
    PERFORM ftp_dir.
  ELSE.
    CONCATENATE 'FTP:'(v02)
                g_ftp_host
                g_command
          INTO l_header SEPARATED BY space.
    PERFORM show_this_q_window USING l_header lt_result.
  ENDIF.
*
ENDFORM.             " ftp_cmd
*
**********************************************************************
*
FORM ftp_dir.
*
  CONSTANTS c_dir(3)         TYPE c VALUE 'dir'.
  DATA lt_result             TYPE n2pqv_dumplin_t.
  DATA l_header              TYPE text80.
*
  IF g_ftp_handle IS INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
      WITH 'Sie haben keine Verbindung zum FTP.'(u05)
           'Bitte vorher anmelden.'(u06).
    RETURN.
  ENDIF.
*
  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      handle                = g_ftp_handle
      command               = c_dir
*     COMPRESS              =
*     RFC_DESTINATION       =
*     VERIFY                =
*   IMPORTING
*     FILESIZE              =
*     FILEDATE              =
*     FILETIME              =
    TABLES
      data                  = lt_result
    EXCEPTIONS
      tcpip_error           = 1
      command_error         = 2
      data_error            = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CONCATENATE 'FTP:'(v02)
              g_ftp_host
              'Verzeichnis'(v01)
          INTO l_header SEPARATED BY space.
  PERFORM show_this_q_window USING l_header lt_result.
*
ENDFORM.             " ftp_dir
*
**********************************************************************
*
FORM ftp_get.
*
  DATA l_blob_length         TYPE i.
  DATA lt_blob               TYPE TABLE OF rn2pcraw255.
*
  IF g_ftp_handle IS INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
      WITH 'Sie haben keine Verbindung zum FTP.'(u05)
           'Bitte vorher anmelden.'(u06).
    RETURN.
  ENDIF.
*
  CALL SCREEN 200
     STARTING AT  5  5
     ENDING AT  100 10.
  IF g_return_code IS INITIAL. RETURN. ENDIF.
*
  CALL FUNCTION 'FTP_SERVER_TO_R3'
    EXPORTING
      handle               = g_ftp_handle
      fname                = g_edit_filename
*     CHARACTER_MODE       =
    IMPORTING
      blob_length          = l_blob_length
    TABLES
      blob                 = lt_blob
*     TEXT                 =
    EXCEPTIONS
      tcpip_error          = 1
      command_error        = 2
      data_error           = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.
*
  PERFORM set_gd_dirname.
  CONCATENATE gd_dirname g_edit_filename
    INTO g_current_file.
*
  CALL FUNCTION 'ISH_N2_WRITE_FILE_APP_SERVER'
    EXPORTING
      ss_filename = g_current_file
      ss_filesize = l_blob_length
    TABLES
      ss_bin_file = lt_blob
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.
*
  MESSAGE ID 'N2PC' TYPE 'S' NUMBER '103'
       WITH 'Datei:'(m02) g_current_file 'übertragen'(m06).
  PERFORM build_new_tree.
*
ENDFORM.             " ftp_get
*
**********************************************************************
*
FORM ftp_put.
*
  DATA l_file_size           TYPE i.
  DATA l_rn2pcraw255         TYPE TABLE OF rn2pcraw255.
  DATA lt_selected_nodes     TYPE lvc_t_nkey.
  DATA lv_selected_node      TYPE lvc_nkey.
  DATA l_fieldname           TYPE lvc_fname.                "#EC NEEDED
*
  IF g_ftp_handle IS INITIAL.
    MESSAGE ID 'N2PC' TYPE 'S' NUMBER '102'
      WITH 'Sie haben keine Verbindung zum FTP.'(u05)
           'Bitte vorher anmelden.'(u06).
    RETURN.
  ENDIF.
*
*get Global wa_t_file_list
  CALL METHOD g_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_selected_nodes.
*
  IF lt_selected_nodes IS INITIAL.
    CALL METHOD g_alv_tree->get_selected_item
      IMPORTING
        e_selected_node = lv_selected_node
        e_fieldname     = l_fieldname.
    IF lv_selected_node IS INITIAL.
      MESSAGE ID 'N2PC' TYPE 'S' NUMBER '101'
        WITH 'Kein Blatt oder Knoten ausgewählt'(s01).
    ELSE.
      READ TABLE gt_t_file_list INTO gw_t_file_list
        WITH KEY g_node = lv_selected_node.
    ENDIF.
* no line selected
  ELSE.
    LOOP AT lt_selected_nodes INTO lv_selected_node.
      IF NOT lv_selected_node IS INITIAL.
        READ TABLE gt_t_file_list INTO gw_t_file_list
          WITH KEY g_node = lv_selected_node.
      ENDIF.
      IF gw_t_file_list-type = 'F'.
        CALL FUNCTION 'ISH_N2_READ_FILE_APP_SERVER'
          EXPORTING
            ss_file      = gw_t_file_list-full_name
          IMPORTING
            ss_file_size = l_file_size
          TABLES
            ss_bin_file  = l_rn2pcraw255
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
*
        CALL FUNCTION 'FTP_R3_TO_SERVER'
          EXPORTING
            handle        = g_ftp_handle
            fname         = gw_t_file_list-name
            blob_length   = l_file_size
          TABLES
            blob          = l_rn2pcraw255
          EXCEPTIONS
            tcpip_error   = 1
            command_error = 2
            data_error    = 3
            OTHERS        = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.
        MESSAGE ID 'N2PC' TYPE 'S' NUMBER '103'
           WITH 'Datei:'(m02) gw_t_file_list-name 'übertragen'(m06).
      ENDIF.
    ENDLOOP.
  ENDIF.
*
ENDFORM.             " ftp_put
*
**********************************************************************
*
FORM show_this_q_window
  USING p_header_caption     TYPE text80
        p_dumpline_t         TYPE n2pqv_dumplin_t.
*
  CONSTANTS c_width          TYPE i VALUE 680.
  CONSTANTS c_height         TYPE i VALUE 200.
*
  DATA l_dialogbox           TYPE REF TO cl_ishmed_data_qview.
*
  IF l_dialogbox IS INITIAL.
    ADD 1 TO gs_count.
    IF gs_count > 10.
      MOVE 100 TO gs_top.
      MOVE 100 TO gs_left.
      CLEAR gs_count.
    ENDIF.
    ADD 10 TO gs_top.
    ADD 10 TO gs_left.
*
    CREATE OBJECT l_dialogbox
      EXPORTING
        width                       = c_width           "680
        height                      = c_height          "200
        top                         = gs_top            "130
        left                        = gs_left           "130
        caption                     = p_header_caption
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID 'N2PC' TYPE 'I' NUMBER '798'
        WITH 'Fehler beim Erzeugen der Dialogbox'(e08) sy-subrc
             space space.
    ENDIF.
*
    CALL METHOD l_dialogbox->set_data
      EXPORTING
        p_dumpline_t      = p_dumpline_t
        caption           = p_header_caption
        p_highlight_first = 'X'
      EXCEPTIONS
        cntl_error        = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE ID 'N2PC' TYPE 'I' NUMBER '798'
        WITH 'Fehler beim Erstellen der Dialogbox'(e09) sy-subrc
             space space.
    ENDIF.
*
  ENDIF.
*
ENDFORM.                               " show_this_q_window
*&---------------------------------------------------------------------*
*&      Form  dynpro_0100_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dynpro_0100_write .
*
  SET PARAMETER ID 'GD_DIRNAME' FIELD gd_dirname.           "#EC EXISTS
*call PAI/PAO
  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'SET_GD_DIRNAME'.
*
ENDFORM.                    " dynpro_0100_write
************************************************************************
FORM set_gd_dirname.
*
  DATA l_string_length       TYPE i.
*
  IF gd_dirname IS INITIAL.
    GET PARAMETER ID 'GD_DIRNAME' FIELD gd_dirname.         "#EC EXISTS
  ENDIF.
  IF gd_dirname IS INITIAL.
    MOVE c_start_dir         TO gd_dirname.
  ENDIF.
  l_string_length = STRLEN( gd_dirname ).
  SUBTRACT 1 FROM l_string_length.
  IF gd_dirname+l_string_length(1) EQ c_separator
  OR gd_dirname+l_string_length(1) EQ c_backslash.
  ELSE.
    ADD 1 TO l_string_length.
    MOVE c_separator         TO gd_dirname+l_string_length(1).
  ENDIF.
*
ENDFORM.                    " set_gd_dirname
************************************************************************

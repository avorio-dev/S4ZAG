*&---------------------------------------------------------------------*
*& Report  ZAG_ZIP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zag_zip.

DATA: go_zip          TYPE REF TO cl_abap_zip,
      go_zip2         TYPE REF TO cl_abap_zip.

*--------------------------------------------------------------------*

PARAMETERS: p_locl RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND ucom,
            p_serv RADIOBUTTON GROUP g1.

*--------------------------------------------------------------------*

START-OF-SELECTION.

  FREE: go_zip, go_zip2.

  CASE 'X'.
    WHEN p_locl.
      PERFORM save_local_zip.

    WHEN p_serv.
      PERFORM save_server_zip.

  ENDCASE.



**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_DESKTOP_DIRECTORY
*&---------------------------------------------------------------------*
FORM get_desktop_directory  CHANGING y_desktop_directory TYPE string.

*  DATA: lv_desktop_directory TYPE string.
*  PERFORM get_desktop_directory CHANGING lv_desktop_directory.

  y_desktop_directory = ''.
  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory = y_desktop_directory
    EXCEPTIONS
      cntl_error        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>update_view.

ENDFORM.                    " GET_DESKTOP_DIRECTORY
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_XSTRING
*&---------------------------------------------------------------------*
FORM convert_to_xstring  USING    x_text      TYPE string
                         CHANGING y_xstr_text TYPE xstring.
  
  CLEAR y_xstr_text.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = x_text
    IMPORTING
      buffer = y_xstr_text
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

ENDFORM.                    " CONVERT_TO_XSTRING
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_IN_ZIP
*&---------------------------------------------------------------------*
FORM get_file_in_zip  USING    x_filename     TYPE string
                               xo_zip         TYPE REF TO cl_abap_zip
                      CHANGING y_subrc        TYPE sy-subrc
                               y_xstr_file    TYPE xstring.

  CLEAR: y_xstr_file, y_subrc.

  CHECK xo_zip IS NOT INITIAL.
  CALL METHOD xo_zip->get
    EXPORTING
      name                    = x_filename
*     index                   = 0
    IMPORTING
      content                 = y_xstr_file
    EXCEPTIONS
      zip_index_error         = 1
      zip_decompression_error = 2
      OTHERS                  = 3.
  y_subrc = sy-subrc.

ENDFORM.                    " GET_FILE_IN_ZIP
*&---------------------------------------------------------------------*
*&      Form  ADD_FILE_TO_ZIP
*&---------------------------------------------------------------------*
FORM add_file_to_zip  USING    x_filename     TYPE string
                               x_xstr_content TYPE xstring
                      CHANGING yo_zip         TYPE REF TO cl_abap_zip.

  CHECK yo_zip IS NOT INITIAL.
  yo_zip->add( name    = x_filename
               content = x_xstr_content ).

ENDFORM.                    " ADD_FILE_TO_ZIP
*&---------------------------------------------------------------------*
*&      Form  LOAD_ZIP_FROM_SERVER
*&---------------------------------------------------------------------*
FORM load_zip_from_server  USING    x_zip_name TYPE string
                           CHANGING yo_zip     TYPE REF TO cl_abap_zip
                                    y_xstr_zip TYPE xstring.

*--------------------------------------------------------------------*

  FREE yo_zip.
  CLEAR: y_xstr_zip.

  OPEN DATASET x_zip_name IN BINARY MODE FOR INPUT.
  IF sy-subrc <> 0.
    CLOSE DATASET x_zip_name.
    EXIT.
  ENDIF.

  READ DATASET x_zip_name INTO y_xstr_zip.
  CREATE OBJECT yo_zip.

  yo_zip->load( y_xstr_zip ).

  CLOSE DATASET x_zip_name.

ENDFORM.                    " LOAD_ZIP_FROM_SERVER
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOCAL_ZIP
*&---------------------------------------------------------------------*
FORM save_local_zip .

  DATA: lv_xstr_content TYPE xstring,

        lv_subrc        TYPE sy-subrc,
        lv_xstr_file    TYPE xstring,

        lv_xstr_zip     TYPE xstring,
        lv_xstr_zip2    TYPE xstring,

        lt_bin          TYPE solix_tab,
        lv_binfilesize  TYPE i,

        lv_desktop_dir  TYPE string,
        lv_outpath      TYPE string.

*--------------------------------------------------------------------*

  CREATE OBJECT go_zip.
  CREATE OBJECT go_zip2.

*--------------------------------------------------------------------*

  PERFORM get_file_in_zip USING    'file 1.txt' go_zip
                          CHANGING lv_subrc lv_xstr_file.


  IF lv_xstr_file IS INITIAL.

    PERFORM convert_to_xstring USING    'Testo di prova'
                               CHANGING lv_xstr_content.

    PERFORM add_file_to_zip    USING    'file 1.txt' lv_xstr_content
                               CHANGING go_zip.

    lv_xstr_zip = go_zip->save( ).

  ENDIF.

*--------------------------------------------------------------------*

  PERFORM get_file_in_zip USING    'file 1.txt' go_zip
                          CHANGING lv_subrc lv_xstr_file.

  IF lv_xstr_file IS INITIAL.

    PERFORM convert_to_xstring USING    'Testo di prova 2'
                               CHANGING lv_xstr_content.

    PERFORM add_file_to_zip    USING    'file 1.txt' lv_xstr_content
                               CHANGING go_zip.

    lv_xstr_zip = go_zip->save( ).

  ENDIF.

*--------------------------------------------------------------------*

  PERFORM get_file_in_zip USING    'file 2.txt' go_zip
                          CHANGING lv_subrc lv_xstr_file.

  IF lv_xstr_file IS INITIAL.

    PERFORM convert_to_xstring USING    'Testo di prova 2'
                               CHANGING lv_xstr_content.

    PERFORM add_file_to_zip    USING    'file 2.txt' lv_xstr_content
                               CHANGING go_zip.

    lv_xstr_zip = go_zip->save( ).

  ENDIF.

*--------------------------------------------------------------------*

  PERFORM add_file_to_zip USING    'ZIP 2.zip' lv_xstr_zip
                          CHANGING go_zip2.

  lv_xstr_zip2 = go_zip2->save( ).

*--------------------------------------------------------------------*

  lt_bin         = cl_bcs_convert=>xstring_to_solix( lv_xstr_zip2 ).
  lv_binfilesize = xstrlen( lv_xstr_zip2 ).

  PERFORM get_desktop_directory CHANGING lv_desktop_dir.

  CONCATENATE lv_desktop_dir 'ZIP 2.zip'
              INTO lv_outpath SEPARATED BY '\'.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename     = lv_outpath
      filetype     = 'BIN'
      bin_filesize = lv_binfilesize
    TABLES
      data_tab     = lt_bin.

  WRITE: /, 'FILE GENERATO:', / , lv_outpath.

ENDFORM.                    " SAVE_LOCAL_ZIP
*&---------------------------------------------------------------------*
*&      Form  SAVE_SERVER_ZIP
*&---------------------------------------------------------------------*
FORM save_server_zip .

  DATA: lv_zip_path     TYPE string VALUE '/tmp/ZAG_ZIP.zip',
        lv_xstr_zip     TYPE xstring,

        lv_xstr_content TYPE xstring,
        lv_xstr_file    TYPE xstring,
        lv_subrc        TYPE sy-subrc,

        lv_desktop_dir  TYPE string,
        lt_bin          TYPE solix_tab,
        lv_binfilesize  TYPE i.


*--------------------------------------------------------------------*

  PERFORM load_zip_from_server USING    lv_zip_path
                               CHANGING go_zip
                                        lv_xstr_zip.

  IF go_zip IS INITIAL.
    CREATE OBJECT go_zip.
  ENDIF.

*--------------------------------------------------------------------*

  PERFORM get_file_in_zip USING    'file 1.txt' go_zip
                          CHANGING lv_subrc lv_xstr_file.

  IF lv_xstr_file IS INITIAL.

    PERFORM convert_to_xstring USING    'Testo di prova'
                               CHANGING lv_xstr_content.

    PERFORM add_file_to_zip    USING    'file 1.txt' lv_xstr_content
                               CHANGING go_zip.

    lv_xstr_zip = go_zip->save( ).

  ENDIF.

*--------------------------------------------------------------------*

  PERFORM get_file_in_zip USING    'file 2.txt' go_zip
                          CHANGING lv_subrc lv_xstr_file.

  IF lv_xstr_file IS INITIAL.

    PERFORM convert_to_xstring USING    'Testo di prova 2'
                               CHANGING lv_xstr_content.

    PERFORM add_file_to_zip    USING    'file 2.txt' lv_xstr_content
                               CHANGING go_zip.

    lv_xstr_zip = go_zip->save( ).

  ENDIF.

*--------------------------------------------------------------------*

  OPEN DATASET lv_zip_path FOR OUTPUT IN BINARY MODE.
  IF sy-subrc <> 0.
    CLOSE DATASET lv_zip_path.
    EXIT.
  ENDIF.

  TRANSFER lv_xstr_zip TO lv_zip_path.

  CLOSE DATASET lv_zip_path.
  WRITE: / , 'FILE GENERATO:', / , lv_zip_path.


  lt_bin         = cl_bcs_convert=>xstring_to_solix( lv_xstr_zip ).
  lv_binfilesize = xstrlen( lv_xstr_zip ).

  PERFORM get_desktop_directory CHANGING lv_desktop_dir.

  CONCATENATE lv_desktop_dir 'ZIP 2.zip'
              INTO lv_zip_path SEPARATED BY '\'.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename     = lv_zip_path
      filetype     = 'BIN'
      bin_filesize = lv_binfilesize
    TABLES
      data_tab     = lt_bin.

  WRITE: / , 'FILE GENERATO:', / , lv_zip_path.

  CHECK 1 = 2.
  DELETE DATASET lv_zip_path.

ENDFORM.                    " SAVE_SERVER_ZIP

CLASS zag_cl_filer DEFINITION
  INHERITING FROM zag_cl_converter
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF tc_file_source,
        local  TYPE char1 VALUE 'L' ##NO_TEXT,
        server TYPE char1 VALUE 'S' ##NO_TEXT,
      END OF tc_file_source,

      BEGIN OF tc_filetype,
        csv  TYPE char3 VALUE 'CSV'  ##NO_TEXT,
        xlsx TYPE char4 VALUE 'XLSX' ##NO_TEXT,
      END OF tc_filetype,

      BEGIN OF tc_user_exit,
        pre_sap_to_string  TYPE string VALUE 'PRE_SAP_TO_STRING',
        post_sap_to_string TYPE string VALUE 'POST_SAP_TO_STRING',
        pre_string_to_sap  TYPE string VALUE 'PRE_STRING_TO_SAP',
        post_string_to_sap TYPE string VALUE 'POST_STRING_TO_SAP',
      END OF tc_user_exit.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      f4_help_dir_input
        IMPORTING
          !xv_source     TYPE abap_char1 DEFAULT tc_file_source-local
        EXPORTING
          !yv_path_input TYPE string,

      f4_help_dir_output
        IMPORTING
          !xv_source      TYPE abap_char1 DEFAULT tc_file_source-local
        EXPORTING
          !yv_path_output TYPE string,

      get_desktop_directory
        RETURNING VALUE(yv_desktop_dir) TYPE string.

    METHODS:
      file_download
        IMPORTING
          !xv_filename     TYPE string
          !xt_sap_table    TYPE table
          !xv_source       TYPE char1         DEFAULT tc_file_source-local
          !xv_header       TYPE os_boolean    DEFAULT abap_true
          !xo_exit_handler TYPE REF TO object OPTIONAL
        EXCEPTIONS
          not_supported_file
          unable_define_structdescr
          unable_open_path,

      file_upload
        IMPORTING
          !xv_filename           TYPE string
          !xv_source             TYPE char1         DEFAULT tc_file_source-local
          !xv_header             TYPE os_boolean    DEFAULT abap_true
          !xo_exit_handler       TYPE REF TO object OPTIONAL
        EXPORTING
          !yt_conversions_errors TYPE tt_conversions_errors
        CHANGING
          !yt_sap_data           TYPE table
        EXCEPTIONS
          not_supported_file
          unable_define_structdescr
          input_error
          unable_open_path
          empty_file.

  PROTECTED SECTION.


  PRIVATE SECTION.

    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF tc_symbols,
        lect_upper   TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'   ##NO_TEXT,
        lect_lower   TYPE string VALUE 'abcdefghijklmnopqrstuvwxyz'   ##NO_TEXT,
        digit        TYPE string VALUE '0123456789'                   ##NO_TEXT,
        symb         TYPE string VALUE '!"%/=?;,.:-_@&+*()[]{}<>€$£'  ##NO_TEXT,
        lect_acc     TYPE string VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'             ##NO_TEXT,
        exp_notation TYPE string VALUE '0123456789.,+-E'              ##NO_TEXT,
      END OF tc_symbols,

      BEGIN OF tc_exception_msg,
        unable_read_file  TYPE string VALUE 'Unable read file'                   ##NO_TEXT,
        unable_def_struct TYPE string VALUE 'Unable define Structure Descriptor' ##NO_TEXT,
        input_error       TYPE string VALUE 'Input error'                        ##NO_TEXT,
        internal_error    TYPE string VALUE 'Internal error occurred'            ##NO_TEXT,
        not_implemented   TYPE string VALUE 'Exit method not implemented'        ##NO_TEXT,
      END OF tc_exception_msg,

      BEGIN OF tc_conversion_msg,
        unmanaged_dtype TYPE string VALUE 'Unmanaged data type' ##NO_TEXT,
        format_number   TYPE string VALUE 'Wrong number format' ##NO_TEXT,
        format_data     TYPE string VALUE 'Wrong data format'   ##NO_TEXT,
        format_time     TYPE string VALUE 'Wrong time format'   ##NO_TEXT,
        implaus_number  TYPE string VALUE 'Implausible number'  ##NO_TEXT,
        implaus_data    TYPE string VALUE 'Implausible data'    ##NO_TEXT,
        implaus_time    TYPE string VALUE 'Implausible time'    ##NO_TEXT,
      END OF tc_conversion_msg.

    CONSTANTS:
      c_mandt TYPE fieldname VALUE 'MANDT' ##NO_TEXT.


    " Data
    "-------------------------------------------------
    CLASS-DATA:
      gr_typekind_charlike TYPE RANGE OF abap_typekind,
      gr_typekind_date     TYPE RANGE OF abap_typekind,
      gr_typekind_numbers  TYPE RANGE OF abap_typekind,
      gr_typekind_time     TYPE RANGE OF abap_typekind.

    DATA:
      gref_sap_data   TYPE REF TO data,
      gv_filename     TYPE string,
      gv_header       TYPE abap_char1,
      go_exit_handler TYPE REF TO object,

      go_structdescr  TYPE REF TO cl_abap_structdescr,
      gt_fcat         TYPE lvc_t_fcat,
      gv_separator    TYPE abap_char1,

      gt_str_data     TYPE string_table.


    " Methods
    "-------------------------------------------------

    METHODS:
      init_instance
        IMPORTING
          !xt_sap_table    TYPE table
          !xv_filename     TYPE string
          !xv_header       TYPE abap_char1    DEFAULT abap_true
          !xo_exit_handler TYPE REF TO object OPTIONAL
        EXCEPTIONS
          unable_define_structdescr,

      download_csv_local
        RAISING
          cx_ai_system_fault,

      download_csv_server
        RAISING
          cx_ai_system_fault,

      download_excel_local
        RAISING
          cx_ai_system_fault,

      download_excel_server
        RAISING
          cx_ai_system_fault,

      upload_csv_local
        RAISING
          cx_ai_system_fault,

      upload_csv_server
        RAISING
          cx_ai_system_fault,

      upload_excel_local
        RAISING
          cx_ai_system_fault.

ENDCLASS.



CLASS zag_cl_filer IMPLEMENTATION.


  METHOD f4_help_dir_input.

    DATA:
      lv_path           TYPE string VALUE IS INITIAL,

      lv_server         TYPE msxxlist-name,
      lv_path_tmp       TYPE dxfields-longpath,
      lv_instancenumber TYPE instanz-systemnr VALUE IS INITIAL,

      lt_filetable      TYPE filetable,
      lcl_ref_itab      TYPE REF TO file_table,
      lv_rc             TYPE i.

    "-------------------------------------------------

    CASE xv_source.

      WHEN tc_file_source-server.

        CALL FUNCTION 'GET_SYSTEM_NUMBER'
          IMPORTING
            instancenumber = lv_instancenumber.

        lv_server = |{ sy-host }_{ sy-sysid }_{ lv_instancenumber }|.
        CONDENSE lv_server NO-GAPS.

        lv_path_tmp = ''.
        CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
          EXPORTING
            i_location_flag = 'A'
            i_server        = lv_server
            filemask        = '*.*'
            fileoperation   = 'R'
          IMPORTING
            o_path          = lv_path_tmp
          EXCEPTIONS
            rfc_error       = 1
            error_with_gui  = 2
            OTHERS          = 3.
        CHECK sy-subrc EQ 0.
        lv_path = lv_path_tmp.

      WHEN tc_file_source-local.

        lv_path = get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          EXPORTING
            initial_directory = lv_path
          CHANGING
            file_table        = lt_filetable
            rc                = lv_rc.
        ASSIGN lt_filetable[ 1 ] TO FIELD-SYMBOL(<filetable>).
        CHECK sy-subrc EQ 0.
        lv_path = <filetable>-filename.

    ENDCASE.

    yv_path_input = lv_path.

  ENDMETHOD.


  METHOD f4_help_dir_output.

    DATA:
      lv_path TYPE string VALUE IS INITIAL.

    "------------------------------------------------

    CASE xv_source.

      WHEN tc_file_source-server.

        CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
          EXPORTING
*           directory        = ''
            filemask         = '.dummysap'
          IMPORTING
            serverfile       = lv_path
          EXCEPTIONS
            canceled_by_user = 1
            OTHERS           = 2.
        CHECK sy-subrc EQ 0.

      WHEN tc_file_source-local.

        lv_path = get_desktop_directory( ).

        CALL METHOD cl_gui_frontend_services=>directory_browse
          EXPORTING
            window_title         = ''
            initial_folder       = lv_path
          CHANGING
            selected_folder      = lv_path
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.
        CHECK sy-subrc EQ 0.

    ENDCASE.

    yv_path_output = lv_path.

  ENDMETHOD.


  METHOD get_desktop_directory.

    yv_desktop_dir = ''.
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = yv_desktop_dir
      EXCEPTIONS
        cntl_error        = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>update_view.

  ENDMETHOD.


  METHOD file_download.

    DATA:
      lv_filetype TYPE char10 VALUE IS INITIAL.

    FIELD-SYMBOLS:
      <gt_sap_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    "Check if file is supported
    "-------------------------------------------------
    IF xv_filename CP '*.xlsx'.
      lv_filetype      = tc_filetype-xlsx.
      me->gv_separator = tc_separator-horizontal_tab.

    ELSEIF xv_filename CP '*.csv'.
      lv_filetype      = tc_filetype-csv.
      me->gv_separator = tc_separator-semicolon.

    ELSE.
      RAISE not_supported_file.

    ENDIF.

    init_instance(
      EXPORTING
        xt_sap_table              = xt_sap_table
        xv_filename               = xv_filename
        xv_header                 = xv_header
        xo_exit_handler           = xo_exit_handler
      EXCEPTIONS
        unable_define_structdescr = 1
        OTHERS                    = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structdescr.
    ENDIF.


    "Convert SAP Table data into String table Data
    "-------------------------------------------------
    ASSIGN me->gref_sap_data->* TO <gt_sap_data>.
    me->gt_str_data = me->conv_tsap_to_ext(
                        xt_tsap_int  = <gt_sap_data>
                        xv_header    = me->gv_header
                        xv_separator = me->gv_separator
    ).


    "-------------------------------------------------
    "Start Download on Local / Server
    "-------------------------------------------------
    TRY.
        CASE lv_filetype.

          WHEN tc_filetype-csv.

            CASE xv_source.
              WHEN tc_file_source-local.

                download_csv_local( ).

              WHEN tc_file_source-server.

                download_csv_server( ).

            ENDCASE.

          WHEN tc_filetype-xlsx.

            CASE xv_source.
              WHEN tc_file_source-local.

                download_excel_local( ).

              WHEN tc_file_source-server.

                download_excel_server( ).

            ENDCASE.
        ENDCASE.

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_except_msg) = lx_ai_system_fault->get_text( ).
        RAISE unable_open_path.
    ENDTRY.

  ENDMETHOD.


  METHOD file_upload.

    DATA:
      lv_filetype TYPE char10 VALUE IS INITIAL.

    "-------------------------------------------------

    CLEAR: yt_sap_data[], yt_conversions_errors[].


    "Check if file is supported
    "-------------------------------------------------
    IF xv_filename CP '*.xlsx'.
      lv_filetype      = tc_filetype-xlsx.
      me->gv_separator = tc_separator-horizontal_tab.

    ELSEIF xv_filename CP '*.csv'.
      lv_filetype      = tc_filetype-csv.
      me->gv_separator = tc_separator-semicolon.

    ELSE.
      RAISE not_supported_file.
    ENDIF.

    init_instance(
      EXPORTING
        xt_sap_table              = yt_sap_data
        xv_filename               = xv_filename
        xv_header                 = xv_header
        xo_exit_handler           = xo_exit_handler
      EXCEPTIONS
        unable_define_structdescr = 1
        OTHERS                    = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structdescr.
    ENDIF.


    "-------------------------------------------------
    "Start Download on Local / Serveer
    "-------------------------------------------------
    TRY.
        CASE lv_filetype.

          WHEN tc_filetype-csv.

            CASE xv_source.
              WHEN tc_file_source-local.

                me->upload_csv_local( ).

              WHEN tc_file_source-server.

                me->upload_csv_server( ).

            ENDCASE.

          WHEN tc_filetype-xlsx.

            CASE xv_source.
              WHEN tc_file_source-local.

                "Upload from XLSX with in-built conversion in SAP Format
                "-------------------------------------------------
                me->upload_excel_local( ).

              WHEN tc_file_source-server.

                RAISE input_error.

            ENDCASE.

          WHEN OTHERS.
            RAISE not_supported_file.

        ENDCASE.

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_excp_msg) = lx_ai_system_fault->get_text( ).
        RAISE unable_open_path.
    ENDTRY.



    "Conversion in SAP Format for CSV
    "-------------------------------------------------
    me->conv_tsap_to_int(
      EXPORTING
        xt_tsap_ext           = me->gt_str_data[]
        xv_header             = me->gv_header
        xv_separator          = me->gv_separator
      CHANGING
        yt_tsap_int           = yt_sap_data
        yt_conversions_errors = yt_conversions_errors
      EXCEPTIONS
        conversion_error      = 1
        plausibility_error    = 2
        OTHERS                = 3
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lines( yt_sap_data ) EQ 0.
      RAISE empty_file.
    ENDIF.

  ENDMETHOD.


  METHOD init_instance.

    FIELD-SYMBOLS:
     <sap_table> TYPE STANDARD TABLE.


    "Set Data Table
    "---------------------------------------------------------------
    CREATE DATA me->gref_sap_data LIKE xt_sap_table.
    me->gv_filename    = xv_filename.
    me->gv_header      = xv_header.
    me->go_exit_handler = xo_exit_handler.

    ASSIGN me->gref_sap_data->* TO <sap_table>.
    APPEND LINES OF xt_sap_table TO <sap_table>.



    "Set Fieldcat and Fields Descriptor
    "---------------------------------------------------------------
    TRY.
        me->get_fieldcat_from_data(
          EXPORTING
            xt_sap_table              = <sap_table>
          IMPORTING
            yo_structdescr            = me->go_structdescr
            yt_fcat                   = me->gt_fcat
        ).

        DELETE me->gt_fcat WHERE fieldname EQ c_mandt.


      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        DATA(lv_excp_msg) = lx_ai_system_fault->get_text( ).
        RAISE unable_define_structdescr.
    ENDTRY.

  ENDMETHOD.


  METHOD download_csv_local.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = me->gv_filename
      CHANGING
        data_tab                = me->gt_str_data[]
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.

  ENDMETHOD.


  METHOD download_csv_server.

    OPEN DATASET me->gv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.

      CLOSE DATASET me->gv_filename.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.

    LOOP AT me->gt_str_data ASSIGNING FIELD-SYMBOL(<str_data>).
      TRANSFER <str_data> TO me->gv_filename.
    ENDLOOP.

    CLOSE DATASET me->gv_filename.

  ENDMETHOD.


  METHOD download_excel_local.

*    TYPES: BEGIN OF ts_string,
*             string TYPE string,
*           END OF ts_string.
*
*    DATA lt_str_data TYPE TABLE OF ts_string.
*
*    get_fieldcat_from_data(
*      EXPORTING
*        xt_sap_table              = lt_str_data
*      IMPORTING
*        yt_fcat                   = DATA(lt_fcat)
*      EXCEPTIONS
*        unable_define_structdescr = 1
*        OTHERS                    = 2
*    ).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    lt_str_data = VALUE #( FOR <str> IN xt_str_data ( string = <str> ) ).
*    DATA(lref_str_data) = REF #( lt_str_data ).
*
*
*    cl_salv_bs_lex=>export_from_result_data_table(
*      EXPORTING
*        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
*        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = lref_str_data
*                                                                           t_fieldcatalog  = lt_fcat )
*      IMPORTING
*        er_result_file       = DATA(lv_xdata) ).



    "Standard Version ->
    "Conversion of fields like data, numbers exc.
    "will be performed by SAP Standard
    "-------------------------------------------------
    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = me->gref_sap_data
                                                                           t_fieldcatalog  = me->gt_fcat )
      IMPORTING
        er_result_file       = DATA(lv_xdata) ).


    DATA(lv_xlength) = xstrlen( lv_xdata ).
    DATA(lt_bin_tab) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_xdata ).

    cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize              = lv_xlength           " File length for binary files
          filename                  = me->gv_filename      " Name of file
          filetype                  = 'BIN'                " File type (ASCII, binary ...)
        CHANGING
          data_tab                  = lt_bin_tab          " Transfer table
        EXCEPTIONS
          file_write_error          = 1                    " Cannot write to file
          no_batch                  = 2                    " Cannot execute front-end function in background
          gui_refuse_filetransfer   = 3                    " Incorrect Front End
          invalid_type              = 4                    " Invalid value for parameter FILETYPE
          no_authority              = 5                    " No Download Authorization
          unknown_error             = 6                    " Unknown error
          header_not_allowed        = 7                    " Invalid header
          separator_not_allowed     = 8                    " Invalid separator
          filesize_not_allowed      = 9                    " Invalid file size
          header_too_long           = 10                   " Header information currently restricted to 1023 bytes
          dp_error_create           = 11                   " Cannot create DataProvider
          dp_error_send             = 12                   " Error Sending Data with DataProvider
          dp_error_write            = 13                   " Error Writing Data with DataProvider
          unknown_dp_error          = 14                   " Error when calling data provider
          access_denied             = 15                   " Access to File Denied
          dp_out_of_memory          = 16                   " Not enough memory in data provider
          disk_full                 = 17                   " Storage medium is full.
          dp_timeout                = 18                   " Data provider timeout
          file_not_found            = 19                   " Could not find file
          dataprovider_exception    = 20                   " General Exception Error in DataProvider
          control_flush_error       = 21                   " Error in Control Framework
          not_supported_by_gui      = 22                   " GUI does not support this
          error_no_gui              = 23                   " GUI not available
          OTHERS                    = 24
        ).
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.

  ENDMETHOD.


  METHOD download_excel_server.

    DATA: lt_data_ref TYPE REF TO data.

    FIELD-SYMBOLS: <t_sap_data> TYPE STANDARD TABLE.

    "-------------------------------------------------

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data          = me->gref_sap_data
                                                                           t_fieldcatalog  = me->gt_fcat )
      IMPORTING
        er_result_file       = DATA(lv_xdata) ).

    DATA(lv_xlength) = xstrlen( lv_xdata ).

    "-------------------------------------------------

    OPEN DATASET me->gv_filename FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.

      CLOSE DATASET me->gv_filename.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.

    TRANSFER lv_xdata TO me->gv_filename.

    CLOSE DATASET me->gv_filename.

  ENDMETHOD.


  METHOD upload_csv_local.

    CLEAR me->gt_str_data[].
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = me->gv_filename
        filetype = 'ASC'
      CHANGING
        data_tab = me->gt_str_data[]
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.

  ENDMETHOD.


  METHOD upload_csv_server.

    DATA:
      ls_str_data TYPE string.

    "-------------------------------------------------

    OPEN DATASET me->gv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.

      CLOSE DATASET me->gv_filename.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.

    DO.

      CLEAR ls_str_data.
      READ DATASET me->gv_filename INTO ls_str_data.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND ls_str_data TO me->gt_str_data.

    ENDDO.

    CLOSE DATASET me->gv_filename.

  ENDMETHOD.


  METHOD upload_excel_local.

    DATA:
      lv_except_msg    TYPE string,
      lt_bin_tab       TYPE solix_tab,
      lcl_excel_ref    TYPE REF TO cl_fdt_xl_spreadsheet,
      lcl_table_descr  TYPE REF TO cl_abap_tabledescr,
      lcl_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <t_excel_data> TYPE STANDARD TABLE,
      <str_data>     TYPE string.

    "-------------------------------------------------

    CLEAR: me->gt_str_data[].

    "Read in binary the excel
    "-------------------------------------------------
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = me->gv_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = DATA(lv_filelen)
        header     = DATA(lv_headerx)
      CHANGING
        data_tab   = lt_bin_tab[]
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-unable_read_file.

    ENDIF.


    "Conversion in XSTRING NB cl_bcs_convert not returns headerx
    "-------------------------------------------------
*      DATA(lv_xstr_tab) = cl_bcs_convert=>solix_to_xstring( it_solix   = lt_bin_tab
*                                                            iv_size    = lv_filelen ).
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelen
      IMPORTING
        buffer       = lv_headerx
      TABLES
        binary_tab   = lt_bin_tab[]
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-internal_error.

    ENDIF.


    "Reading excel content using HEADERX
    "-------------------------------------------------
    TRY.
        lcl_excel_ref = NEW cl_fdt_xl_spreadsheet(
          document_name = me->gv_filename
          xdocument     = lv_headerx
        ).


      CATCH cx_fdt_excel_core INTO DATA(lx_excel_core).
        lv_except_msg = lx_excel_core->get_text( ).
    ENDTRY .

    CHECK lcl_excel_ref IS BOUND.

    lcl_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheets) ).


    "Takes the first worksheet as default
    "-------------------------------------------------
    ASSIGN lt_worksheets[ 1 ] TO FIELD-SYMBOL(<woksheetname>).
    DATA(lcl_data_ref) = lcl_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <woksheetname> ).
    ASSIGN lcl_data_ref->* TO <t_excel_data>.


    "Convert to CSV String format
    "---------------------------------------------------------------
    LOOP AT <t_excel_data> ASSIGNING FIELD-SYMBOL(<excel>).

      APPEND INITIAL LINE TO me->gt_str_data ASSIGNING <str_data>.

      LOOP AT me->go_structdescr->components ASSIGNING FIELD-SYMBOL(<comp>).

        ASSIGN COMPONENT sy-tabix OF STRUCTURE <excel> TO FIELD-SYMBOL(<excel_col>).
        CHECK sy-subrc EQ 0.
        <str_data> = COND #(
          WHEN <str_data> EQ '' THEN <excel_col>
          WHEN <str_data> NE '' THEN |{ <str_data> }{ me->gv_separator }{ <excel_col> }|
        ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
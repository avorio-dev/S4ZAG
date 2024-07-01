REPORT zag_oo_report.

INCLUDE zag_oo_report_def.
INCLUDE zag_oo_report_imp.


"SELECTION SCREEN
**********************************************************************

TABLES: lfa1, lfb1.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_budat TYPE sy-datum.
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr,
                s_bukrs FOR lfb1-bukrs.
SELECTION-SCREEN END OF BLOCK a1.


"MAIN PROCESS
**********************************************************************
INITIALIZATION.
  DATA(lo_selection_screen) = NEW lcl_selection_screen( ).

START-OF-SELECTION.

  TRY.

      " Get Input parameters and checks them
      "-------------------------------------------------
      lo_selection_screen->set_params(
        xs_params = VALUE #( budat = p_budat )
        xs_selopt = VALUE #( lifnr = s_lifnr[]
                             bukrs = s_bukrs[] )
      ).

      " Data Processing
      "-------------------------------------------------
      DATA(lo_data_processor)   = NEW lcl_data_processor( ).
      lo_data_processor->main( lo_selection_screen ).


      " EXCEPTIONS
      "-------------------------------------------------
    CATCH lcx_selscreen INTO DATA(lx_selscreen).
      MESSAGE lx_selscreen->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.

    CATCH lcx_processor INTO DATA(lx_processor).
      MESSAGE lx_processor->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.

  ENDTRY.

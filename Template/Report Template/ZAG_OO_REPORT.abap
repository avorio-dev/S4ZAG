REPORT zag_oo_report.

INCLUDE zag_oo_report_def.
INCLUDE zag_oo_report_imp.


"SELECTION SCREEN
**********************************************************************

TABLES: but000, t001.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_partn FOR but000-partner,
                s_bukrs FOR t001-bukrs.
SELECTION-SCREEN END OF BLOCK a1.


"MAIN PROCESS
**********************************************************************
INITIALIZATION.
  DATA(lo_selection_screen) = NEW lcl_selection_screen( ).
  DATA(lo_data_processor)   = NEW lcl_data_processor( ).

START-OF-SELECTION.

  TRY.

      " Get Input parameters and checks them
      "-------------------------------------------------
      lo_selection_screen->set_params( xr_partner = s_partn[]
                                       xr_bukrs   = s_bukrs[] ).

      lo_selection_screen->check_params( ).



      " Data Processing
      "-------------------------------------------------
      lo_data_processor->main( xo_selection_screen = lo_selection_screen ).



      " EXCEPTIONS
      "-------------------------------------------------
    CATCH lcx_selscreen INTO DATA(lx_selscreen).
      MESSAGE lx_selscreen->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.

    CATCH lcx_generic INTO DATA(lx_generic).
      MESSAGE lx_generic->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.

    CATCH lcx_processor INTO DATA(lx_processor).
      MESSAGE lx_processor->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.

  ENDTRY.

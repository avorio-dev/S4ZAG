CLASS zag_cl_odatav4_vendor_model DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_v4_abs_model_prov
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zag_if_odatav4_vendor.
    METHODS /iwbep/if_v4_mp_basic~define REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
        ts_cds_views         FOR zag_if_odatav4_vendor~ts_cds_views,
        cc_entity_set_names  FOR zag_if_odatav4_vendor~cc_entity_set_names,
        cc_entity_type_names FOR zag_if_odatav4_vendor~cc_entity_type_names,
        cc_nav_prop_names    FOR zag_if_odatav4_vendor~cc_nav_prop_names.

    METHODS:
      define_vendor
        IMPORTING
          io_model TYPE REF TO /iwbep/if_v4_med_model
        RAISING
          /iwbep/cx_gateway,

      define_company
        IMPORTING
          io_model TYPE REF TO /iwbep/if_v4_med_model
        RAISING
          /iwbep/cx_gateway,

      define_purchorg
        IMPORTING
          io_model TYPE REF TO /iwbep/if_v4_med_model
        RAISING
          /iwbep/cx_gateway.

ENDCLASS.


CLASS zag_cl_odatav4_vendor_model IMPLEMENTATION.
  METHOD /iwbep/if_v4_mp_basic~define.

    define_vendor( io_model ).
    define_company( io_model ).
    define_purchorg( io_model ).

  ENDMETHOD.

  METHOD define_vendor.

    DATA: ls_ref_cds_view TYPE ts_cds_views-vendor. " As internal ABAP name we use the name of the CDS view


    " Create Entity Type
    "---------------------------------------------------------------
    DATA(lo_entity_type) = io_model->create_entity_type_by_struct(
        iv_entity_type_name          = cc_entity_type_names-internal-vendor
        is_structure                 = ls_ref_cds_view
        iv_gen_prim_props            = abap_true
        iv_add_conv_to_prim_props    = abap_true
        iv_add_f4_help_to_prim_props = abap_true
    ).
    lo_entity_type->set_edm_name( iv_edm_name = cc_entity_type_names-edm-vendor ).


    " Rename external EDM names of properties so that CamelCase notation is used
    "---------------------------------------------------------------
    lo_entity_type->get_primitive_properties(
      IMPORTING
        et_property = DATA(lt_primitive_prop)
    ).

    LOOP AT lt_primitive_prop ASSIGNING FIELD-SYMBOL(<lo_primitive_prop>).

      <lo_primitive_prop>->set_edm_name(
          iv_edm_name = to_mixed( val = <lo_primitive_prop>->get_internal_name( ) )
      ).

    ENDLOOP.


    " Set Key Fields
    "---------------------------------------------------------------
    DATA(lo_primitive_prop) = lo_entity_type->get_primitive_property(
        iv_property_name = 'LIFNR'
    ).
    lo_primitive_prop->set_is_key( ).


    " Create Navigation Property
    "---------------------------------------------------------------
    DATA(lo_nav_prop) = lo_entity_type->create_navigation_property(
        iv_property_name = cc_nav_prop_names-internal-vendor_to_company
    ).
    lo_nav_prop->set_edm_name( iv_edm_name = cc_nav_prop_names-edm-vendor_to_comapny ).

    lo_nav_prop->set_target_entity_type_name( cc_entity_type_names-internal-company ).
    lo_nav_prop->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ).
    lo_nav_prop->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-none ).


    FREE lo_nav_prop.
    lo_nav_prop = lo_entity_type->create_navigation_property(
    iv_property_name = cc_nav_prop_names-internal-vendor_to_purchorg
    ).
    lo_nav_prop->set_edm_name( iv_edm_name = cc_nav_prop_names-edm-vendor_to_purchorg ).

    lo_nav_prop->set_target_entity_type_name( cc_entity_type_names-internal-purchorg ).
    lo_nav_prop->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ).
    lo_nav_prop->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-none ).


    " Create Entity Set / Add the binding of the navigation path
    "---------------------------------------------------------------
    DATA(lo_entity_set) = lo_entity_type->create_entity_set(
        iv_entity_set_name = cc_entity_set_names-internal-vendor
    ).
    lo_entity_set->set_edm_name( iv_edm_name = cc_entity_set_names-edm-vendor ).

    lo_entity_set->add_navigation_prop_binding(
      EXPORTING
        iv_navigation_property_path = CONV #( cc_nav_prop_names-internal-vendor_to_company )
        iv_target_entity_set        = cc_entity_set_names-internal-company
    ).


  ENDMETHOD.

  METHOD define_company.

    DATA: ls_ref_cds_view   TYPE ts_cds_views-company.


    "Create Entity Type
    "---------------------------------------------------------------
    DATA(lo_entity_type) = io_model->create_entity_type_by_struct(
                             iv_entity_type_name          = cc_entity_type_names-internal-company
                             is_structure                 = ls_ref_cds_view
                             iv_add_conv_to_prim_props    = abap_true
                             iv_add_f4_help_to_prim_props = abap_true
                             iv_gen_prim_props            = abap_true
    ).
    lo_entity_type->set_edm_name( iv_edm_name = cc_entity_type_names-edm-company ).


    " Rename external EDM names of properties so that CamelCase notation is used
    "---------------------------------------------------------------
    lo_entity_type->get_primitive_properties(
      IMPORTING
        et_property = DATA(lt_primitive_prop)
    ).

    LOOP AT lt_primitive_prop ASSIGNING FIELD-SYMBOL(<lo_primitive_prop>).

      <lo_primitive_prop>->set_edm_name(
          iv_edm_name = to_mixed( val = <lo_primitive_prop>->get_internal_name( ) )
      ).

    ENDLOOP.


    " Set Key Fields
    "---------------------------------------------------------------
    DATA(lo_primitive_prop) = lo_entity_type->get_primitive_property(
        iv_property_name = 'LIFNR'
    ).
    lo_primitive_prop->set_is_key( ).

    FREE lo_primitive_prop.
    lo_primitive_prop = lo_entity_type->get_primitive_property(
        iv_property_name = 'BUKRS'
    ).
    lo_primitive_prop->set_is_key( ).


    " Create Entity Set
    "---------------------------------------------------------------
    DATA(lo_entity_set) = lo_entity_type->create_entity_set(
        iv_entity_set_name = cc_entity_set_names-internal-company
    ).
    lo_entity_set->set_edm_name( iv_edm_name = cc_entity_set_names-edm-company ).


  ENDMETHOD.

  METHOD define_purchorg.

    DATA: ls_ref_cds_view TYPE ts_cds_views-purchorg.


    "Create Entity Type
    "---------------------------------------------------------------
    DATA(lo_entity_type) = io_model->create_entity_type_by_struct(
                             iv_entity_type_name          = cc_entity_type_names-internal-purchorg
                             is_structure                 = ls_ref_cds_view
                             iv_add_conv_to_prim_props    = abap_true
                             iv_add_f4_help_to_prim_props = abap_true
                             iv_gen_prim_props            = abap_true
    ).
    lo_entity_type->set_edm_name( iv_edm_name = cc_entity_type_names-edm-purchorg ).


    " Rename external EDM names of properties so that CamelCase notation is used
    "---------------------------------------------------------------
    lo_entity_type->get_primitive_properties(
      IMPORTING
        et_property = DATA(lt_primitive_prop)
    ).

    LOOP AT lt_primitive_prop ASSIGNING FIELD-SYMBOL(<lo_primitive_prop>).

      <lo_primitive_prop>->set_edm_name(
          iv_edm_name = to_mixed( val = <lo_primitive_prop>->get_internal_name( ) )
      ).

    ENDLOOP.


    " Set Key Fields
    "---------------------------------------------------------------
    DATA(lo_primitive_prop) = lo_entity_type->get_primitive_property(
        iv_property_name = 'LIFNR'
    ).
    lo_primitive_prop->set_is_key( ).

    FREE lo_primitive_prop.
    lo_primitive_prop = lo_entity_type->get_primitive_property(
        iv_property_name = 'EKORG'
    ).
    lo_primitive_prop->set_is_key( ).


    " Create Entity Set
    "---------------------------------------------------------------
    DATA(lo_entity_set) = lo_entity_type->create_entity_set(
        iv_entity_set_name = cc_entity_set_names-internal-purchorg
    ).
    lo_entity_set->set_edm_name( iv_edm_name = cc_entity_set_names-edm-purchorg ).


  ENDMETHOD.

ENDCLASS.

class ZCL_ZFT_P_FORMS_MPC_EXT definition
  public
  inheriting from ZCL_ZFT_P_FORMS_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZFT_P_FORMS_MPC_EXT IMPLEMENTATION.


  METHOD define.

    DATA lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

    super->define( ).
    lo_entity_type = model->get_entity_type( iv_entity_name = 'form_language' ).
    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZFT_P_FORMS_MPC_EXT=>TS_DEEP_ENTITY').

  ENDMETHOD.
ENDCLASS.

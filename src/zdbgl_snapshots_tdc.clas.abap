CLASS zdbgl_snapshots_tdc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbgl_snapshots .

    METHODS constructor
      IMPORTING
        VALUE(key_tdc_variant) TYPE zdbgl_tdc_variant_key
        !autosave              TYPE sap_bool OPTIONAL
      RAISING
        zcx_dbgl_snapshot .
    CLASS-METHODS create_default
      IMPORTING
        tdc_name        TYPE etobj_name
        !autosave       TYPE sap_bool OPTIONAL
      RETURNING
        VALUE(instance) TYPE REF TO zif_dbgl_snapshots
      RAISING
        zcx_dbgl_snapshot.
  PROTECTED SECTION.

    METHODS compare
      IMPORTING
        !actual        TYPE any
        !recorded      TYPE any
      RETURNING
        VALUE(unequal) TYPE sap_bool .
  PRIVATE SECTION.
    DATA: tdc_accessor      TYPE REF TO cl_apl_ecatt_tdc_api,
          tdc_key           TYPE etobj_key2,
          in_record_mode    TYPE sap_bool,
          variant           TYPE etvar_id,
          transport_request TYPE trkorr,
          autosave          TYPE sap_bool.

    METHODS get_or_create_tdc
      IMPORTING
        VALUE(key_tdc_variant) TYPE zdbgl_tdc_variant_key
      RAISING
        zcx_dbgl_snapshot.

    METHODS record
      IMPORTING
                name  TYPE clike
                value TYPE any
      RAISING   zcx_dbgl_snapshot.

    METHODS get_parameter_definition
      IMPORTING
                data_descr    TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_dbgl_snapshot.

    " Conditional create means creating if not exists yet
    " or change the parameter definition, if appropriate
    METHODS cond_create_parameter
      IMPORTING
                name  TYPE clike
                value TYPE any
      RAISING   zcx_dbgl_snapshot.

    METHODS retrieve_and_compare
      IMPORTING
                name           TYPE clike
                actual         TYPE any
      RETURNING VALUE(unequal) TYPE sap_bool
      RAISING   zcx_dbgl_snapshot.

    METHODS increment_version_no
      CHANGING
        key_tdc_variant TYPE zdbgl_tdc_variant_key.

    METHODS get_latest_version
      CHANGING
        key_tdc_variant TYPE zdbgl_tdc_variant_key.
ENDCLASS.



CLASS ZDBGL_SNAPSHOTS_TDC IMPLEMENTATION.


  METHOD compare.

    unequal = cl_abap_unit_assert=>assert_equals( exp = recorded
      act = actual quit = if_aunit_constants=>no ).

  ENDMETHOD.


  METHOD cond_create_parameter.

    DATA(type_definition) = get_parameter_definition(
      cl_abap_datadescr=>describe_by_data( value ) ).

    TRY.
        TRY.
            tdc_accessor->create_parameter( i_param_name = name
              i_param_def = type_definition ).
          CATCH cx_ecatt_tdc_access INTO DATA(failure).
            IF failure->textid = cx_ecatt_tdc_access=>parameter_exists.
              IF tdc_accessor->get_param_definition( name ) <> type_definition.
                tdc_accessor->change_parameter( EXPORTING i_param_name = name
                  i_param_def = type_definition ).
              ENDIF.
            ELSE.
              zcx_dbgl_snapshot_tdc=>wrap( failure ).
            ENDIF.
        ENDTRY.
      CATCH cx_ecatt_tdc_access INTO failure.
        zcx_dbgl_snapshot_tdc=>wrap( failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    TRY.
        GET PARAMETER ID 'ZDBGL_SNAP_RECORD' FIELD in_record_mode.
        me->variant = key_tdc_variant-variant_name.
        IF in_record_mode = abap_true AND key_tdc_variant-increment_version_no = abap_true
            AND zdbgl_utils=>must_add_to_transport_request( key_tdc_variant ) = abap_true.
          increment_version_no( CHANGING key_tdc_variant = key_tdc_variant ).
        ELSEIF key_tdc_variant-use_latest_version = abap_true.
          get_latest_version( CHANGING key_tdc_variant = key_tdc_variant ).
        ENDIF.
        tdc_key = VALUE #( type = 'ECTD' name = key_tdc_variant-name
          version = key_tdc_variant-version ).
        get_or_create_tdc( key_tdc_variant ).
        me->autosave = autosave.
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        zcx_dbgl_snapshot_tdc=>wrap( failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD create_default.

    instance ?= NEW zdbgl_snapshots_tdc( key_tdc_variant =
      VALUE #( name = tdc_name variant_name = 'ECATTDEFAULT' use_latest_version = abap_true )
      autosave = autosave ).

  ENDMETHOD.


  METHOD get_latest_version.

    SELECT version FROM ectd_ver
      WHERE name = @key_tdc_variant-name
      ORDER BY version DESCENDING
      INTO @key_tdc_variant-version
      UP TO 1 ROWS.
    ENDSELECT.

  ENDMETHOD.


  METHOD get_or_create_tdc.

    TRY.
        tdc_accessor = cl_apl_ecatt_tdc_api=>get_instance(
          i_testdatacontainer = key_tdc_variant-name
          i_testdatacontainer_version = key_tdc_variant-version
          i_write_access = in_record_mode
        ).
        IF in_record_mode = abap_true.
          zdbgl_utils=>ask_for_transport_request(
            CHANGING key_tdc_variant = key_tdc_variant ).
        ENDIF.
      CATCH cx_ecatt_tdc_access INTO DATA(read_fault).
        IF in_record_mode = abap_true.

          TRY.
              DATA(package_name) = zdbgl_utils=>create_tadir_entry(
                tdc_name = key_tdc_variant-name ).
              cl_apl_ecatt_tdc_api=>create_tdc( EXPORTING
                i_name = key_tdc_variant-name
                i_tr_order = key_tdc_variant-transport_request
                i_version = key_tdc_variant-version
                i_tadir_devclass = package_name
                i_write_access = in_record_mode
                IMPORTING
                  e_tdc_ref = tdc_accessor ).
              RETURN.
            CATCH cx_ecatt_tdc_access INTO DATA(create_fault).
              zcx_dbgl_snapshot_tdc=>wrap( create_fault ).
          ENDTRY.

        ENDIF.
        zcx_dbgl_snapshot_tdc=>wrap( read_fault ).
    ENDTRY.

    transport_request = key_tdc_variant-transport_request.

  ENDMETHOD.


  METHOD get_parameter_definition.

    IF data_descr->kind = data_descr->kind_class OR
      data_descr->kind = data_descr->kind_intf OR
      data_descr->kind = data_descr->kind_ref.
      zcx_dbgl_snapshot_tdc=>wrap( NEW zcx_dbgl_dictionary_access(
        textid = zcx_dbgl_dictionary_access=>class_intf ) ).
    ENDIF.

    IF data_descr->is_ddic_type( ) = abap_true
      OR data_descr->kind = data_descr->kind_elem.
      result = data_descr->get_relative_name( ).
      RETURN.
    ENDIF.

    IF data_descr->kind = data_descr->kind_table.
      DATA(table_descr) = CAST cl_abap_tabledescr( data_descr ).
      DATA(line_type_definition) = get_parameter_definition(
        table_descr->get_table_line_type( ) ).
      result = |STANDARD TABLE OF { line_type_definition }|.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD increment_version_no.

    SELECT version FROM ectd_ver
      WHERE name = @key_tdc_variant-name
      ORDER BY version DESCENDING
      INTO @DATA(max_version)
      UP TO 1 ROWS.
    ENDSELECT.

    key_tdc_variant-version = max_version + 1.

  ENDMETHOD.


  METHOD record.

    cond_create_parameter( name = name value = value ).
    TRY.
        tdc_accessor->set_value( i_param_name = name
          i_variant_name = variant i_param_value = value ).
        IF autosave = abap_true.
          zif_dbgl_snapshots~commit_changes( ).
        ENDIF.
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        zcx_dbgl_snapshot_tdc=>wrap( failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD retrieve_and_compare.
    DATA:
      recorded           TYPE REF TO data,
      initial_parameters TYPE etp_name_tabtype.
    FIELD-SYMBOLS:
      <recorded> TYPE any.

    CREATE DATA recorded LIKE actual.
    ASSIGN recorded->* TO <recorded>.

    " Workaround for initial parameters in non "ECATTDEFAULT" variants
    " tdc_accessor->get_value_ref reads the value for "ECATTDEFAULT" variant
    " when value is initial in another variant.
    IF zdbgl_utils=>is_tdc_parameter_initial(
        tdc_key = tdc_key param_name = name variant_name = variant ) = abap_true.
      unequal = xsdbool( actual IS NOT INITIAL ).
      RETURN.
    ENDIF.

    TRY.
        tdc_accessor->get_value_ref(
          EXPORTING
            i_param_name = name
            i_variant_name = variant
          CHANGING
            e_param_ref = recorded ).
        unequal = compare( recorded = <recorded> actual = actual ).
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        zcx_dbgl_snapshot_tdc=>wrap( failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_dbgl_snapshots~commit_changes.

    IF in_record_mode = abap_false.
      RETURN.
    ENDIF.

    TRY.
        tdc_accessor->commit_changes( EXPORTING i_tr_order = transport_request
          i_release_lock = abap_true ).
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        zcx_dbgl_snapshot_tdc=>wrap( failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_dbgl_snapshots~compare_or_record.

    IF in_record_mode = abap_true.
      record( name = name value = actual ).
      cl_abap_unit_assert=>fail( msg = text-001 level = if_aunit_constants=>tolerable
        quit = if_aunit_constants=>no ).
    ELSE.
      unequal = retrieve_and_compare( name = name actual = actual ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

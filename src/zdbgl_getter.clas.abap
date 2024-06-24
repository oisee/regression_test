CLASS zdbgl_getter DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !values  TYPE string
        !program TYPE program .
    METHODS get_simple
      IMPORTING
        !name  TYPE string
      EXPORTING
        !value TYPE simple
      RAISING
        zcx_dbgl_testcase .
    METHODS get_structur
      IMPORTING
        !name  TYPE string
      EXPORTING
        !value TYPE any
      RAISING
        zcx_dbgl_testcase
        zcx_dbgl_type_not_supported.
    METHODS get_table
      IMPORTING
        !name  TYPE string
      EXPORTING
        !value TYPE ANY TABLE
      RAISING
        zcx_dbgl_testcase .
  PROTECTED SECTION.

    DATA program TYPE program .
  PRIVATE SECTION.

    TYPES:
      xstring_tab TYPE STANDARD TABLE OF xstring .

    DATA json_xtext TYPE xstring .
    DATA abap_conversion TYPE REF TO cl_abap_conv_in_ce .

    METHODS get_hex_value
      IMPORTING
        !name        TYPE string
      RETURNING
        VALUE(value) TYPE string
      RAISING
        zcx_dbgl_testcase .
    METHODS _get_simple
      IMPORTING
        !base64_value TYPE string
      EXPORTING
        !value        TYPE simple .
    METHODS line_has_simple_type
      IMPORTING
                line             TYPE any
      RETURNING VALUE(is_simple) TYPE sap_bool.
ENDCLASS.



CLASS ZDBGL_GETTER IMPLEMENTATION.


  METHOD constructor.
    DATA:
      system_info TYPE rfcsi.

    me->program = program.

    CALL FUNCTION 'RFC_SYSTEM_INFO'
      IMPORTING
        rfcsi_export = system_info.

    json_xtext = cl_abap_codepage=>convert_to( values ).
    abap_conversion = cl_abap_conv_in_ce=>create(
      encoding = CONV abap_encoding( system_info-rfcchartyp ) ).

  ENDMETHOD.


  METHOD get_hex_value.
    DATA: node              TYPE REF TO if_sxml_node,
          node_open_element TYPE REF TO if_sxml_open_element,
          node_found        TYPE sap_bool,
          reader            TYPE REF TO if_sxml_reader,
          attribute         TYPE REF TO if_sxml_attribute,
          object_level      TYPE i.

    " create reader in every call, loop through nodes is statefull
    reader = cl_sxml_string_reader=>create( json_xtext ).
    DO.
      node = reader->read_next_node( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.

      IF node->type = if_sxml_node=>co_nt_element_open.

        node_open_element = CAST if_sxml_open_element( node ).
        LOOP AT node_open_element->get_attributes( ) INTO attribute.
          IF attribute->qname-name = 'name' AND attribute->get_value( ) = name
            AND node_open_element->if_sxml_named~qname-name = 'str'
            AND object_level = 1.
            node_found = abap_true.
          ENDIF.
        ENDLOOP.

        IF node_open_element->if_sxml_named~qname-name = 'object'.
          ADD 1 TO object_level.
        ENDIF.

      ELSEIF node->type = if_sxml_node=>co_nt_value
        AND node_found = abap_true.

        value = CAST if_sxml_value_node( node )->get_value( ).
        RETURN.

      ELSEIF node->type = if_sxml_node=>co_nt_element_close.

        IF CAST if_sxml_close_element( node )->if_sxml_named~qname-name = 'object'.
          object_level = object_level - 1.
        ENDIF.

      ENDIF.

    ENDDO.

    RAISE EXCEPTION TYPE zcx_dbgl_testcase
      EXPORTING
        textid   = zcx_dbgl_testcase=>variable_not_found
        program  = program
        variable = name.

  ENDMETHOD.


  METHOD get_simple.

    _get_simple( EXPORTING base64_value = get_hex_value( name )
      IMPORTING value = value ).

  ENDMETHOD.


  METHOD get_structur.
    DATA: node                       TYPE REF TO if_sxml_node,
          node_open_element          TYPE REF TO if_sxml_open_element,
          node_for_structure_pending TYPE sap_bool,
          reader                     TYPE REF TO if_sxml_reader,
          attribute                  TYPE REF TO if_sxml_attribute,
          object_level               TYPE i.
    FIELD-SYMBOLS: <component> TYPE any.

    " create reader in every call, loop through nodes is statefull
    reader = cl_sxml_string_reader=>create( json_xtext ).
    DO.
      node = reader->read_next_node( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.

      IF node->type = if_sxml_node=>co_nt_element_open.

        node_open_element = CAST if_sxml_open_element( node ).

        IF ( node_open_element->if_sxml_named~qname-name = 'array'
          OR node_open_element->if_sxml_named~qname-name = 'object' )
          AND node_for_structure_pending = abap_true.

          RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
            EXPORTING
              type = |deep structure with name { name }|.

        ENDIF.

        LOOP AT node_open_element->get_attributes( ) INTO attribute.

          IF attribute->qname-name = 'name'.
            IF node_open_element->if_sxml_named~qname-name = 'object'
            AND attribute->get_value( ) = name AND object_level = 1.
              node_for_structure_pending = abap_true.
            ELSEIF node_open_element->if_sxml_named~qname-name = 'str'
            AND node_for_structure_pending = abap_true.
              ASSIGN COMPONENT attribute->get_value( ) OF STRUCTURE value
                TO <component>.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF node_open_element->if_sxml_named~qname-name = 'object'.
          ADD 1 TO object_level.
        ENDIF.

      ELSEIF node->type = if_sxml_node=>co_nt_value
        AND node_for_structure_pending = abap_true.

        _get_simple( EXPORTING base64_value = CAST if_sxml_value_node( node )->get_value( )
          IMPORTING value = <component> ).

      ELSEIF node->type = if_sxml_node=>co_nt_element_close.

        IF CAST if_sxml_close_element( node )->if_sxml_named~qname-name = 'object'.
          IF node_for_structure_pending = abap_true.
            " object finished
            RETURN.
          ENDIF.
          object_level = object_level - 1.
        ENDIF.

      ENDIF.

    ENDDO.

    " value for variable <name> not found
    RAISE EXCEPTION TYPE zcx_dbgl_testcase
      EXPORTING
        textid   = zcx_dbgl_testcase=>variable_not_found
        program  = program
        variable = name.

  ENDMETHOD.


  METHOD get_table.
    DATA: node                   TYPE REF TO if_sxml_node,
          node_for_table_pending TYPE sap_bool,
          reader                 TYPE REF TO if_sxml_reader,
          line                   TYPE REF TO data,
          node_open_element      TYPE REF TO if_sxml_open_element,
          object_element_name    TYPE string,
          attribute              TYPE REF TO if_sxml_attribute,
          object_level           TYPE i.
    FIELD-SYMBOLS: <line>         TYPE any,
                   <line_element> TYPE any.

    CREATE DATA line LIKE LINE OF value.
    ASSIGN line->* TO <line>.

    " create reader in every call, loop through nodes is statefull
    reader = cl_sxml_string_reader=>create( json_xtext ).
    DO.
      node = reader->read_next_node( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.

      IF node->type = if_sxml_node=>co_nt_element_open.

        node_open_element = CAST if_sxml_open_element( node ).
        attribute_handler.
        prepare_table_line.

      ELSEIF node->type = if_sxml_node=>co_nt_value
        AND node_for_table_pending = abap_true.

        _get_simple( EXPORTING base64_value = CAST if_sxml_value_node( node )->get_value( )
          IMPORTING value = <line_element> ).

      ELSEIF node->type = if_sxml_node=>co_nt_element_close.

        close_element_handler.

      ENDIF.

    ENDDO.

    RAISE EXCEPTION TYPE zcx_dbgl_testcase
      EXPORTING
        textid   = zcx_dbgl_testcase=>variable_not_found
        program  = program
        variable = name.

  ENDMETHOD.


  METHOD line_has_simple_type.

    IF cl_abap_typedescr=>describe_by_data( line )->kind =
      cl_abap_typedescr=>kind_elem.
      is_simple = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _get_simple.
    DATA: hex_value TYPE xstring.

    hex_value = cl_http_utility=>decode_x_base64( base64_value ).

    abap_conversion->convert( EXPORTING input = hex_value
      IMPORTING data = value ).

  ENDMETHOD.
ENDCLASS.

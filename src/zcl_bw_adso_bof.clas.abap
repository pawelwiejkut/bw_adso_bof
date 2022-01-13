CLASS zcl_bw_adso_bof DEFINITION PUBLIC
    FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: BEGIN OF t_alv,
             key TYPE abap_bool.
             INCLUDE TYPE cl_rso_adso_api=>tn_s_object.
    TYPES: END OF t_alv.

    TYPES t_ty_alv TYPE STANDARD TABLE OF t_alv.

    METHODS constructor
      IMPORTING iv_data_sep     TYPE char1
                iv_esc_sign     TYPE char1
                iv_thousand_sep TYPE char1
                iv_decimal_sep  TYPE char1
                iv_header       TYPE abap_bool.

    METHODS convert_into_columns
      IMPORTING it_data        TYPE table_of_strings
      EXPORTING et_prop_fields TYPE rsds_t_fields
                et_fields      TYPE rsds_t_fields.

    METHODS post_process
      IMPORTING it_prop_fields TYPE rsds_t_fields
                it_fields      TYPE rsds_t_fields
      EXPORTING et_adso_fields TYPE t_ty_alv.

    METHODS load_data
      IMPORTING it_data        TYPE table_of_strings
                iv_adso_name   TYPE rsoadsonm
                it_adso_fields TYPE t_ty_alv
      EXPORTING et_msg         TYPE rs_t_msg .

    METHODS check_fields
      IMPORTING it_adso_fields   TYPE t_ty_alv
      EXPORTING et_adso_corrected TYPE t_ty_alv.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gv_data_sep     TYPE char1,
          gv_esc_sign     TYPE char1,
          gv_thousand_sep TYPE char1,
          gv_decimal_sep  TYPE char1,
          gv_header       TYPE abap_bool.

ENDCLASS.
CLASS zcl_bw_adso_bof IMPLEMENTATION.

  METHOD convert_into_columns.

    DATA: lv_ctr         TYPE i,
          lt_string      TYPE table_of_strings,
          lv_field_count TYPE i,
          ls_fields      TYPE rsds_s_fields,
          lt_fields      TYPE rsds_t_fields,
          lv_fieldnm     TYPE rsfieldnm,
          lt_prop_fields TYPE rsds_t_fields,
          lv_typenum     TYPE i.

    LOOP AT it_data INTO DATA(ls_data).
      lv_ctr = lv_ctr + 1.
      CALL FUNCTION 'RSDS_CONVERT_CSV'
        EXPORTING
          i_data_sep       = gv_data_sep
          i_esc_char       = gv_esc_sign
          i_record         = ls_data
          i_field_count    = 999
        IMPORTING
          e_t_data         = lt_string
        EXCEPTIONS
          escape_no_close  = 1
          escape_improper  = 2
          conversion_error = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        IF sy-msgno IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE |Error during file processing, line { lv_ctr } Data:{ ls_data(50) }| TYPE 'E'.
        ENDIF.
      ENDIF.

      IF gv_header = abap_true.
        DATA(lv_ignorelines) = 1.
      ENDIF.

      IF lv_ignorelines > 0 AND lv_ctr = lv_ignorelines.
        LOOP AT lt_string INTO DATA(ls_string).
          lv_field_count = lv_field_count + 1.
          IF ls_string IS NOT INITIAL.
            lv_fieldnm = to_upper( ls_string ).

            CALL FUNCTION 'RSDS_PROPOSE_FIELDNM_CHECK'
              CHANGING
                c_fieldname = lv_fieldnm.

            IF lv_fieldnm IS NOT INITIAL.
              ls_fields-fieldnm = lv_fieldnm.
              ls_fields-txtlg   = ls_string.
            ELSE.
              ls_fields-fieldnm = sy-tabix.
            ENDIF.

          ELSE.
            ls_fields-fieldnm = sy-tabix.

          ENDIF.
          ls_fields-posit = lv_field_count.
          APPEND ls_fields TO lt_fields.

        ENDLOOP.

      ELSEIF lv_ctr > lv_ignorelines.
        DATA(lv_index) = 0.
        LOOP AT lt_string INTO ls_string.

          SHIFT ls_string LEFT DELETING LEADING space.
          lv_index = lv_index + 1.
          READ TABLE lt_prop_fields INTO DATA(ls_prop_fields) INDEX lv_index.

          IF sy-subrc <> 0.
            CLEAR ls_prop_fields.
            APPEND ls_prop_fields TO lt_prop_fields.
          ENDIF.
          IF ls_string IS INITIAL.
            CONTINUE.
          ENDIF.
          CALL FUNCTION 'RS_GET_TYPE_FROM_DATA'
            EXPORTING
              i_data       = ls_string
              i_char1000   = gv_thousand_sep
              i_dezichar   = gv_decimal_sep
            CHANGING
              e_datatype   = ls_prop_fields-datatype
              e_typenum    = lv_typenum
              e_convexit   = ls_prop_fields-convexit
              e_convtype   = ls_prop_fields-convtype
              e_leng       = ls_prop_fields-leng
              e_decimal    = ls_prop_fields-decimals
              e_lowercase  = ls_prop_fields-lowercase
              e_unifieldnm = ls_prop_fields-unifieldnm.

          TRY.
              DATA(ls_old_str) = lt_prop_fields[ lv_index ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          IF ls_old_str-leng < ls_prop_fields-leng.
            DATA(lv_leng) = 'LENG'.
          ELSE.
            lv_leng = ''.
          ENDIF.

          IF ls_old_str-decimals < ls_prop_fields-decimals.
            DATA(lv_decimals) = 'DECIMALS'.
          ELSE.
            lv_decimals = ''.
          ENDIF.

          IF ls_old_str-datatype <> 'CHAR'.
            DATA(lv_datatype) = 'DATATYPE'.
          ELSE.
            lv_datatype = ''.
          ENDIF.

          IF ( ls_old_str-datatype = 'DEC'
                 OR ls_old_str-datatype = 'QUAN'
                 OR ls_old_str-datatype = 'CURR'
                 OR ls_old_str-datatype = 'FLTP' ) AND
              ls_prop_fields-datatype CP 'INT*'.
            lv_datatype = ''.
          ELSEIF ls_prop_fields-datatype CP 'INT*'.
            DATA(lv_int_length_new) = ls_prop_fields-datatype+3(1).
            DATA(lv_int_length_old) = ls_old_str-datatype+3(1).
            IF lv_int_length_new > lv_int_length_old.
              lv_datatype = 'DATATYPE'.
            ELSE.
              lv_datatype = ''.
            ENDIF.
          ENDIF.

          MODIFY lt_prop_fields FROM ls_prop_fields INDEX lv_index
          TRANSPORTING  ('CONVEXIT') ('CONVTYPE') ('LOWERCASE') ('UNIFIELDNM')
          (lv_datatype) (lv_leng) (lv_decimals).

        ENDLOOP.
      ENDIF.
    ENDLOOP.
    et_prop_fields = lt_prop_fields.
    et_fields = lt_fields.
  ENDMETHOD.

  METHOD constructor.

    gv_data_sep = iv_data_sep.
    gv_esc_sign = iv_esc_sign.
    gv_thousand_sep = iv_thousand_sep.
    gv_decimal_sep = iv_decimal_sep.
    gv_header = iv_header.

  ENDMETHOD.

  METHOD post_process.

    DATA: lv_index        TYPE i,
          lt_string       TYPE table_of_strings,
          lv_field_count  TYPE i,
          ls_fields       TYPE rsds_s_fields,
          lt_fields       TYPE rsds_t_fields,
          lt_prop_fields  TYPE rsds_t_fields,
          ls_segfd        TYPE rsdssegfd,
          lv_header_count TYPE i,
          lv_line_count   TYPE i.

    lt_prop_fields = it_prop_fields.
    lt_fields = it_fields.
    LOOP AT lt_prop_fields INTO DATA(ls_prop_fields).
      CLEAR ls_fields.
      lv_index = lv_index + 1.

      MOVE-CORRESPONDING ls_prop_fields TO ls_fields.
      ls_fields-outputlen = ls_prop_fields-leng.

      IF ls_fields-datatype IS INITIAL.
        ls_fields-datatype = 'CHAR'.
        ls_fields-leng     = 1.
        ls_fields-transfer = space.
      ELSE.
        ls_fields-transfer = 'X'.
      ENDIF.

      MODIFY lt_fields FROM ls_fields INDEX lv_index TRANSPORTING
          datatype leng convexit convtype decimals outputlen lowercase transfer unifieldnm.

      IF sy-subrc <> 0.
        ls_fields-fieldnm = lv_index.
        ls_fields-txtlg   = lv_index.
        ls_fields-posit = lv_index.
        APPEND ls_fields TO lt_fields.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'RSDS_CUKY_UNIT_CORRECT'
      CHANGING
        c_t_fields = lt_fields.

    LOOP AT lt_fields INTO ls_fields.
      lv_index = sy-tabix.
      CALL FUNCTION 'RSDS_TYPELENGHT_CORRECT'
        EXPORTING
          i_datatype = ls_fields-datatype
          i_leng     = ls_fields-leng
          i_decimals = ls_fields-decimals
        IMPORTING
          e_leng     = ls_fields-leng
          e_decimals = ls_fields-decimals.
      MOVE-CORRESPONDING ls_fields TO ls_segfd.
      CALL FUNCTION 'RSDS_FIELD_GET_CHARLENGTH'
        EXPORTING
          i_s_segfd = ls_segfd
        IMPORTING
          e_length  = ls_fields-outputlen
        EXCEPTIONS
          OTHERS    = 0.

      MODIFY lt_fields FROM ls_fields INDEX lv_index.
    ENDLOOP.

    lv_header_count = lines( lt_fields ).
    lv_line_count = lines( lt_prop_fields ).

    IF lv_header_count > lv_line_count.
      lv_line_count = lv_line_count + 1.
      DELETE lt_fields FROM lv_line_count TO lv_header_count.
    ENDIF.

    et_adso_fields = CORRESPONDING #( lt_fields
            MAPPING fieldname  = fieldnm
                      length   = leng
                      datatp   = datatype
                      decimals = decimals ).

  ENDMETHOD.

  METHOD load_data.

    DATA: lr_adso_table TYPE REF TO data,
          lv_ctr        TYPE i,
          lt_msg        TYPE rs_t_msg,
          lv_cnt        TYPE i,
          lt_output     TYPE table_of_strings.

    FIELD-SYMBOLS: <lt_adso> TYPE STANDARD TABLE.

    TRY.
        DATA(lt_tabname) = cl_rso_adso=>get_tablnm(
          EXPORTING
            i_adsonm        = CONV #( iv_adso_name ) ).
      CATCH cx_rs_not_found.
    ENDTRY.

    TRY.
        DATA(lv_table) = lt_tabname[ dsotabtype = 'AT' ]-name.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    CREATE DATA lr_adso_table TYPE STANDARD TABLE OF (lv_table).
    ASSIGN lr_adso_table->* TO <lt_adso>.

    LOOP AT it_data INTO DATA(ls_data).
      lv_ctr = lv_ctr + 1.
      CALL FUNCTION 'RSDS_CONVERT_CSV'
        EXPORTING
          i_data_sep       = gv_data_sep
          i_esc_char       = gv_esc_sign
          i_record         = ls_data
          i_field_count    = 999
        IMPORTING
          e_t_data         = lt_output
        EXCEPTIONS
          escape_no_close  = 1
          escape_improper  = 2
          conversion_error = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        IF sy-msgno IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE |Error during file processing, line { lv_ctr } Data:{ ls_data(50) }| TYPE 'E'.
        ENDIF.
      ENDIF.

      IF gv_header = abap_true AND lv_ctr = 1.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO <lt_adso> ASSIGNING FIELD-SYMBOL(<ls_adso>).
      DATA(lobj_ref) = CAST cl_abap_structdescr(
                            cl_abap_typedescr=>describe_by_data( p_data = <ls_adso>  ) ).


      DATA(lv_lengt) = lines( it_adso_fields ).

      DO lv_lengt TIMES.

        lv_cnt = lv_cnt + 1.
        DATA(ls_adso_fields) =  it_adso_fields[ lv_cnt ] .

        ASSIGN COMPONENT ls_adso_fields-fieldname OF STRUCTURE <ls_adso> TO FIELD-SYMBOL(<lv_adso_field>).

        DATA(lv_kind) = lobj_ref->components[ name = ls_adso_fields-fieldname ]-type_kind.

        TRY.
            DATA(lv_output) = lt_output[ lv_cnt ].

            IF lv_kind = 'P' OR lv_kind = 'F' .

              IF gv_decimal_sep IS NOT INITIAL.
                REPLACE ALL OCCURRENCES OF gv_decimal_sep IN lv_output WITH '.'.
              ENDIF.

              IF gv_thousand_sep IS NOT INITIAL.
                REPLACE ALL OCCURRENCES OF gv_thousand_sep IN lv_output WITH ''.
              ENDIF.
              CONDENSE lv_output.

            ENDIF.

            <lv_adso_field> = lv_output.

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDDO.
      CLEAR lv_cnt.
    ENDLOOP.

    CALL FUNCTION 'RSDSO_DU_WRITE_API'
      EXPORTING
        i_adsonm            = iv_adso_name
        it_data             = <lt_adso>
      IMPORTING
        et_msg              = lt_msg
      EXCEPTIONS
        write_failed        = 1
        datastore_not_found = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    et_msg = lt_msg.

  ENDMETHOD.

  METHOD check_fields.

    DATA: lv_prev_fieldname TYPE string.
    DATA(lt_fields) = it_adso_fields.
    DATA(lt_adso_fields_result) = it_adso_fields.

    SORT lt_fields BY fieldname.

    LOOP AT lt_fields REFERENCE INTO DATA(lr_field).

      IF lr_field->fieldname = lv_prev_fieldname.

        READ TABLE lt_adso_fields_result WITH KEY fieldname = lr_field->fieldname
        REFERENCE INTO DATA(lr_adso_field).

        DATA(lv_char_trim) = strlen( lr_adso_field->fieldname ) - 1.

        lr_adso_field->fieldname = |{ lr_adso_field->fieldname(lv_char_trim) }|.

        lv_prev_fieldname = lr_field->fieldname.

      ENDIF.

      lv_prev_fieldname = lr_field->fieldname.

    ENDLOOP.


    SELECT *
    FROM trese
    INTO TABLE @DATA(lt_trese)
    FOR ALL ENTRIES IN @it_adso_fields
    WHERE name = @it_adso_fields-fieldname.

    LOOP AT lt_trese REFERENCE INTO DATA(lr_trese).

      READ TABLE lt_adso_fields_result WITH KEY fieldname = lr_trese->name
      REFERENCE INTO DATA(lr_asdo_fields_result).

      lr_asdo_fields_result->fieldname = |{ lr_asdo_fields_result->fieldname(lv_char_trim) }|.

    ENDLOOP.

    et_adso_corrected = lt_adso_fields_result.

  ENDMETHOD.

ENDCLASS.

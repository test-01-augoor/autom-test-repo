*&---------------------------------------------------------------------*
*&  Include           ZHR_HEADCOUNT_REPORT_C01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_scr IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_scr IMPLEMENTATION.


  METHOD start_of_selection.

    CLEAR gst_param.

    gst_param-bukrs[] = s_bukrs[].
    gst_param-evdate = p_evdate.
    gst_param-statoc[] = s_statoc[].
    gst_param-file = p_file.
    gst_param-cb_alv = p_cb_alv.
    gst_param-cb_csv = p_cb_csv.
    gst_param-cb_head = p_cbhead.


    IF p_cb_csv IS NOT INITIAL .

      IF p_file IS INITIAL .
*---& MEnsaje: ingrese la ruta y nombre de archivo
        MESSAGE s001(00) WITH text-m03 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

    ENDIF.

    go_data->get_data( gst_param ).

  ENDMETHOD.                    "start_of_selection

  METHOD end_of_selection.

    IF NOT go_output IS BOUND.

      IF gst_param-cb_csv IS NOT INITIAL.

        CREATE OBJECT go_output
          TYPE
            lcl_output_csv
          EXPORTING
            i_o_data       = go_data.

      ENDIF.

    ENDIF.

    IF go_output IS BOUND.

      go_output->display( ).

      CLEAR go_output .

    ENDIF.

    IF NOT go_output IS BOUND.

      IF gst_param-cb_alv IS NOT INITIAL.

        CREATE OBJECT go_output
          TYPE
            lcl_output_alv
          EXPORTING
            i_o_data       = go_data.
      ENDIF.


    ENDIF.

    IF go_output IS BOUND.

      go_output->display(  ).

      CLEAR go_output .

    ENDIF.


  ENDMETHOD.                    "end_of_selection

  METHOD at_selection_screen_output.

    IF p_cb_csv IS NOT INITIAL.

      LOOP AT SCREEN.

        IF screen-name EQ 'P_FILE'.

          screen-required = 1.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

    ELSE.

      LOOP AT SCREEN.

        IF screen-name EQ 'P_FILE'.

          screen-required = 0.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "at_selection_screen_output

  METHOD at_selection_screen_p_file .

    DATA: lv_file TYPE localfile.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      IMPORTING
        serverfile       = lv_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc EQ 0.

      ch_v_file = lv_file.

    ENDIF.

  ENDMETHOD.                    "at_selection_screen_p_file

ENDCLASS.                    "lcl_scr IMPLEMENTATION
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_data
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.

  METHOD: constructor.

    lo_scr = i_o_scr.

  ENDMETHOD.                    "constructor

  METHOD   get_data.

    DATA: lt_pa TYPE STANDARD TABLE OF ty_pa.

    CLEAR: lt_pa.
*---& Get data with no Zero counts
    SELECT pa0001~bukrs   COUNT( DISTINCT pa0001~pernr )
        INTO TABLE lt_pa
        FROM t001
      INNER JOIN pa0001
      ON t001~bukrs =  pa0001~bukrs
      INNER  JOIN  pa0000
      ON  pa0000~pernr  EQ  pa0001~pernr
*      AND pa0000~begda = pa0001~begda
*      AND pa0000~endda = pa0001~endda
    WHERE t001~bukrs IN i_s_param-bukrs
      AND pa0001~begda LE i_s_param-evdate
      AND pa0001~endda GE i_s_param-evdate
      AND pa0000~begda LE i_s_param-evdate
      AND pa0000~endda GE i_s_param-evdate
      AND pa0000~stat2 IN i_s_param-statoc
      GROUP BY pa0001~bukrs .

    IF sy-subrc EQ 0.

    ENDIF.

    IF i_s_param-cb_head IS NOT INITIAL .

      get_zero_count( EXPORTING i_s_param = i_s_param i_t_pa = lt_pa ).

    ELSE.

      get_non_zero( EXPORTING i_s_param = i_s_param i_t_pa = lt_pa ).

    ENDIF.

  ENDMETHOD.                    "get_data

  METHOD get_zero_count.

    DATA: lt_t001 TYPE ty_t_t001.
    DATA: lv_headcount TYPE char20.

    FIELD-SYMBOLS: <lfs_t001> TYPE ty_t001,
                   <lfs_pa> TYPE ty_pa,
                   <lfs_output> TYPE ty_output,
                   <lfs_output_csv> TYPE ty_csv.

    CLEAR: lv_headcount, lt_t001.

    SELECT bukrs
      FROM t001
      INTO TABLE lt_t001
      WHERE bukrs IN i_s_param-bukrs.

    IF sy-subrc EQ 0.

      LOOP AT lt_t001 ASSIGNING <lfs_t001>.

        READ TABLE i_t_pa ASSIGNING <lfs_pa> WITH KEY bukrs = <lfs_t001>.

        IF sy-subrc EQ 0.

          APPEND INITIAL LINE TO me->gt_output ASSIGNING <lfs_output>.
          <lfs_output>-bukrs = <lfs_pa>-bukrs.
          <lfs_output>-evdate = i_s_param-evdate.
          <lfs_output>-headcount = <lfs_pa>-headcount.

          APPEND INITIAL LINE TO me->gt_output_csv ASSIGNING <lfs_output_csv>.

          lv_headcount = <lfs_pa>-headcount .
*---& Date in format: DDMMYYYY
          CONCATENATE <lfs_pa>-bukrs ',' i_s_param-evdate+6(2) '/' i_s_param-evdate+4(2) '/' i_s_param-evdate(4) ','
          lv_headcount INTO <lfs_output_csv>.
*          lv_headcount INTO <lfs_output_csv>-field.

          UNASSIGN <lfs_pa>.

        ELSE.

          APPEND INITIAL LINE TO me->gt_output ASSIGNING <lfs_output>.
          <lfs_output>-bukrs = <lfs_t001>-bukrs.
          <lfs_output>-evdate = i_s_param-evdate.
          <lfs_output>-headcount = 0.

          APPEND INITIAL LINE TO me->gt_output_csv ASSIGNING <lfs_output_csv>.

          lv_headcount = 0 .

*---& Date in format: DDMMYYYY
          CONCATENATE <lfs_t001>-bukrs ',' i_s_param-evdate+6(2) '/' i_s_param-evdate+4(2) '/' i_s_param-evdate(4) ','
          lv_headcount INTO <lfs_output_csv>.
*          lv_headcount INTO <lfs_output_csv>-field .

        ENDIF.

        UNASSIGN:   <lfs_output>, <lfs_output_csv>.

        CLEAR lv_headcount.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "get_zero_count

  METHOD get_non_zero.

    FIELD-SYMBOLS: <lfs_pa> TYPE ty_pa,
                   <lfs_output> TYPE ty_output,
                   <lfs_output_csv> TYPE ty_csv.

    DATA: lv_headcount TYPE char20.

    LOOP AT i_t_pa ASSIGNING <lfs_pa>.


      APPEND INITIAL LINE TO me->gt_output ASSIGNING <lfs_output>.
      <lfs_output>-bukrs = <lfs_pa>-bukrs.
      <lfs_output>-evdate = i_s_param-evdate.
      <lfs_output>-headcount = <lfs_pa>-headcount.

      APPEND INITIAL LINE TO me->gt_output_csv ASSIGNING <lfs_output_csv>.

      lv_headcount = <lfs_pa>-headcount .
*---& Date in format: DDMMYYYY
      CONCATENATE <lfs_pa>-bukrs ',' i_s_param-evdate+6(2) '/' i_s_param-evdate+4(2) '/' i_s_param-evdate(4) ','
      lv_headcount INTO <lfs_output_csv>.
*      lv_headcount INTO <lfs_output_csv>-field.

      CLEAR lv_headcount.

      UNASSIGN:  <lfs_output>, <lfs_output_csv>.

    ENDLOOP.

    IF <lfs_pa> IS ASSIGNED.

      UNASSIGN <lfs_pa>.

    ENDIF.

  ENDMETHOD.                    "get_non_zero


ENDCLASS.               "lcl_data
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_output
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_output IMPLEMENTATION.

  METHOD: constructor.

    lo_data = i_o_data.

  ENDMETHOD.                    "constructor

ENDCLASS.               "lcl_output
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_output_alv
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_output_alv IMPLEMENTATION.

  METHOD: display .

    DATA: lo_alv TYPE REF TO cl_salv_table.

    TRY.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = me->lo_data->gt_output ).

      CATCH cx_salv_msg .

    ENDTRY.

    me->setup_alv( CHANGING ch_o_alv =  lo_alv ).

    lo_alv->display( ).


  ENDMETHOD.                    "display

  METHOD setup_alv.

    CONSTANTS lc_repid TYPE syrepid VALUE 'ZHR_HEADCOUNT_REPORT'.

    DATA: lo_alv_functions TYPE REF TO cl_salv_functions_list,
          lo_alv_layout TYPE REF TO cl_salv_layout,
          lo_display_set TYPE REF TO cl_salv_display_settings,
          lv_title TYPE lvc_title.

    DATA: lst_key TYPE salv_s_layout_key.

*---& Header
    lo_display_set = ch_o_alv->get_display_settings( ).
    lv_title = lo_display_set->get_list_header( ).
    lv_title = text-001.
    lo_display_set->set_list_header( lv_title ).

    lo_alv_functions = ch_o_alv->get_functions( ).
    lo_alv_functions->set_all( abap_true ).

    lo_alv_layout = ch_o_alv->get_layout( ).
    lst_key-report = lc_repid.
    lo_alv_layout->set_key( lst_key ).
    lo_alv_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    me->setup_fields( CHANGING ch_o_alv = ch_o_alv ).

  ENDMETHOD.                    "setup_alv

  METHOD setup_fields.

    DATA: lv_text TYPE scrtext_s,
          lv_textm TYPE scrtext_m,
          lv_textl TYPE scrtext_l.

    DATA: lo_alv_columns TYPE REF TO cl_salv_columns_table,
          lo_alv_column TYPE REF TO cl_salv_column_table.

    lo_alv_columns = ch_o_alv->get_columns( ).
    lo_alv_columns->set_optimize( value = abap_true ) .



    TRY.
        lv_text = lv_textm = lv_textl  = text-s01. "Fecha de Evaluación

        lo_alv_column ?= lo_alv_columns->get_column( 'BUKRS' ).
        lo_alv_column->set_long_text( lv_textl ).
        lo_alv_column->set_medium_text( lv_textm ).
        lo_alv_column->set_short_text( lv_text ).

      CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.

    ENDTRY.


    TRY.

        lv_text = lv_textm = lv_textl  = text-f01. "Fecha de Evaluación

        lo_alv_column ?= lo_alv_columns->get_column( 'EVDATE' ).
        lo_alv_column->set_long_text( lv_textl ).
        lo_alv_column->set_medium_text( lv_textm ).
        lo_alv_column->set_short_text( lv_text ).

      CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.

    ENDTRY.

    TRY.

        lv_text = lv_textm = lv_textl  = text-h01. "Headcount

        lo_alv_column ?= lo_alv_columns->get_column( 'HEADCOUNT' ).
        lo_alv_column->set_long_text( lv_textl ).
        lo_alv_column->set_medium_text( lv_textm ).
        lo_alv_column->set_short_text( lv_text ).

      CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.

    ENDTRY.

  ENDMETHOD.                    "setup_fields

ENDCLASS.               "lcl_output_alv
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_output_csv
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_output_csv IMPLEMENTATION.

  METHOD display.

    DATA: lv_path TYPE string,
          ls_csv_data TYPE ty_csv.


    FIELD-SYMBOLS: <lfs_output_csv> TYPE ty_csv.
    CLEAR: lv_path.

    CONCATENATE me->lo_data->lo_scr->gst_param-file '_' sy-datum '_' sy-uzeit '.csv' INTO lv_path.

    IF me->lo_data->gt_output_csv IS INITIAL .
*---& No hay datos para escribir en el archivo
      MESSAGE s001(00) WITH text-m04 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      CONCATENATE text-s01 text-f01 text-h01 INTO ls_csv_data SEPARATED BY ','.
      INSERT ls_csv_data INTO me->lo_data->gt_output_csv INDEX 1.
    ENDIF.

    OPEN DATASET lv_path  FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc EQ 0.

      LOOP AT me->lo_data->gt_output_csv ASSIGNING <lfs_output_csv>.
        TRANSFER <lfs_output_csv> TO lv_path.
      ENDLOOP.

      CLOSE DATASET lv_path.

      SUBMIT zscpi_hrpy_send_files
***               WITH p_kpmgar EQ space
               WITH p_lsrc   EQ lv_path
               WITH p_multi  EQ space
               WITH p_rfold  EQ space
               WITH p_single EQ 'X'
               WITH p_iddest EQ 'P_WVHEA'
***               WITH p_wvacc  EQ space
***               WITH p_wvcaf  EQ space
***               WITH p_wvhea  EQ 'X'
               AND RETURN.

*---& El archivo se generó con éxito
      MESSAGE i001(00) WITH text-m01 .

    ELSE.

*---&No se pudo generar el archivo
      MESSAGE s001(00) WITH text-m02 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.                    "display

ENDCLASS.               "lcl_output_csv
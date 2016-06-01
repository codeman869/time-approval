*&---------------------------------------------------------------------*
*&  Include           YMSS_TIME_APPROVAL_REPORT01
*&---------------------------------------------------------------------*

DATA: it_pa9004 TYPE TABLE OF pa9004,
      it_pa9004x TYPE TABLE OF pa9004,
      wa_pa9004 TYPE pa9004,
      it_pa0002 TYPE TABLE OF pa0002,
      wa_pa9002 TYPE pa0002.



"comments


TYPES: BEGIN OF ty_out,
        pernr                   LIKE  pa0001-pernr,
        emp_name                LIKE  pa0001-ename,
        plan_hrs                LIKE  catsdb-catshours,
        act_hrs                 LIKE  catsdb-catshours,
        att_hrs                 LIKE  catsdb-catshours,
        abs_hrs                 LIKE  catsdb-catshours,
        mgr_name                LIKE  pa0001-ename,
        pp                      LIKE  pa0001-ename,

        emp_cert           LIKE  pa0001-ename,
        emp_id  LIKE pa9004-emp_id,
        emp_cert_date  LIKE pa9004-emp_cert_date,
        emp_cert_time  LIKE pa9004-emp_cert_time,
        mgr_cert           LIKE  pa0001-ename,
        mgr_id  LIKE pa9004-mgr_id,
        mgr_cert_date  LIKE pa9004-mgr_cert_date,
        mgr_cert_time  LIKE pa9004-mgr_cert_time,

        mgr_cert_stat TYPE icon-id,
        ee_cert_stat TYPE icon-id,
        cel_color TYPE lvc_t_scol,
        begda TYPE datum,
        endda TYPE datum,
        ORGEH TYPE ORGEH,
        ORGEH_2ND_TEXT TYPE stext,
       END OF ty_out.



DATA: BEGIN OF alv_out4, " For ALV
        pernr                   LIKE  pa0001-pernr,
        emp_name                LIKE  pa0001-ename,
        plan_hrs                LIKE  catsdb-catshours,
        act_hrs                 LIKE  catsdb-catshours,
        att_hrs                 LIKE  catsdb-catshours,
        abs_hrs                 LIKE  catsdb-catshours,
        mgr_name                LIKE  pa0001-ename,
        pp                      LIKE  pa0001-ename,
        emp_cert           LIKE  pa0001-ename,
        emp_id  LIKE pa9004-emp_id,
        emp_cert_date  LIKE pa9004-emp_cert_date,
        emp_cert_time  LIKE pa9004-emp_cert_time,
        mgr_cert           LIKE  pa0001-ename,
        mgr_id  LIKE pa9004-mgr_id,
        mgr_cert_date  LIKE pa9004-mgr_cert_date,
        mgr_cert_time  LIKE pa9004-mgr_cert_time,

        mgr_cert_stat LIKE icon-id,
        ee_cert_stat LIKE icon-id,

        cel_color TYPE lvc_t_scol,
        begda TYPE datum,
        endda TYPE datum,
        ORGEH TYPE ORGEH,
        ORGEH_2ND_TEXT TYPE stext,
       END OF alv_out4.

TYPES: BEGIN OF emp_data_ty,
        pernr                   TYPE  pernr_d,
        emp_name                TYPE  string,
        pp                      TYPE  char10,
        plan_hrs                TYPE  catsdb-catshours,
        act_hrs                 TYPE  catsdb-catshours,
        att_hrs                 TYPE  catsdb-catshours,
        abs_hrs                 TYPE  catsdb-catshours,
        emp_cert                TYPE  string,
    emp_id  LIKE pa9004-emp_id,
  emp_cert_date  LIKE pa9004-emp_cert_date,
  emp_cert_time  LIKE pa9004-emp_cert_time,
        mgr_cert                TYPE  string,
    mgr_id  LIKE pa9004-mgr_id,
  mgr_cert_date  LIKE pa9004-mgr_cert_date,
  mgr_cert_time  LIKE pa9004-mgr_cert_time,
        mgr_btn                 TYPE  string,
        mgr_btn_text            TYPE  string,
        mgr_name                TYPE  string,
        mgr_btn_icon            TYPE  string,
        emp_cert_icon           TYPE  string,
        mgr_cert_icon           TYPE  string,
        emp_type                TYPE  string,
        emp_type_col            TYPE  wdui_textview_sem_col,
        mgr_btn_enable          TYPE  wdy_boolean,
        emp_remind              TYPE  wdy_boolean,
        emp_remind_tooltip      TYPE  string,
        emp_cert_action         TYPE  string,
        mgr_cancel_btn          TYPE  string,
        mgr_cancel_btn_icon     TYPE  string,
        mgr_cancel_btn_text     TYPE  string,
        mgr_cancel_btn_enable   TYPE  wdy_boolean,
        begda TYPE datum,
        endda TYPE datum,
        ORGEH TYPE ORGEH,
        ORGEH_2ND_TEXT TYPE stext,
  END OF emp_data_ty.

DATA: wa_emp_data TYPE emp_data_ty,
      it_emp_data TYPE TABLE OF emp_data_ty,
      it_emp_datax TYPE TABLE OF emp_data_ty.

DATA: BEGIN OF app_data,
        mgr_id TYPE pernr_d,
        mgr_name    TYPE  emnam,
        mgr_user_name   TYPE  syuname,
        mgr_persa   TYPE  persa,
        mgr_pos   TYPE  plans,
        show_manager_ctr    TYPE  wdui_visibility,
        subs_text   TYPE  string,
        time_keeper   TYPE  wdy_boolean,
        time_keeper_mgr   TYPE  wdy_boolean,
        holiday_text_exist    TYPE  wdy_boolean,
        holiday_text    TYPE  string,
        export_data   TYPE  xstring,
        export_data_file    TYPE  string,
        show_pnd_req_cntr   TYPE  wdui_visibility,
        search_persa    TYPE  persa,
        emp_data_rows   TYPE  i,
        multi_select    TYPE  wdy_boolean,
        mgr_pos_text    TYPE  stext,
        kostl   TYPE  kostl,
        kostl_text    TYPE  kostx,
        mgr_title   TYPE  string,
        goto_pabrj    TYPE  pabrj,
        goto_pabrp    TYPE  pabrp,
  END OF app_data.

*DATA: it_out TYPE TABLE OF ty_out,
*      wa_out TYPE ty_out.

DATA: it_out TYPE TABLE OF ty_out, "ZHR_TM_APPR_REPORT_ALV,
      wa_out TYPE ty_out. "ZHR_TM_APPR_REPORT_ALV.

DATA: it_sort TYPE slis_t_sortinfo_alv,
      wa_sort TYPE slis_sortinfo_alv.

DATA: lv_emp_flag TYPE c,
      lv_mgr_flag TYPE c,
      lv_cut_off TYPE datum,
      ls_p0041 TYPE pa0041,
      lv_hire_dt TYPE datum,
      lv_dar TYPE datar,
      lv_dat TYPE dardt,
      lv_seqnr TYPE seqnr,
      it_period TYPE TABLE OF t549q,
  wa_period TYPE t549q.

DATA: lv_e_pernr TYPE pernr_d.
DATA: lv_e_persa TYPE persa.
DATA: lv_e_plans TYPE plans.

DATA: lv_m_pernr TYPE pernr_d.
DATA: lv_m_persa TYPE persa.
DATA: lv_m_plans TYPE plans.

DATA: lv_t_pernr TYPE pernr_d.
DATA: lv_t_persa TYPE persa.
DATA: lv_t_plans TYPE plans.

DATA: frm_pabrp TYPE pabrp,
      to_pabrp TYPE pabrp,
      frm_pabrj TYPE pabrj,
      to_pabrj TYPE pabrj.

DATA: it_pernrs TYPE TABLE OF pernr_d,
      chosen_objects LIKE hrsobid OCCURS 0 WITH HEADER LINE.

DATA: custom_container TYPE REF TO cl_gui_custom_container,
      alv_grid TYPE REF TO cl_gui_alv_grid,
      ok_code LIKE sy-ucomm,
      container_name TYPE scrfname VALUE 'GRID_01',
      o_dyndoc_id  TYPE REF TO cl_dd_document.


*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    METHODS:
    handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object e_interactive,

    handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
ENDCLASS.
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar.
    DATA l_toolbar LIKE LINE OF e_object->mt_toolbar.

    READ TABLE e_object->mt_toolbar WITH KEY function = 'EMAIL'
    TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      l_toolbar-function = 'EMAIL'.
      l_toolbar-icon = ICON_MSG.
      l_toolbar-quickinfo = 'Remind Employee'.
      l_toolbar-disabled = space.
      APPEND l_toolbar TO e_object->mt_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = 'MMAIL'
    TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      l_toolbar-function = 'MMAIL'.
      l_toolbar-icon =  ICON_MAIL_SAP_UNREAD.
      l_toolbar-quickinfo = 'Remind Manager'.
      l_toolbar-disabled = space.
      APPEND l_toolbar TO e_object->mt_toolbar.
    ENDIF.

    CLEAR l_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'EMAIL' or 'MMAIL'.
        DATA: lt_rows TYPE LVC_T_ROW,
              ls_rows LIKE LINE OF lt_rows,
              wa_outtab LIKE LINE OF it_out.

        CALL METHOD alv_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows
*            et_row_no     =
            .

        IF NOT lt_rows IS INITIAL.

          LOOP AT lt_rows INTO ls_rows.
             READ TABLE it_out INTO wa_outtab INDEX ls_rows-index.

             IF e_ucomm = 'EMAIL' AND wa_outtab-emp_cert <> 'Certified'.
               PERFORM send_email using wa_outtab e_ucomm.
             ELSEIF e_ucomm = 'MMAIL' AND
               wa_outtab-mgr_cert <> 'Approved'.
               PERFORM send_email USING wa_outtab e_ucomm.
             ELSE.
*               MESSAGE 'Some emails not sent' TYPE 'I'.
             ENDIF.


          ENDLOOP.

        ELSE.
          MESSAGE 'Please select one or more rows' TYPE 'S'.
*          set SCREEN 100.
        ENDIF.


    ENDCASE.
  ENDMETHOD.

ENDCLASS.               "LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      SET SCREEN '0'.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.
ENDMODULE.
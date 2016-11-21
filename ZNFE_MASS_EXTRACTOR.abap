REPORT  ZNFE_MASS_EXTRACTOR LINE-SIZE 300.

  constants:
    BEGIN OF cv_nfe_pdf_layout,  "EC NOTEXT
      normal_portrait   TYPE char30 VALUE '/XNFE/DANFE_PORTRAIT_FO',
      normal_landscape  TYPE char30 VALUE '/XNFE/DANFE_LANDSCAPE_FO',
      sec_pap_portrait  TYPE char30 VALUE '/XNFE/DANFE_PORTRAIT_SEC_PAP',
      epec_portrait     TYPE char30 VALUE '/XNFE/DANFE_PORTRAIT_EPEC',
      sec_pap_landscape TYPE char30 VALUE '/XNFE/DANFE_LANDSCAPE_SEC_PAP',
      epec_landscape    TYPE char30 VALUE '/XNFE/DANFE_LANDSCAPE_EPEC',
    END OF cv_nfe_pdf_layout .

PARAMETERS: p_list TYPE localfile DEFAULT 'C:\temp\access_key_list.txt',
            p_dest TYPE rfcdest.
SELECTION-SCREEN BEGIN OF BLOCK bl_02 WITH FRAME TITLE text-t02.
PARAMETERS: p_inbd TYPE xfeld RADIOBUTTON GROUP g2,
            p_outb TYPE xfeld RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK bl_02.
SELECTION-SCREEN BEGIN OF BLOCK bl_01 WITH FRAME TITLE text-t01.
PARAMETERS: p_xml TYPE xfeld RADIOBUTTON GROUP g1,
            p_pdf TYPE xfeld RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK bl_01.
START-OF-SELECTION.
  FIELD-SYMBOLS: <acckey>  TYPE /xnfe/nfeid_s,
                 <msg>     TYPE bapiret2.
  DATA: lv_filename        TYPE string,
        lv_path            TYPE string,
        lv_xml             TYPE /xnfe/xmlstring,
        lv_pdf             TYPE fpcontent,
        lv_pdf_xstr        TYPE xstring,
        lt_acckey          TYPE /xnfe/nfeid_t,
        ls_pdf_hd          TYPE /xnfe/danfe_pdf_hd,
        ls_pdf_ide         TYPE /xnfe/danfe_pdf_ide,
        ls_pdf_emi         TYPE /xnfe/danfe_pdf_emi,
        ls_pdf_dest        TYPE /xnfe/danfe_pdf_dest,
        ls_pdf_transp      TYPE /xnfe/danfe_pdf_transp,
        lt_pdf_det         TYPE /xnfe/danfe_pdf_item_t,
        ls_pdf_total       TYPE /xnfe/danfe_pdf_total,
        ls_pdf_issqn       TYPE /xnfe/danfe_pdf_issqn,
        ls_pdf_inf_adic    TYPE /xnfe/danfe_pdf_inf_adic,
        ls_pdf_fatura      TYPE /xnfe/danfe_pdf_fatura.


  PERFORM read_source_file USING p_list
                        CHANGING lt_acckey.

  "For each Access Key, do:
  LOOP AT lt_acckey ASSIGNING <acckey>.
    "Clean it up
    CLEAR: lv_xml,
           lv_pdf,
           lv_path,
           lv_pdf_xstr,
           ls_pdf_hd,
           ls_pdf_ide,
           ls_pdf_emi,
           ls_pdf_dest,
           ls_pdf_transp,
           lt_pdf_det,
           ls_pdf_total,
           ls_pdf_issqn,
           ls_pdf_inf_adic,
           ls_pdf_fatura.

    PERFORM get_xml USING p_dest
                          <acckey>-id
                 CHANGING lv_xml.

    IF lv_xml IS INITIAL.
      CONTINUE.
    ENDIF.

    IF p_xml EQ abap_true.
      PERFORM get_filename USING <acckey>-id p_list CHANGING lv_path.
      PERFORM save_xml USING lv_path lv_xml.
    ELSE.
      PERFORM map_xml_to_pdf USING <acckey>-id
                                   lv_xml
                          CHANGING ls_pdf_hd
                                   ls_pdf_ide
                                   ls_pdf_emi
                                   ls_pdf_dest
                                   ls_pdf_transp
                                   lt_pdf_det
                                   ls_pdf_total
                                   ls_pdf_issqn
                                   ls_pdf_inf_adic
                                   ls_pdf_fatura.

      PERFORM generate_pdf USING <acckey>-id
                                 ls_pdf_hd
                                 ls_pdf_ide
                                 ls_pdf_emi
                                 ls_pdf_dest
                                 ls_pdf_transp
                                 lt_pdf_det
                                 ls_pdf_total
                                 ls_pdf_issqn
                                 ls_pdf_inf_adic
                                 ls_pdf_fatura
                        CHANGING lv_pdf.

      IF lv_pdf IS INITIAL.
        "Error!
        CONTINUE.
      ELSE.
        lv_pdf_xstr = lv_pdf.
      ENDIF.

      PERFORM get_filename USING <acckey>-id p_list CHANGING lv_path.
      PERFORM save_pdf USING lv_path lv_pdf_xstr.
    ENDIF.
  ENDLOOP.



*&---------------------------------------------------------------------*
*&      Form  GET_XML
*&---------------------------------------------------------------------*
FORM GET_XML  USING    i_dest       TYPE rfcdest
                       i_access_key TYPE /xnfe/id
              CHANGING c_xml TYPE /xnfe/xmlstring.
  CONSTANTS: lc_direction_i TYPE /xnfe/direction VALUE 'INBD',
             lc_direction_o TYPE /xnfe/direction VALUE 'OUTB',
             lc_doctype   TYPE /xnfe/doctype VALUE 'NFE'.
  DATA: lt_messages TYPE bapiret2_t,
        lv_direction TYPE /xnfe/direction.

  IF p_inbd EQ abap_true.
    lv_direction = lc_direction_i.
  ELSE.
    lv_direction = lc_direction_o.
  ENDIF.

    "Get XML from remote server
    CALL FUNCTION '/XNFE/CORE_XML_READ'
      DESTINATION i_dest
      EXPORTING
        iv_accesskey = i_access_key
        iv_direction = lv_direction
        iv_doctype   = lc_doctype
      IMPORTING
        ev_xmlstring = c_xml
        et_bapiret2  = lt_messages
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc NE 0.
      "Write error log
      WRITE: / <acckey>-id, 'Failed to retrieve the XML from server'  COLOR COL_NEGATIVE.
    ENDIF.
    "Error messages
    IF lt_messages[] IS NOT INITIAL.
      LOOP AT lt_messages ASSIGNING <msg>.
        "E999(znfe)
        WRITE: / <acckey>-id, ':', <msg>-type, <msg>-NUMBER, '(', <msg>-ID, ') --- ', <msg>-MESSAGE COLOR COL_NEGATIVE.
      ENDLOOP.
    ENDIF.
ENDFORM.                    " GET_XML

*&---------------------------------------------------------------------*
*&      Form  READ_SOURCE_FILE
*&---------------------------------------------------------------------*
FORM READ_SOURCE_FILE  USING    i_filename TYPE localfile
                       CHANGING ct_acckey TYPE /xnfe/nfeid_t.

  DATA: lv_filename TYPE string.
  lv_filename = i_filename.

  "Read file with ACCESS KEY list
  CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD(
    exporting
      FILENAME                = lv_filename
    changing
      DATA_TAB                = ct_acckey
    exceptions
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      NOT_SUPPORTED_BY_GUI    = 17
      ERROR_NO_GUI            = 18
      OTHERS                  = 19
  ).

  if sy-subrc <> 0.
   message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
ENDFORM.                    " READ_SOURCE_FILE

*&---------------------------------------------------------------------*
*&      Form  MAP_XML_TO_PDF
*&---------------------------------------------------------------------*
FORM MAP_XML_TO_PDF  USING    i_acckey          TYPE /xnfe/id
                              i_xml             TYPE /xnfe/xmlstring
                     CHANGING cs_pdf_hd         TYPE /xnfe/danfe_pdf_hd
                              cs_pdf_ide        TYPE /xnfe/danfe_pdf_ide
                              cs_pdf_emi        TYPE /xnfe/danfe_pdf_emi
                              cs_pdf_dest       TYPE /xnfe/danfe_pdf_dest
                              cs_pdf_transp     TYPE /xnfe/danfe_pdf_transp
                              ct_pdf_det        TYPE /xnfe/danfe_pdf_item_t
                              cs_pdf_total      TYPE /xnfe/danfe_pdf_total
                              cs_pdf_issqn      TYPE /xnfe/danfe_pdf_issqn
                              cs_pdf_inf_adic   TYPE /xnfe/danfe_pdf_inf_adic
                              cs_pdf_fatura     TYPE /xnfe/danfe_pdf_fatura.

  field-symbols: <proxy_struc> TYPE /xnfe/mapping_structure_s,
                 <ls_prot>     type /xnfe/danfe_pdf_hd_prot,
                 <ls_ide>      type /xnfe/danfe_pdf_ide,
                 <ls_emit>     type /xnfe/danfe_pdf_emi,
                 <ls_dest>     type /xnfe/danfe_pdf_dest,
                 <lt_vol>      type /xnfe/wd_nfe_vol_t,
                 <ls_vol>      type /xnfe/wd_nfe_vol,
                 <ls_transp>   type /xnfe/danfe_pdf_transp,
                 <lt_pdf_det>  type /xnfe/danfe_pdf_item_t,
                 <ls_pdf_det>  type /xnfe/danfe_pdf_item,
                 <ls_total>    type /xnfe/danfe_pdf_total,
                 <ls_issqn>    type /xnfe/danfe_pdf_issqn,
                 <ls_infadic>  type /xnfe/danfe_pdf_inf_adic,
                 <ls_fat>      type /xnfe/wd_nfe_cobr,
                 <lt_dup>      type /xnfe/wd_nfe_dup_t,
                 <ls_dup>      type /xnfe/wd_nfe_dup.

  DATA: lt_tags TYPE /XNFE/MAPPING_STRUCTURE_T.
  data: mv_inf_compl TYPE string, "Class Attribute
        lv_dummy type char4,
        lv_pesol type /xnfe/pesol,
        lv_pesob type /xnfe/pesob,
        lv_qvol  type /xnfe/qvol,
        lv_entry type abap_bool.

  "Convert the XML into ABAP Structure using Proxy helpers
  call function '/XNFE/TRANSFORM_NFE_TO_DANFE'
            exporting
              iv_xml          = i_xml
            importing
              et_structures   = lt_tags
            exceptions
              technical_error = 1
              others          = 2.

  "Map each tag into its own structure
*--------------------------------------------------------------------*
*  HD
*--------------------------------------------------------------------*
* get NF-e protocol information and map them to DANFE format
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-prot.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_prot>.
    move-corresponding <ls_prot> to cs_pdf_hd.

*   split into date/ time format
    cs_pdf_hd-d_recbto = cs_pdf_hd-dh_recbto(8).
    cs_pdf_hd-h_recbto = cs_pdf_hd-dh_recbto+8(6).
  endif.

  cs_pdf_hd-nfe_id = i_acckey.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  IDE
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-ide.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_ide>.
    cs_pdf_ide = <ls_ide>.

*   adapt some fields
    if cs_pdf_ide-d_emi = '00000000'.
      cs_pdf_ide-d_emi     = cs_pdf_ide-dh_emi(8).
    endif.
    if cs_pdf_ide-d_sai_ent = '00000000'.
      cs_pdf_ide-d_sai_ent = cs_pdf_ide-dh_sai_ent(8).
      cs_pdf_ide-h_sai_ent = cs_pdf_ide-dh_sai_ent+8(6).
    endif.
  endif.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  EMI
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-emit.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_emit>.
    cs_pdf_emi = <ls_emit>.
  endif.

  if cs_pdf_emi-cnpj is initial.
    cs_pdf_emi-cnpj = cs_pdf_emi-cpf.
  endif.

* build address lines
  concatenate cs_pdf_emi-x_lgr ',' into cs_pdf_emi-address_line_1.
  concatenate cs_pdf_emi-address_line_1 cs_pdf_emi-nro cs_pdf_emi-x_cpl
      into cs_pdf_emi-address_line_1 separated by space.

  concatenate cs_pdf_emi-x_bairro '-' cs_pdf_emi-cep
      into cs_pdf_emi-address_line_2 separated by space.

  concatenate cs_pdf_emi-x_mun '-' cs_pdf_emi-uf text-001
      into cs_pdf_emi-address_line_3 separated by space.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  FATURA
*--------------------------------------------------------------------*
  DATA: lv_string type string.

  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-ide.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_ide>.

    cs_pdf_fatura-fatura = /xnfe/cl_wd_util=>get_domvalue_text(
        iv_dom = /xnfe/if_wd_constants=>cv_dom-nfe_indpag iv_value = <ls_ide>-ind_pag ).

*   special case 'PAGAMENTO A PRAZO'
    if <ls_ide>-ind_pag = '1'.
*     fill also from 'COMBR-FAT tag', map only visible fields
      unassign <proxy_struc>.
      read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-fat.
      if <proxy_struc> is assigned.
        assign <proxy_struc>-mapping_data->* to <ls_fat>.

        if <ls_fat>-n_fat is not initial.
          concatenate cs_pdf_fatura-fatura '/' text-003 <ls_fat>-n_fat
              into cs_pdf_fatura-fatura separated by space.
        endif.
        if <ls_fat>-v_orig is not initial.
          lv_string = <ls_fat>-v_orig.
          concatenate cs_pdf_fatura-fatura '/' 'V. Orig.:' lv_string  "#EC NOTEXT
              into cs_pdf_fatura-fatura separated by space.
        endif.
        if <ls_fat>-v_desc is not initial.
          lv_string = <ls_fat>-v_desc.
          concatenate cs_pdf_fatura-fatura '/' 'V. Desc.:' lv_string  "#EC NOTEXT
              into cs_pdf_fatura-fatura separated by space.
        endif.
        if <ls_fat>-v_liq is not initial.
          lv_string = <ls_fat>-v_desc.
          concatenate cs_pdf_fatura-fatura '/' 'V. Liq.:' lv_string  "#EC NOTEXT
              into cs_pdf_fatura-fatura separated by space.
        endif.
      endif.

*     map additional information from tag DUP
      unassign <proxy_struc>.

      read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-dup.
      if <proxy_struc> is assigned.
        assign <proxy_struc>-mapping_data->* to <lt_dup>.

        loop at <lt_dup> assigning <ls_dup>.
          concatenate mv_inf_compl 'Duplicata' '-' into mv_inf_compl separated by space.  "#EC NOTEXT
          if <ls_dup>-n_dup is not initial.
            concatenate mv_inf_compl text-003 <ls_dup>-n_dup into mv_inf_compl separated by space.
            concatenate mv_inf_compl cl_abap_char_utilities=>horizontal_tab into mv_inf_compl.
            lv_entry = abap_true.
          endif.
          if <ls_dup>-d_venc is not initial.
            concatenate mv_inf_compl 'Venc.:' <ls_dup>-d_venc into mv_inf_compl separated by space. "#EC NOTEXT
            concatenate mv_inf_compl cl_abap_char_utilities=>horizontal_tab into mv_inf_compl.
            lv_entry = abap_true.
          endif.
          if <ls_dup>-v_dup is not initial.
            lv_string = <ls_dup>-v_dup.
            concatenate mv_inf_compl text-004 lv_string into mv_inf_compl separated by space.
            lv_entry = abap_true.
          endif.

          if lv_entry = abap_true.
            concatenate mv_inf_compl cl_abap_char_utilities=>cr_lf into mv_inf_compl.
          endif.

          clear lv_entry.
        endloop.
      endif.
    endif.
  endif.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  DEST
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-dest.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_dest>.
    cs_pdf_dest = <ls_dest>.
  endif.

  if cs_pdf_dest-cnpj is initial.
    cs_pdf_dest-cnpj = cs_pdf_dest-cpf.
  endif.

  concatenate cs_pdf_dest-x_lgr ',' into cs_pdf_dest-address.
  concatenate cs_pdf_dest-address cs_pdf_dest-nro cs_pdf_dest-x_cpl
      into cs_pdf_dest-address separated by space.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  TRANSP
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-transp.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_transp>.
    cs_pdf_transp = <ls_transp>.

    if cs_pdf_transp-cnpj is initial.
      cs_pdf_transp-cnpj = cs_pdf_transp-cpf.
    endif.

    cs_pdf_transp-mod_frete_long = /xnfe/cl_wd_util=>get_domvalue_text(
        iv_dom = /xnfe/if_wd_constants=>cv_dom-nfe_modfrete iv_value = cs_pdf_transp-mod_frete ).

    concatenate '(' cs_pdf_transp-mod_frete ')' into lv_dummy.
    concatenate lv_dummy cs_pdf_transp-mod_frete_long
        into cs_pdf_transp-mod_frete_long separated by space.
  endif.

  unassign <proxy_struc>.

* fill additional fields from substructures reboque and vol
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-vol.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <lt_vol>.

*   map 'VOL' fields
    if lines( <lt_vol> ) = 0.
*     vol table is empty, should be handled above. But do nothing here

    elseif lines( <lt_vol> ) = 1.
*     vol table only contains a single line
      read table <lt_vol> index 1 assigning <ls_vol>.
      if sy-subrc = 0 and <ls_vol> is assigned.
        move-corresponding <ls_vol> to cs_pdf_transp.  "#EC ENHOK
      endif.
    else.
      loop at <lt_vol> assigning <ls_vol>.
         lv_qvol  = lv_qvol  + <ls_vol>-q_vol.
         lv_pesob = lv_pesob + <ls_vol>-peso_b.
         lv_pesol = lv_pesol + <ls_vol>-peso_l.

*        concatenate ESP, MARCA, NUM tags into second add info structure
         if <ls_vol>-esp is not initial.
           concatenate mv_inf_compl 'Esp.:' <ls_vol>-esp    "#EC NOTEXT
              into mv_inf_compl separated by space.
           concatenate mv_inf_compl cl_abap_char_utilities=>horizontal_tab
              into mv_inf_compl.
           lv_entry = abap_true.
         endif.
         if <ls_vol>-marca is not initial.
           concatenate mv_inf_compl 'Marca:' <ls_vol>-marca   "#EC NOTEXT
              into mv_inf_compl separated by space.
           concatenate mv_inf_compl cl_abap_char_utilities=>horizontal_tab
              into mv_inf_compl.
           lv_entry = abap_true.
         endif.
         if <ls_vol>-n_vol is not initial.
            concatenate mv_inf_compl 'Num.:' <ls_vol>-n_vol   "#EC NOTEXT
               into mv_inf_compl separated by space.
            lv_entry = abap_true.
         endif.

         if lv_entry = abap_true.
           concatenate mv_inf_compl cl_abap_char_utilities=>cr_lf into mv_inf_compl.
         endif.

         clear lv_entry.
      endloop.

      cs_pdf_transp-peso_b = lv_pesob.
      cs_pdf_transp-peso_l = lv_pesol.
      cs_pdf_transp-q_vol = lv_qvol.
      cs_pdf_transp-esp = cs_pdf_transp-marca = cs_pdf_transp-n_vol = text-002.
    endif.
  endif.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  DET
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-item.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <lt_pdf_det>.
    loop at <lt_pdf_det> assigning <ls_pdf_det>.
      concatenate <ls_pdf_det>-orig <ls_pdf_det>-cst into <ls_pdf_det>-orig_cst.

*     add additional infos (if available) to product description separated by newline
      if <ls_pdf_det>-inf_ad_prod IS NOT INITIAL.
        CONCATENATE <ls_pdf_det>-x_prod cl_abap_char_utilities=>cr_lf <ls_pdf_det>-inf_ad_prod
            INTO <ls_pdf_det>-inf_ad_prod.
      ELSE.
        <ls_pdf_det>-inf_ad_prod = <ls_pdf_det>-x_prod.
      ENDIF.
    endloop.
    ct_pdf_det = <lt_pdf_det>.
  endif.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  TOTAL
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-icmstot.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_total>.
    cs_pdf_total = <ls_total>.
  endif.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  ISSQN
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-issqntot.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_issqn>.
    cs_pdf_issqn = <ls_issqn>.
  endif.
  UNASSIGN <proxy_struc>.
*--------------------------------------------------------------------*
*  INF_ADIC
*--------------------------------------------------------------------*
  read table lt_tags assigning <proxy_struc> with key mapping_group = /xnfe/if_wd_constants=>cv_nfe_proxy-inf_adic.
  if <proxy_struc> is assigned.
    assign <proxy_struc>-mapping_data->* to <ls_infadic>.
    cs_pdf_inf_adic = <ls_infadic>.
  endif.

* map 'INF_CPL'
  if cs_pdf_inf_adic-inf_cpl is not initial.
    concatenate cs_pdf_inf_adic-inf_compl cs_pdf_inf_adic-inf_cpl cl_abap_char_utilities=>cr_lf
        into cs_pdf_inf_adic-inf_compl.
  endif.

* map 'INF_AD_FISCO'
  if cs_pdf_inf_adic-inf_ad_fisco is not initial.
    concatenate cs_pdf_inf_adic-inf_compl cs_pdf_inf_adic-inf_ad_fisco cl_abap_char_utilities=>cr_lf
        into cs_pdf_inf_adic-inf_compl.
  endif.

* also fill secondary additional information box (below item information)
  if mv_inf_compl is not initial.
    cs_pdf_inf_adic-inf_compl2 = mv_inf_compl.
  endif.
ENDFORM.                    " MAP_XML_TO_PDF

*&---------------------------------------------------------------------*
*&      Form  GENERATE_PDF
*&---------------------------------------------------------------------*
FORM GENERATE_PDF  USING i_acckey          TYPE /xnfe/id
                         is_pdf_hd         TYPE /xnfe/danfe_pdf_hd
                         is_pdf_ide        TYPE /xnfe/danfe_pdf_ide
                         is_pdf_emi        TYPE /xnfe/danfe_pdf_emi
                         is_pdf_dest       TYPE /xnfe/danfe_pdf_dest
                         is_pdf_transp     TYPE /xnfe/danfe_pdf_transp
                         it_pdf_det        TYPE /xnfe/danfe_pdf_item_t
                         is_pdf_total      TYPE /xnfe/danfe_pdf_total
                         is_pdf_issqn      TYPE /xnfe/danfe_pdf_issqn
                         is_pdf_inf_adic   TYPE /xnfe/danfe_pdf_inf_adic
                         is_pdf_fatura     TYPE /xnfe/danfe_pdf_fatura
                CHANGING c_pdf             TYPE fpcontent.
*--------------------------------------------------------------------*
  "Based on /XNFE/CL_NFE method DOWNLOAD_DANFE
*--------------------------------------------------------------------*
  DATA: lv_template_source TYPE string,
        lv_fpname          TYPE fpname,
        lv_fmname          type funcname,
        lo_fp_error        type ref to cx_fp_api,
        ls_outputparams    type sfpoutputparams,
        ls_docparams       type sfpdocparams,
        ls_formoutput      type fpformoutput.

  PERFORM define_form_name USING is_pdf_ide CHANGING lv_template_source.

  lv_fpname = lv_template_source.

  try.
      call function 'FP_FUNCTION_MODULE_NAME'
        exporting
          i_name     = lv_fpname
        importing
          e_funcname = lv_fmname.
    catch
      cx_fp_api_repository cx_fp_api_usage cx_fp_api_internal into lo_fp_error.
      DATA: lv_err_msg TYPE string.
      lv_err_msg = lo_fp_error->get_text( ).
      WRITE: / i_acckey, ': ', lv_err_msg COLOR COL_NEGATIVE.
      EXIT.
  endtry.

  ls_outputparams-reqnew = abap_true.
  ls_outputparams-getpdf = abap_true.

  call function 'FP_JOB_OPEN'
    changing
      ie_outputparams = ls_outputparams
    exceptions
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      others          = 5.

  if sy-subrc <> 0.
   WRITE: / i_acckey, ': Ocorreu um erro ao abrir job FP_JOB_OPEN' COLOR COL_NEGATIVE.
   EXIT.
  endif.

  ls_docparams-langu    = sy-langu.
  ls_docparams-fillable = abap_false.
  ls_docparams-dynamic  = abap_false.

  call function lv_fmname
    exporting
      /1bcdwb/docparams  = ls_docparams
      hd                 = ls_pdf_hd
      ide                = ls_pdf_ide
      emit               = ls_pdf_emi
      dest               = ls_pdf_dest
      transp             = ls_pdf_transp
      det                = lt_pdf_det
      total              = ls_pdf_total
      issqn              = ls_pdf_issqn
      inf_adic           = ls_pdf_inf_adic
      fatura             = ls_pdf_fatura
    importing
      /1bcdwb/formoutput = ls_formoutput
    exceptions
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      others             = 4.

  if sy-subrc <> 0.
    WRITE: / i_acckey, ': ', lv_err_msg COLOR COL_NEGATIVE.
    EXIT.
  endif.

  call function 'FP_JOB_CLOSE'
   exceptions
     usage_error          = 1
     system_error         = 2
     internal_error       = 3
     others               = 4.

  if sy-subrc <> 0.
    WRITE: / i_acckey, ': ', lv_err_msg COLOR COL_NEGATIVE.
    EXIT.
  endif.

  c_pdf = ls_formoutput-pdf.

ENDFORM.                    " GENERATE_PDF
*&---------------------------------------------------------------------*
*&      Form  SAVE_PDF
*&---------------------------------------------------------------------*
FORM SAVE_PDF  USING    i_filename   TYPE string
                        i_pdf        TYPE xstring.

  DATA: lv_len      TYPE i,
        lt_pdf      TYPE solix_tab.

  lv_len = xstrlen( i_pdf ).
  lt_pdf = cl_bcs_convert=>xstring_to_solix( i_pdf ).

  cl_gui_frontend_services=>gui_download(
    exporting
      BIN_FILESIZE              = lv_len
      FILENAME                  = i_filename
      FILETYPE                  = 'BIN'
    changing
      DATA_TAB                  = lt_pdf
    exceptions
      FILE_WRITE_ERROR          = 1
      NO_BATCH                  = 2
      GUI_REFUSE_FILETRANSFER   = 3
      INVALID_TYPE              = 4
      NO_AUTHORITY              = 5
      UNKNOWN_ERROR             = 6
      HEADER_NOT_ALLOWED        = 7
      SEPARATOR_NOT_ALLOWED     = 8
      FILESIZE_NOT_ALLOWED      = 9
      HEADER_TOO_LONG           = 10
      DP_ERROR_CREATE           = 11
      DP_ERROR_SEND             = 12
      DP_ERROR_WRITE            = 13
      UNKNOWN_DP_ERROR          = 14
      ACCESS_DENIED             = 15
      DP_OUT_OF_MEMORY          = 16
      DISK_FULL                 = 17
      DP_TIMEOUT                = 18
      FILE_NOT_FOUND            = 19
      DATAPROVIDER_EXCEPTION    = 20
      CONTROL_FLUSH_ERROR       = 21
      NOT_SUPPORTED_BY_GUI      = 22
      ERROR_NO_GUI              = 23
      OTHERS                    = 24
  ).
  if sy-subrc <> 0.
*   message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
ENDFORM.                    " SAVE_PDF
*&---------------------------------------------------------------------*
*&      Form  DEFINE_FORM_NAME
*&---------------------------------------------------------------------*
FORM DEFINE_FORM_NAME  USING    IS_PDF_IDE TYPE /xnfe/danfe_pdf_ide
                       CHANGING c_TEMPLATE_SOURCE TYPE string.
  case is_pdf_ide-tp_imp.
    when '0'.  "no DANFE printed
      c_template_source = cv_nfe_pdf_layout-normal_portrait. "DEFAULT
    when '1' or '3'. "portrait or simple layout
      case is_pdf_ide-tp_emis.
        when '1' or '3' or '6' or '7'.   "normal NF-e, SCAN, SVC (AN), SVC (43)
          c_template_source = cv_nfe_pdf_layout-normal_portrait.
        when '2' or '5'.   "security paper
          c_template_source = cv_nfe_pdf_layout-sec_pap_portrait.
        when '4'.   "EPEC/ DEPEC
          c_template_source = cv_nfe_pdf_layout-epec_portrait.
        when others.
          c_template_source = cv_nfe_pdf_layout-normal_portrait. "DEFAULT
      endcase.
    when '2'.  "landscape
      case is_pdf_ide-tp_emis.
        when '1' or '3' or '6' or '7'.   "normal NF-e, SCAN, SVC (AN), SVC (43)
          c_template_source = cv_nfe_pdf_layout-normal_landscape.
        when '2' or '5'.   "security paper
          c_template_source = cv_nfe_pdf_layout-sec_pap_landscape.
        when '4'.  "EPEC/ DEPEC
          c_template_source = cv_nfe_pdf_layout-epec_landscape.
        when others.
          c_template_source = cv_nfe_pdf_layout-normal_portrait. "DEFAULT
      endcase.
    when others.  " DANFE display not supported
      c_template_source = cv_nfe_pdf_layout-normal_portrait. "DEFAULT
  endcase.
ENDFORM.                    " DEFINE_FORM_NAME

FORM GET_FILENAME USING i_acckey    TYPE /xnfe/id
                        i_list      TYPE localfile
               CHANGING c_filename  TYPE string.
  FIELD-SYMBOLS: <res> TYPE MATCH_RESULT.
  DATA: lt_result   TYPE match_result_tab,
        lv_path     TYPE string.

  FIND ALL OCCURRENCES OF '\' IN i_list RESULTS lt_result.
  IF sy-subrc EQ 0.
    READ TABLE lt_result ASSIGNING <res> INDEX lines( lt_result ).
    IF sy-subrc EQ 0.
      <res>-offset = <res>-offset + 1.
      lv_path = i_list(<res>-offset).
    ENDIF.
  ELSE.
    "Default
    lv_path = 'C:\temp\'.
  ENDIF.

  IF p_xml EQ abap_true.
    concatenate lv_path /xnfe/if_wd_constants=>cv_xml_file_front_nfe
              i_acckey /xnfe/if_wd_constants=>cv_xml_extension into c_filename.
  ELSE.
    concatenate lv_path /xnfe/if_wd_constants=>cv_xml_file_front_nfe
              i_acckey /xnfe/if_wd_constants=>cv_pdf_extension into c_filename.
  ENDIF.

ENDFORM.                    " GET_FILENAME

*&---------------------------------------------------------------------*
*&      Form  SAVE_XML
*&---------------------------------------------------------------------*
FORM SAVE_XML  USING    i_filename   TYPE string
                        i_xml        TYPE xstring.

  DATA: lv_len      TYPE i,
        lt_xml      TYPE solix_tab.

  lv_len = xstrlen( i_xml ).
  lt_xml = cl_bcs_convert=>xstring_to_solix( i_xml ).

  cl_gui_frontend_services=>gui_download(
    exporting
      BIN_FILESIZE              = lv_len
      FILENAME                  = i_filename
      FILETYPE                  = 'BIN'
    changing
      DATA_TAB                  = lt_xml
    exceptions
      FILE_WRITE_ERROR          = 1
      NO_BATCH                  = 2
      GUI_REFUSE_FILETRANSFER   = 3
      INVALID_TYPE              = 4
      NO_AUTHORITY              = 5
      UNKNOWN_ERROR             = 6
      HEADER_NOT_ALLOWED        = 7
      SEPARATOR_NOT_ALLOWED     = 8
      FILESIZE_NOT_ALLOWED      = 9
      HEADER_TOO_LONG           = 10
      DP_ERROR_CREATE           = 11
      DP_ERROR_SEND             = 12
      DP_ERROR_WRITE            = 13
      UNKNOWN_DP_ERROR          = 14
      ACCESS_DENIED             = 15
      DP_OUT_OF_MEMORY          = 16
      DISK_FULL                 = 17
      DP_TIMEOUT                = 18
      FILE_NOT_FOUND            = 19
      DATAPROVIDER_EXCEPTION    = 20
      CONTROL_FLUSH_ERROR       = 21
      NOT_SUPPORTED_BY_GUI      = 22
      ERROR_NO_GUI              = 23
      OTHERS                    = 24
  ).
  if sy-subrc <> 0.
*   message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
ENDFORM.                    " SAVE_XML

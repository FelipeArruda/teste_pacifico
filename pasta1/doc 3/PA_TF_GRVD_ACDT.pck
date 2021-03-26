CREATE OR REPLACE PACKAGE ADM_TIG.PA_TF_GRVD_ACDT IS

  -- AUTHOR  : 30024746
  -- CREATED : 16/11/2015 17:01:18
  -- PURPOSE :
  PROCEDURE "MAIN"("STATUS" OUT NOCOPY VARCHAR2);
  PROCEDURE CARGA_GRAVIDADE;
  PROCEDURE CARGA_QT_VEIC_ACIDENTADOS;
  PROCEDURE CARGA_THP;
  PROCEDURE CARGA_VITIMAS;
  PROCEDURE CARGA_PESOS;

END PA_TF_GRVD_ACDT;
/
CREATE OR REPLACE PACKAGE BODY ADM_TIG.PA_TF_GRVD_ACDT IS

  VA_STATUS           VARCHAR2(4000);
  V_PS_THP            NUMBER := 0;
  V_PS_QT_LOCO_ACDD   NUMBER := 0;
  V_PS_CUSTO_LOCO     NUMBER := 0;
  V_PS_QT_VAGAO_ACDD  NUMBER := 0;
  V_PS_CUSTO_VAGAO    NUMBER := 0;
  V_PS_CUSTO_VIA      NUMBER := 0;
  V_PS_CUSTO_ELEL     NUMBER := 0;
  V_PS_IC_VTMA        NUMBER := 0;
  V_PS_TP_ACDT        NUMBER := 0;
  VL_CD_CRIT_THP      NUMBER := 0;
  VL_CD_CRIT_QT_LOCO  NUMBER := 0;
  VL_CD_CRIT_VL_LOCO  NUMBER := 0;
  VL_CD_CRIT_QT_VAGAO NUMBER := 0;
  VL_CD_CRIT_VL_VAGAO NUMBER := 0;
  VL_CD_CRIT_VL_VIA   NUMBER := 0;
  VL_CD_CRIT_VL_ELEL  NUMBER := 0;
  VL_CD_CRIT_VTMA     NUMBER := 1;
  V_VL_TT_GRVD_ACDT   NUMBER := 0;

  PROCEDURE MAIN("STATUS" OUT NOCOPY VARCHAR2) IS
  
  BEGIN
    CARGA_GRAVIDADE;
    CARGA_QT_VEIC_ACIDENTADOS;
    CARGA_THP;
    CARGA_VITIMAS;
    CARGA_PESOS;
  END;

  PROCEDURE CARGA_GRAVIDADE IS
    CURSOR C_ACDT IS
      SELECT A.CD_ACDT,
             D.ID_TP_DI,
             (SELECT TRANSLATE(A.DC_ACDT, 'ÁÇÉÍÓÚÀÈÌÒÙÂÊÎÔÛÃÕËÜÁÇÉÍÓÚÀÈÌÒÙÂÊÎÔÛÃÕËÜ´', 'ACEIOUAEIOUAEIOUAOEUACEIOUAEIOUAEIOUAOEU.')
                FROM DUAL) DC_ACDT,
             A.CD_TIPO_ACDT,
             NVL(A.CD_TIPO_CAUSA_EFTV, 3304) CD_TIPO_CAUSA_EFTV, --SE TIPO DA CAUSA FOR NULO FORÇA O CÓDIGO 3304(OUTROS).
             PA.SG_PATIO_FRVR,
             NVL(VL_CUSTO_LOCO, 0) VL_CUSTO_LOCO,
             NVL(VL_CUSTO_VAGAO, 0) VL_CUSTO_VAGAO,
             NVL(VL_CUSTO_VIA, 0) VL_CUSTO_VIA,
             NVL(VL_CUSTO_ELEL, 0) VL_CUSTO_ELEL
        FROM TIG_ACDT_TREM A,
             TIG_D_TP_DI D,
             (SELECT SUM(CASE
                           WHEN CP.CD_TIPO_ITEM_APUR = 12 AND V.IC_TIPO_VEIC = 'L' THEN
                            CP.QT_ITEM * CP.PR_VENDA_ITEM
                           ELSE
                            0
                         END) VL_CUSTO_LOCO,
                     SUM(CASE
                           WHEN CP.CD_TIPO_ITEM_APUR = 12 AND V.IC_TIPO_VEIC = 'V' THEN
                            CP.QT_ITEM * CP.PR_VENDA_ITEM
                           ELSE
                            0
                         END) VL_CUSTO_VAGAO,
                     CD_ACDT
                FROM TIG_PVIS_CUSTO_MCNC_ITEM_ACDT CP,
                     TIG_VEIC                      V
               WHERE CP.ID_VEIC = V.PID(+)
               GROUP BY CD_ACDT) CP,
             (SELECT SUM(CASE
                           WHEN CI.CD_TIPO_ITEM_APUR = 13 THEN
                            CI.QT_ITEM * CI.PR_VENDA_ITEM
                           ELSE
                            0
                         END) VL_CUSTO_VIA,
                     SUM(CASE
                           WHEN CI.CD_TIPO_ITEM_APUR = 14 THEN
                            CI.QT_ITEM * CI.PR_VENDA_ITEM
                           ELSE
                            0
                         END) VL_CUSTO_ELEL,
                     CD_ACDT
                FROM TIG_PVIS_CUSTO_ITEM_ACDT CI
               GROUP BY CD_ACDT) CI,
             ---TIG_VEIC V,
             TIG_TIPO_ACDT  TA,
             TIG_PATIO_FRVR PA
       WHERE TRUNC(A.DT_ACDT) = D.DT
         AND A.IC_SITC_APUR_ACDT != 'I'
         AND A.CD_ACDT = CP.CD_ACDT(+)
         AND A.CD_ACDT = CI.CD_ACDT(+)
         AND TA.CD_TIPO_ACDT = A.CD_TIPO_ACDT
         AND A.ID_PATIO_ANTR = PA.PID(+);
    --   AND A.CD_ACDT = 134153
  
  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE TF_GRVD_ACDT';
    FOR R IN C_ACDT LOOP
    
      INSERT INTO TIG_F_GRVD_ACDT
        (ID_TP_DI, CD_ACDT, DC_ACDT, VL_CUSTO_LOCO, VL_CUSTO_VAGAO, VL_CUSTO_VIA, VL_CUSTO_ELEL, CD_TIPO_ACDT, TP_CAUSA, SG_PATIO)
      VALUES
        (R.ID_TP_DI,
         R.CD_ACDT,
         R.DC_ACDT,
         R.VL_CUSTO_LOCO,
         R.VL_CUSTO_VAGAO,
         R.VL_CUSTO_VIA,
         R.VL_CUSTO_ELEL,
         R.CD_TIPO_ACDT,
         R.CD_TIPO_CAUSA_EFTV,
         R.SG_PATIO_FRVR);
      COMMIT;
    END LOOP;
    NULL;
  END CARGA_GRAVIDADE;

  PROCEDURE CARGA_QT_VEIC_ACIDENTADOS IS
    CURSOR C_VEIC IS
      SELECT V.CD_ACDT,
             SUM(DECODE(UPPER(DC_TIPO_VEIC), 'LOCO', 1, 0) + (DECODE(UPPER(DC_TIPO_VEIC), 'LOCOMOTIVA', 1, 0))) QT_LOCO_ACDD,
             SUM(DECODE(UPPER(DC_TIPO_VEIC), 'VAGÃO', 1, 0) + DECODE(UPPER(DC_TIPO_VEIC), 'VAGAO', 1, 0)) QT_VAGAO_ACDD
        FROM TIG_VEIC_ACDD V,
             TIG_ACDT_TREM A
       WHERE V.CD_ACDT(+) = A.CD_ACDT
      --   AND A.CD_ACDT IN (133253,191573,192076)
       GROUP BY V.CD_ACDT;
  
  BEGIN
    FOR V IN C_VEIC LOOP
    
      UPDATE TF_GRVD_ACDT
         SET QT_VAGAO_ACDD = V.QT_VAGAO_ACDD, QT_LOCO_ACDD = V.QT_LOCO_ACDD
       WHERE CD_ACDT = V.CD_ACDT;
      COMMIT;
    END LOOP;
  
  END CARGA_QT_VEIC_ACIDENTADOS;

  PROCEDURE CARGA_THP IS
    CURSOR C_THP IS
      select ATV.NU_SEQC_ACDT,
             ROUND(SUM((ATV.DT_ECRM_INTV_EXEC_ATVD_INTN - ATV.DT_INIC_INTV_EXEC_ATVD_INTN) * 24), 2) TP_ATVD_ACDT
        from (SELECT ATV.NU_SEQC_ACDT,
                     ATV.DT_INIC_INTV_EXEC_ATVD_INTN,
                     ATV.DT_ECRM_INTV_EXEC_ATVD_INTN
              
                FROM TIG_ATVD_INTN_TRPT ATV,
                     TIG_TIPO_ATVD_INTN TIP,
                     TIG_LCLZ_RCUR_FRVR LRF,
                     TIG_PATIO_FRVR     PF
               WHERE ATV.NU_SEQC_ACDT > 0
                 AND ATV.ID_PATIO_FRVR = PF.PID
                 AND ATV.ID_TREM = LRF.PID
                 AND TIP.IC_ATVD_ACDT = 'T'
                 AND ATV.IC_ESTD_ATVD_INTN_TRPT in ('ENCERRADA', 'EXECUTADA_PARCIALMENTE')
                 AND ATV.ID_TIPO_ATVD_TREM = TIP.PID
              
              union
              
              SELECT ATV.NU_SEQC_ACDT,
                     ATV_REF.DT_INIC_INTV_EXEC_ATVD_INTN,
                     ATV_REF.DT_ECRM_INTV_EXEC_ATVD_INTN
                FROM TIG_ATVD_INTN_TRPT ATV_REF,
                     TIG_ATVD_INTN_TRPT ATV,
                     TIG_TIPO_ATVD_INTN TIP_REF,
                     TIG_TIPO_ATVD_INTN TIP,
                     TIG_LCLZ_RCUR_FRVR LRF,
                     TIG_PATIO_FRVR     PF
               WHERE ATV_REF.ID_ATVD_INTN_REFR = ATV.PID
                 AND ATV_REF.ID_PATIO_FRVR = PF.PID
                 AND ATV_REF.ID_TREM = LRF.PID
                 AND ATV_REF.ID_ATVD_INTN_REFR is not null
                 AND ATV.NU_SEQC_ACDT > 0
                 AND TIP.IC_ATVD_ACDT = 'T'
                 AND ATV_REF.IC_ESTD_ATVD_INTN_TRPT in ('ENCERRADA', 'EXECUTADA_PARCIALMENTE')
                 AND ATV_REF.ID_TIPO_ATVD_TREM = TIP_REF.PID
                 AND ATV.ID_TIPO_ATVD_TREM = TIP.PID) ATV
       GROUP BY NU_SEQC_ACDT;
    /* --Alteração 07/07/2016 
    SELECT NU_SEQC_ACDT,
           ROUND(SUM((DT_ECRM_INTV_EXEC_ATVD_INTN -
                     DT_INIC_INTV_EXEC_ATVD_INTN) * 24),
                 2) TP_ATVD_ACDT
      FROM T_ATVD_INTN_TRPT A, TIG_TIPO_ATVD_INTN B
     WHERE B.PID = A.ID_TIPO_ATVD_TREM
       AND IC_ESTD_ATVD_INTN_TRPT IN
           ('ENCERRADA', 'EXECUTADA PARCIALMENTE')
       AND NU_SEQC_ACDT IS NOT NULL
     GROUP BY NU_SEQC_ACDT;
    */
  
  BEGIN
    FOR T IN C_THP LOOP
    
      UPDATE TF_GRVD_ACDT
         SET VL_THP = T.TP_ATVD_ACDT
       WHERE CD_ACDT = T.NU_SEQC_ACDT;
      COMMIT;
    END LOOP;
  END CARGA_THP;

  PROCEDURE CARGA_VITIMAS IS
    CURSOR C_VTMA IS
      SELECT CD_ACDT,
             (DECODE(IC_PSOA_ACDD, 1, 'S', 0, 'N')) IC_VTMA
        FROM TIG_ACDT_TREM;
  BEGIN
    FOR P IN C_VTMA LOOP
      UPDATE TF_GRVD_ACDT
         SET IC_VTMA = P.IC_VTMA
       WHERE CD_ACDT = P.CD_ACDT;
      COMMIT;
    END LOOP;
  END CARGA_VITIMAS;

  PROCEDURE CARGA_PESOS IS
  
    CURSOR C_PESOS IS
      SELECT CD_ACDT,
             CD_TIPO_ACDT,
             VL_THP,
             QT_LOCO_ACDD,
             VL_CUSTO_LOCO,
             QT_VAGAO_ACDD,
             VL_CUSTO_VAGAO,
             VL_CUSTO_VIA,
             VL_CUSTO_ELEL,
             IC_VTMA
        FROM TIG_F_GRVD_ACDT;
  BEGIN
    FOR L IN C_PESOS LOOP
      --PEGA VALOR DO PESO DO TIPO DO ACIDENTE
      IF L.CD_TIPO_ACDT > 0 THEN
        SELECT B.VL_PS_CRIT_GRVD_ACDT
          INTO V_PS_TP_ACDT
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = 1
           AND A.CD_TIPO_ACDT = L.CD_TIPO_ACDT
           AND A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT;
      END IF;
      --PESO THP
      IF L.VL_THP > 0 THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_THP,
               VL_CD_CRIT_THP
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 2
           AND L.VL_THP >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
           AND L.VL_THP < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
      END IF;
      --PESO QUANTIDADE DE LOCOS
      IF L.QT_LOCO_ACDD > 0 THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_QT_LOCO_ACDD,
               VL_CD_CRIT_QT_LOCO
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 5
           AND L.QT_LOCO_ACDD >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
           AND L.QT_LOCO_ACDD < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
      END IF;
      --PESO CUSTO LOCO
      IF L.VL_CUSTO_LOCO > 0 THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_CUSTO_LOCO,
               VL_CD_CRIT_VL_LOCO
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 4
           AND L.VL_CUSTO_LOCO >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
           AND L.VL_CUSTO_LOCO < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
      END IF;
      --PESO QUANTIDADE DE VAGÕES
      IF L.QT_VAGAO_ACDD > 0 THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_QT_VAGAO_ACDD,
               VL_CD_CRIT_QT_VAGAO
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 7
           AND L.QT_VAGAO_ACDD >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
           AND L.QT_VAGAO_ACDD < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
      END IF;
      --PESO CUSTO VAGÃO
      IF L.VL_CUSTO_VAGAO > 0 THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_CUSTO_VAGAO,
               VL_CD_CRIT_VL_VAGAO
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 6
           AND L.VL_CUSTO_VAGAO >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
           AND L.VL_CUSTO_VAGAO < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
      END IF;
      --PESO CUSTO VIA
      IF L.VL_CUSTO_VIA > 0 THEN
        begin
          SELECT VL_PS_CRIT_GRVD_ACDT,
                 B.CD_CRIT_PRMT_GRVD_ACDT
            INTO V_PS_CUSTO_VIA,
                 VL_CD_CRIT_VL_VIA
            FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
                 ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
           WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
             AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
             AND A.CD_PRMT_GRVD_ACDT = 8
             AND L.VL_CUSTO_VIA >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
             AND L.VL_CUSTO_VIA < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
        exception
          when no_data_found then
            dbms_output.put_line(l.VL_CUSTO_VIA);
        end;
      END IF;
      --PESO CUSTO ELETRO
      IF L.VL_CUSTO_ELEL > 0 THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_CUSTO_ELEL,
               VL_CD_CRIT_VL_ELEL
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 9
           AND L.VL_CUSTO_ELEL >= A.VL_INIL_FAIXA_CRIT_GRVD_ACDT
           AND L.VL_CUSTO_ELEL < A.VL_FINAL_FAIXA_CRIT_GRVD_ACDT;
      END IF;
      --PESO SE HOUVE VÍTIMA
      IF L.IC_VTMA = 'S' THEN
        SELECT VL_PS_CRIT_GRVD_ACDT,
               B.CD_CRIT_PRMT_GRVD_ACDT
          INTO V_PS_IC_VTMA,
               VL_CD_CRIT_VTMA
          FROM ADM_TIG.T_CRIT_PRMT_GRVD_ACDT    A,
               ADM_TIG.T_PS_CRIT_PRMT_GRVD_ACDT B
         WHERE A.CD_PRMT_GRVD_ACDT = B.CD_PRMT_GRVD_ACDT
           AND A.CD_CRIT_PRMT_GRVD_ACDT = B.CD_CRIT_PRMT_GRVD_ACDT
           AND A.CD_PRMT_GRVD_ACDT = 3
           AND A.CD_CRIT_PRMT_GRVD_ACDT = 2;
      END IF;
    
      /*V_VL_TT_GRVD_ACDT := ROUND(((V_PS_THP + V_PS_QT_LOCO_ACDD + V_PS_CUSTO_LOCO +
      V_PS_QT_VAGAO_ACDD + V_PS_CUSTO_VAGAO +
      V_PS_CUSTO_VIA + V_PS_CUSTO_ELEL + V_PS_IC_VTMA)/100),2);*/
    
      V_VL_TT_GRVD_ACDT := ((V_PS_THP + V_PS_QT_LOCO_ACDD + V_PS_CUSTO_LOCO + V_PS_QT_VAGAO_ACDD + V_PS_CUSTO_VAGAO + V_PS_CUSTO_VIA + V_PS_CUSTO_ELEL +
                           V_PS_IC_VTMA + V_PS_TP_ACDT) / 100);
    
      UPDATE TIG_F_GRVD_ACDT
         SET PS_THP             = V_PS_THP,
             CD_CRIT_THP        = VL_CD_CRIT_THP,
             PS_QT_LOCO         = V_PS_QT_LOCO_ACDD,
             CD_CRIT_QT_LOCO    = VL_CD_CRIT_QT_LOCO,
             PS_CUSTO_LOCO_ACDD = V_PS_CUSTO_LOCO,
             CD_CRIT_VL_LOCO    = VL_CD_CRIT_VL_LOCO,
             PS_QT_VAGAO        = V_PS_QT_VAGAO_ACDD,
             CD_CRIT_QT_VAGAO   = VL_CD_CRIT_QT_VAGAO,
             PS_VL_VAGAO        = V_PS_CUSTO_VAGAO,
             CD_CRIT_VL_VAGAO   = VL_CD_CRIT_VL_VAGAO,
             PS_VL_VIA          = V_PS_CUSTO_VIA,
             CD_CRIT_VL_VIA     = VL_CD_CRIT_VL_VIA,
             PS_VL_ELEL         = V_PS_CUSTO_ELEL,
             CD_CRIT_VL_ELEL    = VL_CD_CRIT_VL_ELEL,
             PS_VTMA            = V_PS_IC_VTMA,
             CD_CRIT_VTMA       = VL_CD_CRIT_VTMA,
             PS_TIPO_ACDT       = V_PS_TP_ACDT,
             VL_TT_GRVD_ACDT    = V_VL_TT_GRVD_ACDT
       WHERE CD_ACDT = L.CD_ACDT;
      COMMIT;
    
      V_PS_THP            := 0;
      V_PS_QT_LOCO_ACDD   := 0;
      V_PS_CUSTO_LOCO     := 0;
      V_PS_QT_VAGAO_ACDD  := 0;
      V_PS_CUSTO_VAGAO    := 0;
      V_PS_CUSTO_VIA      := 0;
      V_PS_CUSTO_ELEL     := 0;
      V_PS_IC_VTMA        := 0;
      V_PS_TP_ACDT        := 0;
      VL_CD_CRIT_THP      := 0;
      VL_CD_CRIT_QT_LOCO  := 0;
      VL_CD_CRIT_VL_LOCO  := 0;
      VL_CD_CRIT_QT_VAGAO := 0;
      VL_CD_CRIT_VL_VAGAO := 0;
      VL_CD_CRIT_VL_VIA   := 0;
      VL_CD_CRIT_VL_ELEL  := 0;
      VL_CD_CRIT_VTMA     := 1;
      V_VL_TT_GRVD_ACDT   := 0;
    END LOOP;
  END CARGA_PESOS;

BEGIN
  -- INITIALIZATION
  NULL;
END PA_TF_GRVD_ACDT;
/

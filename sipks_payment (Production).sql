CREATE OR REPLACE PACKAGE FCUBEUSER.sipks_payment
AS
/*----------------------------------------------------------------------------------------------------
**
** File Name     : SIPAYMNT.SPC
**
** Module        : SI
**
** This source is part of the FLEXCUBE Corporate - Corporate Banking Software System
** and is copyrighted by Oracle Financial Services Software Limited.
**
**All rights reserved.  No part of this work may be reproduced,
**stored in a retrieval system, adopted or transmitted in any form or by
**any means,electronic, mechanical, photographic, graphic, optic
**recording or otherwise,translated in any language or computer
**language, without the prior written permission of
**Oracle Financial Services Software Limited.
**
** Oracle Financial Services Software Limited.
** 10-11, SDF I, SEEPZ, Andheri (East),
** MUMBAI - 400 096.
** INDIA
**
** Copyright © 1998 - 2009 Oracle Financial Services Software Limited.
----------------------------------------------------------------------------------------------------*/

g_eventcode				CSTBS_CONTRACT.curr_event_code%TYPE;

FUNCTION fn_execute_payments (
										 p_process    IN    SITMS_PRODUCT_PRF.processing_time%TYPE
                    ,p_errcode    OUT   ERTBS_MSGS.err_code%TYPE
                    ,p_params     OUT   VARCHAR2) return BOOLEAN;

FUNCTION fn_process_payments (p_cycle		IN SITBS_CYCLE_DUE_EXEC%ROWTYPE
														,p_errcode	OUT	ERTBS_MSGS.err_code%TYPE
														,p_params		OUT	VARCHAR2) return BOOLEAN;


FUNCTION fn_process_suxs(p_esn  IN    CSTBS_CONTRACT.latest_event_seq_no%TYPE
                        ,p_cycle  IN  SITBS_CYCLE_DUE_EXEC%ROWTYPE
                        ,p_errtype OUT CHAR
                        ,p_errcode  OUT ERTBS_MSGS.err_code%TYPE
                        ,p_params   OUT VARCHAR2)return BOOLEAN;


FUNCTION fn_process_reject(p_esn  IN    CSTBS_CONTRACT.latest_event_seq_no%TYPE
                        ,p_cycle  IN  SITBS_CYCLE_DUE_EXEC%ROWTYPE
                        ,p_errtype OUT CHAR
                        ,p_errcode  OUT ERTBS_MSGS.err_code%TYPE
                        ,p_params   OUT VARCHAR2)return BOOLEAN;

FUNCTION fn_process_pexc(p_esn  IN    CSTBS_CONTRACT.latest_event_seq_no%TYPE
                        ,p_cycle  IN SITBS_CYCLE_DUE_EXEC%ROWTYPE
                        ,p_acc_ccy IN SITBS_CONTRACT_MASTER.si_amt_ccy%TYPE
                        ,p_errtype OUT CHAR
                        ,p_errcode  OUT ERTBS_MSGS.err_code%TYPE
                        ,p_params   OUT VARCHAR2)return BOOLEAN;

END sipks_payment;
/


DROP SYNONYM FCUBEUSER.SIPKSS_PAYMENT;

CREATE SYNONYM FCUBEUSER.SIPKSS_PAYMENT FOR FCUBEUSER.SIPKS_PAYMENT;


GRANT EXECUTE, DEBUG ON FCUBEUSER.SIPKS_PAYMENT TO FC_DBA_PRIV;


DROP PACKAGE BODY FCUBEUSER.SIPKS_PAYMENT;

CREATE OR REPLACE PACKAGE BODY FCUBEUSER.sipks_payment AS
/*----------------------------------------------------------------------------------------------------
**
** File Name  : SIPAYMNT.SQL
**
** Module    : STANDING INSTRUCTIONS
**
**
** This source is part of the FLEXCUBE Corporate - Corporate Banking Software System
**and is copyrighted by Oracle Financial Services Software Limited.
**
**All rights reserved.  No part of this work may be reproduced,
**stored in a retrieval system, adopted or transmitted in any form or by
**any means,electronic, mechanical, photographic, graphic, optic
**recording or otherwise,translated in any language or computer
**language, without the prior written permission of
**Oracle Financial Services Software Limited.
**
**Oracle Financial Services Software Limited.
**10-11, SDF I, SEEPZ, Andheri (East),
**Mumbai - 400 096.
**India

Copyright © 1997- 2013 by Oracle Financial Services Software Limited.

----------------------------------------------------------------------------------------*/

/* Change History

 Changed by      : Pragya
 Change date     : 16-May-12, 20-May-12
 Change Desc     : 1. AEDMBUFCC0488 retro = SI getting rejected without proper error code. 
           2. DEFLT00014141 retro = Tivoli is not assigning the pkg_variables again, 
              so contracts are not getting executed other than first branch of the group
 Search String   : JORUBJFCC0573


04-Jul-2016 3-12694450741 changes

 -- Retro Sources for 3-12694450741 changes
   AEDNIBFCC1936 Changes done to handle rollback properly.
          During SI contracts processing, if error occurs, system roll's back the error logging and proceeds with the next transaction.
          Added code to handle the SI execution for the retry count exceeded contracts.
   AEDNIBFCC2222 ,CONDITION SHD BE... IF RETRY COUNT IN CYCLE (WHICH IS INCREMENTED BY 1 FOR ALL 	 RECORDS) IS > MAX RETRY AT CONTRACT LEVEL... THEN UPDATE STATUS AS IGNORE...

  AEDDIBFCC0181 Additional exception logging changes.

----------------------------------------------------------------------------------------*/


pkg_branch    varchar2(3);
pkg_appdate    date;
pkg_lcy    varchar2(3);
pkg_user    varchar2(12);

--31-JUL-2012 JORUBJFCC0452 get available balance is not considering limit amount
-- MTLCBM fix TIL 289 changed all pkg variables to global variables
--        pkg variables not getting set during AEOD .
--Fcc5.3 Changes starts
FUNCTION Fn_limit_check
            (p_cycle        IN        SITBS_CYCLE_DUE_EXEC%ROWTYPE
            ,p_limit_check        IN OUT    varchar2
            ,p_errcode            OUT        ERTBS_MSGS.err_code%TYPE
            ,p_params            OUT        VARCHAR2)
return BOOLEAN;
--Fcc5.3 Changes Ends

FUNCTION fn_execute_payments (p_process        IN        SITMS_PRODUCT_PRF.processing_time%TYPE
                    ,p_errcode        OUT        ERTBS_MSGS.err_code%TYPE
                    ,p_params            OUT        VARCHAR2) return BOOLEAN
IS

CURSOR    cycles
IS
        -- 3-12694450741 Changes Starts
		/*
        SELECT a.*
        FROM    sitbs_cycle_due_exec a, sitbs_contract_master b
        WHERE substr(a.contract_ref_no,1,3) = pkg_branch
        AND        a.si_exec_status IN ('U','R')
        AND        a.product_type IN ('P','V')
        AND        a.processing_time = p_process
        AND        a.contract_ref_no = b.contract_ref_no
        AND        b.version_no = (select max(version_no)
                                from sitbs_contract_master
                                where contract_ref_no = a.contract_ref_no)
        ORDER BY    b.dr_account asc, a.si_value_date asc, a.priority desc;*/
		
		    SELECT a.*
    FROM  sitbs_cycle_due_exec a, sitbs_contract_master b
    WHERE substr(a.contract_ref_no,1,3) = pkg_branch
    AND    a.si_exec_status IN ('U','R')
    AND    a.product_type IN ('P','V')
    AND    a.processing_time = p_process
    AND    a.contract_ref_no = b.contract_ref_no
    AND    a.version_no = (select max(version_no)
                from sitbs_contract_master
                --where contract_ref_no = a.contract_ref_no and event_code <> ' CLOS')
		--Evette Started
		--where contract_ref_no = a.contract_ref_no AND dr_acc_br = pkg_branch)
		where contract_ref_no = a.contract_ref_no)
		--Evette Ended
    AND EXISTS (SELECT 1 FROM CSTBS_CONTRACT WHERE MODULE_CODE = 'SI' AND CONTRACT_REf_NO = A.CONTRACT_REF_NO AND CONTRACT_STATUS <> 'S')
    ORDER BY  b.dr_account asc, a.si_value_date asc, a.priority desc;
    -- 3-12694450741 Changes Ends

l_errcode            ERTBS_MSGS.err_code%TYPE := '';
l_params            VARCHAR2(2000) := '';
l_commit_freq    CSTBS_COMMITFREQ.bod_commit_count%TYPE:=25;
l_commit_count    NUMBER (4) := 0;
log_exception    EXCEPTION;
--FCC5.3 Changes starts
l_product_code        SITMS_PRODUCT_PRF.product_code%TYPE;
l_referral_required    SITMS_PRODUCT_PRF.REFERRAL_REQUIRED%TYPE;
l_referral_required_ac    SITMS_PRODUCT_PRF.REFERRAL_REQUIRED%TYPE;
l_dr_account        SITB_CONTRACT_MASTER.DR_ACCOUNT%TYPE;
l_limit_check        VARCHAR2(5) := 'FALSE';
--FCC5.3 Changes Ends

BEGIN

        debug.pr_debug ('SI','In fn_execute_payments for branch '||global.current_branch);
        --JORUBJFCC0573 retro of DEFLT00014141 changes TO REINAITAILAIZE THE PACKAGE VARIBALES
            pkg_branch  := global.current_branch;
            pkg_appdate := global.application_date;
            pkg_lcy     := global.lcy;
            pkg_user    := global.user_id;
        debug.pr_debug ('SI','||pkg_branch'||pkg_branch);
         --JORUBJFCC0573 retro of DEFLT00014141 changes TO REINAITAILAIZE THE PACKAGE VARIBALES
        BEGIN

            IF p_process = 'E'
            THEN
            debug.pr_debug ('SI','Processing Time is EOD');

                SELECT eod_commit_count
                INTO     l_commit_freq
                FROM     cstbs_commitfreq
                WHERE     module_id = 'SI'
                AND         function_id = 'SIPAYMNT';

            ELSE
            debug.pr_debug ('SI','Processing Time is BOD');

                SELECT bod_commit_count
                INTO     l_commit_freq
                FROM     cstbs_commitfreq
                WHERE     module_id = 'SI'
                AND         function_id = 'SIPAYMNT';

            END IF;

        EXCEPTION

            WHEN no_data_found THEN
            l_commit_freq := 25;
            WHEN others THEN
            return FALSE;
        END;


        FOR p_cycle in cycles
        LOOP
            l_limit_check := 'FALSE' ; --Fcc5.3 Changes
            SAVEPOINT process;
            BEGIN
                --Fcc5.3 Changes starts
                debug.pr_debug ('SI','start inside loop');
                If p_cycle.action_code_amt = 'W' and p_process = 'B'
                Then

                    select     nvl(referral_required, 'N')
                    into         l_referral_required
                    from     SITMS_PRODUCT_PRF
                    where
                        --INSTRUCTION_CODE = substr(p_cycle.CONTRACT_REF_NO,4,4);  --Fcc5.3 Changes Remmed out
                        product_CODE = substr(p_cycle.CONTRACT_REF_NO,4,4);


                    SELECT dr_account
                    INTO     l_dr_account
                    FROM     sitbs_contract_master
                    WHERE     contract_ref_no = p_cycle.contract_ref_no
                    AND         version_no = p_cycle.version_no;

                    debug.pr_debug('SI','the l_dr_account '||l_dr_account);

                    if l_referral_required = 'Y'
                    Then
                        --Call the Limit Check function
                        select nvl(referral_required, 'N')
                        INTO l_referral_required_ac
                        from sttms_cust_Account
                        where
                            branch_code = pkg_branch
                        and    cust_ac_no = l_dr_account
                        and     Auth_stat = 'A';

                        if l_referral_required_ac = 'Y'
                        then
                            IF NOT fn_limit_check
                                    (p_cycle
                                    ,l_limit_check
                                    ,l_errcode
                                    ,l_params)
                            THEN
                                debug.pr_debug ('SI','Failed in fn_limit_check '||p_cycle.contract_ref_no);
                                --return FALSE; --remmed out Fcc53ITR1 SFR# 100
								-- 3-12694450741 changes starts
									IF l_errcode IS NULL			
									THEN
									l_errcode := 'SI-PMT-95';
									END if;

									IF l_params IS NULL	
									then
									l_params := SUBSTR(sqlerrm,1,50);
									END if;
								-- 3-12694450741 Changes ends
								
                                RAISE log_exception; --Fcc53ITR1 SFR# 100
                            END IF;
                        End if;
                    End if;
                End if;

                IF l_limit_check = 'FALSE'
                Then
                --Fcc5.3 Changes Ends
                    debug.pr_debug ('SI','Processing Contract '||p_cycle.contract_ref_no);

                    IF NOT fn_process_payments(p_cycle,l_errcode,l_params)
                    THEN
                        debug.pr_debug ('SI','Failed in process of '||p_cycle.contract_ref_no);
						-- 3-12694450741 Changes Starts
							IF l_errcode IS NULL			
							THEN
							l_errcode := 'SI-PMT-94';
							END if;

							IF l_params IS NULL	
							then
							l_params := SUBSTR(sqlerrm,1,50);
							END if;
						-- 3-12694450741 Changes ends
                        RAISE log_exception;
                    END IF;
                End if; -- Fcc5.3 Changes Limit Check


                IF l_commit_count >= l_commit_freq
                THEN
                    COMMIT;
                    l_commit_count := 0;
                ELSE
                    l_commit_count := l_commit_count + 1;
                END IF;


            EXCEPTION WHEN log_exception THEN
                   -- ROLLBACK TO    process; -- 3-12694450741 Changes
                    debug.pr_debug('SI','B4 LOG EXCEPTION '||l_errcode||'~'||l_params);
					  -- 3-12694450741 Changes starts
						IF l_errcode IS NULL			
						THEN
						l_errcode := 'SI-PMT-99';
						l_params := SUBSTR(sqlerrm,1,50);
						END if;
					  -- 3-12694450741 Changes Ends	
                    IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no
                                                ,'SUXS'
                                                ,l_errcode
                                                ,l_params)
                    THEN

                            debug.pr_debug ('SI','Insert into Exception returns false');
							p_errcode := l_errcode;	--3-12694450741 Changes
                            return FALSE;

                    END IF;

            END;

        END LOOP;

        COMMIT;

        debug.pr_debug ('SI','Exec Payments thru');
        return TRUE;

EXCEPTION
    WHEN others THEN
        debug.pr_debug ('SI','Motherhood excp. of Exec Payments'||sqlerrm);
		-- 3-12694450741 Changes Starts
		IF l_errcode IS NULL			
		THEN
		l_errcode := 'SI-PMT-96';
		END if;

		IF l_params IS NULL	
		then
		l_params := SUBSTR(sqlerrm,1,50);
		END if;


		p_errcode := l_errcode;	
		-- 3-12694450741 Changes Ends
        return FALSE;
END fn_execute_payments;

FUNCTION fn_process_payments(p_cycle        IN        SITBS_CYCLE_DUE_EXEC%ROWTYPE
                    ,p_errcode        OUT        ERTBS_MSGS.err_code%TYPE
                    ,p_params        OUT        VARCHAR2) return BOOLEAN

IS

process_next    EXCEPTION;
l_errcode            ERTBS_MSGS.err_code%TYPE;
l_params            VARCHAR2(2000) := '';
l_ovdcodes        VARCHAR2(2000) := '';
l_cstb_cont        CSTBS_CONTRACT%ROWTYPE;
l_sitb_inst        SITBS_INSTRUCTION%ROWTYPE;
l_sitb_cont        SITBS_CONTRACT_MASTER%ROWTYPE;
l_cycle_det        SITBS_CYCLE_DETAIL%ROWTYPE;
l_event_code    CSTBS_CONTRACT.curr_event_code%TYPE;
l_retry_count    SITBS_CYCLE_DUE_EXEC.retry_count%TYPE;
l_action_code    SITBS_CYCLE_DUE_EXEC.action_code_amt%TYPE;
l_new_esn            CSTBS_CONTRACT.latest_event_seq_no%TYPE;
l_acc_bal            SITBS_CONTRACT_MASTER.si_amt%TYPE := 0;
l_amt_pending    SITBS_CYCLE_DUE_EXEC.si_amt_pending%TYPE := 0;
l_rate                CYTMS_RATES.mid_rate%TYPE := 0;
l_errtype            CHAR(1)    ;
l_advupd            NUMBER:=0;
l_ratetype        SITBS_INSTRUCTION.rate_type%TYPE;
l_max_retry        NUMBER:=0;
l_referral_required_ac    SITMS_PRODUCT_PRF.REFERRAL_REQUIRED%TYPE; --Fcc5.3 Changes



BEGIN

 debug.pr_debug ('SI','In fn_process_payments for branch '||global.current_branch);
 l_errtype := NULL; --JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes

-- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes start
  pkg_branch  := global.current_branch;
  pkg_appdate := global.application_date;
  pkg_lcy     := global.lcy;
  pkg_user    := global.user_id;
  debug.pr_debug ('SI','pkg_branch '||pkg_branch);
-- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes end

IF NVL(p_cycle.si_amount,0)= 0 THEN
    l_errcode:='SI-PA0001';
    l_params:=p_cycle.contract_ref_no;
    RAISE process_next;
END IF;

            BEGIN
                SELECT    nvl(max_retry_count,1)
                INTO        l_max_retry
                FROM sitbs_contract_master
                WHERE     contract_ref_no = p_cycle.contract_ref_no
                AND version_no = p_cycle.version_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select of max retry count ');
                return FALSE;
            END;
        BEGIN

            SELECT *
            INTO     l_sitb_cont
            FROM     sitbs_contract_master
            WHERE     contract_ref_no = p_cycle.contract_ref_no
            AND         version_no = p_cycle.version_no;

        EXCEPTION
            WHEN others THEN
            debug.pr_debug ('SI','Failed in select from Contract Master');
            return FALSE;
        END;

                    l_retry_count := NVL(p_cycle.retry_count,0) + 1;
                    debug.pr_debug ('SI','New Retry Count is '||l_retry_count);
                BEGIN

                    SELECT *
                    INTO l_cstb_cont
                    FROM cstbs_contract
                    WHERE contract_ref_no  = p_cycle.contract_ref_no;

                l_new_esn := l_cstb_cont.latest_event_seq_no + 1;


                EXCEPTION
                    WHEN others THEN
                    debug.pr_debug ('SI','Failed in select from cstb contract');
                    return FALSE;
                END;


            BEGIN

                    UPDATE sitbs_cycle_due_exec
                    SET retry_count = l_retry_count
                    WHERE    contract_ref_no = p_cycle.contract_ref_no
                    AND    cycle_seq_no = p_cycle.cycle_seq_no
                    AND    version_no = p_cycle.version_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in update of Due Exec');
                return FALSE;
            END;

            BEGIN
                debug.pr_debug ('SI','before insert into Cycle Detail');

                debug.pr_debug ('SI','Ref No. is '||p_cycle.contract_ref_no);
                debug.pr_debug ('SI','CSN is '||p_cycle.cycle_seq_no);
                debug.pr_debug ('SI','retry count is '||l_retry_count);
                debug.pr_debug ('SI','Dr acc is '||l_sitb_cont.dr_account);
                debug.pr_debug ('SI','Dr acc br is '||l_sitb_cont.dr_acc_br);
                debug.pr_debug ('SI','Dr acc ccy is '||l_sitb_cont.dr_acc_ccy);
                debug.pr_debug ('SI','Cr acc ccy is '||l_sitb_cont.cr_acc_ccy);
                debug.pr_debug ('SI','Cr acc br is '||l_sitb_cont.cr_acc_br);
                debug.pr_debug ('SI','Cr acc is '||l_sitb_cont.cr_account);
                debug.pr_debug ('SI','Priority is '||p_cycle.priority);
                debug.pr_debug ('SI','Apply chg Suxs is '||p_cycle.apply_chg_suxs);
                debug.pr_debug ('SI','Apply chg rejt is '||p_cycle.apply_chg_rejt);
                debug.pr_debug ('SI','Apply chg pexc is '||p_cycle.apply_chg_pexc);
                debug.pr_debug ('SI','Action code is '||p_cycle.action_code_amt);

                debug.pr_debug ('SI','Before actual insert into Cycle detail');

                INSERT INTO sitbs_cycle_detail
                (CONTRACT_REF_NO
                ,CYCLE_SEQ_NO
                ,RETRY_SEQ_NO
                ,RETRY_DATE
                ,EVENT_SEQ_NO
                ,EVENT_CODE
                ,DR_ACC_BR
                ,DR_ACC_CCY
                ,DR_ACCOUNT
                ,AMT_DEBITED
                ,CR_ACC_BR
                ,CR_ACC_CCY
                ,CR_ACCOUNT
                ,AMT_CREDITED
                ,AMT_EXECUTED_LCY
                ,SI_AMT_EXECUTED
                ,PRIORITY
                ,ACTION_CODE_AMT
                ,APPLY_CHG_SUXS
                ,APPLY_CHG_REJT
                ,APPLY_CHG_PEXC)

        VALUES

                (p_cycle.contract_ref_no
                ,p_cycle.cycle_seq_no
                ,l_retry_count
                ,global.application_date
                ,''
                ,''
                ,l_sitb_cont.dr_acc_br
                ,l_sitb_cont.dr_acc_ccy
                ,l_sitb_cont.dr_account
                ,0
                ,l_sitb_cont.cr_acc_br
                ,l_sitb_cont.cr_acc_ccy
                ,l_sitb_cont.cr_account
                ,0
                ,0
                ,0
                ,p_cycle.priority
                ,p_cycle.action_code_amt
                ,p_cycle.apply_chg_suxs
                ,p_cycle.apply_chg_rejt
                ,p_cycle.apply_chg_pexc);

        EXCEPTION

            WHEN others THEN
            debug.pr_debug ('SI','Failed in Insert into Cycle Detail with '||sqlerrm);
            return FALSE;
        END;

            l_action_code := p_cycle.action_code_amt;

            debug.pr_debug ('SI','Action Code is '||l_action_code);

                    IF l_action_code = 'E'
                    THEN

                            debug.pr_debug ('SI','Action Code is Force');

                        SAVEPOINT try_suxs;

                        IF NOT fn_process_suxs (l_new_esn
                                                                        ,p_cycle
                                                                        ,l_errtype
                                                                        ,l_errcode
                                                                        ,l_params)
                        THEN

                            ROLLBACK TO try_suxs;

                            IF l_errtype = 'A'
                            THEN
								--3-12694450741 CHANGES STARTS 

								IF l_errcode IS NULL		
								THEN
								l_errcode := 'SI-PMT-97';
								END if;

								IF l_params IS NULL	
								then
								l_params := SUBSTR(sqlerrm,1,50);
								END if;
								
								-- 3-12694450741 changes ends 
                                debug.pr_debug ('SI','Fn process suxs returned Accounting error');
                                -- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes start
                                  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
                                                        'SUXS',
                                                        l_errcode,
                                                        l_params) THEN

                                    debug.pr_debug('SI', 'Insert into Exception returns false');
                                  END IF;    
                                -- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes end
                                IF l_retry_count = 1
                                THEN
                                    SAVEPOINT try_rejt;

                                    IF NOT fn_process_reject(l_new_esn
                                                    ,p_cycle
                                                    ,l_errtype
                                                    ,l_errcode
                                                    ,l_params)

                                    THEN
									ROLLBACK TO try_rejt; --AEDNIBFCC1936 CHANGES
                                        IF l_errtype = 'A'
                                        THEN
                                                debug.pr_debug ('SI','Fn process reject returned Accounting error');
                                            --ROLLBACK TO try_rejt;    --3-12694450741 CHANGES
												--3-12694450741 CHANGES STARTS HERE

												IF l_errcode IS NULL			
												THEN
												l_errcode := 'SI-PMT-98';
												END if;

												IF l_params IS NULL	
												then
												l_params := SUBSTR(sqlerrm,1,50);
												END if;

												IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
												'REJT',
												l_errcode,
												l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
												debug.pr_debug('SI', 'Insert into Exception returns false');
												return FALSE;
												END IF;
												--3-12694450741 CHANGES ENDS HERE
                                            RAISE process_next;
                                        ELSE
                                                debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                            return FALSE;
                                        END IF;
											-- 3-12694450741 Changes starts
											ELSE
										 IF p_errcode IS NOT NULL THEN
											  BEGIN
												   INSERT INTO CSTBS_CONTRACT_EXCEPTION
														(contract_ref_no,
														 event_seq_no,
														 event_code,
														 counterparty,
														 error_code,
														 module,
														 branch,
														 branch_date,
														 PARAMETERS)
												   VALUES
														(p_cycle.contract_ref_no,
														 l_new_esn,
														 'REJT',
														 l_cstb_cont.counterparty,
														 l_errcode,
														 'SI',
														 pkg_branch,
														 pkg_appdate,
														 l_params || 'Acc Bal: ' || l_acc_bal || '. SI Amount: ' || l_amt_pending
											 );
											  EXCEPTION
												   WHEN OTHERS THEN
														NULL;
											  END;
										  ELSE
											  BEGIN
												   INSERT INTO CSTBS_CONTRACT_EXCEPTION
														(contract_ref_no,
														 event_seq_no,
														 event_code,
														 counterparty,
														 error_code,
														 module,
														 branch,
														 branch_date,
														 PARAMETERS)
												   VALUES
														(p_cycle.contract_ref_no,
														 l_new_esn,
														 'REJT',
														 l_cstb_cont.counterparty,
														 'SI-CON-90',
														 'SI',
														 pkg_branch,
														 pkg_appdate,
														 'Account Balance: ' || l_acc_bal || '. SI Amount: ' || l_amt_pending
											 );
											  EXCEPTION
												   WHEN OTHERS THEN
														NULL;
											  END;


										  END IF;
										-- 3-12694450741 Changes Ends
										
                                    END IF;
                                END IF;
                                -- KERNEL7.3 IMPSUPP RETRO. SFR#47. starts
                                RAISE process_next;
                                -- KERNEL7.3 IMPSUPP RETRO. ends
                        ELSE
                            debug.pr_debug ('SI','Fn process suxs returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                return FALSE;
                        END IF;
        END IF;

        ELSIF l_action_code = 'F'
        THEN
                debug.pr_debug ('SI','Action code is Full Pending');

                IF NOT SIPKSS_SWEEP.fn_acc_is_gl(l_sitb_cont.dr_account
                                                                            ,l_sitb_cont.dr_acc_br
                                                                            ,l_errcode) THEN

                debug.pr_debug ('SI','Account is not a GL');

                BEGIN
/* USDFBME Upgrade 220999 - sanjeev */
    -- USDFBME fromBlr getavlbal changed to getavlbal_with_od
      --     IF NOT sipkss_sweep.fn_GetAvlBal(l_sitb_cont.dr_acc_br
/*    SIPKSS_SWEEP.fn_GetAvlBal changed to ACPKSS_MISC.fn_GetAvlBal
    for FINWARE INTERFACE CHANGES - 10/9/99 - STP1 */
--           IF NOT sipkss_sweep.fn_GetAvlBal_with_od(l_sitb_cont.dr_acc_br

-- JORUBJFCC0452 Start
/* end of Upgrade */
         /* IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                      ,l_sitb_cont.dr_account
                                      ,l_acc_bal) THEN
          debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;
*/

          IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                      ,l_sitb_cont.dr_account
                                      ,l_acc_bal
                                      ,'Y') THEN
          debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;
-- JORUBJFCC0452 End          
          
          END;

          ELSE
                debug.pr_debug ('SI','Account is a GL');

        IF NOT SIPKSS_SWEEP.fn_gl_avlbal (l_sitb_cont.dr_account
                            ,l_sitb_cont.dr_acc_br
                            ,l_sitb_cont.dr_acc_ccy
                            ,l_acc_bal
                            ,l_errcode)
        THEN

          debug.pr_debug ('SI','Failed in getting GL balance with '||l_errcode);
          p_errcode := l_errcode;
          return FALSE;

                END IF;
            END IF;

            BEGIN

                    IF l_sitb_cont.dr_acc_ccy <> p_cycle.si_ccy
                    THEN
                            l_rate:=null;
                            debug.pr_debug ('SI','Account Ccy and SI Ccy are different');
                            debug.pr_debug ('SI','SI CCY is '||p_cycle.si_ccy);
                            debug.pr_debug ('SI','Account Ccy is '||l_sitb_cont.dr_acc_ccy);
                            debug.pr_debug ('SI','out params are '||l_amt_pending||l_rate);

                            BEGIN
                                SELECT rate_type into l_ratetype
                                FROM sitms_product_prf
                                WHERE product_code=substr(p_cycle.contract_ref_no,4,4);
                            EXCEPTION
                                WHEN others THEN
                                    l_ratetype:='STANDARD';
                            END;

                            IF NOT CYPKSS.fn_amt1_to_amt2(pkg_branch
                                                ,p_cycle.si_ccy
                                                ,l_sitb_cont.dr_acc_ccy
                                                ,l_ratetype
                                                ,'M'
                                                ,p_cycle.si_amt_pending
                                                ,'Y'
                                                ,l_amt_pending
                                                ,l_rate
                                                ,l_errcode) THEN

                            debug.pr_debug ('SI','Ccy conversion Failed with '||l_errcode);
                            p_errcode := l_errcode;
                            return FALSE;
                            END IF;
                    ELSE
                        debug.pr_debug ('SI','Ccy conversion thru');
                            l_amt_pending := p_cycle.si_amt_pending;
                    END IF;

                END;

                    IF l_acc_bal >= l_amt_pending THEN
                        debug.pr_debug ('SI','Account Balance is greater than or equal to amt pending');


                        SAVEPOINT try_suxs;

                        IF NOT fn_process_suxs(l_new_esn
                                                                    ,p_cycle
                                                                    ,l_errtype
                                                                    ,l_errcode
                                                                    ,l_params) THEN

                            ROLLBACK TO try_suxs;
                            IF l_errtype = 'A'
                            THEN
									  --3-12694450741 CHANGES STARTS HERE


								IF l_errcode IS NULL			
								THEN
								l_errcode := 'SI-PMT-82';
								END if;

								IF l_params IS NULL	
								then
								l_params := SUBSTR(sqlerrm,1,50);
								END if;
								--3-12694450741 CHANGES Ends HERE
                            debug.pr_debug ('SI','Fn process suxs returned Accounting error');

                                -- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes start
                                IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
                                                        'SUXS',
                                                        l_errcode,
                                                        l_params) THEN

                                    debug.pr_debug('SI', 'Insert into Exception returns false');
                                END IF;
                                -- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes end

                                IF l_retry_count = 1
                                THEN
                                    SAVEPOINT try_rejt;

                                    IF NOT fn_process_reject(l_new_esn
                                                                                    ,p_cycle
                                                                                    ,l_errtype
                                                                                    ,l_errcode
                                                                                    ,l_params) THEN
											--3-12694450741 CHANGES STARTS HERE
											ROLLBACK TO try_rejt;
											--3-12694450741 CHANGES ENDS HERE

                                        IF l_errtype = 'A'
                                        THEN
                                            debug.pr_debug ('SI','Fn process reject returned Accounting error');
                                                --ROLLBACK TO try_rejt;    --3-12694450741 CHANGES
													   --3-12694450741 CHANGES STARTS HERE

										IF l_errcode IS NULL			
										THEN
										l_errcode := 'SI-PMT-81';
										END if;

										IF l_params IS NULL	
										then
										l_params := SUBSTR(sqlerrm,1,50);
										END if;

										IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																					'REJT',
																					l_errcode,
																					l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
										debug.pr_debug('SI', 'Insert into Exception returns false');
										return FALSE;
										END IF;
										--3-12694450741 CHANGES ENDS HERE
                                                RAISE    process_next;
                                        ELSE
                                            debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                return FALSE;
                                        END IF;
										-- 3-12694450741 Changes starts
											ELSE
											 IF p_errcode IS NOT NULL THEN
												  BEGIN
													   INSERT INTO CSTBS_CONTRACT_EXCEPTION
															(contract_ref_no,
															 event_seq_no,
															 event_code,
															 counterparty,
															 error_code,
															 module,
															 branch,
															 branch_date,
															 PARAMETERS)
													   VALUES
															(p_cycle.contract_ref_no,
															 l_new_esn,
															 'REJT',
															 l_cstb_cont.counterparty,
															 l_errcode,
															 'SI',
															 pkg_branch,
															 pkg_appdate,
															 l_params||'Acc Bal: ' || l_acc_bal || '. SI Amount: ' || l_amt_pending);
												  EXCEPTION
													   WHEN OTHERS THEN
															NULL;
												  END;
											  ELSE
												  BEGIN
													   INSERT INTO CSTBS_CONTRACT_EXCEPTION
															(contract_ref_no,
															 event_seq_no,
															 event_code,
															 counterparty,
															 error_code,
															 module,
															 branch,
															 branch_date,
															 PARAMETERS)
													   VALUES
															(p_cycle.contract_ref_no,
															 l_new_esn,
															 'REJT',
															 l_cstb_cont.counterparty,
															 'SI-CON-91',
															 'SI',
															 pkg_branch,
															 pkg_appdate,
															 'Account Balance: ' || l_acc_bal || '. SI Amount: ' || l_amt_pending);
												  EXCEPTION
													   WHEN OTHERS THEN
															NULL;
												  END;


											  END IF;
										-- 3-12694450741 Changes Ends
                                    END IF;

                                END IF;

                        ELSE
                            debug.pr_debug ('SI','Fn process suxs returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                            return FALSE;

                        END IF;
                END IF;

                ELSE
                  -- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes start
                  l_errcode := 'SI-OVD01';
                      p_errcode:= l_errcode;--DEFLT00021554  CHANGES
                  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
                                        'REJT',
                                        l_errcode,
                                        l_params) THEN

                    debug.pr_debug('SI', 'Insert into Exception returns false');
                  END IF;
                  -- JORUBJFCC0573 RETRO OF AEDMBUFCC0488 changes end

                 IF l_retry_count =1
                 THEN
						-- 3-12694450741 CHANGES STARTS HERE
						l_errcode := 'SI-PMT-70;';
						l_params  := l_sitb_cont.dr_account || '~' ||
						l_sitb_cont.dr_acc_br || '~;';
						IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
												'SUXS',
												l_errcode,
												l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
						debug.pr_debug('SI', 'Insert into Exception returns false');
						return FALSE;
						END IF;
						-- 3-12694450741 CHANGES ENDS HERE
                    SAVEPOINT try_rejt;
                    IF NOT fn_process_reject(l_new_esn
                                                                    ,p_cycle
                                                                    ,l_errtype
                                                                    ,l_errcode
                                                                    ,l_params) THEN
									ROLLBACK TO try_rejt; --AEDNIBFCC1936 CHANGES

                            IF l_errtype = 'A'
                            THEN
                            debug.pr_debug ('SI','Fn process reject returned Accounting error');
										--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

											   --3-12694450741 CHANGES STARTS HERE

								  IF l_errcode IS NULL			
								  THEN
									l_errcode := 'SI-PMT-84';
								  END if;

								  IF l_params IS NULL	
								  then
									l_params := SUBSTR(sqlerrm,1,50);
								  END if;

								  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																			'REJT',
																			l_errcode,
																			l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
									debug.pr_debug('SI', 'Insert into Exception returns false');
									return FALSE;
								  END IF;
								  --3-12694450741 CHANGES ENDS HERE
                                RAISE process_next;
                            ELSE
                            debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                return FALSE;

                            END IF;
							 -- 3-12694450741 Changes Ends
								  ELSE
										  BEGIN
											   INSERT INTO CSTBS_CONTRACT_EXCEPTION
													(contract_ref_no,
													 event_seq_no,
													 event_code,
													 counterparty,
													 error_code,
													 module,
													 branch,
													 branch_date,
													 PARAMETERS)
											   VALUES
													(p_cycle.contract_ref_no,
													 l_new_esn,
													 'REJT',
													 l_cstb_cont.counterparty,
													 'SI-CON-92',
													 'SI',
													 pkg_branch,
													 pkg_appdate,
													 'Account Balance: ' || l_acc_bal || '. SI Amount: ' || l_amt_pending);
										  EXCEPTION
											   WHEN OTHERS THEN
													NULL;
										  END;
						-- 3-12694450741 Changes End


                    END IF;

                 END IF;

                END IF;



        ELSIF l_action_code = 'W'
        THEN
            --select the referral required from product and account tables. if both are 'Y' then

                debug.pr_debug ('SI','Action code is to wait till further Instructions');
            IF NOT SIPKSS_SWEEP.fn_acc_is_gl(l_sitb_cont.dr_account
                                ,l_sitb_cont.dr_acc_br
                                ,l_errcode)
            THEN
                debug.pr_debug ('SI','Account is not a GL');
                --Fcc5.3 Changes starts
                --select the referral_required from account
                select nvl(referral_required, 'N')
                INTO l_referral_required_ac
                from sttms_cust_Account
                where
                    branch_code = l_sitb_cont.dr_acc_br
                and    cust_ac_no = l_sitb_cont.dr_account
                and     Auth_stat = 'A';
                 --Fcc5.3 Changes ends


                BEGIN
                    /* USDFBME Upgrade 220999 - sanjeev */
                      --     IF NOT sipkss_sweep.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                    -- USDFBME fromBlr getavlbal changed to getavlbal_with_od
                    --           IF NOT sipkss_sweep.fn_GetAvlBal_with_od(l_sitb_cont.dr_acc_br
                    /* End of Upgrade */
                    --Fcc5.3 Changes starts
                    if l_referral_required_ac = 'Y'
                    then
                          debug.pr_debug ('SI','l_referral_required starts '||l_referral_required_ac);

                              IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                                            ,l_sitb_cont.dr_account
                                                            ,l_acc_bal
                                                ,'Y'
                                                        --22-JUN-02004 BNDBAD#549 Starts
                                                --,'Y'
                                                        --22-JUN-02004 BNDBAD#549 Ends
                                                                      ) THEN
                                  debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
                                  p_errcode := 'SI-SW0001;';
                                  p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
                                  return FALSE;
                           END IF;
                    Else
                        --Fcc5.3 Changes Ends
-- JORUBJFCC0452 Start            
            /*
                   IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                                            ,l_sitb_cont.dr_account
                                                            ,l_acc_bal) THEN
                                  debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
                                  p_errcode := 'SI-SW0001;';
                                  p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
                                  return FALSE;
                           END IF;
                    End if;
          */
                                IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                      ,l_sitb_cont.dr_account
                                      ,l_acc_bal
                                      ,'Y') THEN
          debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
            end if ;
          END IF;
-- JORUBJFCC0452 End 

                  END;
            
  

            ELSE
                debug.pr_debug ('SI','Account is a GL');
                  IF NOT SIPKSS_SWEEP.fn_gl_avlbal (l_sitb_cont.dr_account
                                            ,l_sitb_cont.dr_acc_br
                                    ,l_sitb_cont.dr_acc_ccy
                                            ,l_acc_bal
                                                ,l_errcode)
                  THEN

                      debug.pr_debug ('SI','Failed in getting GL balance with '||l_errcode);
                      p_errcode := l_errcode;
                      return FALSE;
                END IF;
            END IF;

            BEGIN

                    IF l_sitb_cont.dr_acc_ccy <> p_cycle.si_ccy
                    THEN
                            l_rate:=null;
                            debug.pr_debug ('SI','Account Ccy and SI Ccy are different');
                            debug.pr_debug ('SI','SI CCY is '||p_cycle.si_ccy);
                            debug.pr_debug ('SI','Account Ccy is '||l_sitb_cont.dr_acc_ccy);
                            debug.pr_debug ('SI','out params are '||l_amt_pending||l_rate);

                            BEGIN
                                SELECT rate_type into l_ratetype
                                FROM sitms_product_prf
                                WHERE product_code=substr(p_cycle.contract_ref_no,4,4);
                            EXCEPTION
                                WHEN others THEN
                                    l_ratetype:='STANDARD';
                            END;

                            IF NOT CYPKSS.fn_amt1_to_amt2(pkg_branch
                                                ,p_cycle.si_ccy
                                                ,l_sitb_cont.dr_acc_ccy
                                                ,l_ratetype
                                                ,'M'
                                                ,p_cycle.si_amt_pending
                                                ,'Y'
                                                ,l_amt_pending
                                                ,l_rate
                                                ,l_errcode) THEN

                            debug.pr_debug ('SI','Ccy conversion Failed with '||l_errcode);
                            p_errcode := l_errcode;
                            return FALSE;
                            END IF;
                    ELSE
                        debug.pr_debug ('SI','Ccy conversion thru');
                            l_amt_pending := p_cycle.si_amt_pending;
                    END IF;

                END;

                    IF l_acc_bal >= l_amt_pending THEN
                        debug.pr_debug ('SI','Account Balance is greater than or equal to amt pending');
                SAVEPOINT try_suxs;

                IF NOT fn_process_suxs(l_new_esn
                                                            ,p_cycle
                                                            ,l_errtype
                                                            ,l_errcode
                                                            ,l_params) THEN

                        ROLLBACK TO try_suxs;

                        IF l_errtype = 'A'
                        THEN
													 
							--3-12694450741 CHANGES STARTS HERE

						  IF l_errcode IS NULL			
						  THEN
							l_errcode := 'SI-PMT-85';
						  END if;

						  IF l_params IS NULL	
						  then
							l_params := SUBSTR(sqlerrm,1,50);
						  END if;


							IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																	  'SUXS',
																	  l_errcode,
																	  l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
							  debug.pr_debug('SI', 'Insert into Exception returns false');
							  return FALSE;
							END IF;
							--3-12694450741 CHANGES ENDS HERE
                            debug.pr_debug ('SI','Fn process suxs returned Accounting error');
                            IF l_retry_count = 1
                            THEN
                                SAVEPOINT try_rejt;
                                IF NOT fn_process_reject(l_new_esn
                                                                            ,p_cycle
                                                                            ,l_errtype
                                                                            ,l_errcode
                                                                            ,l_params) THEN
												ROLLBACK TO try_rejt; --3-12694450741 CHANGES

                                        IF l_errtype = 'A'
                                        THEN
                                            debug.pr_debug ('SI','Fn process reject returned Accounting error');
													-- ROLLBACK TO try_rejt; --3-12694450741 CHANGES

															   --3-12694450741 CHANGES STARTS HERE

													IF l_errcode IS NULL			
													THEN
													l_errcode := 'SI-PMT-86';
													END if;

													IF l_params IS NULL	
													then
													l_params := SUBSTR(sqlerrm,1,50);
													END if;

													IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																							'REJT',
																							l_errcode,
																							l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
													debug.pr_debug('SI', 'Insert into Exception returns false');
													return FALSE;
													END IF;
													--3-12694450741 CHANGES ENDS HERE
                                                RAISE process_next;
                                        ELSE
                                            debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                return FALSE;
                                        END IF;
                                END IF;

                            BEGIN
                                UPDATE sitbs_cycle_due_exec
                                SET         si_exec_status = 'W'
                                WHERE     contract_ref_no = p_cycle.contract_ref_no
                                AND         cycle_seq_no = p_cycle.cycle_seq_no
                                AND      version_no  = p_cycle.version_no
                                AND      si_exec_status<>'I';

                            EXCEPTION
                                WHEN others THEN
                                debug.pr_debug ('SI','Failed in update of Due Exec for W ');
                                return FALSE;
                            END;

                        ELSE

                            BEGIN
                                UPDATE sitbs_cycle_due_exec
                                SET         si_exec_status = 'W'
                                WHERE     contract_ref_no = p_cycle.contract_ref_no
                                AND         cycle_seq_no = p_cycle.cycle_seq_no
                                AND      version_no  = p_cycle.version_no
                                AND      si_exec_status<>'I';

                            EXCEPTION
                                WHEN others THEN
                                debug.pr_debug ('SI','Failed in update of Due Exec for W ');
                                return FALSE;
                            END;

                        END IF;

                    ELSE
                            debug.pr_debug ('SI','Fn process suxs returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                            RETURN FALSE;

                    END IF;

            END IF;
        ELSE

                            IF l_retry_count = 1
                            THEN
									 --3-12694450741 CHANGES STARTS HERE
								l_errcode := 'SI-PMT-71;';
								l_params  := l_sitb_cont.dr_account || '~' ||
								l_sitb_cont.dr_acc_br || '~;';
								IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
													'SUXS',
													l_errcode,
													l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
								debug.pr_debug('SI', 'Insert into Exception returns false');
								return FALSE;
								END IF;
								--3-12694450741 CHANGES ENDS HERE
                                SAVEPOINT try_rejt;
                                IF NOT fn_process_reject(l_new_esn
                                                                            ,p_cycle
                                                                            ,l_errtype
                                                                            ,l_errcode
                                                                            ,l_params) THEN
											ROLLBACK TO try_rejt; --3-12694450741 CHANGES				  

                                        IF l_errtype = 'A'
                                        THEN
                                            debug.pr_debug ('SI','Fn process reject returned Accounting error');
											--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

														   --3-12694450741 CHANGES STARTS HERE

											  IF l_errcode IS NULL			
											  THEN
												l_errcode := 'SI-PMT-87';
											  END if;

											  IF l_params IS NULL	
											  then
												l_params := SUBSTR(sqlerrm,1,50);
											  END if;

											  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																						'REJT',
																						l_errcode,
																						l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
												debug.pr_debug('SI', 'Insert into Exception returns false');
												return FALSE;
											  END IF;
											  --3-12694450741 CHANGES ENDS HERE
                                                RAISE process_next;
                                        ELSE
                                            debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                return FALSE;
                                        END IF;
                                END IF;

                            BEGIN
                                UPDATE sitbs_cycle_due_exec
                                SET         si_exec_status = 'W'
                                WHERE     contract_ref_no = p_cycle.contract_ref_no
                                AND         cycle_seq_no = p_cycle.cycle_seq_no
                                AND      version_no  = p_cycle.version_no
                                AND         si_exec_status<>'I';

                            EXCEPTION
                                WHEN others THEN
                                debug.pr_debug ('SI','Failed in update of Due Exec for W ');
                                return FALSE;
                            END;

                        ELSE

                            BEGIN
                                UPDATE sitbs_cycle_due_exec
                                SET         si_exec_status = 'W'
                                WHERE     contract_ref_no = p_cycle.contract_ref_no
                                AND         cycle_seq_no = p_cycle.cycle_seq_no
                                AND      version_no  = p_cycle.version_no
                                AND       si_exec_status<>'I';

                            EXCEPTION
                                WHEN others THEN
                                debug.pr_debug ('SI','Failed in update of Due Exec for W ');
                                return FALSE;
                            END;

                        END IF;
            END IF;

        ELSIF l_action_code = 'I'
        THEN

                debug.pr_debug ('SI','Action code is Ignore');


                IF NOT SIPKSS_SWEEP.fn_acc_is_gl(l_sitb_cont.dr_account
                                                                            ,l_sitb_cont.dr_acc_br
                                                                            ,l_errcode) THEN

                debug.pr_debug ('SI','Account is not a GL');

                BEGIN
/* USDFBME Upgrade 220999 - sanjeev */
      --     IF NOT sipkss_sweep.fn_GetAvlBal(l_sitb_cont.dr_acc_br
    -- USDFBME fromBlr getavlbal changed to getavlbal_with_od
/*    SIPKSS_SWEEP.fn_GetAvlBal changed to ACPKSS_MISC.fn_GetAvlBal
    for FINWARE INTERFACE CHANGES - 10/9/99 - STP1 */
--           IF NOT sipkss_sweep.fn_GetAvlBal_with_od(l_sitb_cont.dr_acc_br
/* End of Upgrade */

-- JORUBJFCC0452 start
           /*        IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                            ,l_sitb_cont.dr_account
                                            ,l_acc_bal) THEN
          debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;
          */
          IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                      ,l_sitb_cont.dr_account
                                      ,l_acc_bal
                                      ,'Y') THEN
            debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;
-- JORUBJFCC0452 End  

          END;

          ELSE
                debug.pr_debug ('SI','Account is a GL');

        IF NOT SIPKSS_SWEEP.fn_gl_avlbal (l_sitb_cont.dr_account
                            ,l_sitb_cont.dr_acc_br
                            ,l_sitb_cont.dr_acc_ccy
                            ,l_acc_bal
                            ,l_errcode)
        THEN

          debug.pr_debug ('SI','Failed in getting GL balance with '||l_errcode);
          p_errcode := l_errcode;
          return FALSE;

                END IF;
            END IF;

            BEGIN

                    IF l_sitb_cont.dr_acc_ccy <> p_cycle.si_ccy
                    THEN
                            l_rate:=null;
                            debug.pr_debug ('SI','Account Ccy and SI Ccy are different');
                            debug.pr_debug ('SI','SI CCY is '||p_cycle.si_ccy);
                            debug.pr_debug ('SI','Account Ccy is '||l_sitb_cont.dr_acc_ccy);
                            debug.pr_debug ('SI','out params are '||l_amt_pending||l_rate);

                            BEGIN
                                SELECT rate_type into l_ratetype
                                FROM sitms_product_prf
                                WHERE product_code=substr(p_cycle.contract_ref_no,4,4);
                            EXCEPTION
                                WHEN others THEN
                                    l_ratetype:='STANDARD';
                            END;

                            IF NOT CYPKSS.fn_amt1_to_amt2(pkg_branch
                                                ,p_cycle.si_ccy
                                                ,l_sitb_cont.dr_acc_ccy
                                                ,l_ratetype
                                                ,'M'
                                                ,p_cycle.si_amt_pending
                                                ,'Y'
                                                ,l_amt_pending
                                                ,l_rate
                                                ,l_errcode) THEN

                            debug.pr_debug ('SI','Ccy conversion Failed with '||l_errcode);
                            p_errcode := l_errcode;
                            return FALSE;
                            END IF;
                    ELSE
                        debug.pr_debug ('SI','Ccy conversion thru');
                            l_amt_pending := p_cycle.si_amt_pending;
                    END IF;

                END;

                    IF l_acc_bal >= l_amt_pending THEN
                        debug.pr_debug ('SI','Account Balance is greater than or equal to amt pending');
                SAVEPOINT try_suxs;

                IF NOT fn_process_suxs (l_new_esn
                                                            ,p_cycle
                                                            ,l_errtype
                                                            ,l_errcode
                                                            ,l_params) THEN

                    ROLLBACK TO try_suxs;

                    IF l_errtype = 'A'
                    THEN
						--3-12694450741 CHANGES STARTS HERE

						  IF l_errcode IS NULL			
						  THEN
							l_errcode := 'SI-PMT-88';
						  END if;

						  IF l_params IS NULL	
						  then
							l_params := SUBSTR(sqlerrm,1,50);
						  END if;

								IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																		  'SUXS',
																		  l_errcode,
																		  l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
								  debug.pr_debug('SI', 'Insert into Exception returns false');
								  return FALSE;
								END IF;
					 --3-12694450741 CHANGES ENDS HERE
                            debug.pr_debug ('SI','Fn process suxs returned Accounting error');
                            IF l_retry_count = 1
                            THEN
                                SAVEPOINT    try_rejt;
                                IF NOT fn_process_reject(l_new_esn
                                                                                ,p_cycle
                                                                                ,l_errtype
                                                                                ,l_errcode
                                                                                ,l_params) THEN
										ROLLBACK TO try_rejt; --3-12694450741 CHANGES					

                                        IF l_errtype = 'A'
                                        THEN
                                            debug.pr_debug ('SI','Fn process reject returned Accounting error');
												--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

														   --3-12694450741 CHANGES STARTS HERE

											  IF l_errcode IS NULL			
											  THEN
												l_errcode := 'SI-PMT-89';
											  END if;

											  IF l_params IS NULL	
											  then
												l_params := SUBSTR(sqlerrm,1,50);
											  END if;

											  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																						'REJT',
																						l_errcode,
																						l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
												debug.pr_debug('SI', 'Insert into Exception returns false');
												return FALSE;
											  END IF;
											  --3-12694450741 CHANGES ENDS HERE
                                            RAISE    process_next;

                                        ELSE
                                            debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                return FALSE;

                                        END IF;

                                END IF;

                                BEGIN

                                        UPDATE    sitbs_cycle_due_exec
                                        SET            si_exec_status = 'I'
                                                        ,si_suxs_date    = global.application_date
                                        WHERE    contract_ref_no = p_cycle.contract_ref_no
                                        AND        cycle_seq_no = p_cycle.cycle_seq_no
                                        AND        version_no = p_cycle.version_no;

                                EXCEPTION
                                        WHEN others THEN
                                        debug.pr_debug ('SI','Update of Due Exec with I failed');
                                        RETURN FALSE;

                                END;

                            ELSE

                                BEGIN

                                        UPDATE    sitbs_cycle_due_exec
                                        SET            si_exec_status = 'I'
                                                        ,si_suxs_date    = global.application_date
                                        WHERE    contract_ref_no = p_cycle.contract_ref_no
                                        AND        cycle_seq_no = p_cycle.cycle_seq_no
                                        AND        version_no = p_cycle.version_no;

                                EXCEPTION
                                        WHEN others THEN
                                        debug.pr_debug ('SI','Update of Due Exec with I failed');
                                        RETURN FALSE;

                                END;
                            END IF;

                    ELSE
                            debug.pr_debug ('SI','Fn process suxs returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                        RETURN FALSE;
                    END IF;
            END IF;
        ELSE
                            IF l_retry_count = 1
                            THEN
                                SAVEPOINT    try_rejt;
                                IF NOT fn_process_reject(l_new_esn
                                                                                ,p_cycle
                                                                                ,l_errtype
                                                                                ,l_errcode
                                                                                ,l_params) THEN
											ROLLBACK TO try_rejt; --3-12694450741 CHANGES
                                        IF l_errtype = 'A'
                                        THEN
                                            debug.pr_debug ('SI','Fn process reject returned Accounting error');
												--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

														   --3-12694450741 CHANGES STARTS HERE

											  IF l_errcode IS NULL			
											  THEN
												l_errcode := 'SI-PMT-75';
											  END if;

											  IF l_params IS NULL	
											  then
												l_params := SUBSTR(sqlerrm,1,50);
											  END if;

											  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																						'REJT',
																						l_errcode,
																						l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
												debug.pr_debug('SI', 'Insert into Exception returns false');
												return FALSE;
											  END IF;
											  --3-12694450741 CHANGES ENDS HERE
                                            RAISE    process_next;

                                        ELSE
                                            debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                return FALSE;

                                        END IF;

                                END IF;

                                BEGIN

                                        UPDATE    sitbs_cycle_due_exec
                                        SET            si_exec_status = 'I'
                                                    ,si_suxs_date    = global.application_date
                                        WHERE    contract_ref_no = p_cycle.contract_ref_no
                                        AND        cycle_seq_no = p_cycle.cycle_seq_no
                                        AND        version_no = p_cycle.version_no;

                                EXCEPTION
                                        WHEN others THEN
                                        debug.pr_debug ('SI','Update of Due Exec with I failed');
                                        RETURN FALSE;

                                END;

                            ELSE

                                BEGIN

                                        UPDATE    sitbs_cycle_due_exec
                                        SET            si_exec_status = 'I'
                                                    ,si_suxs_date    = global.application_date                                        WHERE    contract_ref_no = p_cycle.contract_ref_no
                                        AND        cycle_seq_no = p_cycle.cycle_seq_no
                                        AND        version_no = p_cycle.version_no;

                                EXCEPTION
                                        WHEN others THEN
                                        debug.pr_debug ('SI','Update of Due Exec with I failed');
                                        RETURN FALSE;

                                END;
                            END IF;
            END IF;


        ELSIF l_action_code = 'P' THEN

                debug.pr_debug ('SI','Action code is Partial Execution ');

                IF NOT SIPKSS_SWEEP.fn_acc_is_gl(l_sitb_cont.dr_account
                                                                            ,l_sitb_cont.dr_acc_br
                                                                            ,l_errcode) THEN

                    debug.pr_debug ('SI','Account is not a GL');

                BEGIN
/* USDFBME Upgrade 220999 - sanjeev */
      --     IF NOT sipkss_sweep.fn_GetAvlBal(l_sitb_cont.dr_acc_br
    -- USDFBME fromBlr getavlbal changed to getavlbal_with_od
/*      SIPKSS_SWEEP.fn_GetAvlBal changed to ACPKSS_MISC.fn_GetAvlBal
     for FINWARE INTERFACE CHANGES - 10/9/99 - STP1 */
--           IF NOT sipkss_sweep.fn_GetAvlBal_with_od(l_sitb_cont.dr_acc_br
/* End of Upgrade */

-- JORUBJFCC0452 Start
/*
           IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                            ,l_sitb_cont.dr_account
                                            ,l_acc_bal) THEN
          debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;
*/           
          IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                      ,l_sitb_cont.dr_account
                                      ,l_acc_bal
                                      ,'Y') THEN
            debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;
-- JORUBJFCC0452 End  
        
          

          END;

          ELSE

                debug.pr_debug ('SI','Account is GL');

        IF NOT SIPKSS_SWEEP.fn_gl_avlbal (l_sitb_cont.dr_account
                            ,l_sitb_cont.dr_acc_br
                            ,l_sitb_cont.dr_acc_ccy
                            ,l_acc_bal
                            ,l_errcode)
        THEN

          debug.pr_debug ('SI','Failed in getting GL balance with '||l_errcode);
          p_errcode := l_errcode;
          return FALSE;

                END IF;
            END IF;

            BEGIN

                    IF l_sitb_cont.dr_acc_ccy <> p_cycle.si_ccy
                    THEN
                        l_rate:=null;
                        debug.pr_debug ('SI','Account Ccy and SI ccy are different');
                            BEGIN
                                SELECT rate_type into l_ratetype
                                FROM sitms_product_prf
                                WHERE product_code=substr(p_cycle.contract_ref_no,4,4);
                            EXCEPTION
                                WHEN others THEN
                                    l_ratetype:='STANDARD';
                            END;

                            IF NOT CYPKSS.fn_amt1_to_amt2(pkg_branch
                                                ,p_cycle.si_ccy
                                                ,l_sitb_cont.dr_acc_ccy
                                                ,l_ratetype
                                                ,'M'
                                                ,p_cycle.si_amt_pending
                                                ,'Y'
                                                ,l_amt_pending
                                                ,l_rate
                                                ,l_errcode) THEN

                            debug.pr_debug ('SI','Could not convert SI amount to A/c ccy');
                            p_errcode := l_errcode;
                            return FALSE;
                            END IF;
                    ELSE
                            l_amt_pending := p_cycle.si_amt_pending;
                            debug.pr_debug ('SI','SI amount in A/c ccy is '|| l_amt_pending);
                    END IF;

            END;


            IF l_acc_bal > 0
            THEN
                    debug.pr_debug ('SI','Account Balance is greater than 0');

                    IF l_acc_bal >= l_amt_pending THEN
                            debug.pr_debug ('SI','Account Balance is greater than or equal to amt pending');

                        SAVEPOINT try_suxs;

                        IF NOT fn_process_suxs(l_new_esn
                                                                    ,p_cycle
                                                                    ,l_errtype
                                                                    ,l_errcode
                                                                    ,l_params) THEN

                                ROLLBACK TO try_suxs;
                                IF l_errtype = 'A'
                                THEN
								 --3-12694450741 CHANGES STARTS HERE

								  IF l_errcode IS NULL			
								  THEN
									l_errcode := 'SI-PMT-76';
								  END if;

								  IF l_params IS NULL	
								  then
									l_params := SUBSTR(sqlerrm,1,50);
								  END if;

										  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																					'SUXS',
																					l_errcode,
																					l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
											debug.pr_debug('SI', 'Insert into Exception returns false');
											return FALSE;
										  END IF;
										  --3-12694450741 CHANGES ENDS HERE
                        debug.pr_debug ('SI','Account Balance is Greater than equal to amt pending');
                                    debug.pr_debug ('SI','Fn process suxs returned Accounting error');

                                        SAVEPOINT try_pexc;

                                        IF NOT fn_process_pexc(l_new_esn
                                                                                        ,p_cycle
                                                                                        ,l_sitb_cont.dr_acc_ccy
                                                                                        ,l_errtype
                                                                                        ,l_errcode
                                                                                        ,l_params) THEN

                                        ROLLBACK TO try_pexc;
                                        IF l_errtype = 'A' THEN
													

												   --3-12694450741 CHANGES STARTS HERE

									  IF l_errcode IS NULL			
									  THEN
										l_errcode := 'SI-PMT-77';
									  END if;

									  IF l_params IS NULL	
									  then
										l_params := SUBSTR(sqlerrm,1,50);
									  END if;

												  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																							'PEXC',
																							l_errcode,
																							l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
													debug.pr_debug('SI',
																   'Insert into Exception returns false');
													return FALSE;
												  END IF;
												  --3-12694450741 CHANGES ENDS HERE
                                        debug.pr_debug ('SI','Fn process pexc returned Accounting error');
                                            IF l_retry_count = 1 THEN
                                                SAVEPOINT try_rejt;
                                                IF NOT fn_process_reject(l_new_esn
                                                                                                ,p_cycle
                                                                                                ,l_errtype
                                                                                                ,l_errcode
                                                                                                ,l_params) THEN
														ROLLBACK TO try_rejt; --3-12694450741 CHANGES					
                                                    IF l_errtype = 'A' THEN
                                                        debug.pr_debug ('SI','Fn process reject returned Accounting error');
															--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

															--3-12694450741 CHANGES STARTS HERE

															IF l_errcode IS NULL			
															THEN
															l_errcode := 'SI-PMT-78';
															END if;

															IF l_params IS NULL	
															then
															l_params := SUBSTR(sqlerrm,1,50);
															END if;

															IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																			'REJT',
																			l_errcode,
																			l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
															debug.pr_debug('SI', 'Insert into Exception returns false');
															return FALSE;
															END IF;
															--3-12694450741 CHANGES ENDS HERE
                                                            RAISE process_next;
                                                    ELSE
                                                        debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                            return FALSE;
                                                    END IF;
                                                END IF;
                                            END IF;
                                        ELSE
                                            debug.pr_debug ('SI','Fn process pexc returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                            return FALSE;
                                    END IF;
                                END IF;
                            ELSE
                                    debug.pr_debug ('SI','Fn process suxs returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                        return FALSE;

                                END IF;

                        END IF;

                ELSE

                        debug.pr_debug ('SI','Account Balance is Greater than 0 but less than amt pending');

                        SAVEPOINT try_pexc;

                        IF NOT fn_process_pexc(l_new_esn
                                                                    ,p_cycle
                                                                    ,l_sitb_cont.dr_acc_ccy
                                                                    ,l_errtype
                                                                    ,l_errcode
                                                                    ,l_params) THEN

                            ROLLBACK TO try_pexc;
                            IF l_errtype = 'A'
                            THEN
								--3-12694450741 CHANGES STARTS HERE

							  IF l_errcode IS NULL			
							  THEN
								l_errcode := 'SI-PMT-61';
							  END if;

							  IF l_params IS NULL	
							  then
								l_params := SUBSTR(sqlerrm,1,50);
							  END if;

									  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																				'PEXC',
																				l_errcode,
																				l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
										debug.pr_debug('SI', 'Insert into Exception returns false');
										return FALSE;
									  END IF;
									  --3-12694450741 CHANGES ENDS HERE
                                debug.pr_debug ('SI','Fn process pexc returned Accounting error');
                                    IF l_retry_count  = 1
                                    THEN
                                            SAVEPOINT try_rejt;
                                            IF NOT fn_process_reject (l_new_esn
                                                                                            ,p_cycle
                                                                                            ,l_errtype
                                                                                            ,l_errcode
                                                                                            ,l_params) THEN
																ROLLBACK TO try_rejt; --3-12694450741 CHANGES
                                                IF l_errtype = 'A'
                                                THEN
                                                    debug.pr_debug ('SI','Fn process reject returned Accounting error');
														--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

																   --3-12694450741 CHANGES STARTS HERE

													  IF l_errcode IS NULL			
													  THEN
														l_errcode := 'SI-PMT-62';
													  END if;

													  IF l_params IS NULL	
													  then
														l_params := SUBSTR(sqlerrm,1,50);
													  END if;

													  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																								'REJT',
																								l_errcode,
																								l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
														debug.pr_debug('SI', 'Insert into Exception returns false');
														return FALSE;
													  END IF;
													  --3-12694450741 CHANGES ENDS HERE
                                                    RAISE process_next;
                                                ELSE
                                                    debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                                    return FALSE;
                                                END IF;

                                            END IF;
                                    END IF;

                            ELSE
                                debug.pr_debug ('SI','Fn process pexc returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                    return FALSE;

                            END IF;

                    END IF;

                END IF;

        ELSE

                debug.pr_debug ('SI','Account Balance is less than 0');

                IF l_retry_count = 1
                THEN
							-- 3-12694450741 CHANGES STARTS HERE
						  l_errcode := 'SI-PMT-90;';
						  l_params  := l_sitb_cont.dr_account || '~' ||
									   l_sitb_cont.dr_acc_br || '~;';
						  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																	'SUXS',
																	l_errcode,
																	l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
							debug.pr_debug('SI', 'Insert into Exception returns false');
							return FALSE;
						  END IF;
						  -- 3-12694450741 CHANGES ENDS HERE
                        SAVEPOINT try_rejt;
                        IF NOT fn_process_reject (l_new_esn
                                                                        ,p_cycle
                                                                        ,l_errtype
                                                                        ,l_errcode
                                                                        ,l_params) THEN
											ROLLBACK TO try_rejt; -- 3-12694450741 CHANGES				
                                IF l_errtype = 'A'
                                THEN
                                    debug.pr_debug ('SI','Fn process reject returned Accounting error');
										--ROLLBACK TO try_rejt;    --3-12694450741 CHANGES

										--3-12694450741 CHANGES STARTS HERE

										  IF l_errcode IS NULL			
										  THEN
											l_errcode := 'SI-PMT-63';
										  END if;

										  IF l_params IS NULL	
										  then
											l_params := SUBSTR(sqlerrm,1,50);
										  END if;

										  IF NOT SIPKSS_COLLECTION.fn_ins_exception(p_cycle.contract_ref_no,
																					'REJT',
																					l_errcode,
																					l_params||'~'||'Acc Bal: ' || l_acc_bal) THEN
											debug.pr_debug('SI', 'Insert into Exception returns false');
											return FALSE;
										  END IF;
										  --3-12694450741 CHANGES ENDS HERE
                                        RAISE process_next;
                                ELSE
                                    debug.pr_debug ('SI','Fn process reject returned Non Accounting error');
                                                p_errcode:=l_errcode;
                                                p_params:=l_params;
                                        return FALSE;
                                END IF;

                        END IF;
                END IF;

    END IF;

END IF;
                            debug.pr_debug ('SI','Should be Checking for retry exceeded');

        debug.pr_debug ('SI','Updating exec advice ');

        BEGIN
            INSERT INTO SITBS_DLY_MSG_OUT
            (INSTRUCTION_NO,
            CYCLE_SEQ_NO)
            VALUES
            (p_cycle.instruction_no,
            p_cycle.cycle_seq_no);

            IF NOT SIPKSS_ACCADV.fn_si_advices (pkg_branch,
                                    pkg_user,
                                    pkg_lcy,
                                    global.application_date,
                                    p_cycle.contract_ref_no,
                                    nvl(g_eventcode,'REJT'),
                                    p_cycle.cycle_seq_no,
                                    l_errcode,
                                    l_params) THEN

                    p_errcode := l_errcode;
                    p_params := l_params;

                debug.pr_debug ('SI','Advices failed with '||l_errcode);

                    return FALSE;
            END IF;
        EXCEPTION
            WHEN DUP_VAL_ON_INDEX THEN
            NULL;
            WHEN OTHERS THEN
            RETURN FALSE;
        END;

/*

    BEGIN
        l_advupd:=0;
        SELECT count(*) into l_advupd
        FROM mstbs_dly_msg_out
        WHERE reference_no=p_cycle.instruction_no
        AND esn=p_cycle.cycle_seq_no
        AND from_date=global.application_date
        AND msg_status='N'
        AND msg_type='EXEC_ADVICE';
    EXCEPTION WHEN others THEN
        debug.pr_debug ('SI','Error in selecting from dly_msg_out'||sqlerrm);
        Return FALSE;
    END;
        debug.pr_debug ('SI','Advice updated is  '||l_advupd);
                    IF l_advupd=0 THEN
                        IF NOT SIPKSS_ACCADV.fn_si_advices (global.current_branch,
                                                                                                global.user_id,
                                                                                                global.lcy,
                                                                                                global.application_date,
                                                                                                p_cycle.contract_ref_no,
                                                                                                nvl(g_eventcode,'REJT'),
                                                                                                p_cycle.cycle_seq_no,
                                                                                                l_errcode,
                                                                                                l_params) THEN


                                debug.pr_debug ('SI','Advices failed with '||l_errcode);
                        END IF;
                    END IF;

*/                            debug.pr_debug ('SI','Should be Checking for retry exceeded');
                            debug.pr_debug ('SI','Max retry is'||l_max_retry);
			-- 3-12694450741 Changes starts
    		--IF l_max_retry = nvl(p_cycle.retry_count, 0) + 1 THEN
		IF nvl(p_cycle.retry_count,0)+1 >= l_max_retry Then
		   -- 3-12694450741 Changes ends
                            debug.pr_debug ('SI','Retry is exceeded');
                        BEGIN

                            UPDATE sitbs_cycle_due_exec
                            SET         si_exec_status = 'I'
                                    ,si_suxs_date = global.application_date
                            WHERE contract_ref_no = p_cycle.contract_ref_no
                            AND        cycle_seq_no = p_cycle.cycle_seq_no
                            AND     si_exec_status = 'R';

                    EXCEPTION
                            WHEN others THEN
                            debug.pr_debug ('SI','Failed in retry exceeded update of Due Exec in reject with'||sqlerrm);
                            return FALSE;

                    END;
    END IF;


return TRUE;


EXCEPTION
    WHEN process_next THEN
        p_errcode:=l_errcode;
        p_params:=l_params;
			--3-12694450741 starts
      debug.pr_debug('SI', 'In process_next EXCEPTION');
      debug.pr_debug('SI',
                     'In process_next EXCEPTION-p_cycle.contract_ref_no' ||
                     p_cycle.contract_ref_no);
      debug.pr_debug('SI',
                     'In process_next EXCEPTION-p_cycle.cycle_seq_no' ||
                     p_cycle.cycle_seq_no);
      debug.pr_debug('SI',
                     'In process_next EXCEPTION -l_max_retry' ||
                     l_max_retry);
      debug.pr_debug('SI',
                     'In process_next EXCEPTION-p_cycle.retry_count' ||
                     p_cycle.retry_count);
      BEGIN

        UPDATE sitbs_cycle_due_exec
           SET si_exec_status = 'R', si_suxs_date = null
         WHERE contract_ref_no = p_cycle.contract_ref_no
           AND cycle_seq_no = p_cycle.cycle_seq_no;

      EXCEPTION
        WHEN others THEN
          NULL;

      END;
      debug.pr_debug('SI',
                     'In process_next EXCEPTION -l_max_retry' ||
                     l_max_retry);
      debug.pr_debug('SI',
                     'In process_next EXCEPTION-p_cycle.retry_count' ||
                     p_cycle.retry_count);

      IF nvl(p_cycle.retry_count, 0) + 1 >= l_max_retry Then
        debug.pr_debug('SI',
                       'Retry is exceeded and updating in process_next exception');
        BEGIN

          UPDATE sitbs_cycle_due_exec
             SET si_exec_status = 'I',
                 si_suxs_date   = global.application_date
           WHERE contract_ref_no = p_cycle.contract_ref_no
             AND cycle_seq_no = p_cycle.cycle_seq_no
             and si_exec_status in ('U', 'R');
          --    AND   si_exec_status = 'R';

        EXCEPTION
          WHEN others THEN
            debug.pr_debug('SI',
                           'exception of process_next event updation' ||
                           sqlerrm);
            NULL;

        END;
      END IF;
      -- 3-12694450741 ends
    Return FALSE;

    WHEN others THEN
    debug.pr_debug ('SI','Motherhood of Process payments with '||sqlerrm);
				--3-12694450741 CHANGES STARTS HERE
			  p_errcode := l_errcode;
			  p_params  := l_params;

			  IF p_errcode IS NULL THEN
				p_errcode := 'ED-10105';
			  END IF;

			  --3-12694450741 CHANGES ENDS HERE
    return FALSE;

END fn_process_payments;

FUNCTION fn_process_suxs(p_esn    IN        CSTBS_CONTRACT.latest_event_seq_no%TYPE
                                                ,p_cycle    IN    SITBS_CYCLE_DUE_EXEC%ROWTYPE
                                                ,p_errtype OUT    CHAR
                                                ,p_errcode    OUT    ERTBS_MSGS.err_code%TYPE
                                                ,p_params        OUT    VARCHAR2)return BOOLEAN

IS

prm_sett_tags    VARCHAR2(2000) := '';
prm_sett_ccys    VARCHAR2(2000) := '';
prm_iccf_tags    VARCHAR2(2000) := '';
prm_iccf_ccys    VARCHAR2(2000) := '';
prm_tax_tags    VARCHAR2(2000) := '';
prm_tax_ccys    VARCHAR2(2000) := '';

l_auth_status            CSTBS_CONTRACT_EVENT_LOG.auth_status%TYPE;
l_checker_id        CSTBS_CONTRACT_EVENT_LOG.maker_id%TYPE;
l_checker_dt_stamp      CSTBS_CONTRACT_EVENT_LOG.checker_dt_stamp%TYPE;
l_contract_status        CSTBS_CONTRACT_EVENT_LOG.contract_status%TYPE;
l_event_code        CSTBS_CONTRACT.curr_event_code%TYPE;
l_errcode            ERTBS_MSGS.err_code%TYPE;
l_params            VARCHAR2(2000) := '';
l_cycle_seq_no        SITBS_CYCLE_DUE_EXEC.CYCLE_SEQ_NO%type ;--volpert

BEGIN

        debug.pr_debug ('SI','In fn process suxs');

            --credit function interface for ADB starts keeps the events unauthorized

             IF    global.X9$ ='BEFADB'
               THEN

                     l_checker_id :=NULL;
                    l_checker_dt_stamp :=NULL;
                    l_auth_status :='U';
            ELSE
                         l_checker_id    :='SYSTEM';
                    l_checker_dt_stamp:=sipkss_batch.fn_get_date_time(global.application_date);
                    l_auth_status :='A';
            END IF;

            BEGIN
                SELECT    contract_status
                INTO        l_contract_status
                FROM         cstbs_contract
                WHERE     contract_ref_no = p_cycle.contract_ref_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select of contract status from cstb contract');
                p_errtype := 'G';
                return FALSE;
            END;

                BEGIN

                        UPDATE    cstbs_contract
                        SET        latest_event_seq_no = p_esn,
                                    curr_event_code = 'SUXS',
                                    latest_event_date = global.application_date
                        WHERE    contract_ref_no = p_cycle.contract_ref_no;

                EXCEPTION
                    WHEN others THEN
                    debug.pr_debug ('SI','Failed in Update of Latest esn of cstbs contract');
                p_errtype := 'G';
                    return false;
                END;

            BEGIN

                debug.pr_debug ('SI','ESN before update of cycle det is '||p_esn);
                debug.pr_debug ('SI','Ref no for update of cycle det is '||p_cycle.contract_ref_no);
                debug.pr_debug ('SI','CSN for update of cycle det is '||p_cycle.cycle_seq_no);
                debug.pr_debug ('SI','RSN for update of cycle det is '||p_cycle.retry_count);

                UPDATE    sitbs_cycle_detail
                SET            event_seq_no = p_esn,
                                event_code = 'SUXS'
                WHERE        contract_ref_no = p_cycle.contract_ref_no
                AND            cycle_seq_no = p_cycle.cycle_seq_no
                AND         retry_seq_no = nvl(p_cycle.retry_count,0)+1;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in Update of Cycle Detail');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN

                debug.pr_debug ('SI','Before insert into cstbs event log');

                INSERT INTO cstbs_contract_event_log
                (MODULE
                ,CONTRACT_REF_NO
                ,MAKER_ID
                ,MAKER_DT_STAMP
                ,CHECKER_ID
                ,CHECKER_DT_STAMP
                ,EVENT_SEQ_NO
                ,EVENT_DATE
                ,EVENT_CODE
                ,CONTRACT_STATUS
                ,AUTH_STATUS
                ,NEW_VERSION_INDICATOR
                ,REVERSED_EVENT_SEQ_NO)

                VALUES

                ('SI'
                ,p_cycle.contract_ref_no
                ,'SYSTEM'
                ,sipkss_batch.fn_get_date_time(global.application_date)
                ,l_checker_id
                ,l_checker_dt_stamp
                ,p_esn
                ,global.application_date
                ,'SUXS'
                ,l_contract_status
                ,l_auth_status
                ,'N'
                ,'');

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in Insert into event Log');
                p_errtype := 'G';
                return FALSE;
            END;

                    SIPKSS_SUBSYSTEM.pr_reset_waive;

                    prm_iccf_tags := '';
          prm_iccf_ccys:= '';
          prm_tax_tags:= '';
          prm_tax_ccys:= '';
          prm_sett_tags:= '';
          prm_sett_ccys:= '';

                    IF p_cycle.apply_chg_suxs = 'N'
                    THEN
                            debug.pr_debug ('SI','Apply chg on suxs is No');
                            SIPKSS_SUBSYSTEM.pr_waive_suxs;
                    END IF;

                    SIPKSS_SUBSYSTEM.pr_no_iccf;
                    --SIPKSS_SUBSYSTEM.pr_yes_settle;

                        debug.pr_debug ('SI','Before Settlement Pickup');
                        debug.pr_debug ('SI','Version no is '||p_cycle.version_no);
                        debug.pr_debug ('SI','ESN is '||p_esn );

                    IF NOT SIPKSS_SUBSYSTEM.fn_settlement_pickup(
                                                p_cycle.contract_ref_no
                                                ,p_cycle.version_no
                                                ,p_esn
                                                ,pkg_branch
                                                ,'SUXS'
                                                ,global.application_date
                                                ,pkg_lcy
                                                ,prm_iccf_tags
                                                ,prm_iccf_ccys
                                                ,prm_tax_tags
                                                ,prm_tax_ccys
                                                ,prm_sett_tags
                                                ,prm_sett_ccys
                                                ,l_errcode) THEN

                            debug.pr_debug ('SI','Failed in settlement pickup');
                            debug.pr_debug ('SI','Errcode is '||l_errcode);

                            p_errcode := l_errcode;
                            p_errtype := 'G';
                            return FALSE;

                            ELSE

                            debug.pr_debug ('SI','settlement pickup success');
                            debug.pr_debug ('SI','ICCF Tags are '||prm_iccf_tags);
                            debug.pr_debug ('SI','TAX Tags are '||prm_tax_tags);
                            debug.pr_debug ('SI','SETTLE Tags are '||prm_sett_tags);
                            debug.pr_debug ('SI','ICCF Ccys are '||prm_iccf_ccys);
                            debug.pr_debug ('SI','TAX Ccys are '||prm_tax_ccys);
                            debug.pr_debug ('SI','SETTLE Ccys are '||prm_sett_ccys);


                            END IF;

                            l_event_code := 'SUXS';

                            debug.pr_debug ('SI','Before Fn si Entries');
                            debug.pr_debug ('SI','Cycle Sequence No. is '||p_cycle.cycle_seq_no);
                            l_cycle_seq_no := p_cycle.cycle_seq_no ;--volpert
                            IF NOT SIPKSS_ACCADV.fn_si_entries (p_cycle.contract_ref_no
                                                    ,pkg_branch
                                                    ,pkg_user
                                                    ,pkg_lcy
                                                    ,global.application_date
                                                    ,l_event_code
                                                    ,l_cycle_seq_no
                                                    ,'N'
                                                    ,'B'
                                                    ,l_errcode
                                                    ,l_params) THEN

                                debug.pr_debug ('SI','Failed in Accounting entries');
                                debug.pr_debug ('SI','Errcode is '||l_errcode||'-'||l_params);

                                p_errcode := l_errcode;
                                p_params  := l_params;

                                IF l_errcode is NOT NULL AND substr(l_errcode,1,2) = 'AC'
                                THEN
                                        p_errtype := 'A';
                                ELSE
                                        p_errtype := 'G';
                                END IF;

                                return FALSE;


                            END IF;


                            debug.pr_debug ('SI','Accounting Thru');


                        BEGIN

                            UPDATE sitbs_cycle_due_exec
                            SET         si_exec_status = 'S'
                                        ,si_suxs_date = global.application_date
                                        ,si_amt_pending = 0
                            WHERE contract_ref_no = p_cycle.contract_ref_no
                            AND        cycle_seq_no = p_cycle.cycle_seq_no;

                    EXCEPTION
                            WHEN others THEN
                            p_errtype := 'G';
                            debug.pr_debug ('SI','Failed in update of Due Exec In suxs ');
                            return FALSE;

                    END;
        g_eventcode:='SUXS';
        return TRUE;


EXCEPTION
        WHEN others THEN
        debug.pr_debug ('SI','MotherHood exc. of process suxs');
        p_errtype := 'G';
        return FALSE;

END fn_process_suxs;


FUNCTION fn_process_reject(p_esn    IN        CSTBS_CONTRACT.latest_event_seq_no%TYPE
                                                ,p_cycle    IN    SITBS_CYCLE_DUE_EXEC%ROWTYPE
                                                ,p_errtype OUT    CHAR
                                                ,p_errcode    OUT    ERTBS_MSGS.err_code%TYPE
                                                ,p_params        OUT    VARCHAR2)return BOOLEAN

IS

prm_sett_tags    VARCHAR2(2000) := '';
prm_sett_ccys    VARCHAR2(2000) := '';
prm_iccf_tags    VARCHAR2(2000) := '';
prm_iccf_ccys    VARCHAR2(2000) := '';
prm_tax_tags    VARCHAR2(2000) := '';
prm_tax_ccys    VARCHAR2(2000) := '';

l_auth_status        CSTBS_CONTRACT_EVENT_LOG.auth_status%TYPE;
l_checker_id    CSTBS_CONTRACT_EVENT_LOG.maker_id%TYPE;
l_checker_dt_stamp CSTBS_CONTRACT_EVENT_LOG.checker_dt_stamp%TYPE;
l_contract_status    CSTBS_CONTRACT_EVENT_LOG.contract_status%TYPE;
l_event_code    CSTBS_CONTRACT.curr_event_code%TYPE;
l_errcode        ERTBS_MSGS.err_code%TYPE;
l_params        VARCHAR2(2000) := '';
l_max_retry        NUMBER:=0;
l_cycle_seq_no    SITBS_CYCLE_DUE_EXEC.CYCLE_SEQ_NO%type ;--volpert

BEGIN

            debug.pr_debug ('SI','In fn process reject ');

            --credit function interface for ADB starts keeps the events unauthorized

              IF    global.X9$ ='BEFADB'
                THEN
                    l_auth_status := 'U';
                     l_checker_id :=NULL;
                    l_checker_dt_stamp :=NULL;

            ELSE
                    l_auth_status        :='A';
                          l_checker_id    :='SYSTEM';
                      l_checker_dt_stamp:=        sipkss_batch.fn_get_date_time(global.application_date);
            END IF;

            BEGIN
                SELECT    max_retry_count
                INTO        l_max_retry
                FROM sitbs_contract_master
                WHERE     contract_ref_no = p_cycle.contract_ref_no
                AND version_no = p_cycle.version_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select of max retry count ');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN
                SELECT    contract_status
                INTO        l_contract_status
                FROM cstbs_contract
                WHERE     contract_ref_no = p_cycle.contract_ref_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select of contract status from cstb contract');
                p_errtype := 'G';
                return FALSE;
            END;

                BEGIN

                        UPDATE    cstbs_contract
                        SET        latest_event_seq_no = p_esn,
                                    curr_event_code = 'REJT',
                                    latest_event_date = global.application_date
                        WHERE    contract_ref_no = p_cycle.contract_ref_no;

                EXCEPTION
                    WHEN others THEN
                    debug.pr_debug ('SI','Failed in Update of Latest esn of cstbs contract');
                p_errtype := 'G';
                    return false;
                END;

            BEGIN

                debug.pr_debug ('SI','ESN before update of cycle det is '||p_esn);
                debug.pr_debug ('SI','Ref no for update of cycle det is '||p_cycle.contract_ref_no);
                debug.pr_debug ('SI','CSN for update of cycle det is '||p_cycle.cycle_seq_no);
                debug.pr_debug ('SI','RSN for update of cycle det is '||p_cycle.retry_count);

                UPDATE    sitbs_cycle_detail
                SET            event_seq_no = p_esn,
                                event_code = 'REJT'
                WHERE        contract_ref_no = p_cycle.contract_ref_no
                AND            cycle_seq_no = p_cycle.cycle_seq_no
                AND         retry_seq_no = nvl(p_cycle.retry_count,0)+1;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in Update of Cycle Detail');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN

                debug.pr_debug ('SI','Before insert into cstbs event log');

                INSERT INTO cstbs_contract_event_log
                (MODULE
                ,CONTRACT_REF_NO
                ,MAKER_ID
                ,MAKER_DT_STAMP
                ,CHECKER_ID
                ,CHECKER_DT_STAMP
                ,EVENT_SEQ_NO
                ,EVENT_DATE
                ,EVENT_CODE
                ,CONTRACT_STATUS
                ,AUTH_STATUS
                ,NEW_VERSION_INDICATOR
                ,REVERSED_EVENT_SEQ_NO)

                VALUES

                ('SI'
                ,p_cycle.contract_ref_no
                ,'SYSTEM'
                ,sipkss_batch.fn_get_date_time(global.application_date)
                ,l_checker_id
                ,l_checker_dt_stamp
                ,p_esn
                ,global.application_date
                ,'REJT'
                ,l_contract_status
                ,l_auth_status
                ,'N'
                ,'');

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in Insert into event Log');
                p_errtype := 'G';
                return FALSE;
            END;

                    SIPKSS_SUBSYSTEM.pr_reset_waive;

                    prm_iccf_tags := '';
          prm_iccf_ccys:= '';
          prm_tax_tags:= '';
          prm_tax_ccys:= '';
          prm_sett_tags:= '';
          prm_sett_ccys:= '';

                    IF p_cycle.apply_chg_rejt = 'N'
                    THEN
                            debug.pr_debug ('SI','Apply chg on rejt is No');
                            SIPKSS_SUBSYSTEM.pr_waive_rejt;
                    END IF;

                    SIPKSS_SUBSYSTEM.pr_no_iccf;
                    --SIPKSS_SUBSYSTEM.pr_yes_settle;

                        debug.pr_debug ('SI','Before Settlement Pickup');
                        debug.pr_debug ('SI','Version no is '||p_cycle.version_no);
                        debug.pr_debug ('SI','ESN is '||p_esn);

                    IF NOT SIPKSS_SUBSYSTEM.fn_settlement_pickup(
                                                p_cycle.contract_ref_no
                                                ,p_cycle.version_no
                                                ,p_esn
                                                ,pkg_branch                                                ,'REJT'
                                                ,global.application_date
                                                ,pkg_lcy
                                                ,prm_iccf_tags
                                                ,prm_iccf_ccys
                                                ,prm_tax_tags
                                                ,prm_tax_ccys
                                                ,prm_sett_tags
                                                ,prm_sett_ccys
                                                ,l_errcode) THEN

                            debug.pr_debug ('SI','Failed in settlement pickup');
                            debug.pr_debug ('SI','Errcode is '||l_errcode);

                            p_errcode := l_errcode;
                            p_errtype := 'G';
                            return FALSE;

                            ELSE

                            debug.pr_debug ('SI','settlement pickup success');
                            debug.pr_debug ('SI','ICCF Tags are '||prm_iccf_tags);
                            debug.pr_debug ('SI','TAX Tags are '||prm_tax_tags);
                            debug.pr_debug ('SI','SETTLE Tags are '||prm_sett_tags);
                            debug.pr_debug ('SI','ICCF Ccys are '||prm_iccf_ccys);
                            debug.pr_debug ('SI','TAX Ccys are '||prm_tax_ccys);
                            debug.pr_debug ('SI','SETTLE Ccys are '||prm_sett_ccys);


                            END IF;

                            l_event_code := 'REJT';

                            debug.pr_debug ('SI','Before Fn si Entries');
                            debug.pr_debug ('SI','Cycle Sequence No. is '||p_cycle.cycle_seq_no);
                            l_cycle_seq_no :=p_cycle.cycle_seq_no ;--volpert
                            IF NOT SIPKSS_ACCADV.fn_si_entries (p_cycle.contract_ref_no
                                                    ,pkg_branch
                                                    ,pkg_user
                                                    ,pkg_lcy
                                                    ,global.application_date
                                                    ,l_event_code
                                                    ,l_cycle_seq_no
                                                    ,'N'
                                                    ,'B'
                                                    ,l_errcode
                                                    ,l_params) THEN

                                debug.pr_debug ('SI','Failed in Accounting entries');
                                debug.pr_debug ('SI','Errcode is '||l_errcode||'-'||l_params);

                                p_errcode := l_errcode;
                                p_params  := l_params;

                                IF l_errcode is NOT NULL AND substr(l_errcode,1,2) = 'AC'
                                THEN
                                        p_errtype := 'A';
                                ELSE
                                        p_errtype := 'G';
                                END IF;

                                return FALSE;


                            END IF;


                            debug.pr_debug ('SI','Accounting Thru');


                        BEGIN

                            UPDATE sitbs_cycle_due_exec
                            SET         si_exec_status = 'R'
                                        ,si_suxs_date = null
                            WHERE contract_ref_no = p_cycle.contract_ref_no
                            AND        cycle_seq_no = p_cycle.cycle_seq_no;

                    EXCEPTION
                            WHEN others THEN
                            p_errtype := 'G';
                            debug.pr_debug ('SI','Failed in update of Due Exec in reject with'||sqlerrm);
                            return FALSE;

                    END;
                            debug.pr_debug ('SI','Checking for retry exceeded');

                            debug.pr_debug ('SI','Max retry is'||l_max_retry);
		-- 3-12694450741 Changes starts					
   -- IF l_max_retry= nvl(p_cycle.retry_count,0)+1 THEN -- Code commented
		IF nvl(p_cycle.retry_count,0)+1 >= l_max_retry Then
	   -- 3-12694450741 Changes Ends
                            debug.pr_debug ('SI','Retry is exceeded');
                        BEGIN

                            UPDATE sitbs_cycle_due_exec
                            SET         si_exec_status = 'I'
                                        ,si_suxs_date = global.application_date
                            WHERE contract_ref_no = p_cycle.contract_ref_no
                            AND        cycle_seq_no = p_cycle.cycle_seq_no;

                    EXCEPTION
                            WHEN others THEN
                            p_errtype := 'G';
                            debug.pr_debug ('SI','Failed in retry exceeded update of Due Exec in reject with'||sqlerrm);
                            return FALSE;

                    END;
    END IF;

        g_eventcode:='REJT';
        return TRUE;


EXCEPTION
        WHEN others THEN
        debug.pr_debug ('SI','MotherHood exc. of process reject with '||sqlerrm);
        p_errtype := 'G';
        return FALSE;

END fn_process_reject;
-------------------------------------------------------------------------------------------------------------------
--FCC5.3 Changes starts
FUNCTION Fn_limit_check
            (p_cycle        IN        SITBS_CYCLE_DUE_EXEC%ROWTYPE
            ,p_limit_check        IN OUT    varchar2
            ,p_errcode            OUT        ERTBS_MSGS.err_code%TYPE
            ,p_params            OUT        VARCHAR2)
return BOOLEAN
IS
l_errcode        ERTBS_MSGS.err_code%TYPE;
l_params        VARCHAR2(2000) := '';
l_sitb_cont        SITBS_CONTRACT_MASTER%ROWTYPE;
l_acc_bal        SITBS_CONTRACT_MASTER.si_amt%TYPE := 0;
l_amt_pending    SITBS_CYCLE_DUE_EXEC.si_amt_pending%TYPE := 0;
l_rate        CYTMS_RATES.mid_rate%TYPE := 0;
l_ratetype        SITBS_INSTRUCTION.rate_type%TYPE;

BEGIN
    debug.pr_debug('SI','Start of SI Fn_limit_account');
    --FCC53ITR1 SFR# 100 Changes starts
    IF NVL(p_cycle.si_amount,0)= 0 THEN
        l_errcode:='SI-PA0001';
        l_params:=p_cycle.contract_ref_no;
        return false;
    END IF;
    --FCC53ITR1 SFR# 100 Changes ends
    BEGIN
        SELECT *
        INTO     l_sitb_cont
        FROM     sitbs_contract_master
        WHERE     contract_ref_no = p_cycle.contract_ref_no
        AND         version_no = p_cycle.version_no;
    EXCEPTION
        WHEN others THEN
        debug.pr_debug ('SI','Failed in select from Contract Master');
        return FALSE;
    END;
    debug.pr_debug ('SI','after sitbs_contract_master select ');
    debug.pr_debug ('SI','Ref No. is '||p_cycle.contract_ref_no);
    debug.pr_debug ('SI','Dr acc is '||l_sitb_cont.dr_account);
    debug.pr_debug ('SI','Dr acc br is '||l_sitb_cont.dr_acc_br);
    debug.pr_debug ('SI','Dr acc ccy is '||l_sitb_cont.dr_acc_ccy);
    debug.pr_debug ('SI','Cr acc ccy is '||l_sitb_cont.cr_acc_ccy);
    debug.pr_debug ('SI','Cr acc br is '||l_sitb_cont.cr_acc_br);

    IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                   ,l_sitb_cont.dr_account
                                   ,l_acc_bal
                         ,'Y'
                                     --22-JUN-02004 BNDBAD#549 Starts
                         --,'Y'
                                     --22-JUN-02004 BNDBAD#549 ends
                                     ) THEN
        debug.pr_debug ('SI','Limit amount '||l_acc_bal);
        p_errcode := 'SI-SW0001;';
        p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
        return FALSE;
    END IF;

    debug.pr_debug ('SI','after getavlbal limit balance  '||l_acc_bal);
    debug.pr_debug ('SI','after p_cycle.si_ccy  '||p_cycle.si_ccy);
    debug.pr_debug ('SI','after l_sitb_cont.dr_acc_ccy  '||l_sitb_cont.dr_acc_ccy);

    IF l_sitb_cont.dr_acc_ccy <> p_cycle.si_ccy
    THEN
        l_rate:=null;
        debug.pr_debug ('SI','Account Ccy and SI Ccy are different');
        debug.pr_debug ('SI','SI CCY is '||p_cycle.si_ccy);
        debug.pr_debug ('SI','Account Ccy is '||l_sitb_cont.dr_acc_ccy);
        debug.pr_debug ('SI','out params are '||l_amt_pending||l_rate);
        BEGIN
            SELECT rate_type into l_ratetype
            FROM sitms_product_prf
            WHERE product_code=substr(p_cycle.contract_ref_no,4,4);
        EXCEPTION
            WHEN others THEN
            l_ratetype:='STANDARD';
        END;
            debug.pr_debug ('SI','after select of rate type  '||l_ratetype);

            IF NOT CYPKSS.fn_amt1_to_amt2(pkg_branch
                                ,p_cycle.si_ccy
                                ,l_sitb_cont.dr_acc_ccy
                                ,l_ratetype
                                ,'M'
                                ,p_cycle.si_amt_pending
                                ,'Y'
                                ,l_amt_pending
                                ,l_rate
                                ,l_errcode) THEN
                debug.pr_debug ('SI','Ccy conversion Failed with '||l_errcode);
                p_errcode := l_errcode;
                return FALSE;
            END IF;
    ELSE
        debug.pr_debug ('SI','limit Ccy conversion thru');
        l_amt_pending := p_cycle.si_amt_pending;
    END IF;
        debug.pr_debug ('SI','after the amount conversion l_amt_pending  '||l_amt_pending );
    IF l_acc_bal < l_amt_pending
    THEN
        p_limit_check := 'TRUE';
        insert into STTBS_REFERRAL_QUEUE
                (BRANCH_CODE,
                MODULE_CODE,
                --cust_no,
                CUST_AC_NO,
                POSTING_DATE,
                CONTRACT_REF_NO,
                version_no,
                event_seq_no,
                REFERRAL_DESCRIPTION,
                DR_AMOUNT,
                ENTRIES_STATUS,
                pay_flg,
                unpay_flg,
                Waive_charges,
                processed_flg,
                REASON)
        Values
                (l_sitb_cont.dr_acc_br,
                'SI',
                --i.cust_no,
                l_sitb_cont.dr_account,
                sipkss_batch.fn_get_date_time(p_cycle.si_value_date), --Processing_date,
                p_cycle.contract_ref_no,
                p_cycle.version_no,
                '', --each_row.LATEST_EVENT_SEQ_NO,
                '', --REFERRAL_DESCRIPTION,
                p_cycle.si_amt_pending,
                'N',--each_row.ENTRIES_STATUS,
                'N', --pay_flg,
                'N', --unpay_flg,
                'N', --Waive_charges, --FCC53ITR1 SFR# 87
                'N',
                '' --REASON
                );
    else
        p_limit_check := 'FALSE';
    end if;
    RETURN TRUE;

EXCEPTION WHEN OTHERS THEN
    p_errcode := 'FT-AU-0010'; --Limit check failed to get limit amount
    RETURN FALSE;
END Fn_limit_check;

--FCC5.3 Changes ends
-------------------------------------------------------------------------------------------------------------------


FUNCTION fn_process_pexc(p_esn    IN        CSTBS_CONTRACT.latest_event_seq_no%TYPE
                                                ,p_cycle    IN    SITBS_CYCLE_DUE_EXEC%ROWTYPE
                                                ,p_acc_ccy IN    SITBS_CONTRACT_MASTER.si_amt_ccy%TYPE
                                                ,p_errtype OUT    CHAR
                                                ,p_errcode    OUT    ERTBS_MSGS.err_code%TYPE
                                                ,p_params        OUT    VARCHAR2)return BOOLEAN

IS

prm_sett_tags    VARCHAR2(2000) := '';
prm_sett_ccys    VARCHAR2(2000) := '';
prm_iccf_tags    VARCHAR2(2000) := '';
prm_iccf_ccys    VARCHAR2(2000) := '';
prm_tax_tags    VARCHAR2(2000) := '';
prm_tax_ccys    VARCHAR2(2000) := '';

l_auth_status        CSTBS_CONTRACT_EVENT_LOG.auth_status%TYPE;
l_checker_id        CSTBS_CONTRACT_EVENT_LOG.maker_id%TYPE;
l_checker_dt_stamp CSTBS_CONTRACT_EVENT_LOG.checker_dt_stamp%TYPE;
l_contract_status    CSTBS_CONTRACT_EVENT_LOG.contract_status%TYPE;
l_event_code    CSTBS_CONTRACT.curr_event_code%TYPE;
l_partial_amt SITBS_CYCLE_DUE_EXEC.si_amt_pending%TYPE;
l_amt_pending SITBS_CYCLE_DUE_EXEC.si_amt_pending%TYPE;
l_acc_bal            SITBS_CYCLE_DUE_EXEC.si_amt_pending%TYPE;
l_avl_bal            SITBS_CYCLE_DUE_EXEC.si_amt_pending%TYPE;
l_sitb_cont     SITBS_CONTRACT_MASTER%ROWTYPE;
l_rate                CYTMS_RATES.mid_rate%TYPE;
l_errcode            ERTBS_MSGS.err_code%TYPE;
l_params            VARCHAR2(2000) := '';
l_max_retry        NUMBER;
l_ratetype         SITBS_INSTRUCTION.rate_type%TYPE;
l_cycle_seq_no    SITBS_CYCLE_DUE_EXEC.CYCLE_SEQ_NO%type ;--volpert

BEGIN

            debug.pr_debug ('SI','In fn process pexc');

            -- credit function interface enhancement for ADB starts

            IF    global.X9$ ='BEFADB'
                 THEN
                    l_auth_status :='U';
                     l_checker_id :=NULL;
                    l_checker_dt_stamp :=NULL;
            ELSE
                       l_auth_status :='A';
                          l_checker_id    :='SYSTEM';
                     l_checker_dt_stamp:=        sipkss_batch.fn_get_date_time(global.application_date);
            END IF;
            -- ends
            BEGIN
                SELECT    max_retry_count
                INTO        l_max_retry
                FROM sitbs_contract_master
                WHERE     contract_ref_no = p_cycle.contract_ref_no
                AND version_no = p_cycle.version_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select of max retry count ');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN
                SELECT    contract_status
                INTO        l_contract_status
                FROM cstbs_contract
                WHERE     contract_ref_no = p_cycle.contract_ref_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select of contract status from cstb contract');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN
                SELECT *
                INTO l_sitb_cont
                FROM sitbs_contract_master
                WHERE contract_ref_no = p_cycle.contract_ref_no
                AND        version_no = p_cycle.version_no;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in select from Contract Master');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN

                IF NOT SIPKSS_SWEEP.fn_acc_is_gl (l_sitb_cont.dr_account
                                                                        ,l_sitb_cont.dr_acc_br
                                                                        ,l_errcode)
                THEN
                BEGIN
/* USDFBME Upgrade 220999 - sanjeev */
    -- USDFBME fromBlr getavlbal changed to getavlbal_with_od
    --     IF NOT sipkss_sweep.fn_GetAvlBal(l_sitb_cont.dr_acc_br
/*    SIPKSS_SWEEP.fn_GetAvlBal changed to ACPKSS_MISC.fn_GetAvlBal_od
    for FINWARE INTERFACE CHANGES - 10/9/99 - STP1 */

--        IF NOT sipkss_sweep.fn_GetAvlBal_with_od(l_sitb_cont.dr_acc_br
     IF NOT ACPKSS_MISC.fn_GetAvlBal(l_sitb_cont.dr_acc_br
                                        ,l_sitb_cont.dr_account
                                        ,l_acc_bal) THEN
          debug.pr_debug ('SI','LDPKSS failed for Balance of Debit A/c');
            p_errcode := 'SI-SW0001;';
            p_params := l_sitb_cont.dr_account ||'~'||l_sitb_cont.dr_acc_br||'~;';
            return FALSE;
          END IF;

          END;

          ELSE

        IF NOT SIPKSS_SWEEP.fn_gl_avlbal (l_sitb_cont.dr_account
                            ,l_sitb_cont.dr_acc_br
                            ,l_sitb_cont.dr_acc_ccy
                            ,l_acc_bal
                            ,l_errcode)
        THEN

          debug.pr_debug ('SI','Failed in getting GL balance with '||l_errcode);
          p_errcode := l_errcode;
          return FALSE;

                END IF;

            END IF;

        EXCEPTION
            WHEN others THEN
            debug.pr_debug ('SI','Failed in getting the Avl. Bal of Dr a/c');
            p_errtype := 'G';
            return FALSE;
        END;


            BEGIN

                IF p_acc_ccy <> p_cycle.si_ccy
                THEN
                l_rate:=null;
                            BEGIN
                                SELECT rate_type into l_ratetype
                                FROM sitms_product_prf
                                WHERE product_code=substr(p_cycle.contract_ref_no,4,4);
                            EXCEPTION
                                WHEN others THEN
                                    l_ratetype:='STANDARD';
                            END;

                        IF NOT CYPKSS.fn_amt1_to_amt2(pkg_branch
                                            ,p_acc_ccy
                                            ,p_cycle.si_ccy
                                            ,l_ratetype
                                            ,'M'
                                            ,l_acc_bal
                                            ,'Y'
                                            ,l_avl_bal
                                            ,l_rate
                                            ,l_errcode) THEN
                        debug.pr_debug ('SI','Failed in Ccy conversion of Acc ccy to Si ccy');
                        p_errcode := l_errcode;
                        return FALSE;

                        END IF;

                ELSE
                    l_avl_bal := l_acc_bal;

                END IF;

                debug.pr_debug ('SI','Avl. Bal. of Account is '||l_avl_bal);
                l_amt_pending := p_cycle.si_amt_pending;
                debug.pr_debug ('SI','Amount pending is '||l_amt_pending);
                IF l_avl_bal < l_amt_pending THEN
                    l_partial_amt := l_avl_bal;
                ELSE
                    l_partial_amt := l_amt_pending;
                END IF;
                debug.pr_debug ('SI','Partial ampunt to be executed is '||l_partial_amt);


                END;

                BEGIN

                    UPDATE sitbs_cycle_due_exec
                    SET         si_amt_pending = l_partial_amt
                    WHERE     contract_ref_no = p_cycle.contract_ref_no
                    AND cycle_seq_no = p_cycle.cycle_seq_no;

                EXCEPTION
                    WHEN others THEN
                    debug.pr_debug ('SI','Failed in update of Due Exec First time');
                    p_errtype := 'G';
                    return FALSE;

                END;

                BEGIN

                        UPDATE    cstbs_contract
                        SET        latest_event_seq_no = p_esn,
                                    curr_event_code = 'PEXC',
                                    latest_event_date = global.application_date
                        WHERE    contract_ref_no = p_cycle.contract_ref_no;

                EXCEPTION
                    WHEN others THEN
                    debug.pr_debug ('SI','Failed in Update of Latest esn of cstbs contract');
                    p_errtype := 'G';
                    return false;
                END;

            BEGIN

                debug.pr_debug ('SI','ESN before update of cycle det is '||p_esn);
                debug.pr_debug ('SI','Ref no for update of cycle det is '||p_cycle.contract_ref_no);
                debug.pr_debug ('SI','CSN for update of cycle det is '||p_cycle.cycle_seq_no);
                debug.pr_debug ('SI','RSN for update of cycle det is '||p_cycle.retry_count);

                UPDATE    sitbs_cycle_detail
                SET            event_seq_no = p_esn,
                                event_code = 'PEXC'
                WHERE        contract_ref_no = p_cycle.contract_ref_no
                AND            cycle_seq_no = p_cycle.cycle_seq_no
                AND            retry_seq_no = nvl(p_cycle.retry_count,0)+1;

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in Update of Cycle Detail');
                p_errtype := 'G';
                return FALSE;
            END;

            BEGIN

                debug.pr_debug ('SI','Before insert into cstbs event log');

                INSERT INTO cstbs_contract_event_log
                (MODULE
                ,CONTRACT_REF_NO
                ,MAKER_ID
                ,MAKER_DT_STAMP
                ,CHECKER_ID
                ,CHECKER_DT_STAMP
                ,EVENT_SEQ_NO
                ,EVENT_DATE
                ,EVENT_CODE
                ,CONTRACT_STATUS
                ,AUTH_STATUS
                ,NEW_VERSION_INDICATOR
                ,REVERSED_EVENT_SEQ_NO)

                VALUES

                ('SI'
                ,p_cycle.contract_ref_no
                ,'SYSTEM'
                ,sipkss_batch.fn_get_date_time(global.application_date)
                ,l_checker_id
                ,l_checker_dt_stamp
                ,p_esn
                ,global.application_date
                ,'PEXC'
                ,l_contract_status
                ,l_auth_status
                ,'N'
                ,'');

            EXCEPTION
                WHEN others THEN
                debug.pr_debug ('SI','Failed in Insert into event Log');
                p_errtype := 'G';
                return FALSE;
            END;

                    SIPKSS_SUBSYSTEM.pr_reset_waive;

        prm_iccf_tags := '';
          prm_iccf_ccys:= '';
          prm_tax_tags:= '';
          prm_tax_ccys:= '';
          prm_sett_tags:= '';
          prm_sett_ccys:= '';

                    IF p_cycle.apply_chg_pexc = 'N'
                    THEN
                            debug.pr_debug ('SI','Apply chg on pexc is No');
                            SIPKSS_SUBSYSTEM.pr_waive_pexc;
                    END IF;

                    SIPKSS_SUBSYSTEM.pr_no_iccf;
                    --SIPKSS_SUBSYSTEM.pr_yes_settle;

                        debug.pr_debug ('SI','Before Settlement Pickup');
                        debug.pr_debug ('SI','Version no is '||p_cycle.version_no);
                        debug.pr_debug ('SI','ESN is '||p_esn);

                    IF NOT SIPKSS_SUBSYSTEM.fn_settlement_pickup(
                                                    p_cycle.contract_ref_no
                                                    ,p_cycle.version_no
                                                    ,p_esn
                                                    ,pkg_branch
                                                    ,'PEXC'
                                                    ,global.application_date
                                                    ,pkg_lcy
                                                    ,prm_iccf_tags
                                                    ,prm_iccf_ccys
                                                    ,prm_tax_tags
                                                    ,prm_tax_ccys
                                                    ,prm_sett_tags
                                                    ,prm_sett_ccys
                                                    ,l_errcode) THEN

                            debug.pr_debug ('SI','Failed in settlement pickup');
                            debug.pr_debug ('SI','Errcode is '||l_errcode);

                            p_errcode := l_errcode;
                            p_errtype := 'G';
                            return FALSE;

                            ELSE

                            debug.pr_debug ('SI','settlement pickup success');
                            debug.pr_debug ('SI','ICCF Tags are '||prm_iccf_tags);
                            debug.pr_debug ('SI','TAX Tags are '||prm_tax_tags);
                            debug.pr_debug ('SI','SETTLE Tags are '||prm_sett_tags);
                            debug.pr_debug ('SI','ICCF Ccys are '||prm_iccf_ccys);
                            debug.pr_debug ('SI','TAX Ccys are '||prm_tax_ccys);
                            debug.pr_debug ('SI','SETTLE Ccys are '||prm_sett_ccys);


                            END IF;

                            l_event_code := 'PEXC';

                            debug.pr_debug ('SI','Before Fn si Entries');
                            debug.pr_debug ('SI','Cycle Sequence No. is '||p_cycle.cycle_seq_no);
                            l_cycle_seq_no :=p_cycle.cycle_seq_no ; --volpert
                            IF NOT SIPKSS_ACCADV.fn_si_entries (p_cycle.contract_ref_no
                                                    ,pkg_branch
                                                    ,pkg_user
                                                    ,pkg_lcy
                                                    ,global.application_date
                                                    ,l_event_code
                                                    ,l_cycle_seq_no
                                                    ,'N'
                                                    ,'B'
                                                    ,l_errcode
                                                    ,l_params) THEN

                                debug.pr_debug ('SI','Failed in Accounting entries');
                                debug.pr_debug ('SI','Errcode is '||l_errcode||'-'||l_params);

                                p_errcode := l_errcode;
                                p_params  := l_params;

                                IF l_errcode is NOT NULL AND substr(l_errcode,1,2) = 'AC'
                                THEN
                                        p_errtype := 'A';
                                ELSE
                                        p_errtype := 'G';
                                END IF;

                                return FALSE;


                            END IF;


                            debug.pr_debug ('SI','Accounting Thru');


                        BEGIN

                            UPDATE sitbs_cycle_due_exec
                            SET         si_exec_status = 'R'
                                        ,si_suxs_date = null
                                        ,si_amt_pending = (l_amt_pending - l_partial_amt)
                            WHERE contract_ref_no = p_cycle.contract_ref_no
                            AND        cycle_seq_no = p_cycle.cycle_seq_no;

                    EXCEPTION
                            WHEN others THEN
                            p_errtype := 'G';
                            debug.pr_debug ('SI','Failed in update of Due Exec in Pexc');
                            return FALSE;

                    END;

                            debug.pr_debug ('SI','Checking for retry exceeded');
                            debug.pr_debug ('SI','Max retry is'||l_max_retry);
			-- 3-12694450741 Changes Starts
		--IF l_max_retry = nvl(p_cycle.retry_count, 0) + 1 THEN
		IF nvl(p_cycle.retry_count,0)+1 >= l_max_retry THEN
			-- 3-12694450741 Changes End
                            debug.pr_debug ('SI','Retry is exceeded');
                        BEGIN

                            UPDATE sitbs_cycle_due_exec
                            SET         si_exec_status = 'I'
                                        ,si_suxs_date = global.application_date
                            WHERE contract_ref_no = p_cycle.contract_ref_no
                            AND        cycle_seq_no = p_cycle.cycle_seq_no;

                    EXCEPTION
                            WHEN others THEN
                            p_errtype := 'G';
                            debug.pr_debug ('SI','Failed in retry exceeded update of Due Exec in reject with'||sqlerrm);
                            return FALSE;

                    END;
    END IF;


        g_eventcode:='PEXC';
        return TRUE;


EXCEPTION
        WHEN others THEN
        debug.pr_debug ('SI','MotherHood exc. of process pexc with '||sqlerrm);
        p_errtype := 'G';
        return FALSE;

END fn_process_pexc;
begin

pkg_branch     := global.current_branch;
pkg_appdate := global.application_date;
pkg_lcy    := global.lcy;
pkg_user    := global.user_id;

END sipks_payment;
/


DROP SYNONYM FCUBEUSER.SIPKSS_PAYMENT;

CREATE SYNONYM FCUBEUSER.SIPKSS_PAYMENT FOR FCUBEUSER.SIPKS_PAYMENT;


GRANT EXECUTE, DEBUG ON FCUBEUSER.SIPKS_PAYMENT TO FC_DBA_PRIV;


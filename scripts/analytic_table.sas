/* ================================================================================================
Cohort construction for WFS cardiorespiratory acute care analysis
Years: 2015 - 2018
Author: Ethan Hume
================================================================================================ */

/* ---- Library definitions ---- */

LIBNAME mkt 	"R:/MarketscanMerative"; 								/* Marketscan data root */
LIBNAME expo 	"E:/wawildfires/data"; 									/* MSA-day exposure file */
LIBNAME out 	"E:/wawildfires/data"; 									/* Output destination */

/* ================================================================================================
Make exposure table a SAS accessible table
================================================================================================ */
PROC IMPORT DATAFILE 	= "E:/wawildfires/data/exposure_msa_day.csv"
				OUT		= expo.exposure_msa_day
				DBMS	= CSV
				REPLACE;
			GETNAMES = YES;
RUN;

/* ================================================================================================
ICD CODE LISTS (From CMS Chronic Conditions Warehouse updated 3/23
================================================================================================ */
/* Asthma codes */
%let icd_asthma 	= 	'49300', '49301', '49302', '49310', '49311', '49312', '49320', '49321', '49322', 
						'49381', '49382', '49390', '49391', '49392', 'J4520', 'J4521', 'J4522', 'J4530', 
						'J4531', 'J4532', 'J4540', 'J4541', 'J4542', 'J4550', 'J4551', 'J4552', 'J45901', 
						'J45902', 'J45909', 'J45990', 'J45991', 'J45998', 'J8283 ';
/* COPD codes */
%let icd_copd 		=  	'490', '4910', '4911', '49120', '49121', '49122', '4918', '4919', '4920', 
						'4928', '4940', '4941', '496', 'J40', 'J410', 'J411', 'J418', 'J42', 'J430', 
						'J431', 'J432', 'J438', 'J439', 'J440', 'J441', 'J449', 'J470', 'J471', 'J479';

/* Qualification threshold - differs from CCW criteria of 2 outpatient events and looks for only 1
to booset sensitivity to chronic conditions in the eligible population */
%let QUAL_THRESHOLD = 1;

/* ================================================================================================
VISIT CLASSIFICATION CODE LISTS
================================================================================================ */

/* ED revenue codes */
%let ed_revcodes 	= '0450', '0451', '0452', '0453', '0454', '0455', '0456', '0457', '0458', '0459', '0981';

/* ED CPT procedure codes (only used when PROCTYP = 1) */
%let ed_procs	 	= '99281', '99282', '99283', '99284', '99285';

/* ED service category codes */
%let ed_serv_cats 	= '10120', '10220', '10320', '10420', '10520', '12220', '20120', '20220', '21120', 
                 	'21220', '22120', '22320', '30120', '30220', '30320', '30420', '30520', '30620',
                 	'31120', '31220', '31320', '31420', '31520', '31620';

/* PCP revenue codes */
%let pc_revcodes 	= '0510', '0515', '0517', '0519', '0521', '0522';

/* PCP place of service codes */
%let pc_stdplacs 	= 11, 50, 72;

/* PCP CPT procedure codes (only used when PROCTYP = 1) */
%let pc_procs		= '99202', '99203', '99204', '99205', '99206', '99207', '99208', '99209', '99210', '99211', '99212', '99213', 
					'99214', '99215', '99381', '99382', '99383', '99384', '99385', '99386', '99387', '99388', '99389', '99390', 
					'99391', '99392', '99393', '99394', '99395', '99396', '99397';

/* MDC codes for cardiorespiratory restriction */
%let cr_mdc = '04', '05';


/* ================================================================================================
MACRO: BUILD_COHORT_YEAR
================================================================================================ */

%macro build_cohort_year(
	YEAR		= ,
	YM1			= ,
	COMM_OP 	= ,
	MED_OP		= ,
	COMM_OP_Y1 	= ,
	MED_OP_Y1 	= ,
	COMM_IP 	= ,
	MED_IP		= ,
	COMM_IP_Y1 	= ,
	MED_IP_Y1	= ,
	COMM_EL		= ,
	MED_EL		=
);

%put NOTE: =====================================;
%put NOTE: Building cohort for &YEAR.;
%put NOTE: =====================================;

/* -------------------------------------------------------------
STEP 1: Outpatient condition qualifiers
Reference window: Jan 1 [Y-1] through Jun 30 [Y]
Source files: commercial outpatient + Medicare outpatient
------------------------------------------------------------ */

/* Outpatient commercial YM1 - check DX1 through DX4 */
PROC SQL;
	CREATE TABLE work.op_comm_ym1_&YEAR. AS
	SELECT 	ENROLID,
			SUM(asthma) AS n_asthma,
			SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) IN (&icd_asthma.) 	OR
				TRIM(DX2) IN (&icd_asthma.) 	OR
				TRIM(DX3) IN (&icd_asthma.) 	OR
				TRIM(DX4) IN (&icd_asthma.)) 	AS asthma,
		 		(TRIM(DX1) IN (&icd_copd.) 		OR
				TRIM(DX2) IN (&icd_copd.) 		OR
				TRIM(DX3) IN (&icd_copd.) 		OR
				TRIM(DX4) IN (&icd_copd.)) 		AS copd
		FROM mkt.&COMM_OP_Y1.
		WHERE EGEOLOC 	= '65'
			AND SVCDATE >= MDY(1, 1, &YM1.)
			AND SVCDATE <= MDY(12, 31, &YM1.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Outpatient Medicare YM1 - check DX1 through DX4 */
PROC SQL;
	CREATE TABLE work.op_med_ym1_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) IN (&icd_asthma.) 	OR
				TRIM(DX2) IN (&icd_asthma.) 	OR
				TRIM(DX3) IN (&icd_asthma.) 	OR
				TRIM(DX4) IN (&icd_asthma.)) 	AS asthma,
		 		(TRIM(DX1) IN (&icd_copd.) 		OR
				TRIM(DX2) IN (&icd_copd.) 		OR
				TRIM(DX3) IN (&icd_copd.) 		OR
				TRIM(DX4) IN (&icd_copd.)) 		AS copd
		FROM mkt.&MED_OP_Y1.
		WHERE EGEOLOC = '65'
			AND SVCDATE >= MDY(1, 1, &YM1.)
			AND SVCDATE <= MDY(12, 31, &YM1.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Outpatient commercial YEAR - check DX1 through DX4 */
PROC SQL;
	CREATE TABLE work.op_comm_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) IN (&icd_asthma.) 	OR
				TRIM(DX2) IN (&icd_asthma.) 	OR
				TRIM(DX3) IN (&icd_asthma.) 	OR
				TRIM(DX4) IN (&icd_asthma.)) 	AS asthma,
		 		(TRIM(DX1) IN (&icd_copd.) 		OR
				TRIM(DX2) IN (&icd_copd.) 		OR
				TRIM(DX3) IN (&icd_copd.) 		OR
				TRIM(DX4) IN (&icd_copd.)) 		AS copd
		FROM mkt.&COMM_OP.
		WHERE EGEOLOC = '65'
			AND SVCDATE >= MDY(1, 1, &YEAR.)
			AND SVCDATE <= MDY(6, 30, &YEAR.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Outpatient Medicare YEAR - check DX1 through DX4 */
PROC SQL;
	CREATE TABLE work.op_med_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) IN (&icd_asthma.) 	OR
				TRIM(DX2) IN (&icd_asthma.) 	OR
				TRIM(DX3) IN (&icd_asthma.) 	OR
				TRIM(DX4) IN (&icd_asthma.)) 	AS asthma,
		 		(TRIM(DX1) IN (&icd_copd.) 		OR
				TRIM(DX2) IN (&icd_copd.) 		OR
				TRIM(DX3) IN (&icd_copd.) 		OR
				TRIM(DX4) IN (&icd_copd.)) 		AS copd
		FROM mkt.&MED_OP.
		WHERE EGEOLOC = '65'
			AND SVCDATE >= MDY(1, 1, &YEAR.)
			AND SVCDATE <= MDY(6, 30, &YEAR.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Union outpatient qualifiers - one row per qualifying ENROLID */
PROC SQL;
	CREATE TABLE work.op_qualifiers_&YEAR. AS
	SELECT DISTINCT ENROLID FROM work.op_comm_ym1_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.op_med_ym1_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.op_comm_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.op_med_&YEAR.;
QUIT;

PROC DELETE DATA = 	work.op_comm_&YEAR. 
					work.op_med_&YEAR. 
					work.op_comm_ym1_&YEAR. 
					work.op_med_ym1_&YEAR.; 
RUN;


/* -------------------------------------------------------------
STEP 2: Inpatient condition qualifiers
Reference window: Jan 1 [Y-1] through Jun 30 [Y]
Source files: commercial inpatient + Medicare inpatient
------------------------------------------------------------ */

/* Inpatient commercial YM1 */
PROC SQL;
	CREATE TABLE work.ip_comm_ym1_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) 	IN (&icd_asthma.) 	OR
				TRIM(DX2)	IN (&icd_asthma.) 	OR
				TRIM(DX3) 	IN (&icd_asthma.) 	OR
				TRIM(DX4)	IN (&icd_asthma.) 	OR
				TRIM(DX5) 	IN (&icd_asthma.) 	OR
				TRIM(DX6) 	IN (&icd_asthma.) 	OR
				TRIM(DX7) 	IN (&icd_asthma.) 	OR
				TRIM(DX8) 	IN (&icd_asthma.) 	OR
				TRIM(DX9) 	IN (&icd_asthma.) 	OR
				TRIM(DX10) 	IN (&icd_asthma.) 	OR
				TRIM(DX11) 	IN (&icd_asthma.) 	OR
				TRIM(DX12) 	IN (&icd_asthma.) 	OR
				TRIM(DX13) 	IN (&icd_asthma.) 	OR
				TRIM(DX14) 	IN (&icd_asthma.) 	OR
				TRIM(DX15) 	IN (&icd_asthma.)) 	AS asthma,
				(TRIM(DX1) 	IN (&icd_copd.) 	OR
				TRIM(DX2)	IN (&icd_copd.) 	OR
				TRIM(DX3) 	IN (&icd_copd.) 	OR
				TRIM(DX4)	IN (&icd_copd.) 	OR
				TRIM(DX5) 	IN (&icd_copd.) 	OR
				TRIM(DX6) 	IN (&icd_copd.) 	OR
				TRIM(DX7) 	IN (&icd_copd.) 	OR
				TRIM(DX8) 	IN (&icd_copd.) 	OR
				TRIM(DX9) 	IN (&icd_copd.) 	OR
				TRIM(DX10) 	IN (&icd_copd.) 	OR
				TRIM(DX11) 	IN (&icd_copd.) 	OR
				TRIM(DX12) 	IN (&icd_copd.) 	OR
				TRIM(DX13) 	IN (&icd_copd.) 	OR
				TRIM(DX14) 	IN (&icd_copd.) 	OR
				TRIM(DX15) 	IN (&icd_copd.)) 	AS copd
		FROM mkt.&COMM_IP_Y1.
		WHERE EGEOLOC = '65'
			AND ADMDATE >= MDY(1, 1, &YM1.)
			AND ADMDATE <= MDY(12, 31, &YM1.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Inpatient Medicare YM1 */
PROC SQL;
	CREATE TABLE work.ip_med_ym1_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) 	IN (&icd_asthma.) 	OR
				TRIM(DX2)	IN (&icd_asthma.) 	OR
				TRIM(DX3) 	IN (&icd_asthma.) 	OR
				TRIM(DX4)	IN (&icd_asthma.) 	OR
				TRIM(DX5) 	IN (&icd_asthma.) 	OR
				TRIM(DX6) 	IN (&icd_asthma.) 	OR
				TRIM(DX7) 	IN (&icd_asthma.) 	OR
				TRIM(DX8) 	IN (&icd_asthma.) 	OR
				TRIM(DX9) 	IN (&icd_asthma.) 	OR
				TRIM(DX10) 	IN (&icd_asthma.) 	OR
				TRIM(DX11) 	IN (&icd_asthma.) 	OR
				TRIM(DX12) 	IN (&icd_asthma.) 	OR
				TRIM(DX13) 	IN (&icd_asthma.) 	OR
				TRIM(DX14) 	IN (&icd_asthma.) 	OR
				TRIM(DX15) 	IN (&icd_asthma.)) 	AS asthma,
				(TRIM(DX1) 	IN (&icd_copd.) 	OR
				TRIM(DX2)	IN (&icd_copd.) 	OR
				TRIM(DX3) 	IN (&icd_copd.) 	OR
				TRIM(DX4)	IN (&icd_copd.) 	OR
				TRIM(DX5) 	IN (&icd_copd.) 	OR
				TRIM(DX6) 	IN (&icd_copd.) 	OR
				TRIM(DX7) 	IN (&icd_copd.) 	OR
				TRIM(DX8) 	IN (&icd_copd.) 	OR
				TRIM(DX9) 	IN (&icd_copd.) 	OR
				TRIM(DX10) 	IN (&icd_copd.) 	OR
				TRIM(DX11) 	IN (&icd_copd.) 	OR
				TRIM(DX12) 	IN (&icd_copd.) 	OR
				TRIM(DX13) 	IN (&icd_copd.) 	OR
				TRIM(DX14) 	IN (&icd_copd.) 	OR
				TRIM(DX15) 	IN (&icd_copd.)) 	AS copd
		FROM mkt.&MED_IP_Y1.
		WHERE EGEOLOC = '65'
			AND ADMDATE >= MDY(1, 1, &YM1.)
			AND ADMDATE <= MDY(12, 31, &YM1.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Inpatient commercial YEAR */
PROC SQL;
	CREATE TABLE work.ip_comm_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) 	IN (&icd_asthma.) 	OR
				TRIM(DX2)	IN (&icd_asthma.) 	OR
				TRIM(DX3) 	IN (&icd_asthma.) 	OR
				TRIM(DX4)	IN (&icd_asthma.) 	OR
				TRIM(DX5) 	IN (&icd_asthma.) 	OR
				TRIM(DX6) 	IN (&icd_asthma.) 	OR
				TRIM(DX7) 	IN (&icd_asthma.) 	OR
				TRIM(DX8) 	IN (&icd_asthma.) 	OR
				TRIM(DX9) 	IN (&icd_asthma.) 	OR
				TRIM(DX10) 	IN (&icd_asthma.) 	OR
				TRIM(DX11) 	IN (&icd_asthma.) 	OR
				TRIM(DX12) 	IN (&icd_asthma.) 	OR
				TRIM(DX13) 	IN (&icd_asthma.) 	OR
				TRIM(DX14) 	IN (&icd_asthma.) 	OR
				TRIM(DX15) 	IN (&icd_asthma.)) 	AS asthma,
				(TRIM(DX1) 	IN (&icd_copd.) 	OR
				TRIM(DX2)	IN (&icd_copd.) 	OR
				TRIM(DX3) 	IN (&icd_copd.) 	OR
				TRIM(DX4)	IN (&icd_copd.) 	OR
				TRIM(DX5) 	IN (&icd_copd.) 	OR
				TRIM(DX6) 	IN (&icd_copd.) 	OR
				TRIM(DX7) 	IN (&icd_copd.) 	OR
				TRIM(DX8) 	IN (&icd_copd.) 	OR
				TRIM(DX9) 	IN (&icd_copd.) 	OR
				TRIM(DX10) 	IN (&icd_copd.) 	OR
				TRIM(DX11) 	IN (&icd_copd.) 	OR
				TRIM(DX12) 	IN (&icd_copd.) 	OR
				TRIM(DX13) 	IN (&icd_copd.) 	OR
				TRIM(DX14) 	IN (&icd_copd.) 	OR
				TRIM(DX15) 	IN (&icd_copd.)) 	AS copd
		FROM mkt.&COMM_IP.
		WHERE EGEOLOC = '65'
			AND ADMDATE >= MDY(1, 1, &YEAR.)
			AND ADMDATE <= MDY(12, 31, &YEAR.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;

/* Inpatient Medicare YEAR */
PROC SQL;
	CREATE TABLE work.ip_med_&YEAR. AS
	SELECT ENROLID,
		SUM(asthma) AS n_asthma,
		SUM(copd)	AS n_copd
	FROM (
		SELECT 	ENROLID,
				(TRIM(DX1) 	IN (&icd_asthma.) 	OR
				TRIM(DX2)	IN (&icd_asthma.) 	OR
				TRIM(DX3) 	IN (&icd_asthma.) 	OR
				TRIM(DX4)	IN (&icd_asthma.) 	OR
				TRIM(DX5) 	IN (&icd_asthma.) 	OR
				TRIM(DX6) 	IN (&icd_asthma.) 	OR
				TRIM(DX7) 	IN (&icd_asthma.) 	OR
				TRIM(DX8) 	IN (&icd_asthma.) 	OR
				TRIM(DX9) 	IN (&icd_asthma.) 	OR
				TRIM(DX10) 	IN (&icd_asthma.) 	OR
				TRIM(DX11) 	IN (&icd_asthma.) 	OR
				TRIM(DX12) 	IN (&icd_asthma.) 	OR
				TRIM(DX13) 	IN (&icd_asthma.) 	OR
				TRIM(DX14) 	IN (&icd_asthma.) 	OR
				TRIM(DX15) 	IN (&icd_asthma.)) 	AS asthma,
				(TRIM(DX1) 	IN (&icd_copd.) 	OR
				TRIM(DX2)	IN (&icd_copd.) 	OR
				TRIM(DX3) 	IN (&icd_copd.) 	OR
				TRIM(DX4)	IN (&icd_copd.) 	OR
				TRIM(DX5) 	IN (&icd_copd.) 	OR
				TRIM(DX6) 	IN (&icd_copd.) 	OR
				TRIM(DX7) 	IN (&icd_copd.) 	OR
				TRIM(DX8) 	IN (&icd_copd.) 	OR
				TRIM(DX9) 	IN (&icd_copd.) 	OR
				TRIM(DX10) 	IN (&icd_copd.) 	OR
				TRIM(DX11) 	IN (&icd_copd.) 	OR
				TRIM(DX12) 	IN (&icd_copd.) 	OR
				TRIM(DX13) 	IN (&icd_copd.) 	OR
				TRIM(DX14) 	IN (&icd_copd.) 	OR
				TRIM(DX15) 	IN (&icd_copd.)) 	AS copd
		FROM mkt.&MED_IP.
		WHERE EGEOLOC = '65'
			AND ADMDATE >= MDY(1, 1, &YEAR.)
			AND ADMDATE <= MDY(12, 31, &YEAR.)
		)
	GROUP BY ENROLID
	HAVING n_asthma 	>= &QUAL_THRESHOLD.
		OR n_copd	 	>= &QUAL_THRESHOLD.;
QUIT;


/* Union inpatient qualifiers */
PROC SQL;
	CREATE TABLE work.ip_qualifiers_&YEAR. AS
	SELECT DISTINCT ENROLID FROM work.ip_comm_ym1_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.ip_med_ym1_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.ip_comm_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.ip_med_&YEAR.;
QUIT;

PROC DELETE DATA = 	work.ip_comm_&YEAR. 
					work.ip_med_&YEAR.
					work.ip_comm_ym1_&YEAR.
					work.ip_med_ym1_&YEAR.; 
RUN;

/* -------------------------------------------------------------
STEP 3: Clinical qualifiers combined
An enrollee qualifies if they appear in EITHER op_qualfiers OR
ip_qualifiers
------------------------------------------------------------ */

PROC SQL;
	CREATE TABLE work.clin_qualifiers_&YEAR. AS
	SELECT DISTINCT ENROLID FROM work.op_qualifiers_&YEAR.
	UNION
	SELECT DISTINCT ENROLID FROM work.ip_qualifiers_&YEAR.
QUIT;

PROC DELETE DATA = work.op_qualifiers_&YEAR. work.ip_qualifiers_&YEAR.; RUN;

/* -------------------------------------------------------------
STEP 4: Continuous enrollment qualifier
Requirement: enrolled Jan 1 [Y] through Sep 30 [Y], no 
gaps in coverage
------------------------------------------------------------ */

/* Stack commercial + Medicare eligibility */
DATA work.elig_&YEAR.;
	SET mkt.&COMM_EL. mkt.&MED_EL.;
	WHERE EGEOLOC = '65';
	/* Keep only enrollment spans that overlap Jan 1 [Y] - Sep 30 [Y] */
	IF 	DTEND 	>= MDY(1, 1, &YEAR.)
	AND DTSTART <= MDY(9, 30, &YEAR.);
RUN;

PROC SORT DATA = work.elig_&YEAR.
		OUT = work.elig_sorted_&YEAR.;
	BY ENROLID DTSTART;
RUN;


/* Gap detection */
DATA work.elig_gaps_&YEAR.;
	SET work.elig_sorted_&YEAR.;
	BY ENROLID;
	RETAIN prev_end;
	IF first.ENROLID THEN prev_end = .;
	/* Gap exists if this span doesn't immediate follow previous */
	has_gap_row = (NOT first.ENROLID) AND (DTSTART > prev_end + 1);
	prev_end = MAX(prev_end, DTEND);
	DROP prev_end;
RUN;

/* Summarize to ENROLID level */
PROC SQL;
	CREATE TABLE work.elig_qualifiers_&YEAR. AS
	SELECT ENROLID,
		/* covers_start: earliest DTSTART must be <= Jan 1 */
		(MIN(DTSTART) <= MDY(1, 1, &YEAR.)) AS covers_start,
		/* covers_end: latest DTEND must be >= Sep 30 */
		(MAX(DTEND) >= MDY(9, 30, &YEAR.)) AS covers_end,
		/* has_gap: any row flagged above */
		MAX(has_gap_row) AS has_gap
	FROM work.elig_gaps_&YEAR.
	GROUP BY ENROLID
	HAVING covers_start = 1
		AND covers_end 	= 1
		AND has_gap 	= 0;
QUIT;

PROC DELETE DATA = 	work.elig_&YEAR.
					work.elig_sorted_&YEAR.
					work.elig_gaps_&YEAR.; RUN;

/* -------------------------------------------------------------
STEP 5: Final cohort
clinical qualifier AND enrollment qualifier
------------------------------------------------------------ */

PROC SQL;
	CREATE TABLE work.cohort_&YEAR. AS
	SELECT DISTINCT c.ENROLID
	FROM work.clin_qualifiers_&YEAR. c
	INNER JOIN work.elig_qualifiers_&YEAR. e
	ON c.ENROLID = e.ENROLID;
QUIT;

PROC DELETE DATA =  work.clin_qualifiers_&YEAR.
					work.elig_qualifiers_&YEAR.; RUN;

/* Check that this all worked by printing N */
PROC SQL;
	SELECT COUNT(*) AS n_enrollees_&YEAR.
	FROM work.cohort_&YEAR.;
QUIT;

	
/* -------------------------------------------------------------
STEP 6A: Pull outpatient claims for the analysis window 
(Jul 1 - Sep 30, [Y]) for cohort members.
Flag ED, PCP, and cardiorespiratory restriction.
------------------------------------------------------------ */

PROC SQL;
	CREATE TABLE work.op_analytic_&YEAR. AS
	SELECT ENROLID,
		MSA,
		(SVCDATE - MDY(7, 1, &YEAR.) + 1) 				AS season_day,
		(FLOOR((SVCDATE - MDY(7, 1, &YEAR.)) / 7) + 1) 	AS week,

		/* All-cause ED: 4-part definition */
		((STDPLAC = 23)
		OR (SVCSCAT IN (&ed_serv_cats.))
		OR (REVCODE IN (&ed_revcodes.))
		OR (PROCTYP = '1' AND PROC1 IN (&ed_procs.))) AS any_ed,

		/* All-cause PCP: 3-part definition */
		((STDPLAC IN (&pc_stdplacs.))
		OR (REVCODE IN (&pc_revcodes.))
		OR (PROCTYP = '1' AND PROC1 IN (&pc_procs))) AS any_pcp,

		/* Cardiorespiratory restriction */
		((MDC IN (&cr_mdc.))
		OR (TRIM(DX1) IN (&icd_asthma.) OR TRIM(DX2) IN (&icd_asthma.) 	OR
			TRIM(DX3) IN (&icd_asthma.) OR TRIM(DX4) IN (&icd_asthma.) 	OR
			TRIM(DX1) IN (&icd_copd.) 	OR TRIM(DX2) IN (&icd_copd.) 	OR
			TRIM(DX3) IN (&icd_copd.) 	OR TRIM(DX4) IN (&icd_copd.))) 	AS cr_flag

	FROM mkt.&COMM_OP. a
	WHERE 	a.EGEOLOC = '65'
		AND a.ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
		AND a.SVCDATE >= MDY(7, 1, &YEAR.)
		AND a.SVCDATE <= MDY(9, 30, &YEAR.)
	UNION ALL
	SELECT ENROLID,
		MSA,
		(SVCDATE - MDY(7, 1, &YEAR.) + 1) 				AS season_day,
		(FLOOR((SVCDATE - MDY(7, 1, &YEAR.)) / 7) + 1) 	AS week,

		/* All-cause ED: 4-part definition */
		((STDPLAC = 23)
		OR (SVCSCAT IN (&ed_serv_cats.))
		OR (REVCODE IN (&ed_revcodes.))
		OR (PROCTYP = '1' AND PROC1 IN (&ed_procs.))) AS any_ed,

		/* All-cause PCP: 3-part definition */
		((STDPLAC IN (&pc_stdplacs.))
		OR (REVCODE IN (&pc_revcodes.))
		OR (PROCTYP = '1' AND PROC1 IN (&pc_procs))) AS any_pcp,

		/* Cardiorespiratory restriction */
		((MDC IN (&cr_mdc.))
		OR (TRIM(DX1) IN (&icd_asthma.) OR TRIM(DX2) 	IN (&icd_asthma.) 	OR
			TRIM(DX3) IN (&icd_asthma.) OR TRIM(DX4) 	IN (&icd_asthma.) 	OR
			TRIM(DX1) IN (&icd_copd.) OR TRIM(DX2) 		IN (&icd_copd.) 	OR
			TRIM(DX3) IN (&icd_copd.) OR TRIM(DX4) 		IN (&icd_copd.))) 	AS cr_flag

	FROM mkt.&MED_OP. b
	WHERE 	b.EGEOLOC = '65'
		AND b.ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
		AND b.SVCDATE >= MDY(7, 1, &YEAR.)
		AND b.SVCDATE <= MDY(9, 30, &YEAR.);
QUIT;

/* Derive cr_ed and cr_pcp, aggregate to person-week */
PROC SQL;
	CREATE TABLE work.op_week_&YEAR. AS
	SELECT ENROLID,
			MSA,
			week,
			MAX(cr_flag AND any_ed) AS CR_ED_WEEK,
			MAX(cr_flag AND any_pcp) AS CR_PCP_WEEK,
			MAX(any_ed) AS ANY_ED_WEEK,
			MAX(any_pcp) AS ANY_PCP_WEEK
	FROM work.op_analytic_&YEAR.
	GROUP BY ENROLID, MSA, week;
QUIT;

PROC DELETE DATA = work.op_analytic_&YEAR.; RUN;


/* -------------------------------------------------------------
STEP 6B: Pull inpatient admissions for the analysis window 
(Jul 1 - Sep 30, [Y]) for cohort members.
Flag ED, PCP, and cardiorespiratory restriction.
------------------------------------------------------------ */
PROC SQL;
	CREATE TABLE work.ip_analytic_&YEAR. AS
	SELECT ENROLID,
		MSA,
		(FLOOR((ADMDATE - MDY(7, 1, &YEAR.)) / 7) + 1) 	AS week,

		/* All inpatient admissions count as any_ip by definition */
		1 AS any_ip,

		/* Cardiorespiratory restriction */
		((MDC IN (&cr_mdc.))
		OR (TRIM(DX1) 	IN (&icd_asthma.) 	OR TRIM(DX2) 	IN (&icd_asthma.) 	OR
			TRIM(DX3) 	IN (&icd_asthma.) 	OR TRIM(DX4)	IN (&icd_asthma.) 	OR
			TRIM(DX5) 	IN (&icd_asthma.) 	OR TRIM(DX6) 	IN (&icd_asthma.) 	OR
			TRIM(DX7) 	IN (&icd_asthma.) 	OR TRIM(DX8) 	IN (&icd_asthma.) 	OR
			TRIM(DX9) 	IN (&icd_asthma.) 	OR TRIM(DX10) 	IN (&icd_asthma.) 	OR
			TRIM(DX11) 	IN (&icd_asthma.) 	OR TRIM(DX12) 	IN (&icd_asthma.) 	OR
			TRIM(DX13) 	IN (&icd_asthma.) 	OR TRIM(DX14)	IN (&icd_asthma.) 	OR
			TRIM(DX15) 	IN (&icd_asthma.) 	OR TRIM(DX1) 	IN (&icd_copd.) 	OR
			TRIM(DX2) 	IN (&icd_copd.) 	OR TRIM(DX3) 	IN (&icd_copd.) 	OR
			TRIM(DX4) 	IN (&icd_copd.) 	OR TRIM(DX5) 	IN (&icd_copd.) 	OR
			TRIM(DX6) 	IN (&icd_copd.) 	OR TRIM(DX7) 	IN (&icd_copd.) 	OR
			TRIM(DX8) 	IN (&icd_copd.) 	OR TRIM(DX9) 	IN (&icd_copd.) 	OR
			TRIM(DX10) 	IN (&icd_copd.) 	OR TRIM(DX11) 	IN (&icd_copd.) 	OR
			TRIM(DX12) 	IN (&icd_copd.) 	OR TRIM(DX13) 	IN (&icd_copd.) 	OR
			TRIM(DX14) 	IN (&icd_copd.) 	OR TRIM(DX15) 	IN (&icd_copd.))) 	AS cr_flag

	FROM mkt.&COMM_IP. a
	WHERE 	a.EGEOLOC = '65'
		AND a.ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
		AND a.ADMDATE >= MDY(7, 1, &YEAR.)
		AND a.ADMDATE <= MDY(9, 30, &YEAR.)
	UNION ALL
	SELECT ENROLID,
		MSA,
		(FLOOR((ADMDATE - MDY(7, 1, &YEAR.)) / 7) + 1) 	AS week,

		/* All inpatient admissions count as any_ip by definition */
		1 AS any_ip,

		/* Cardiorespiratory restriction */
		((MDC IN (&cr_mdc.))
		OR (TRIM(DX1) 	IN (&icd_asthma.) 	OR TRIM(DX2) 	IN (&icd_asthma.) 	OR
			TRIM(DX3) 	IN (&icd_asthma.) 	OR TRIM(DX4)	IN (&icd_asthma.) 	OR
			TRIM(DX5) 	IN (&icd_asthma.) 	OR TRIM(DX6) 	IN (&icd_asthma.) 	OR
			TRIM(DX7) 	IN (&icd_asthma.) 	OR TRIM(DX8) 	IN (&icd_asthma.) 	OR
			TRIM(DX9) 	IN (&icd_asthma.) 	OR TRIM(DX10) 	IN (&icd_asthma.) 	OR
			TRIM(DX11) 	IN (&icd_asthma.) 	OR TRIM(DX12) 	IN (&icd_asthma.) 	OR
			TRIM(DX13) 	IN (&icd_asthma.) 	OR TRIM(DX14)	IN (&icd_asthma.) 	OR
			TRIM(DX15) 	IN (&icd_asthma.) 	OR TRIM(DX1) 	IN (&icd_copd.) 	OR
			TRIM(DX2) 	IN (&icd_copd.) 	OR TRIM(DX3) 	IN (&icd_copd.) 	OR
			TRIM(DX4) 	IN (&icd_copd.) 	OR TRIM(DX5) 	IN (&icd_copd.) 	OR
			TRIM(DX6) 	IN (&icd_copd.) 	OR TRIM(DX7) 	IN (&icd_copd.) 	OR
			TRIM(DX8) 	IN (&icd_copd.) 	OR TRIM(DX9) 	IN (&icd_copd.) 	OR
			TRIM(DX10) 	IN (&icd_copd.) 	OR TRIM(DX11) 	IN (&icd_copd.) 	OR
			TRIM(DX12) 	IN (&icd_copd.) 	OR TRIM(DX13) 	IN (&icd_copd.) 	OR
			TRIM(DX14) 	IN (&icd_copd.) 	OR TRIM(DX15) 	IN (&icd_copd.))) 	AS cr_flag

	FROM mkt.&MED_IP. b
	WHERE 	b.EGEOLOC = '65'
		AND b.ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
		AND b.ADMDATE >= MDY(7, 1, &YEAR.)
		AND b.ADMDATE <= MDY(9, 30, &YEAR.);
QUIT;

/* Aggregate npatient to person week */
PROC SQL;
	CREATE TABLE work.ip_week_&YEAR. AS
	SELECT ENROLID,
		MSA,
		week,
		MAX(cr_flag) 	AS CR_IP_WEEK,
		MAX(any_ip) 	AS ANY_IP_WEEK
	FROM work.ip_analytic_&YEAR.
	GROUP BY ENROLID, MSA, week;
QUIT;

PROC DELETE DATA = work.ip_analytic_&YEAR. ; RUN;

/* -------------------------------------------------------------
STEP 7: Build full person-week spine and merge outcomes
------------------------------------------------------------ */

/* One MSA per enrollee from eligibility */
PROC SQL;
	CREATE TABLE work.enrollee_msa_&YEAR. AS
	SELECT ENROLID, MIN(MSA) AS MSA
	FROM (
		SELECT ENROLID, MSA FROM mkt.&COMM_EL.
		WHERE ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
			AND EGEOLOC = '65'
			AND MSA NE 0
		UNION ALL
		SELECT ENROLID, MSA FROM mkt.&MED_EL.
		WHERE ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
			AND EGEOLOC = '65'
			AND MSA NE 0
	)
	GROUP BY ENROLID;
QUIT;

/* Week spine 1-13 */
DATA work.week_spine;
	DO week = 1 TO 13; OUTPUT; END;
RUN;

/* Cross join: enrollees x weeks */
PROC SQL;
	CREATE TABLE work.spine_&YEAR. AS
	SELECT e.ENROLID, e.MSA, &YEAR. AS year, w.week
	FROM work.enrollee_msa_&YEAR. e
	CROSS JOIN work.week_spine w;
QUIT;

PROC SQL;
	CREATE TABLE work.analytic_&YEAR. AS
	SELECT s.ENROLID,
		s.MSA,
		s.year,
		s.week,
		COALESCE(o.CR_ED_WEEK, 		0) AS CR_ED_WEEK,
		COALESCE(o.CR_PCP_WEEK, 	0) AS CR_PCP_WEEK,
		COALESCE(i.CR_IP_WEEK,	 	0) AS CR_IP_WEEK,
		COALESCE(o.ANY_ED_WEEK, 	0) AS ANY_ED_WEEK,
		COALESCE(o.ANY_PCP_WEEK, 	0) AS ANY_PCP_WEEK,
		COALESCE(i.ANY_IP_WEEK, 	0) AS ANY_IP_WEEK
	FROM work.spine_&YEAR. s
	LEFT JOIN work.op_week_&YEAR. o
	ON s.ENROLID = o.ENROLID AND s.MSA = o.MSA AND s.week = o.week
	LEFT JOIN work.ip_week_&YEAR. i
	ON s.ENROLID = i.ENROLID AND s.MSA = i.MSA AND s.week = i.week;
QUIT;

PROC DELETE DATA = 	work.op_week_&YEAR.
					work.ip_week_&YEAR.
					work.enrollee_msa_&YEAR.
					work.spine_&YEAR.; RUN;

/* -------------------------------------------------------------
STEP 8: Merge in covariates from eligibility file
------------------------------------------------------------ */

PROC SQL;
	CREATE TABLE work.analytic_covars_&YEAR. AS
	SELECT 
		a.*,
		e.AGE,
		e.SEX,
		e.EESTATU,
		e.EECLASS,
		e.EMPREL,
		e.INDSTRY,
		e.PLANTYP,
		e.MHSACOVG
	FROM work.analytic_&YEAR. a
	LEFT JOIN (
		SELECT ENROLID, AGE, SEX, EESTATU, EECLASS, EMPREL, INDSTRY, 
			PLANTYP, MHSACOVG
		FROM (
			SELECT ENROLID, AGE, SEX, EESTATU, EECLASS, EMPREL,
					INDSTRY, PLANTYP, MHSACOVG, DTSTART
			FROM mkt.&COMM_EL.
			WHERE ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
				AND YEAR(DTSTART) = &YEAR.
			UNION ALL
			SELECT ENROLID, AGE, SEX, EESTATU, EECLASS, EMPREL,
					INDSTRY, PLANTYP, MHSACOVG, DTSTART
			FROM mkt.&MED_EL.
			WHERE ENROLID IN (SELECT ENROLID FROM work.cohort_&YEAR.)
			AND YEAR(DTSTART) = &YEAR.
		)
		GROUP BY ENROLID
		HAVING DTSTART = MAX(DTSTART)
	) e ON a. ENROLID = e.ENROLID;
QUIT;

PROC DELETE DATA = work.analytic_&YEAR.; RUN;


/* -------------------------------------------------------------
STEP 9: Merge in exposure data
------------------------------------------------------------ */

/* First aggregate exposure to MSA-week level. Weeks are defined 
relative to Jul 1, consistent with the person-week spine in cohort 
table. Jun 24 - 30 is coded as week 0 to serve as the lag source for 
week 1 */
PROC SQL;
	CREATE TABLE work.expo_weekly_&YEAR. AS
	SELECT msa,
		CASE
			WHEN date >= MDY(6, 24, &YEAR.) AND date <= MDY(6, 30, &YEAR.) 
				THEN 0
			WHEN date >= MDY(7, 1, &YEAR.) 	AND date <= MDY(9, 30, &YEAR.) 
				THEN FLOOR((date - MDY(7, 1, &YEAR.)) / 7) + 1
		END AS week,
		MEAN(msa_con) AS PM25_WEEK,
		SUM(smoke = 'TRUE') AS SMOKE_DAYS,
		SUM(active_fire = 1) AS ACTIVE_FIRE_DAYS
	FROM expo.exposure_msa_day
	WHERE 	date >= MDY(6, 24, &YEAR.)
		AND date <= MDY(9, 30, &YEAR.)
	GROUP BY msa, week
	HAVING week IS NOT NULL;
QUIT;

/* Self-join to attach lag values: join week N to week N-1 */
PROC SQL;
	CREATE TABLE work.expo_with_lag_&YEAR. AS
	SELECT 	c.msa,
			c.week,
			c.PM25_WEEK,
			c.SMOKE_DAYS,
			c.ACTIVE_FIRE_DAYS,
			l.PM25_WEEK AS PM25_LAG1,
			l.SMOKE_DAYS AS SMOKE_DAYS_LAG1,
			l.ACTIVE_FIRE_DAYS AS ACTIVE_FIRE_DAYS_LAG1
	FROM work.expo_weekly_&YEAR. c
	LEFT JOIN work.expo_weekly_&YEAR. l
	ON c.msa = l.msa
	AND c.week = l.week + 1
	/* Keep only analysis weeks 1-13, drop week 0 */
	WHERE c.week BETWEEN 1 AND 13;
QUIT;

PROC DELETE DATA = work.expo_weekly_&YEAR.; RUN;

/* Join weekly exposure onto analytic dataset */
PROC SQL;
	CREATE TABLE work.analytic_expo_&YEAR. AS
	SELECT	a.*,
		x.PM25_WEEK,
		x.SMOKE_DAYS,
		x.ACTIVE_FIRE_DAYS,
		x.PM25_LAG1,
		x.SMOKE_DAYS_LAG1,
		x.ACTIVE_FIRE_DAYS_LAG1
	FROM work.analytic_covars_&YEAR. a
	LEFT JOIN work.expo_with_lag_&YEAR. x
	ON	a.msa	= x.msa
	AND a.week 	= x.week;
QUIT;

PROC DELETE DATA = 	work.analytic_covars_&YEAR.
					work.expo_with_lag_&YEAR.; RUN;


/* -------------------------------------------------------------
STEP 10: Write final analytic file to output library
------------------------------------------------------------ */

DATA out.analytic_&YEAR.;
	SET work.analytic_expo_&YEAR.;
	LABEL
		ENROLID 				= "Enrollee ID"
		MSA 					= "Metropolitan Statistical Area"
		year 					= "Study year"
		week					= "Fire season week (1-13, starting Jul 1)"
		CR_ED_WEEK 				= "Any cardiorespiratory ED visit this week (0/1)"
		CR_PCP_WEEK 			= "Any cardiorespiratory PCP visit this week (0/1)"
		CR_IP_WEEK				= "Any cardiorespiratory IP admission this week (0/1)"
		ANY_ED_WEEK 			= "Any all-cause ED visit this week (0/1)"
		ANY_PCP_WEEK 			= "Any all-cause PCP visit this week (0/1)"
		ANY_IP_WEEK				= "Any all-cause IP admission this week (0/1)"
		PM25_WEEK				= "Mean daily PM2.5 (ug/m3) for current week"
		SMOKE_DAYS				= "Count of smoke days in current week (0-7)"
		ACTIVE_FIRE_DAYS		= "Count of active fire days in current week"
		PM25_LAG1				= "Mean daily PM2.5 (ug/m3) for prior week"
		SMOKE_DAYS_LAG1			= "Count of smoke days in prior week (0-7)"
		ACTIVE_FIRE_DAYS_LAG1	= "Count of active fire days in prior week"
		AGE						= "Age at time of service"
		SEX 					= "Sex"
		EESTATU					= "Employment status"
		EECLASS					= "Employment class"
		EMPREL					= "Relationship to subscriber"
		INDSTRY					= "Industry"
		PLANTYP					= "Plan type"
		MHSACOVG				= "Mental health/substance abuse coverage flag";
RUN;

PROC DELETE DATA = work.analytic_expo_&YEAR.; RUN; 

%put NOTE: Finished year &YEAR.;

%mend build_cohort_year;


/* ================================================================================================
CALL MACRO FOR EACH TRAINING YEAR
================================================================================================ */

/* 2015 */
%build_cohort_year(
	YEAR		= 2015,
	YM1			= 2014,
	COMM_OP		= ccaeo151,
	MED_OP		= mdcro151,
	COMM_OP_Y1	= ccaeo141,
	MED_OP_Y1	= mdcro141,
	COMM_IP		= ccaei151,
	MED_IP		= mdcri151,
	COMM_IP_Y1	= ccaei141,
	MED_IP_Y1	= mdcri141,
	COMM_EL		= ccaet151,
	MED_EL		= mdcrt151
);

/* 2016 */
%build_cohort_year(
	YEAR		= 2016,
	YM1			= 2015,
	COMM_OP		= ccaeo161,
	MED_OP		= mdcro161,
	COMM_OP_Y1	= ccaeo151,
	MED_OP_Y1	= mdcro151,
	COMM_IP		= ccaei161,
	MED_IP		= mdcri161,
	COMM_IP_Y1	= ccaei151,
	MED_IP_Y1	= mdcri151,
	COMM_EL		= ccaet161,
	MED_EL		= mdcrt161
);

/* 2017 */
%build_cohort_year(
	YEAR		= 2017,
	YM1			= 2016,
	COMM_OP		= ccaeo171,
	MED_OP		= mdcro171,
	COMM_OP_Y1	= ccaeo161,
	MED_OP_Y1	= mdcro161,
	COMM_IP		= ccaei171,
	MED_IP		= mdcri171,
	COMM_IP_Y1	= ccaei161,
	MED_IP_Y1	= mdcri161,
	COMM_EL		= ccaet171,
	MED_EL		= mdcrt171
);
/* 2018 */
%build_cohort_year(
	YEAR		= 2018,
	YM1			= 2017,
	COMM_OP		= ccaeo181,
	MED_OP		= mdcro181,
	COMM_OP_Y1	= ccaeo171,
	MED_OP_Y1	= mdcro171,
	COMM_IP		= ccaei181,
	MED_IP		= mdcri181,
	COMM_IP_Y1	= ccaei171,
	MED_IP_Y1	= mdcri171,
	COMM_EL		= ccaet181,
	MED_EL		= mdcrt181
);


/* ================================================================================================
STACK ALL YEARS INTO ONE FILE
================================================================================================ */

DATA out.analytic_2015_2018;
	SET out.analytic_2015
		out.analytic_2016
		out.analytic_2017
		out.analytic_2018;
RUN;

/* ================================================================================================
EXPORT STACKED FILE
================================================================================================ */

PROC EXPORT DATA = out.analytic_2015_2018
	OUTFILE = "E:/wawildfires/data/analytic_2015_2018.csv"
	DBMS = CSV
	REPLACE;
RUN;

/* ================================================================================================
DATA VALIDATION FOR 2015
================================================================================================ */

PROC SQL;
	SELECT COUNT(*) AS n_enrollees_2015
	FROM work.cohort_2015;
QUIT;

PROC SQL;
	SELECT COUNT(DISTINCT ENROLID) AS n_unique_enrollees_2015
	FROM work.enrollee_msa_2015;
QUIT;

PROC CONTENTS DATA = out.analytic_2015; RUN;

PROC PRINT DATA = out.analytic_2015 (OBS = 100); RUN;

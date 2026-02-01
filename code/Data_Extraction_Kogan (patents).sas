%let wrds = wrds-cloud.wharton.upenn.edu 4016; options comamid=TCP remote=WRDS; signon username=_prompt_; run;
Libname rwork slibref=work server=wrds;

libname tc 'C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research\Data';

proc import datafile='C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research\Data\patentawards.xlsx' 
	out=work.contrawards dbms=xlsx replace;
	sheet=Sheet1;
run;

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Step 1: Length of announcement window;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
rsubmit;
libname taq '/wrds/taq/sasdata/';
libname compq '/wrds/comp/sasdata/naa';
libname compx '/wrds/comp/sasdata/nam/';
libname crspl '/wrds/crsp/sasdata/a_ccm/'; *this lib is for the link table;

proc upload data=work.contrawards out=contrawards; run;

* Step 1: Setting the DB;
data comp; set comp.funda(keep = gvkey datadate fyear indfmt datafmt popsrc consol curcd prcc_f csho ceq dvc at sale ibc intan ppent capx dlc dltt ni xrd);
	where	(fyear ge 1979) and (fyear le 2021)
			and (indfmt='INDL') and (datafmt='STD') and (popsrc='D') and (consol='C') and (curcd='USD') /*and (gvkey = '005047') and (at > 0) and (ceq > 1)*/;

	if gvkey = lag1(gvkey) and intck('month', lag1(datadate), datadate) = 12 then ym1 = 1; else ym1 = .;

	mve 				= prcc_f * csho;
	ceq_ym1 			= lag(ceq) * ym1;
	mve_ym1 			= lag(mve) * ym1;
	at_ym1 				= lag(at) * ym1;
	sale_ym1 			= lag(sale) * ym1;
	ib					= ibc / at;
	intang 				= intan / at;
	tang 				= ppent / at;
	inv					= (capx) / at;
	lev 				= (dltt + dlc) / at;
	if ni < 0 then loss = 1; else loss = 0;
	if dvc > 0 then div = 1; else div = 0;
	mtb					= mve / ceq;
    rd_at				= xrd / at;
    rd_sales			= xrd / sale;
	roa 				= ibc / at_ym1; 
	salegr 				= (sale - sale_ym1) / sale_ym1;
	size 				= log(mve_ym1);

	drop indfmt datafmt popsrc consol curcd ceq ibc intan ppent capx dltt dlc ni dvc xrd at_ym1 sale_ym1 mve_ym1 ceq_ym1 prcc_f csho ym1 mve;
run;

proc sort data=comp nodupkey; by gvkey datadate; run;

proc sql; create table comp as select
	a.*, b.naics, b.sic from comp as a left join comp.names as b
	on a.gvkey eq b.gvkey
	order by gvkey, datadate;
quit;

proc sql;
	create table comp2 as select a.*, b.rdq
    from comp a left join compa.fundq b
    on (a.gvkey = b.gvkey) and (a.datadate = b.datadate) where (a.gvkey = b.gvkey) and (a.datadate = b.datadate);
quit;

proc sort data=comp2 nodupkey; by gvkey datadate; run;

proc sql; create table comp3 as select 
	a.*, b.lpermno as permno, b.lpermno as permno_adj
    from comp2 as a left join crspl.ccmxpf_linktable
	(where=(linktype in ('LU', 'LC', 'LD', 'LF', 'LN', 'LO', 'LS', 'LX'))) as b
    on (a.gvkey = b.gvkey) 
	and (b.linkdt <= a.datadate or b.linkdt = .B) 
	and (a.datadate <= b.linkenddt or b.linkenddt = .E)
	order by gvkey, datadate;
quit;

proc sql;
	create table permno as
	select distinct permno_adj
	from contrawards;
quit;

/*
proc sql;
	create table permno as
	select distinct permno_adj, gvkey
	from contrawards;
quit;
*/

proc sql; create table comp3 as select a.*, b.permno_adj as permno, b.permno_adj
    from comp3 as a left join permno as b
    on (a.permno_adj = b.permno_adj) where (a.permno_adj = b.permno_adj)
	order by gvkey, fyear;
quit;

* Add beta and return volatility;
data beta; set comp3(keep = permno_adj datadate); run;

* Generate stock beta;
proc sql;
    create table beta1 as select a.*, b.ret as stockret, b.date, abs(b.prc) as stockprice
    from beta a left join crsp.dsf(where=((year(date) between 1978 and 2023))) b
    on (a.permno_adj = b.permno) and (intck('weekday', a.datadate, b.date) between -300 and -46)
    order by permno_adj, datadate, date desc;
quit;

proc sql;
    create table beta2 as select a.*, b.vwretd as mktret
    from beta1 a left join crsp.dsi b
    on (a.date = b.date)
    order by permno_adj, datadate, date desc;
quit;

proc reg data = beta2 outest = beta3 noprint;
	by permno_adj datadate;
	model stockret = mktret;
quit;

proc sort data=beta3 nodupkey; by permno_adj datadate; run;

proc sql;
    create table comp3 as select a.*, b.mktret as beta_erc
    from comp3 a left join beta3 b
    on (a.permno_adj = b.permno_adj) and (a.datadate = b.datadate)
    order by permno_adj, datadate desc;
quit;

proc sql;
    create table beta2 as select distinct permno_adj, datadate, cv(stockprice)/100 as retvol 
    from beta1 group by permno_adj, datadate;
quit;

proc sort data=beta2 nodupkey; by permno_adj datadate; run;

proc sql;
    create table comp3 as select a.*, b.retvol
    from comp3 a left join beta2 b
    on (a.permno_adj = b.permno_adj) and (a.datadate = b.datadate)
    order by permno_adj, datadate desc;
quit;

proc sort data=comp3 nodupkey; by gvkey datadate; run;

/* Create daily return and volume database */
* Add firm returns and volume;
proc sql;
    create table comp4 as select a.*, abs(b.prc) as prc, b.ret, b.vol, b.shrout, b.date
	from comp3 a left join crsp.dsf(where=((year(date) between 1979 and 2023))) b
	on (a.permno_adj = b.permno) and a.fyear = year(b.date)
	where (a.permno_adj = b.permno) and a.fyear = year(b.date)
	order by permno_adj, datadate, date desc;
quit;

* Add market returns;
proc sql;
    create table comp5 as select a.vwretd as mktret, b.*
    from crsp.dsi(where=((year(date) between 1979 and 2023))) a left join comp4 b
	on (b.date eq a.date) where (b.date eq a.date)      
	order by permno_adj, datadate, date desc;
quit;

proc delete data=comp comp2 comp3 comp4 beta beta1 beta2 beta3; run;

* Add patent awards;
proc sql; create table comp5 as select a.*, b.*
    from comp5 as a left join contrawards as b
    on (a.permno_adj = b.permno_adj) and (a.date = b.granteddate)
	order by permno_adj, datadate, date desc;
quit;

proc sort data=comp5 nodupkey; by permno_adj datadate date; run;
proc download data=comp5 out=ret; run;
endrsubmit;

proc export data = ret
            outfile = "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research\Data\ret_patents.dta" 
            dbms = stata replace;
run;

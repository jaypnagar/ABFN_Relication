options ls=120 nocenter ps=max formdlim=' ' nonumber mprint nolabel nodate;
title;
libname c '.';


proc format;
    value indfmt
     0='Not assigned'
     1='Mining/Construction'
     2='Food'
     3='Textiles/Print/Publish'
     4='Chemicals'
     5='Pharmaceuticals'
     6='Extractive'
     7='Manf:Rubber/glass/etc'
     8='Manf:Metal'
     9='Manf:Machinery'
    10='Manf:ElectricalEqpt'
    11='Manf:TransportEqpt'
    12='Manf:Instruments'
    13='Manf:Misc.'
    14='Computers'
    15='Transportation'
    16='Utilities'
    17='Retail:Wholesale'
    18='Retail:Misc.'
    19='Retail:Restaurant'
    20='Financial'
    21='Insurance/RealEstate'
    22='Services';
run;


%macro firmreg(dset=, yvar=, xvars=);

proc sort data=&dset;
    by gvkey;
run;

proc glm data=&dset;
    absorb gvkey;
    class fyear;
    model &yvar = &xvars fyear / solution;
    ods select NObs FitStatistics ParameterEstimates;
run;

%mend firmreg;

%macro clustreg(dset, yvar, xvars);

title2 "Y = &yvar";

proc surveyreg data=&dset; 
    cluster fyear ind;
    class fyear ind;
    model &yvar= &xvars fyear ind / solution;
    ods select DataSummary FitStatistics;
    ods output ParameterEstimates = xpe;
    run;

proc print data=xpe noobs;
    var Parameter Estimate tValue;
    where substr(Parameter, 1, 3) not in ('ind' 'fye' 'Int');
    run;

%mend clustreg;



%macro logitreg(dset, yvar, xvars);

proc logistic data=&dset descending;
    class fyear ind / param = ref ref=first;
    model &yvar = &xvars fyear ind / rsquare ridging=absolute;
    ods select NObs RSquare;
    ods output ParameterEstimates = xpe;
    run;

data xpe;
    set xpe;
    tValue = Estimate / Stderr;
    run;

proc print data=xpe noobs;
    var Variable Estimate tValue;
    where substr(Variable, 1, 3) not in ('ind' 'yea' 'Int');
    run;

%mend;








*****************************************
Winsorize macro
* byvar = none for no byvar;
* type  = delete/windsor;
****************************************;

%macro winsor(dsetin=, dsetout=, byvar=none, vars=, type=winsor, pctl=1 99);

%if &dsetout = %then %let dsetout = &dsetin;
    
%let varL=;
%let varH=;
%let xn=1;

%do %until ( %scan(&vars,&xn)= );
    %let token = %scan(&vars,&xn);
    %let varL = &varL &token.L;
    %let varH = &varH &token.H;
    %let xn=%EVAL(&xn + 1);
%end;

%let xn=%eval(&xn-1);

data xtemp;
    set &dsetin;
    run;

%let dropvar = ;
%if &byvar = none %then %do;

    data xtemp;
        set xtemp;
        xbyvar = 1;
        run;

    %let byvar = xbyvar;
    %let dropvar = xbyvar;

%end;

proc sort data = xtemp;
    by &byvar;
    run;

proc univariate data = xtemp noprint;
    by &byvar;
    var &vars;
    output out = xtemp_pctl PCTLPTS = &pctl PCTLPRE = &vars PCTLNAME = L H;
    run;

data &dsetout;
    merge xtemp xtemp_pctl;
    by &byvar;
    array trimvars{&xn} &vars;
    array trimvarl{&xn} &varL;
    array trimvarh{&xn} &varH;

    do xi = 1 to dim(trimvars);

        %if &type = winsor %then %do;
            if trimvars{xi} ne . then do;
              if (trimvars{xi} < trimvarl{xi}) then trimvars{xi} = trimvarl{xi};
              if (trimvars{xi} > trimvarh{xi}) then trimvars{xi} = trimvarh{xi};
            end;
        %end;

        %else %do;
            if trimvars{xi} ne . then do;
              if (trimvars{xi} < trimvarl{xi}) then delete;
              if (trimvars{xi} > trimvarh{xi}) then delete;
            end;
        %end;

    end;
    drop &varL &varH &dropvar xi;
    run;

%mend winsor;


******************************
Add industries
*****************************;

%macro addinds(dset=,newvarname=);

data &dset;
    set &dset;
    &newvarname = 0;
    if sic ge 1000 and sic le 1999 then &newvarname = 1;
    if sic ge 2000 and sic le 2111 then &newvarname = 2;
    if sic ge 2200 and sic le 2780 then &newvarname = 3;
    if sic ge 2800 and sic le 2824 then &newvarname = 4;
    if sic ge 2840 and sic le 2899 then &newvarname = 4;
    if sic ge 2830 and sic le 2836 then &newvarname = 5;
    if sic ge 2900 and sic le 2999 then &newvarname = 6;
    if sic ge 1300 and sic le 1399 then &newvarname = 6;
    if sic ge 3000 and sic le 3299 then &newvarname = 7;
    if sic ge 3300 and sic le 3499 then &newvarname = 8;
    if sic ge 3500 and sic le 3599 then &newvarname = 9;
    if sic ge 3600 and sic le 3699 then &newvarname = 10;
    if sic ge 3700 and sic le 3799 then &newvarname = 11;
    if sic ge 3800 and sic le 3899 then &newvarname = 12;
    if sic ge 3900 and sic le 3999 then &newvarname = 13;
    if sic ge 3570 and sic le 3579 then &newvarname = 14;
    if sic ge 3670 and sic le 3679 then &newvarname = 14;
    if sic ge 4000 and sic le 4899 then &newvarname = 15;
    if sic ge 4900 and sic le 4999 then &newvarname = 16;
    if sic ge 5000 and sic le 5199 then &newvarname = 17;
    if sic ge 5200 and sic le 5999 then &newvarname = 18;
    if sic ge 5800 and sic le 5899 then &newvarname = 19;
    if sic ge 6000 and sic le 6411 then &newvarname = 20;
    if sic ge 6500 and sic le 6999 then &newvarname = 21;
    if sic ge 7000 and sic le 8999 then &newvarname = 22;
    if sic ge 7370 and sic le 7379 then &newvarname = 14;
    run;


proc format;
    value indfmt
         0='Not assigned'
         1='Mining/Construction'
         2='Food'
         3='Textiles/Print/Publish'
         4='Chemicals'
         5='Pharmaceuticals'
         6='Extractive'
         7='Manf:Rubber/glass/etc'
         8='Manf:Metal'
         9='Manf:Machinery'
        10='Manf:ElectricalEqpt'
        11='Manf:TransportEqpt'
        12='Manf:Instruments'
        13='Manf:Misc.'
        14='Computers'
        15='Transportation'
        16='Utilities'
        17='Retail:Wholesale'
        18='Retail:Misc.'
        19='Retail:Restaurant'
        20='Financial'
        21='Insurance/RealEstate'
        22='Services';
    run;

%mend;



%macro corrps(dset=,vars=);

%let i=1;
%do %until (%SCAN(&vars,&i,%STR( ))=);
    %let v&i=%SCAN(&vars,&i,%STR( ));
    %let i=%EVAL(&i+1);
%end;
%let nvars = %eval(&i - 1);

    %macro cor_rep(sta,fin);
    %do i=&sta %to &fin;
        , (xp.&&v&i + xs.&&v&i) as &&v&i
    %end;
    %mend;

    %macro cor_rep_p(sta,fin);
    %do i=&sta %to &fin;
        , (xp.P&&v&i + xs.P&&v&i) as &&v&i
    %end;
    %mend;

ods listing close;
proc corr data=&dset Pearson Spearman;
    var &vars;
    ods output PearsonCorr = xp SpearmanCorr = xs;
run;
ods listing;

* Pearson;
data xp;
    set xp;
	array vars[*] _numeric_;
	* Set bottom correlations to 0;
	do i = 1 to _N_;
        vars[i] = 0;
    end;
	* Set bottom p-values to 0;
	do i = (1 + &nvars) to (_N_ + &nvars);
		vars[i] = 0;
	end;
	* Set diagonal correlations to 1;
	do i = _N_ to _N_;
		vars[i] = 1;
	end;
	* Set diagonal p-values to missing;
	do i = (_N_ + &nvars) to (_N_ + &nvars);
		vars[i] = .;
	end;
run;

* Spearman;
data xs;
    set xs;
	array vars[*] _numeric_;
	* Set top correlations to 0;
	do i = _N_ to &nvars;
        vars[i] = 0;
    end;
	* Set top p-values to 0;
	do i =(_N_ + &nvars) to (&nvars + &nvars);
		vars[i] = 0;
	end;
run;

* Combine correlations;
proc sql;
    create table xcorr as
    select xp.Variable %cor_rep(1,&nvars)
    from xp, xs where (xp.Variable = xs.Variable);
quit;

proc print data=xcorr noobs;
    title2 'Pearson (above) / Spearman (below) Correlations';
run;

* Combine p-values;
proc sql;
    create table xcorr as
    select xp.Variable %cor_rep_p(1,&nvars)
    from xp, xs where (xp.Variable = xs.Variable);
quit;

proc print data=xcorr noobs;
    title2 'Pearson (above) / Spearman (below) P-Values';
run;

title2;

%mend;


%macro get_ecc(dset=, firstyr=, lastyr=);

* Prepare translation file;
proc sort data=crsp.stocknames(keep=cusip ncusip where=(ncusip ne '')) out=xnames nodupkey;
    by cusip ncusip;
run;

*** Get IBES data;
proc sql undo_policy = none;
    
    * Get eps forecasts 1 year out (June); * December;
    create table xibes as select ticker, cusip, medest as fp1, actual as eps, statpers
    from ibes.statsum_epsus(keep = ticker cusip statpers fpi medest actual
                            where = ((fpi = '1') and
                             (month(statpers) = 12) and
                             (year(statpers) between &firstyr and &lastyr) and
                             (not missing(actual)) and (not missing(cusip))));

    * Add eps forecasts 2 years out;
    create table xibes as select a.*, b.medest as fp2
    from xibes a left join ibes.statsum_epsus b
    on (a.ticker   = b.ticker) and
       (a.statpers = b.statpers) and
       (b.fpi      = '2');

    * Add eps forecasts 3 years out;
    create table xibes as select a.*, b.medest as fp3
    from xibes a left join ibes.statsum_epsus b
    on (a.ticker   = b.ticker) and
       (a.statpers = b.statpers) and
       (b.fpi      = '3');

    * Add long-term growth forecasts;
    create table xibes as select a.*, b.medest/100 as ltg
    from xibes a left join ibes.statsum_epsus b
    on (a.ticker   = b.ticker) and
       (a.statpers = b.statpers) and
       (b.fpi      = '0') and
       (b.fiscalp  = 'LTG');

    * Add identifiers;
    create table xibes as select a.*, substr(b.cusip,1,6) as cusip6 from
    xibes a left join xnames b
    on (a.cusip = b.ncusip);

    * Add stock price;
    create table xibes as select a.*, b.price, b.shout as shares, year(a.statpers) as year
    from xibes a left join ibes.actpsum_epsus b
    on (a.ticker   = b.ticker) and
       (a.statpers = b.statpers)
    where (price > 0) and (shares > 0);
quit;

proc sort data=xibes nodupkey;
    by ticker year;
run;


*** Create dataset of Compustat data;    
data xcstat;
    set compx.funda(keep = gvkey datadate fyear cusip sich at dvc ceq indfmt datafmt popsrc consol curcd);
    where (fyear between &firstyr and &lastyr)
    and (indfmt='INDL') and (datafmt='STD') and (popsrc='D') and (consol='C') and (curcd='USD') and (at > 0);
    rename dvc = div ceq = bve;
    cusip6 = substr(cusip,1,6);
    drop indfmt datafmt popsrc consol curcd cusip;
run;

proc sort data=xcstat nodupkey;
    by gvkey datadate;
    run;

data xcstat;
    set xcstat;
    if gvkey = lag1(gvkey) and intck('MONTH', lag1(datadate), datadate) = 12 then m1 = 1; else m1 = .;
    bvem1 = lag(bve)*m1;
    divm1 = lag(div)*m1;
    atm1  = lag(at)*m1;

    if bvem1 <= 0 then delete;
    if divm1 <  0 then delete;

    ind = int(sich/100);

    drop m1 bve div;
run;

* Add IBES to Compustat;
proc sql undo_policy = none;

    create table xcstat as select a.*, b.eps as epsm1, (b.eps*b.shares/a.bvem1) as roem1
    from xcstat a left join xibes b
    on (a.cusip6 = b.cusip6) and (a.fyear-1 = b.year);
    
    create table xcstat as select a.*, b.fp1, b.fp2, fp3, b.ltg, b.price, b.shares
    from xcstat a left join xibes b
    on (a.cusip6 = b.cusip6) and (a.fyear = b.year);
quit;

proc sort data=xcstat nodupkey;
    by gvkey datadate;
run;

data xcstat;
    set xcstat;
    array vars fp1 fp2 ltg price shares ind;
    do over vars;
      if missing(vars) then delete;
    end;
    if roem1 < 0 then roem1 = .; * exclude losses from median roe calculation;
run;

proc sort data=xcstat;
    by ind fyear;
run;
    
proc means data=xcstat noprint;
    by ind fyear;
    var roem1;
    output out=xmedians median=medroe;
run;
        
proc sql undo_policy = none;

    create table xcstat as select a.*, b.rf/100 as rf
    from xcstat a left join xrates b
    on (a.fyear = b.year);

    create table xcstat as select a.*, a.rf - b.rf/100 as rfd1
    from xcstat a left join xrates b
    on (a.fyear = (b.year+1));

    create table xcstat as select a.*, b.medroe
    from xcstat a left join xmedians b
    on (a.ind = b.ind) and (a.fyear = b.fyear)
    order by gvkey, fyear;
quit;

data xcstat; 
    set xcstat;

    bpsm1 = bvem1 / shares;
    dpsm1 = divm1 / shares;
    if missing(fp3) then fp3 = fp2 * (1+ltg);
    fp4 = fp3 * (1+ltg);
    fp5 = fp4 * (1+ltg);
    if epsm1 > 0 then k = dpsm1 / epsm1; else k = 0.06 * atm1;
    if k > 1 then k = 1;
    g = rf - 0.03;

    * Gebhardt, Lee, and Swaminathan (GL, 2001) T=5;

    bps1_gl = bpsm1   + fp1*(1-k);
    bps2_gl = bps1_gl + fp2*(1-k);
    bps3_gl = bps2_gl + fp3*(1-k);
    froe1 = fp1 / bpsm1;
    froe2 = fp2 / bps1_gl;
    froe3 = fp3 / bps2_gl;
    froe4 = (froe3 + medroe) / 2;
    froe5 = medroe;
    bps4_gl = bps3_gl + bps3_gl*froe4*(1-k);   * not clear from paper;
    r_gl = 0.12;

    * Claus and Thomas (CT, 2001);

    bps1_ct = bpsm1   + 0.5 * fp1;
    bps2_ct = bps1_ct + 0.5 * fp2;
    bps3_ct = bps2_ct + 0.5 * fp3;    
    bps4_ct = bps3_ct + 0.5 * fp4;
    r_ct = 0.12;

    * Gode and Mohanram (GM, 2003);

    a = 0.5 * (g + dpsm1 / price);
    if min(fp1,fp2) > 0 then g_gm = (fp2 - fp1) / fp1; else g_gm = .;
    if g_gm >= g        then r_gm = a + sqrt(a**2 + (fp1 / price) * (g_gm - g)); else r_gm = .;

    * Easton (EA, 2004);

    r_ea = 0.12;

    run;

options nonotes; ods listing close;
proc model data=xcstat noprint maxerrors=1;
    by gvkey fyear;
*    bounds 0 <= r_gl r_ct r_ea <= 1;

    * Gebhardt, Lee, and Swaminathan (GL, 2001);
    eq.gl = -1*price + bpsm1 + bpsm1  *(froe1-r_gl)/(1+r_gl)    + bps1_gl*(froe2-r_gl)/(1+r_gl)**2
                             + bps2_gl*(froe3-r_gl)/(1+r_gl)**3 + bps3_gl*(froe4-r_gl)/(1+r_gl)**4
                             + bps4_gl*(froe5-r_gl)/(r_gl*(1+r_gl)**4);

    * Claus and Thomas (CT, 2001);
    eq.ct = -1*price + bpsm1 + (fp1-r_ct*bpsm1  )/(1+r_ct)    + (fp2-r_ct*bps1_ct)/(1+r_ct)**2 + (fp3-r_ct*bps2_ct)/(1+r_ct)**3 
                             + (fp4-r_ct*bps3_ct)/(1+r_ct)**4 + (fp5-r_ct*bps4_ct)/(1+r_ct)**5
                             + (fp5-r_ct*bps4_ct)*(1+g)/((r_ct-g)*(1+r_ct)**5);

    * Easton (EA, 2004);
    eq.ea = -1*r_ea*price + (fp2+r_ea*dpsm1-fp1)/(r_ea);

    solve r_gl r_ct r_ea /  out=xresults maxiter = 150 maxsubit = 20;
    id gvkey fyear r_gl r_ct r_ea;
    run;
options notes; ods listing;

data xcstat;
    merge xcstat xresults;
    by gvkey fyear;
    if (fp1 <= 0) or (fp2 < fp1) then r_ea = .;
    array vars r_gl r_ct r_gm r_ea;
    do over vars;
        if vars < 0.0 then vars = .;
        if vars > 0.5 then vars = .;
    end;
    ecc = mean(r_gl, r_ct, r_gm, r_ea);
    gvkey_n = 1 * gvkey;
run;

proc sql undo_policy=none;
    create table &dset as select a.*, b.r_gl, b.r_ct, b.r_gm, b.r_ea, b.ecc
    from &dset a left join xcstat b
    on (a.gvkey = b.gvkey_n) and (a.datadate = b.datadate);
quit;

%mend;




* Add CRSP returns, prices, etc.;

%macro get_crsp(dsetin=, dsetout=, refdate=, permno=,
                price=, priceadj=1,
                ret=, dlret=,
                abret=, dlabret=, ermport=1,
                std=,
                dlyearge=1900, dlyearle=9999);

data xtemp; 
    set &dsetin (keep = gvkey datadate &permno &refdate);
    %if &refdate = %then linkdate = datadate;
                   %else linkdate = &refdate;;
    run;

%*** Get PERMNO if needed;

%if &permno ne %then %do;

  data xtemp;
      set xtemp;
      rename &permno = npermno;
      firstdt = -9999;
      run; 

  %let drop_permno = npermno;

%end;
%else %do;

  %* Get PERMNO and corresponding valid date range;
    proc sql undo_policy=none;
      create table xtemp as
      select a.*, b.lpermno as npermno, b.linkdt as firstdt
      from xtemp a left join crspl.ccmxpf_linktable b
      on (a.gvkey = b.gvkey)
      where (b.linktype in ("LU", "LC", "LD", "LF", "LN", "LO", "LS", "LX")) and
            (b.linkdt <= a.linkdate or b.linkdt = .B) and
            (a.linkdate <= b.linkenddt or b.linkenddt =.E);
      quit;

  %let drop_permno = ;

%end;

proc sort data=xtemp;
    by gvkey datadate;
    run;


%*** Get PRICE if needed, skip to RET otherwise;

%if %SCAN(&price,1,%STR( ))= %then %goto RET;

%scan_range_price(range=&price);
%var_suffix_price;
%printdates_price;

data xtemp0;
    set xtemp(keep = gvkey datadate npermno linkdate firstdt);
    &printdates;
    run;

%* Get stock price data from crsp;

%if &priceadj=1 %then %do;

  proc sql undo_policy = none;
      create table xtemp0 as select
      a.*, b.date as crspdate, (abs(b.prc)/b.cfacpr) as price, (b.shrout*b.cfacshr/1000) as shares
      from xtemp0 a left join crsp.msf b on
      (a.npermno = b.permno) and (intck('Month',a.cstdate,b.date) in (&price)) and
      (a.firstdt <= b.date);
      quit;

%end;
%else %do;

  proc sql undo_policy = none;
      create table xtemp0 as select
      a.*, b.date as crspdate, abs(b.prc) as prc, b.shrout, b.cfacpr, b.cfacshr
      from xtemp0 a left join crsp.msf b on
      (a.npermno = b.permno) and (intck('Month',a.cstdate,b.date) in (&price)) and
      (a.firstdt <= b.date);
      quit;

%end;

%* Merge the months with the dataset and rename monthly data appropriately;

%let xtempmerge=;

%do i=1 %to &num_months;

  %if &priceadj=1 %then %do;

  proc sql undo_policy = none;
      create table xtemp&i as
      select gvkey, datadate, price as price_&&monsuf&i, shares as shares_&&monsuf&i
      from xtemp0 where intck('Month',cstdate,crspdate)=&&month&i
      order by gvkey, datadate;
      quit;

  %end;
  %else %do;

  proc sql undo_policy = none;
      create table xtemp&i as
      select gvkey, datadate, prc as prc_&&monsuf&i, shrout as shrout_&&monsuf&i,
             cfacpr as cfacpr_&&monsuf&i, cfacshr as cfacshr_&&monsuf&i
      from xtemp0 where intck('Month',cstdate,crspdate)=&&month&i
      order by gvkey, datadate;
      quit;

  %end;

  %let xtempmerge=&xtempmerge xtemp&i;

%end;

%* Create output dataset;
data xtemp;
    merge xtemp &xtempmerge;
    by gvkey datadate;
    run;

%* Finished with PRICE portion of macro;



%*** Get RET if needed, skip to ABRET otherwise;
%RET:
%if %SCAN(&ret,1,%STR( ))= %then %goto ABRET;

%scan_range_ret(range=&ret);
%var_suffix_ret;
%printdates_ret;

data xtemp0;
    set xtemp(keep = gvkey datadate npermno linkdate firstdt);
    &printdates;
    run;

%* Get monthly returns from CRSP over the range selected;
proc sql undo_policy = none;
    create table xtemp0 as select
    a.*, b.ret, b.date as crspdate
    from xtemp0 a left join crsp.msf b
    on (a.npermno = b.permno) and
       (a.start&min <= b.date <= a.end&max) and
       (a.firstdt <= b.date)
    where (ret >= -1);
    quit;

%* Calculate compounded monthly returns for each range;
%let xtempmerge=;

%do i=1 %to &num_range;

  proc sql undo_policy = none;
      create table xtemp&i as select distinct
      gvkey, datadate, exp(sum(log(1+ret)))-1 as ret_&&stsuf&i.._&&endsuf&i
      from xtemp0(where=((start&i <= crspdate <= end&i)))
      group by gvkey, datadate
      having n(ret) = (intck('MONTH',start&i,end&i) + 1)
      order by gvkey, datadate;
      quit;

  %let xtempmerge=&xtempmerge xtemp&i;

%end;

%* Create output dataset;
data xtemp;
    merge xtemp &xtempmerge;
    by gvkey datadate;
    run;
    
proc datasets;
    delete &xtempmerge;
    run;
    
%* Finished with RET portion of macro;

    

%*** Get ABRET if needed, skip to DLRET otherwise;
%ABRET:
%if %SCAN(&abret,1,%STR( ))= %then %goto DLRET;

%scan_range_ret(range=&abret);
%var_suffix_ret;
%printdates_ret;

data xtemp0;
    set xtemp(keep = gvkey datadate npermno linkdate firstdt);
    &printdates;
    run;

%* Get monthly returns from CRSP over the range selected;
proc sql undo_policy = none;
    create table xtemp0 as select
    a.*, b.ret, b.decret, b.date as crspdate
    from xtemp0 a left join crsp.ermport&ermport b
    on (a.npermno = b.permno) and
       (a.start&min <= b.date <= a.end&max) and
       (a.firstdt <= b.date)
    where (ret >= -1) and (decret >= -1);
    quit;

%* Calculate compounded monthly returns for each range;
%let xtempmerge=;

%do i=1 %to &num_range;

  proc sql undo_policy = none;
      create table xtemp&i as select distinct
      gvkey, datadate, exp(sum(log(1+ret))) - exp(sum(log(1+decret))) as abret_&&stsuf&i.._&&endsuf&i
      from xtemp0(where=((start&i <= crspdate <= end&i)))
      group by gvkey, datadate
      having n(ret) = (intck('MONTH',start&i,end&i) + 1)
      order by gvkey, datadate;
      quit;
    
  %let xtempmerge=&xtempmerge xtemp&i;

%end;

%* Create output dataset;
data xtemp;
    merge xtemp &xtempmerge;
    by gvkey datadate;
    run;

proc datasets;
    delete &xtempmerge;
    run;
    
%* Finished with ABRET portion of macro;



%*** Get DLRET if needed, skip to DLABRET otherwise;
%DLRET:
%if %SCAN(&dlret,1,%STR( ))= %then %goto DLABRET;

%scan_range_ret(range=&dlret);
%var_suffix_ret;
%printdates_ret(startdate='BEGINNING');

data xtemp0;
    set xtemp(keep = gvkey datadate npermno linkdate firstdt); 
    &printdates;
    run;

proc sql undo_policy = none;

%* Get delisting information (follows Beaver, McNichols, Price 2007);
    create table xdlst as select a.*, b.date as dldate, b.dlpdt, b.dlstcd, b.dlret
    from xtemp0 a inner join crsp.mse b
    on (a.npermno = b.permno)
    where (b.dlstcd > 199) and
          (a.start&min <= b.date <= a.end&max) and
          (a.firstdt <= b.date);

%* Calculate missing delisting return substitution values;
    create table avg_dlret as select distinct dlstcd, mean(dlret) as rv
    from crsp.dse(where = ((dlstcd > 199) and
                          (year(date) between &dlyearge and &dlyearle)))
    group by dlstcd;

    create table xdlst as select a.*, b.rv
    from xdlst a left join avg_dlret b
    on (a.dlstcd = b.dlstcd);

%* Get monthly returns from CRSP over the range selected;
    create table xtemp0 as select a.*, b.ret, b.date as crspdate
    from xtemp0 a left join crsp.msf b
    on (a.npermno = b.permno) and
       (a.start&min <= b.date <= a.end&max) and
       (a.firstdt <= b.date)
    where (ret >= -1);
    quit;


%* Calculate compounded monthly returns for each range;
%let xtempmerge=;

%do i=1 %to &num_range;

  proc sql undo_policy = none;

      create table xtemp&i as select
      a.*, b.dlret, b.dlstcd, b.rv, b.dldate, b.dlpdt
      from xtemp0(where=(start&i <= crspdate <= end&i)) a left join xdlst b
      on (a.npermno = b.npermno) and
         (month(a.crspdate) = month(b.dldate)) and
         (year(a.crspdate) = year(b.dldate));
      quit;

  data xtemp&i;
      set xtemp&i;
      
      %* Add substitution value;
      if not missing(dlstcd) and missing(dlret) then dlret=rv;
      else if not missing(dlstcd) and dlpdt <= dldate and not missing(dlret)
      then dlret=(1+dlret)*(1+rv)-1;

      %* Incorporate delistings;
      if not missing(dlstcd) and missing(ret) then ret=dlret;
      else if not missing(dlstcd) and not missing(ret)
      then ret=(1+ret)*(1+dlret)-1;
      run;

  proc sql undo_policy = none;
      create table xtemp&i as select distinct
      gvkey, datadate, exp(sum(log(1+ret)))-1 as dlret_&&stsuf&i.._&&endsuf&i
      from xtemp&i
      group by gvkey, datadate
      having (n(ret) = (intck('MONTH',start&i,end&i) + 1) or max(not missing(dlstcd)) > 0)
      order by gvkey, datadate;
      quit;

  %let xtempmerge=&xtempmerge xtemp&i;

%end;

%* Create output dataset;
data xtemp;
    merge xtemp &xtempmerge;
    by gvkey datadate;
    run;

proc datasets;
    delete &xtempmerge;
    run;
    
%* Finished with DLRET portion of macro;




%*** Get DLABRET if needed, skip to STD otherwise;
%DLABRET:
%if %SCAN(&dlabret,1,%STR( ))= %then %goto STD;

%scan_range_ret(range=&dlabret);
%var_suffix_ret;
%printdates_ret(startdate='BEGINNING');

data xtemp0;
    set xtemp(keep = gvkey datadate npermno linkdate firstdt);
    &printdates;
    run;

proc sql undo_policy = none;

%* Get delisting information;
    create table xdlst as select
    a.*, b.date as date_dl, b.dlpdt, b.dlstcd, b.dlret
    from xtemp0 a inner join crsp.mse b
    on (a.npermno = b.permno)
/*    where (b.dlstcd not in (., 100)) and */
    where (b.dlret > .) and
          (a.start&min <= b.date <= a.end&max) and
          (a.firstdt <= b.date);

%* Get monthly returns from CRSP over the range selected;
    create table xtemp0 as select
    a.*, b.ret, b.decret, b.date as crspdate
    from xdlst a left join crsp.ermport&ermport b
    on (a.npermno = b.permno) and
       (a.start&min <= b.date <= a.end&max) and
       (a.firstdt <= b.date <= a.lastdt)
    where (ret >= -1) and (decret >= -1);

    create table avg_dlret as select distinct
    dlstcd, n(dlret) as n_dlret, mean(dlret) as rv
    from crsp.dse (where = ((dlstcd not in (. .T .S .P 100)) and
                            (dlstcd >= &mindlstcd)           and
                            (year(date) between &dlyearge and &dlyearle)))
    group by dlstcd;

    create table xdlst as select
    a.*, b.rv
    from xdlst a left join avg_dlret b
    on (a.dlstcd = b.dlstcd);

    quit;

%get_last_decret;
    
%* Calculate compounded monthly returns for each range;
%let xtempmerge=;

%do i=1 %to &num_range;

    proc sql undo_policy = none; 
        create table xtemp&i as select
	a.*, b.crspdate, b.ret, b.decret, intck('MONTH', a.start&i, a.date_dl) as nmonths
	from xdlst a left join xtemp0 b
        on (a.gvkey    = b.gvkey)   and
           (a.npermno  = b.npermno) and
	   (a.datadate = b.datadate) 
        where (a.start&i <= b.crspdate <= a.end&i) and
              (a.start&i <= a.date_dl  <= a.end&i);

        create table xtemp&i as select
        a.*, b.crspdate, b.ret, b.decret, b.nmonths
        from xdlst (where = (date_dl between start&i and end&i)) a left join xtemp&i b
        on (a.gvkey    = b.gvkey)   and
           (a.npermno  = b.npermno) and
	   (a.datadate = b.datadate);

        quit;
        
    data xtemp&i;
	set xtemp&i;
	if crspdate = . and intck('MONTH',start&i,date_dl) = 0 then do;
	    crspdate=date_dl-1;
	    nmonths=1;
	    ret=0;
	    decret=0;
	end;
        run;

    %* determine whether delisting occurs on last trading day of month;

    proc sql undo_policy = none;

        create table delist_occurs as select
        gvkey, npermno, max(crspdate) as max_crspdate, datadate, date_dl
	from xtemp&i (where = (ret ne .))
	group by gvkey, npermno, datadate, date_dl;
        
        create table xtemp&i as select 
	a.*, b.max_crspdate
        from xtemp&i a left join delist_occurs b
        on (a.gvkey   = b.gvkey)   and
           (a.npermno = b.npermno) and
	   (a.datadate = b.datadate) and
           (a.date_dl = b.date_dl);

        create table xtemp_last as
        select a.*, b.capn
        from delist_occurs a left join crsp.mport&ermport b
        on (a.npermno = b.permno) and
           (year(a.date_dl) = b.year);
        
        create table xtemp_last as
        select a.*, b.decret as last_decret
        from xtemp_last a left join last_decret b
        on ( year(a.date_dl) =  year(b.caldt)) and 
           (month(a.date_dl) = month(b.caldt)) and
           (a.capn = b.capn);

	create table xtemp&i as select
	a.*, b.last_decret
	from xtemp&i a left join xtemp_last b
        on (a.gvkey   = b.gvkey)   and
           (a.npermno = b.npermno) and
	   (a.datadate = b.datadate);

        quit;

    data xtempb&i;
	set xtemp&i;
	if max_crspdate = date_dl then nobs=nmonths+1;
	else if max_crspdate < date_dl then nobs=nmonths;	
        run;
    
    proc sql undo_policy = none;
	create table xtempb&i as select distinct
	gvkey, npermno, datadate, max_crspdate, nobs, sum(log(1+ret)) as cumret, sum(log(1+decret)) as cumdecret,
	date_dl, dlret, rv, dlpdt, last_decret, dlstcd
	from xtempb&i
	group by gvkey, npermno, datadate, max_crspdate, nobs, date_dl, dlret, rv, dlpdt, last_decret, dlstcd
	having n(ret) = nobs
	order by gvkey, datadate;
	quit;

    data xtempb&i; 
	set xtempb&i;

        %* If delisting occurs on last trading day of month, decile return is already included;
	if max_crspdate = date_dl then do;
	    if dlret not in (. .S .T .P) then cumret = cumret + log(1+dlret);
                                 	 else cumret = cumret + log(1+rv);
	end;
	else if max_crspdate < date_dl then do;
	    if dlpdt > date_dl and dlret not in (. .S .T .P) then do;
		if dlret > -1 then cumret    = cumret + log(1+dlret);
		                    cumdecret = cumdecret + log(1+last_decret);
	    end;
	    else if dlpdt le date_dl and dlret not in (. .S .T .P) then do;
		if dlret > -1 then cumret = cumret + log(1+dlret) + log(1+rv);
		                    cumdecret = cumdecret + log(1+last_decret);
	    end;
	end;	    

        if dlret = -1 then dlabret_&&stsuf&i.._&&endsuf&i = 0 - exp(cumdecret);
                      else dlabret_&&stsuf&i.._&&endsuf&i = exp(cumret) - exp(cumdecret);    
    run;

    %let xtempmerge=&xtempmerge xtempb&i;

%end;

%* Create output dataset;
data xtemp;
    merge xtemp &xtempmerge;
    by gvkey datadate;
    drop max_crspdate nobs cumret cumdecret rv last_decret date_dl dlpdt dlret dlstcd;
    run;

proc datasets;
    delete &xtempmerge;
    run;
    
%* Finished with DLABRET portion of macro;




%*** Get STD if needed, skip to FINISH otherwise;
%STD:
%if %SCAN(&std,1,%STR( ))= %then %goto FINISH;

%scan_range_ret(range=&std);
%var_suffix_ret;
%printdates_ret(startdate='BEGINNING');

data xtemp0;
    set xtemp (keep = gvkey datadate npermno linkdate firstdt);
    &printdates;
    run;

%* Get monthly returns from CRSP over the range selected;
proc sql undo_policy = none;
    create table xtemp0 as select
    a.*, b.ret, b.date as crspdate
    from xtemp0 a left join crsp.msf b
    on (a.npermno = b.permno) and
       (a.start&min <= b.date <= a.end&max) and
       (a.firstdt <= b.date)
    where (ret >= -1);
    quit;

%* Calculate standard deviation for each range;
%let xtempmerge=;

%do i=1 %to &num_range;

  proc sql undo_policy = none;
      create table xtemp&i as select distinct
      gvkey, datadate, n(ret) as sigma_n_&&stsuf&i.._&&endsuf&i, std(log(1+ret)) as sigma_&&stsuf&i.._&&endsuf&i
      from xtemp0(where=((start&i <= crspdate) and (end&i >= crspdate)))
      group by gvkey, datadate
      order by gvkey, datadate;
      quit;

  %let xtempmerge=&xtempmerge xtemp&i;

%end;

%* Create output dataset;
data xtemp;
    merge xtemp &xtempmerge;
    by gvkey datadate;
    run;

%* Finished with STD portion of macro;

    

%*** Finish;
%FINISH:

%* Create final output dataset;

%if &dsetout = %then %let dsetout = &dsetin;

data &dsetout;
    merge &dsetin xtemp(drop = firstdt linkdate &drop_permno &refdate);
    by gvkey datadate;
    run;

proc datasets;
    delete xtemp;
    run;
        
%mend;



%macro scan_range_price(range=);

%* Scan the range string and put each start and end value in its own macro variable;

%global num_months;

%let j=1;
%do %until (%SCAN(&range,&j,%STR( ))=);
    %let j=%EVAL(&j+1);
%end;    
%let num_months=%EVAL(&j-1);

%global %global_vars(month,&num_months);

%let j=1;
%do %until (%SCAN(&range,&j,%STR( ))=);

    %let month&j=%SCAN(&range,&j,%STR( ));
    %let j=%EVAL(&j+1);

%end;    
%let num_months=%EVAL(&j-1);

%mend scan_range_price;



%macro scan_range_ret(range=);

%* Scan the range string and put each start and end value in its own macro variable;

%global num_range;

%let i=1;
%let j=1;
%do %until (%SCAN(&range,&j,%STR( ))=);
    %let i=%EVAL(&i+1);
    %let j=%EVAL(&j+2);
%end;    
%let num_range=%EVAL(&i-1);

%global %global_vars(start,&num_range) %global_vars(end,&num_range) min max;

%let i=1; 
%let j=1;
%do %until (%SCAN(&range,&j,%STR( ))=);

    %let start&i=%SCAN(&range,&j,%STR( ));
    %* determine the minimum relative month for downloading the
    %* necessary range of data to compute all requested returns;
    %if &j=1 %then %let min=&i;
    %else %if &&start&i < &&start&min %then %let min=&i;

    %let j=%EVAL(&j+1);
    %let end&i=%SCAN(&range,&j,%STR( ));
    %if &j=2 %then %let max=&i;
    %else %if &&end&i > &&end&max %then %let max=&i;

    %let i=%EVAL(&i+1); 
    %let j=%EVAL(&j+1);

%end;    

%mend scan_range_ret;



%macro var_suffix_price;

%* for naming price variables need absolute value of range month and will add m or p to indicate sign;

%global %global_vars(monsuf,&num_months);

%do y=1 %to &num_months;

    %if &&month&y < 0 %then %do;
        %let tmp = %EVAL(&&month&y * -1);
        %let monsuf&y = m&tmp;
    %end;
    %else %do;
        %let tmp = &&month&y;
        %let monsuf&y = p&tmp;
    %end;

%end;

%mend var_suffix_price;



%macro var_suffix_ret;

%* for naming return variables need absolute value of range month and will add m or p to indicate sign;

%global %global_vars(stsuf,&num_range) %global_vars(endsuf,&num_range);

%do y=1 %to &num_range;

    %if &&start&y < 0 %then %do;
        %let tmp = %EVAL(&&start&y * -1);
        %let stsuf&y = m&tmp;
    %end;
    %else %do;
        %let tmp = &&start&y;
        %let stsuf&y = p&tmp;
    %end;

    %if &&end&y < 0 %then %do;
        %let tmp = %EVAL(&&end&y * -1);
        %let endsuf&y = m&tmp;
    %end;
    %else %do;
        %let tmp = &&end&y;
        %let endsuf&y = p&tmp;
    %end;

%end;

%mend var_suffix_ret;



%macro printdates_price;

%* need to create code for data step to create variables for the requested start and end dates;

%global printdates;
%let printdates=%STR(cstdate=intnx('Month',linkdate,0 ,'MIDDLE'););

%mend printdates_price;



%macro printdates_ret(startdate='MIDDLE');

%* need to create code for data step to create variables for the requested start and end dates;

%global printdates;
%let printdates=;
%let i=1;

%do %until (&i > &num_range);
    %let printdates=%STR(&printdates start&i=intnx('Month',linkdate,&&start&i , &startdate);); 
    %let printdates=%STR(&printdates end&i=intnx('Month',linkdate,&&end&i ,'END'););
    %let i=%EVAL(&i+1);
%end;

%mend printdates_ret;





%macro global_vars(prefix,num);

%do i = 1 %to &num;
    &prefix.&i
%end;

%mend global_vars;




%macro get_last_decret;
    
%* the files mport1 -- mport5 contain portfolio assignments for each firm;
%* The files msia, msib, msic, msio and msix contain decile returns for each decile;

      %if &ermport=1 %then %let ermporta = x;
%else %if &ermport=2 %then %let ermporta = c;
%else %if &ermport=3 %then %let ermporta = o;
%else %if &ermport=4 %then %let ermporta = a;
%else %if &ermport=5 %then %let ermporta = b;

%do l=1 %to 10;
    data _m&l;
	set crsp.msi&ermporta;
        capn = &l;
        decret = decret&l;
        keep caldt capn decret;
 	run;
%end;

data last_decret;
    set _m1 _m2 _m3 _m4 _m5 _m6 _m7 _m8 _m9 _m10;
    run;    

%mend get_last_decret;


/*  
    Macro:          nAnalysts.sas
    Created by:     Joost Impink
    Date:           March 2014
  
    Macro to count the number of analysts following; the number of 
    analysts is measured as the number of analysts that have issued
    a forecast for the current  fiscal year, in the last month of 
    that fiscal year.
   
    Variables required:
        dsin            dataset in (needs to have key, gkvey, 
                        fyear, permno and datadate)
 
        dsout           dataset out
 
    The sample below constructs key as: key = gvkey || fyear; 
 
    Dependencies:
        - the macro requires local access to crsp.dsenames, 
          ibes.idsum and ibes.STATSUMU_EPSUS in a local libray
*/
  
%macro numest (dsin=, dsout=);
 
/* keep minimum number of vars to collect number of analysts*/
data num1 (keep = key permno datadate rdq);
set &dsin;
if permno ne .;
run;
 
/* retrieve historic cusip */
PROC SQL;
  create table num2 as
  select a.*, b.ncusip
  from num1 a, crsp.dsenames b
  where 
        a.permno = b.PERMNO
    and b.namedt <= a.datadate <= b.nameendt
    and b.ncusip ne "";
  quit;
 
/* force unique records */
proc sort data=num2 nodupkey; by key;run;
 
/* get ibes ticker */
PROC SQL;
  create table num3 as
  select distinct a.*, b.ticker as ibes_ticker
  from num2 a, ibes.idsum b
  where 
        a.NCUSIP = b.CUSIP
    and a.datadate > b.SDATES 
;
quit;
 
/* get number of estimates -- last month of fiscal year*/
 
PROC SQL;
  create table num4 as
  select a.*, b.STATPERS, b.numest as num_analysts
  from num3 a, ibes.STATSUMU_EPSUS b
  where 
        a.ibes_ticker = b.ticker
    and b.MEASURE="EPS"
    and b.FISCALP="ANN"
    and b.FPI = "1"
    and a.datadate - 30 < b.STATPERS < a.datadate 
    and a.datadate -5 <= b.FPEDATS <= a.datadate +5
;
quit;
 
/* force unique records */
proc sort data=num4 nodupkey; by key;run;
 
proc sql;
    create table &dsout as 
    select a.*, b.num_analysts 
    from &dsin a 
    left join num4 b 
    on a.key=b.key;
quit;
 
/* missing num_analysts means no analysts following */
data &dsout;
set &dsout;
if num_analysts eq . then num_analysts = 0;
run;
 
/* cleanup */
proc datasets library=work;
   delete num1-num4;
run;
%mend;


/*  
    Macro:          nAnalysts.sas
    Created by:     Joost Impink
    Date:           March 2014
  
    Macro to count the number of analysts following; the number of 
    analysts is measured as the number of analysts that have issued
    a forecast for the current  fiscal year, in the last month of 
    that fiscal year.
   
    Variables required:
        dsin            dataset in (needs to have key, gkvey, 
                        fyear, permno and datadate)
 
        dsout           dataset out
 
    The sample below constructs key as: key = gvkey || fyear; 
 
    Dependencies:
        - the macro requires local access to crsp.dsenames, 
          ibes.idsum and ibes.STATSUMU_EPSUS in a local libray
*/
  
%macro numest2 (dsin=, dsout=);
 
/* keep minimum number of vars to collect number of analysts*/
data num1 (keep = key permno datadate rdq);
set &dsin;
if permno ne .;
run;
 
/* retrieve historic cusip */
PROC SQL;
  create table num2 as
  select a.*, b.ncusip
  from num1 a, crsp.dsenames b
  where 
        a.permno = b.PERMNO
    and b.namedt <= a.datadate <= b.nameendt
    and b.ncusip ne "";
  quit;
 
/* force unique records */
proc sort data=num2 nodupkey; by key;run;
 
/* get ibes ticker */
PROC SQL;
  create table num3 as
  select distinct a.*, b.ticker as ibes_ticker
  from num2 a, ibes.idsum b
  where 
        a.NCUSIP = b.CUSIP
    and a.datadate > b.SDATES 
;
quit;
 
/* get number of estimates -- last month of fiscal year*/
 
proc sql;
  create table num4 as
  select a.*, b.STATPERS, b.numest as num_analysts
  from num3 a, ibes.STATSUMU_EPSUS b
  where 
        a.ibes_ticker = b.ticker
    and b.MEASURE="EPS"
    and b.FISCALP="ANN"
    and b.FPI = "1"
    and a.datadate - 30 < b.STATPERS < a.datadate 
    and a.datadate -5 <= b.FPEDATS <= a.datadate +5
;
quit;
 
/* force unique records */
proc sort data=num4 nodupkey; by key;run;
 
proc sql;
    create table &dsout as 
    select a.*, b.num_analysts 
    from &dsin a 
    left join num4 b 
    on a.key=b.key;
quit;
 
/* missing num_analysts means no analysts following */
data &dsout;
set &dsout;
if num_analysts eq . then num_analysts = 0;
run;
 
/* cleanup */
proc datasets library=work;
   delete num1-num4;
run;
%mend;

/*Strips all formats and labels from the dataset*/
%Macro strip(data);
proc datasets lib=work memtype=data noprint;
   modify &data;
     attrib _all_ label=' ';
     attrib _all_ format=;
quit;
%mend;

*Macro to add lags and leads (negative lags). If the data is time-series, enter "none" as the cross sectional identifier;
*Programmer: Oliver Binz;
*Date: 08/20/2018;
*Cvar = Cross sectional identifier, Tvar = Time series identifier;
*Lag = # of lags to add, Lead = Indicator that you want leads instead of lags;
*Examples:
%Lags(in=ratio7,out=test,cvar=gvkey,tvar=fyear,vars=ROCE MSR ROTCE,lags=3,lead=0)
%Lags(in=ratio7,out=test,cvar=none,tvar=fyear,vars=ROCE MSR ROTCE,lags=3,lead=0);
options mprint;
%macro Lags(in,out,cvar,tvar,vars,lags,lead);
data &out; set &in;run;
%do i = 1 %to %sysfunc(countw(&vars));
%do j = 1 %to &lags;
	proc sql; create table &out
	as select a.* 
		%if &lead=0 %then %do;
			,b.%scan(&vars, &i, ' ') as %scan(&vars, &i, ' ')_lag&j
		%end;
		%if &lead=1 %then %do;
			,b.%scan(&vars, &i, ' ') as %scan(&vars, &i, ' ')_lead&j
		%end;
	from &out a
	left join &in b on 
	/*Only add cross sectional identifier when cvar is not equal to "none"*/
	%if &cvar~=none %then %do;
	a.&cvar=b.&cvar & 
	%end;
	%if &lead=0 %then %do;
		a.&tvar=b.&tvar+&j
	%end;
	%else %do;
		a.&tvar=b.&tvar-&j
	%end;
	;quit;
%end;%end;
%mend;

*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;
*CREATES FAMA-FRENCH 10 INDUSTRY CLASSIFICATIONS
*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;

%macro ff10(data=,newvarname=industry,sic=sic,out=&data);
data &out;
set &data;
if (100<=&sic<=999 | 2000<=&sic<=2399 | 2700<=&sic<=2749 | 2770<=&sic<=2799 | 3100<=&sic<=3199 | 3940<=&sic<=3989)
then &newvarname = 1;
else if (2500<=&sic<=2519 | 2590<=&sic<=2599 | 3630<=&sic<=3659 | 3710<=&sic<=3711 | &sic=3714 | &sic=3716 | 3750<=&sic<=3751| &sic=3792| 3900<=&sic<=3939| 3990<=&sic<=3999)	
then &newvarname = 2;
else if (2520<=&sic<=2589 | 2600<=&sic<=2699 | 2750<=&sic<=2769 | 2800<=&sic<=2829 | 2840<=&sic<=2899| 3000<=&sic<=3099 | 3200<=&sic<=3569 | 3580<=&sic<=3621 | 3623<=&sic<=3629 | 3700<=&sic<=3709 | 3712<=&sic<=3713 | &sic=3715 | 3717<=&sic<=3749 | 3752<=&sic<=3791 | 3793<=&sic<=3799 | 3860<=&sic<=3899)	
then &newvarname = 3;
else if (1200<=&sic<=1399 | 2900<=&sic<=2999)
then &newvarname = 4;
else if (3570<=&sic<=3579 | &sic=3622 | 3660<=&sic<=3692 | 3694<=&sic<=3699 | 3810<=&sic<=3839| 7370<=&sic<=7379 | &sic=7391 | 8730<=&sic<=8734)
then &newvarname = 5;
else if (4800<=&sic<=4899)
then &newvarname = 6;
else if (5000<=&sic<=5999 | 7200<=&sic<=7299 | 7600<=&sic<=7699)
then &newvarname = 7;
else if (2830<=&sic<=2839 | &sic=3693 | 3840<=&sic<=3859 | 8000<=&sic<=8099)
then &newvarname = 8;
else if (4900<=&sic<=4949)
then &newvarname = 9;
else &newvarname = 10;
run;

proc format;
value ff
 1	="Consumer NonDurables"
 2	="Consumer Durables"
 3	="Manufacturing"
 4	="Oil, Gas, and Coal Extraction and Products"
 5	="Business Equipment"
 6	="Telephone and Television Transmission"
 7	="Wholesale, Retail, and Some Services"
 8	="Healthcare, Medical Equipment, and Drugs"
 9	="Utilities"
10	="Other";
run;
%mend;


*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;
*CREATES FAMA-FRENCH 17 INDUSTRY CLASSIFICATIONS
*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;

%macro ff17(data=,newvarname=industry,sic=sic,out=&data);

data &out;
    set &data;
    &newvarname = 17;
if	100	<=	&sic	<=	199	then	&newvarname	=	17	;
else	if	200	<=	&sic	<=	299	then	&newvarname	=	1	;
else	if	700	<=	&sic	<=	799	then	&newvarname	=	1	;
else	if	900	<=	&sic	<=	999	then	&newvarname	=	1	;
else	if	2000	<=	&sic	<=	2009	then	&newvarname	=	1	;
else	if	2010	<=	&sic	<=	2019	then	&newvarname	=	1	;
else	if	2020	<=	&sic	<=	2029	then	&newvarname	=	1	;
else	if	2030	<=	&sic	<=	2039	then	&newvarname	=	1	;
else	if	2040	<=	&sic	<=	2046	then	&newvarname	=	1	;
else	if	2047	<=	&sic	<=	2047	then	&newvarname	=	1	;
else	if	2048	<=	&sic	<=	2048	then	&newvarname	=	1	;
else	if	2050	<=	&sic	<=	2059	then	&newvarname	=	1	;
else	if	2060	<=	&sic	<=	2063	then	&newvarname	=	1	;
else	if	2064	<=	&sic	<=	2068	then	&newvarname	=	1	;
else	if	2070	<=	&sic	<=	2079	then	&newvarname	=	1	;
else	if	2080	<=	&sic	<=	2080	then	&newvarname	=	1	;
else	if	2082	<=	&sic	<=	2082	then	&newvarname	=	1	;
else	if	2083	<=	&sic	<=	2083	then	&newvarname	=	1	;
else	if	2084	<=	&sic	<=	2084	then	&newvarname	=	1	;
else	if	2085	<=	&sic	<=	2085	then	&newvarname	=	1	;
else	if	2086	<=	&sic	<=	2086	then	&newvarname	=	1	;
else	if	2087	<=	&sic	<=	2087	then	&newvarname	=	1	;
else	if	2090	<=	&sic	<=	2092	then	&newvarname	=	1	;
else	if	2095	<=	&sic	<=	2095	then	&newvarname	=	1	;
else	if	2096	<=	&sic	<=	2096	then	&newvarname	=	1	;
else	if	2097	<=	&sic	<=	2097	then	&newvarname	=	1	;
else	if	2098	<=	&sic	<=	2099	then	&newvarname	=	1	;
else	if	5140	<=	&sic	<=	5149	then	&newvarname	=	1	;
else	if	5150	<=	&sic	<=	5159	then	&newvarname	=	1	;
else	if	5180	<=	&sic	<=	5182	then	&newvarname	=	1	;
else	if	5191	<=	&sic	<=	5191	then	&newvarname	=	1	;
else	if	1000	<=	&sic	<=	1009	then	&newvarname	=	2	;
else	if	1010	<=	&sic	<=	1019	then	&newvarname	=	2	;
else	if	1020	<=	&sic	<=	1029	then	&newvarname	=	2	;
else	if	1030	<=	&sic	<=	1039	then	&newvarname	=	2	;
else	if	1040	<=	&sic	<=	1049	then	&newvarname	=	2	;
else	if	1060	<=	&sic	<=	1069	then	&newvarname	=	2	;
else	if	1080	<=	&sic	<=	1089	then	&newvarname	=	2	;
else	if	1090	<=	&sic	<=	1099	then	&newvarname	=	2	;
else	if	1200	<=	&sic	<=	1299	then	&newvarname	=	2	;
else	if	1400	<=	&sic	<=	1499	then	&newvarname	=	2	;
else	if	5050	<=	&sic	<=	5052	then	&newvarname	=	2	;
else	if	1300	<=	&sic	<=	1300	then	&newvarname	=	3	;
else	if	1310	<=	&sic	<=	1319	then	&newvarname	=	3	;
else	if	1320	<=	&sic	<=	1329	then	&newvarname	=	3	;
else	if	1380	<=	&sic	<=	1380	then	&newvarname	=	3	;
else	if	1381	<=	&sic	<=	1381	then	&newvarname	=	3	;
else	if	1382	<=	&sic	<=	1382	then	&newvarname	=	3	;
else	if	1389	<=	&sic	<=	1389	then	&newvarname	=	3	;
else	if	2900	<=	&sic	<=	2912	then	&newvarname	=	3	;
else	if	5170	<=	&sic	<=	5172	then	&newvarname	=	3	;
else	if	2200	<=	&sic	<=	2269	then	&newvarname	=	4	;
else	if	2270	<=	&sic	<=	2279	then	&newvarname	=	4	;
else	if	2280	<=	&sic	<=	2284	then	&newvarname	=	4	;
else	if	2290	<=	&sic	<=	2295	then	&newvarname	=	4	;
else	if	2296	<=	&sic	<=	2296	then	&newvarname	=	4	;
else	if	2297	<=	&sic	<=	2297	then	&newvarname	=	4	;
else	if	2298	<=	&sic	<=	2298	then	&newvarname	=	4	;
else	if	2299	<=	&sic	<=	2299	then	&newvarname	=	4	;
else	if	2300	<=	&sic	<=	2390	then	&newvarname	=	4	;
else	if	2391	<=	&sic	<=	2392	then	&newvarname	=	4	;
else	if	2393	<=	&sic	<=	2395	then	&newvarname	=	4	;
else	if	2396	<=	&sic	<=	2396	then	&newvarname	=	4	;
else	if	2397	<=	&sic	<=	2399	then	&newvarname	=	4	;
else	if	3020	<=	&sic	<=	3021	then	&newvarname	=	4	;
else	if	3100	<=	&sic	<=	3111	then	&newvarname	=	4	;
else	if	3130	<=	&sic	<=	3131	then	&newvarname	=	4	;
else	if	3140	<=	&sic	<=	3149	then	&newvarname	=	4	;
else	if	3150	<=	&sic	<=	3151	then	&newvarname	=	4	;
else	if	3963	<=	&sic	<=	3965	then	&newvarname	=	4	;
else	if	5130	<=	&sic	<=	5139	then	&newvarname	=	4	;
else	if	2510	<=	&sic	<=	2519	then	&newvarname	=	5	;
else	if	2590	<=	&sic	<=	2599	then	&newvarname	=	5	;
else	if	3060	<=	&sic	<=	3069	then	&newvarname	=	5	;
else	if	3070	<=	&sic	<=	3079	then	&newvarname	=	5	;
else	if	3080	<=	&sic	<=	3089	then	&newvarname	=	5	;
else	if	3090	<=	&sic	<=	3099	then	&newvarname	=	5	;
else	if	3630	<=	&sic	<=	3639	then	&newvarname	=	5	;
else	if	3650	<=	&sic	<=	3651	then	&newvarname	=	5	;
else	if	3652	<=	&sic	<=	3652	then	&newvarname	=	5	;
else	if	3860	<=	&sic	<=	3861	then	&newvarname	=	5	;
else	if	3870	<=	&sic	<=	3873	then	&newvarname	=	5	;
else	if	3910	<=	&sic	<=	3911	then	&newvarname	=	5	;
else	if	3914	<=	&sic	<=	3914	then	&newvarname	=	5	;
else	if	3915	<=	&sic	<=	3915	then	&newvarname	=	5	;
else	if	3930	<=	&sic	<=	3931	then	&newvarname	=	5	;
else	if	3940	<=	&sic	<=	3949	then	&newvarname	=	5	;
else	if	3960	<=	&sic	<=	3962	then	&newvarname	=	5	;
else	if	5020	<=	&sic	<=	5023	then	&newvarname	=	5	;
else	if	5064	<=	&sic	<=	5064	then	&newvarname	=	5	;
else	if	5094	<=	&sic	<=	5094	then	&newvarname	=	5	;
else	if	5099	<=	&sic	<=	5099	then	&newvarname	=	5	;
else	if	2800	<=	&sic	<=	2809	then	&newvarname	=	6	;
else	if	2810	<=	&sic	<=	2819	then	&newvarname	=	6	;
else	if	2820	<=	&sic	<=	2829	then	&newvarname	=	6	;
else	if	2860	<=	&sic	<=	2869	then	&newvarname	=	6	;
else	if	2870	<=	&sic	<=	2879	then	&newvarname	=	6	;
else	if	2890	<=	&sic	<=	2899	then	&newvarname	=	6	;
else	if	5160	<=	&sic	<=	5169	then	&newvarname	=	6	;
else	if	2100	<=	&sic	<=	2199	then	&newvarname	=	7	;
else	if	2830	<=	&sic	<=	2830	then	&newvarname	=	7	;
else	if	2831	<=	&sic	<=	2831	then	&newvarname	=	7	;
else	if	2833	<=	&sic	<=	2833	then	&newvarname	=	7	;
else	if	2834	<=	&sic	<=	2834	then	&newvarname	=	7	;
else	if	2840	<=	&sic	<=	2843	then	&newvarname	=	7	;
else	if	2844	<=	&sic	<=	2844	then	&newvarname	=	7	;
else	if	5120	<=	&sic	<=	5122	then	&newvarname	=	7	;
else	if	5194	<=	&sic	<=	5194	then	&newvarname	=	7	;
else	if	800	<=	&sic	<=	899	then	&newvarname	=	8	;
else	if	1500	<=	&sic	<=	1511	then	&newvarname	=	8	;
else	if	1520	<=	&sic	<=	1529	then	&newvarname	=	8	;
else	if	1530	<=	&sic	<=	1539	then	&newvarname	=	8	;
else	if	1540	<=	&sic	<=	1549	then	&newvarname	=	8	;
else	if	1600	<=	&sic	<=	1699	then	&newvarname	=	8	;
else	if	1700	<=	&sic	<=	1799	then	&newvarname	=	8	;
else	if	2400	<=	&sic	<=	2439	then	&newvarname	=	8	;
else	if	2440	<=	&sic	<=	2449	then	&newvarname	=	8	;
else	if	2450	<=	&sic	<=	2459	then	&newvarname	=	8	;
else	if	2490	<=	&sic	<=	2499	then	&newvarname	=	8	;
else	if	2850	<=	&sic	<=	2859	then	&newvarname	=	8	;
else	if	2950	<=	&sic	<=	2952	then	&newvarname	=	8	;
else	if	3200	<=	&sic	<=	3200	then	&newvarname	=	8	;
else	if	3210	<=	&sic	<=	3211	then	&newvarname	=	8	;
else	if	3240	<=	&sic	<=	3241	then	&newvarname	=	8	;
else	if	3250	<=	&sic	<=	3259	then	&newvarname	=	8	;
else	if	3261	<=	&sic	<=	3261	then	&newvarname	=	8	;
else	if	3264	<=	&sic	<=	3264	then	&newvarname	=	8	;
else	if	3270	<=	&sic	<=	3275	then	&newvarname	=	8	;
else	if	3280	<=	&sic	<=	3281	then	&newvarname	=	8	;
else	if	3290	<=	&sic	<=	3293	then	&newvarname	=	8	;
else	if	3420	<=	&sic	<=	3429	then	&newvarname	=	8	;
else	if	3430	<=	&sic	<=	3433	then	&newvarname	=	8	;
else	if	3440	<=	&sic	<=	3441	then	&newvarname	=	8	;
else	if	3442	<=	&sic	<=	3442	then	&newvarname	=	8	;
else	if	3446	<=	&sic	<=	3446	then	&newvarname	=	8	;
else	if	3448	<=	&sic	<=	3448	then	&newvarname	=	8	;
else	if	3449	<=	&sic	<=	3449	then	&newvarname	=	8	;
else	if	3450	<=	&sic	<=	3451	then	&newvarname	=	8	;
else	if	3452	<=	&sic	<=	3452	then	&newvarname	=	8	;
else	if	5030	<=	&sic	<=	5039	then	&newvarname	=	8	;
else	if	5070	<=	&sic	<=	5078	then	&newvarname	=	8	;
else	if	5198	<=	&sic	<=	5198	then	&newvarname	=	8	;
else	if	5210	<=	&sic	<=	5211	then	&newvarname	=	8	;
else	if	5230	<=	&sic	<=	5231	then	&newvarname	=	8	;
else	if	5250	<=	&sic	<=	5251	then	&newvarname	=	8	;
else	if	3300	<=	&sic	<=	3300	then	&newvarname	=	9	;
else	if	3310	<=	&sic	<=	3317	then	&newvarname	=	9	;
else	if	3320	<=	&sic	<=	3325	then	&newvarname	=	9	;
else	if	3330	<=	&sic	<=	3339	then	&newvarname	=	9	;
else	if	3340	<=	&sic	<=	3341	then	&newvarname	=	9	;
else	if	3350	<=	&sic	<=	3357	then	&newvarname	=	9	;
else	if	3360	<=	&sic	<=	3369	then	&newvarname	=	9	;
else	if	3390	<=	&sic	<=	3399	then	&newvarname	=	9	;
else	if	3410	<=	&sic	<=	3412	then	&newvarname	=	10	;
else	if	3443	<=	&sic	<=	3443	then	&newvarname	=	10	;
else	if	3444	<=	&sic	<=	3444	then	&newvarname	=	10	;
else	if	3460	<=	&sic	<=	3469	then	&newvarname	=	10	;
else	if	3470	<=	&sic	<=	3479	then	&newvarname	=	10	;
else	if	3480	<=	&sic	<=	3489	then	&newvarname	=	10	;
else	if	3490	<=	&sic	<=	3499	then	&newvarname	=	10	;
else	if	3510	<=	&sic	<=	3519	then	&newvarname	=	11	;
else	if	3520	<=	&sic	<=	3529	then	&newvarname	=	11	;
else	if	3530	<=	&sic	<=	3530	then	&newvarname	=	11	;
else	if	3531	<=	&sic	<=	3531	then	&newvarname	=	11	;
else	if	3532	<=	&sic	<=	3532	then	&newvarname	=	11	;
else	if	3533	<=	&sic	<=	3533	then	&newvarname	=	11	;
else	if	3534	<=	&sic	<=	3534	then	&newvarname	=	11	;
else	if	3535	<=	&sic	<=	3535	then	&newvarname	=	11	;
else	if	3536	<=	&sic	<=	3536	then	&newvarname	=	11	;
else	if	3540	<=	&sic	<=	3549	then	&newvarname	=	11	;
else	if	3550	<=	&sic	<=	3559	then	&newvarname	=	11	;
else	if	3560	<=	&sic	<=	3569	then	&newvarname	=	11	;
else	if	3570	<=	&sic	<=	3579	then	&newvarname	=	11	;
else	if	3580	<=	&sic	<=	3580	then	&newvarname	=	11	;
else	if	3581	<=	&sic	<=	3581	then	&newvarname	=	11	;
else	if	3582	<=	&sic	<=	3582	then	&newvarname	=	11	;
else	if	3585	<=	&sic	<=	3585	then	&newvarname	=	11	;
else	if	3586	<=	&sic	<=	3586	then	&newvarname	=	11	;
else	if	3589	<=	&sic	<=	3589	then	&newvarname	=	11	;
else	if	3590	<=	&sic	<=	3599	then	&newvarname	=	11	;
else	if	3600	<=	&sic	<=	3600	then	&newvarname	=	11	;
else	if	3610	<=	&sic	<=	3613	then	&newvarname	=	11	;
else	if	3620	<=	&sic	<=	3621	then	&newvarname	=	11	;
else	if	3622	<=	&sic	<=	3622	then	&newvarname	=	11	;
else	if	3623	<=	&sic	<=	3629	then	&newvarname	=	11	;
else	if	3670	<=	&sic	<=	3679	then	&newvarname	=	11	;
else	if	3680	<=	&sic	<=	3680	then	&newvarname	=	11	;
else	if	3681	<=	&sic	<=	3681	then	&newvarname	=	11	;
else	if	3682	<=	&sic	<=	3682	then	&newvarname	=	11	;
else	if	3683	<=	&sic	<=	3683	then	&newvarname	=	11	;
else	if	3684	<=	&sic	<=	3684	then	&newvarname	=	11	;
else	if	3685	<=	&sic	<=	3685	then	&newvarname	=	11	;
else	if	3686	<=	&sic	<=	3686	then	&newvarname	=	11	;
else	if	3687	<=	&sic	<=	3687	then	&newvarname	=	11	;
else	if	3688	<=	&sic	<=	3688	then	&newvarname	=	11	;
else	if	3689	<=	&sic	<=	3689	then	&newvarname	=	11	;
else	if	3690	<=	&sic	<=	3690	then	&newvarname	=	11	;
else	if	3691	<=	&sic	<=	3692	then	&newvarname	=	11	;
else	if	3693	<=	&sic	<=	3693	then	&newvarname	=	11	;
else	if	3694	<=	&sic	<=	3694	then	&newvarname	=	11	;
else	if	3695	<=	&sic	<=	3695	then	&newvarname	=	11	;
else	if	3699	<=	&sic	<=	3699	then	&newvarname	=	11	;
else	if	3810	<=	&sic	<=	3810	then	&newvarname	=	11	;
else	if	3811	<=	&sic	<=	3811	then	&newvarname	=	11	;
else	if	3812	<=	&sic	<=	3812	then	&newvarname	=	11	;
else	if	3820	<=	&sic	<=	3820	then	&newvarname	=	11	;
else	if	3821	<=	&sic	<=	3821	then	&newvarname	=	11	;
else	if	3822	<=	&sic	<=	3822	then	&newvarname	=	11	;
else	if	3823	<=	&sic	<=	3823	then	&newvarname	=	11	;
else	if	3824	<=	&sic	<=	3824	then	&newvarname	=	11	;
else	if	3825	<=	&sic	<=	3825	then	&newvarname	=	11	;
else	if	3826	<=	&sic	<=	3826	then	&newvarname	=	11	;
else	if	3827	<=	&sic	<=	3827	then	&newvarname	=	11	;
else	if	3829	<=	&sic	<=	3829	then	&newvarname	=	11	;
else	if	3830	<=	&sic	<=	3839	then	&newvarname	=	11	;
else	if	3950	<=	&sic	<=	3955	then	&newvarname	=	11	;
else	if	5060	<=	&sic	<=	5060	then	&newvarname	=	11	;
else	if	5063	<=	&sic	<=	5063	then	&newvarname	=	11	;
else	if	5065	<=	&sic	<=	5065	then	&newvarname	=	11	;
else	if	5080	<=	&sic	<=	5080	then	&newvarname	=	11	;
else	if	5081	<=	&sic	<=	5081	then	&newvarname	=	11	;
else	if	3710	<=	&sic	<=	3710	then	&newvarname	=	12	;
else	if	3711	<=	&sic	<=	3711	then	&newvarname	=	12	;
else	if	3714	<=	&sic	<=	3714	then	&newvarname	=	12	;
else	if	3716	<=	&sic	<=	3716	then	&newvarname	=	12	;
else	if	3750	<=	&sic	<=	3751	then	&newvarname	=	12	;
else	if	3792	<=	&sic	<=	3792	then	&newvarname	=	12	;
else	if	5010	<=	&sic	<=	5015	then	&newvarname	=	12	;
else	if	5510	<=	&sic	<=	5521	then	&newvarname	=	12	;
else	if	5530	<=	&sic	<=	5531	then	&newvarname	=	12	;
else	if	5560	<=	&sic	<=	5561	then	&newvarname	=	12	;
else	if	5570	<=	&sic	<=	5571	then	&newvarname	=	12	;
else	if	5590	<=	&sic	<=	5599	then	&newvarname	=	12	;
else	if	3713	<=	&sic	<=	3713	then	&newvarname	=	13	;
else	if	3715	<=	&sic	<=	3715	then	&newvarname	=	13	;
else	if	3720	<=	&sic	<=	3720	then	&newvarname	=	13	;
else	if	3721	<=	&sic	<=	3721	then	&newvarname	=	13	;
else	if	3724	<=	&sic	<=	3724	then	&newvarname	=	13	;
else	if	3725	<=	&sic	<=	3725	then	&newvarname	=	13	;
else	if	3728	<=	&sic	<=	3728	then	&newvarname	=	13	;
else	if	3730	<=	&sic	<=	3731	then	&newvarname	=	13	;
else	if	3732	<=	&sic	<=	3732	then	&newvarname	=	13	;
else	if	3740	<=	&sic	<=	3743	then	&newvarname	=	13	;
else	if	3760	<=	&sic	<=	3769	then	&newvarname	=	13	;
else	if	3790	<=	&sic	<=	3790	then	&newvarname	=	13	;
else	if	3795	<=	&sic	<=	3795	then	&newvarname	=	13	;
else	if	3799	<=	&sic	<=	3799	then	&newvarname	=	13	;
else	if	4000	<=	&sic	<=	4013	then	&newvarname	=	13	;
else	if	4100	<=	&sic	<=	4100	then	&newvarname	=	13	;
else	if	4110	<=	&sic	<=	4119	then	&newvarname	=	13	;
else	if	4120	<=	&sic	<=	4121	then	&newvarname	=	13	;
else	if	4130	<=	&sic	<=	4131	then	&newvarname	=	13	;
else	if	4140	<=	&sic	<=	4142	then	&newvarname	=	13	;
else	if	4150	<=	&sic	<=	4151	then	&newvarname	=	13	;
else	if	4170	<=	&sic	<=	4173	then	&newvarname	=	13	;
else	if	4190	<=	&sic	<=	4199	then	&newvarname	=	13	;
else	if	4200	<=	&sic	<=	4200	then	&newvarname	=	13	;
else	if	4210	<=	&sic	<=	4219	then	&newvarname	=	13	;
else	if	4220	<=	&sic	<=	4229	then	&newvarname	=	13	;
else	if	4230	<=	&sic	<=	4231	then	&newvarname	=	13	;
else	if	4400	<=	&sic	<=	4499	then	&newvarname	=	13	;
else	if	4500	<=	&sic	<=	4599	then	&newvarname	=	13	;
else	if	4600	<=	&sic	<=	4699	then	&newvarname	=	13	;
else	if	4700	<=	&sic	<=	4700	then	&newvarname	=	13	;
else	if	4710	<=	&sic	<=	4712	then	&newvarname	=	13	;
else	if	4720	<=	&sic	<=	4729	then	&newvarname	=	13	;
else	if	4730	<=	&sic	<=	4739	then	&newvarname	=	13	;
else	if	4740	<=	&sic	<=	4742	then	&newvarname	=	13	;
else	if	4780	<=	&sic	<=	4780	then	&newvarname	=	13	;
else	if	4783	<=	&sic	<=	4783	then	&newvarname	=	13	;
else	if	4785	<=	&sic	<=	4785	then	&newvarname	=	13	;
else	if	4789	<=	&sic	<=	4789	then	&newvarname	=	13	;
else	if	4900	<=	&sic	<=	4900	then	&newvarname	=	14	;
else	if	4910	<=	&sic	<=	4911	then	&newvarname	=	14	;
else	if	4920	<=	&sic	<=	4922	then	&newvarname	=	14	;
else	if	4923	<=	&sic	<=	4923	then	&newvarname	=	14	;
else	if	4924	<=	&sic	<=	4925	then	&newvarname	=	14	;
else	if	4930	<=	&sic	<=	4931	then	&newvarname	=	14	;
else	if	4932	<=	&sic	<=	4932	then	&newvarname	=	14	;
else	if	4939	<=	&sic	<=	4939	then	&newvarname	=	14	;
else	if	4940	<=	&sic	<=	4942	then	&newvarname	=	14	;
else	if	5260	<=	&sic	<=	5261	then	&newvarname	=	15	;
else	if	5270	<=	&sic	<=	5271	then	&newvarname	=	15	;
else	if	5300	<=	&sic	<=	5300	then	&newvarname	=	15	;
else	if	5310	<=	&sic	<=	5311	then	&newvarname	=	15	;
else	if	5320	<=	&sic	<=	5320	then	&newvarname	=	15	;
else	if	5330	<=	&sic	<=	5331	then	&newvarname	=	15	;
else	if	5334	<=	&sic	<=	5334	then	&newvarname	=	15	;
else	if	5390	<=	&sic	<=	5399	then	&newvarname	=	15	;
else	if	5400	<=	&sic	<=	5400	then	&newvarname	=	15	;
else	if	5410	<=	&sic	<=	5411	then	&newvarname	=	15	;
else	if	5412	<=	&sic	<=	5412	then	&newvarname	=	15	;
else	if	5420	<=	&sic	<=	5421	then	&newvarname	=	15	;
else	if	5430	<=	&sic	<=	5431	then	&newvarname	=	15	;
else	if	5440	<=	&sic	<=	5441	then	&newvarname	=	15	;
else	if	5450	<=	&sic	<=	5451	then	&newvarname	=	15	;
else	if	5460	<=	&sic	<=	5461	then	&newvarname	=	15	;
else	if	5490	<=	&sic	<=	5499	then	&newvarname	=	15	;
else	if	5540	<=	&sic	<=	5541	then	&newvarname	=	15	;
else	if	5550	<=	&sic	<=	5551	then	&newvarname	=	15	;
else	if	5600	<=	&sic	<=	5699	then	&newvarname	=	15	;
else	if	5700	<=	&sic	<=	5700	then	&newvarname	=	15	;
else	if	5710	<=	&sic	<=	5719	then	&newvarname	=	15	;
else	if	5720	<=	&sic	<=	5722	then	&newvarname	=	15	;
else	if	5730	<=	&sic	<=	5733	then	&newvarname	=	15	;
else	if	5734	<=	&sic	<=	5734	then	&newvarname	=	15	;
else	if	5735	<=	&sic	<=	5735	then	&newvarname	=	15	;
else	if	5736	<=	&sic	<=	5736	then	&newvarname	=	15	;
else	if	5750	<=	&sic	<=	5750	then	&newvarname	=	15	;
else	if	5800	<=	&sic	<=	5813	then	&newvarname	=	15	;
else	if	5890	<=	&sic	<=	5890	then	&newvarname	=	15	;
else	if	5900	<=	&sic	<=	5900	then	&newvarname	=	15	;
else	if	5910	<=	&sic	<=	5912	then	&newvarname	=	15	;
else	if	5920	<=	&sic	<=	5921	then	&newvarname	=	15	;
else	if	5930	<=	&sic	<=	5932	then	&newvarname	=	15	;
else	if	5940	<=	&sic	<=	5940	then	&newvarname	=	15	;
else	if	5941	<=	&sic	<=	5941	then	&newvarname	=	15	;
else	if	5942	<=	&sic	<=	5942	then	&newvarname	=	15	;
else	if	5943	<=	&sic	<=	5943	then	&newvarname	=	15	;
else	if	5944	<=	&sic	<=	5944	then	&newvarname	=	15	;
else	if	5945	<=	&sic	<=	5945	then	&newvarname	=	15	;
else	if	5946	<=	&sic	<=	5946	then	&newvarname	=	15	;
else	if	5947	<=	&sic	<=	5947	then	&newvarname	=	15	;
else	if	5948	<=	&sic	<=	5948	then	&newvarname	=	15	;
else	if	5949	<=	&sic	<=	5949	then	&newvarname	=	15	;
else	if	5960	<=	&sic	<=	5963	then	&newvarname	=	15	;
else	if	5980	<=	&sic	<=	5989	then	&newvarname	=	15	;
else	if	5990	<=	&sic	<=	5990	then	&newvarname	=	15	;
else	if	5992	<=	&sic	<=	5992	then	&newvarname	=	15	;
else	if	5993	<=	&sic	<=	5993	then	&newvarname	=	15	;
else	if	5994	<=	&sic	<=	5994	then	&newvarname	=	15	;
else	if	5995	<=	&sic	<=	5995	then	&newvarname	=	15	;
else	if	5999	<=	&sic	<=	5999	then	&newvarname	=	15	;
else	if	6010	<=	&sic	<=	6019	then	&newvarname	=	16	;
else	if	6020	<=	&sic	<=	6020	then	&newvarname	=	16	;
else	if	6021	<=	&sic	<=	6021	then	&newvarname	=	16	;
else	if	6022	<=	&sic	<=	6022	then	&newvarname	=	16	;
else	if	6023	<=	&sic	<=	6023	then	&newvarname	=	16	;
else	if	6025	<=	&sic	<=	6025	then	&newvarname	=	16	;
else	if	6026	<=	&sic	<=	6026	then	&newvarname	=	16	;
else	if	6028	<=	&sic	<=	6029	then	&newvarname	=	16	;
else	if	6030	<=	&sic	<=	6036	then	&newvarname	=	16	;
else	if	6040	<=	&sic	<=	6049	then	&newvarname	=	16	;
else	if	6050	<=	&sic	<=	6059	then	&newvarname	=	16	;
else	if	6060	<=	&sic	<=	6062	then	&newvarname	=	16	;
else	if	6080	<=	&sic	<=	6082	then	&newvarname	=	16	;
else	if	6090	<=	&sic	<=	6099	then	&newvarname	=	16	;
else	if	6100	<=	&sic	<=	6100	then	&newvarname	=	16	;
else	if	6110	<=	&sic	<=	6111	then	&newvarname	=	16	;
else	if	6112	<=	&sic	<=	6112	then	&newvarname	=	16	;
else	if	6120	<=	&sic	<=	6129	then	&newvarname	=	16	;
else	if	6140	<=	&sic	<=	6149	then	&newvarname	=	16	;
else	if	6150	<=	&sic	<=	6159	then	&newvarname	=	16	;
else	if	6160	<=	&sic	<=	6163	then	&newvarname	=	16	;
else	if	6172	<=	&sic	<=	6172	then	&newvarname	=	16	;
else	if	6199	<=	&sic	<=	6199	then	&newvarname	=	16	;
else	if	6200	<=	&sic	<=	6299	then	&newvarname	=	16	;
else	if	6300	<=	&sic	<=	6300	then	&newvarname	=	16	;
else	if	6310	<=	&sic	<=	6312	then	&newvarname	=	16	;
else	if	6320	<=	&sic	<=	6324	then	&newvarname	=	16	;
else	if	6330	<=	&sic	<=	6331	then	&newvarname	=	16	;
else	if	6350	<=	&sic	<=	6351	then	&newvarname	=	16	;
else	if	6360	<=	&sic	<=	6361	then	&newvarname	=	16	;
else	if	6370	<=	&sic	<=	6371	then	&newvarname	=	16	;
else	if	6390	<=	&sic	<=	6399	then	&newvarname	=	16	;
else	if	6400	<=	&sic	<=	6411	then	&newvarname	=	16	;
else	if	6500	<=	&sic	<=	6500	then	&newvarname	=	16	;
else	if	6510	<=	&sic	<=	6510	then	&newvarname	=	16	;
else	if	6512	<=	&sic	<=	6512	then	&newvarname	=	16	;
else	if	6513	<=	&sic	<=	6513	then	&newvarname	=	16	;
else	if	6514	<=	&sic	<=	6514	then	&newvarname	=	16	;
else	if	6515	<=	&sic	<=	6515	then	&newvarname	=	16	;
else	if	6517	<=	&sic	<=	6519	then	&newvarname	=	16	;
else	if	6530	<=	&sic	<=	6531	then	&newvarname	=	16	;
else	if	6532	<=	&sic	<=	6532	then	&newvarname	=	16	;
else	if	6540	<=	&sic	<=	6541	then	&newvarname	=	16	;
else	if	6550	<=	&sic	<=	6553	then	&newvarname	=	16	;
else	if	6611	<=	&sic	<=	6611	then	&newvarname	=	16	;
else	if	6700	<=	&sic	<=	6700	then	&newvarname	=	16	;
else	if	6710	<=	&sic	<=	6719	then	&newvarname	=	16	;
else	if	6720	<=	&sic	<=	6722	then	&newvarname	=	16	;
else	if	6723	<=	&sic	<=	6723	then	&newvarname	=	16	;
else	if	6724	<=	&sic	<=	6724	then	&newvarname	=	16	;
else	if	6725	<=	&sic	<=	6725	then	&newvarname	=	16	;
else	if	6726	<=	&sic	<=	6726	then	&newvarname	=	16	;
else	if	6730	<=	&sic	<=	6733	then	&newvarname	=	16	;
else	if	6790	<=	&sic	<=	6790	then	&newvarname	=	16	;
else	if	6792	<=	&sic	<=	6792	then	&newvarname	=	16	;
else	if	6794	<=	&sic	<=	6794	then	&newvarname	=	16	;
else	if	6795	<=	&sic	<=	6795	then	&newvarname	=	16	;
else	if	6798	<=	&sic	<=	6798	then	&newvarname	=	16	;
else	if	6799	<=	&sic	<=	6799	then	&newvarname	=	16	;
else	if	2520	<=	&sic	<=	2549	then	&newvarname	=	17	;
else	if	2600	<=	&sic	<=	2639	then	&newvarname	=	17	;
else	if	2640	<=	&sic	<=	2659	then	&newvarname	=	17	;
else	if	2661	<=	&sic	<=	2661	then	&newvarname	=	17	;
else	if	2670	<=	&sic	<=	2699	then	&newvarname	=	17	;
else	if	2700	<=	&sic	<=	2709	then	&newvarname	=	17	;
else	if	2710	<=	&sic	<=	2719	then	&newvarname	=	17	;
else	if	2720	<=	&sic	<=	2729	then	&newvarname	=	17	;
else	if	2730	<=	&sic	<=	2739	then	&newvarname	=	17	;
else	if	2740	<=	&sic	<=	2749	then	&newvarname	=	17	;
else	if	2750	<=	&sic	<=	2759	then	&newvarname	=	17	;
else	if	2760	<=	&sic	<=	2761	then	&newvarname	=	17	;
else	if	2770	<=	&sic	<=	2771	then	&newvarname	=	17	;
else	if	2780	<=	&sic	<=	2789	then	&newvarname	=	17	;
else	if	2790	<=	&sic	<=	2799	then	&newvarname	=	17	;
else	if	2835	<=	&sic	<=	2835	then	&newvarname	=	17	;
else	if	2836	<=	&sic	<=	2836	then	&newvarname	=	17	;
else	if	2990	<=	&sic	<=	2999	then	&newvarname	=	17	;
else	if	3000	<=	&sic	<=	3000	then	&newvarname	=	17	;
else	if	3010	<=	&sic	<=	3011	then	&newvarname	=	17	;
else	if	3041	<=	&sic	<=	3041	then	&newvarname	=	17	;
else	if	3050	<=	&sic	<=	3053	then	&newvarname	=	17	;
else	if	3160	<=	&sic	<=	3161	then	&newvarname	=	17	;
else	if	3170	<=	&sic	<=	3171	then	&newvarname	=	17	;
else	if	3172	<=	&sic	<=	3172	then	&newvarname	=	17	;
else	if	3190	<=	&sic	<=	3199	then	&newvarname	=	17	;
else	if	3220	<=	&sic	<=	3221	then	&newvarname	=	17	;
else	if	3229	<=	&sic	<=	3229	then	&newvarname	=	17	;
else	if	3230	<=	&sic	<=	3231	then	&newvarname	=	17	;
else	if	3260	<=	&sic	<=	3260	then	&newvarname	=	17	;
else	if	3262	<=	&sic	<=	3263	then	&newvarname	=	17	;
else	if	3269	<=	&sic	<=	3269	then	&newvarname	=	17	;
else	if	3295	<=	&sic	<=	3299	then	&newvarname	=	17	;
else	if	3537	<=	&sic	<=	3537	then	&newvarname	=	17	;
else	if	3640	<=	&sic	<=	3644	then	&newvarname	=	17	;
else	if	3645	<=	&sic	<=	3645	then	&newvarname	=	17	;
else	if	3646	<=	&sic	<=	3646	then	&newvarname	=	17	;
else	if	3647	<=	&sic	<=	3647	then	&newvarname	=	17	;
else	if	3648	<=	&sic	<=	3649	then	&newvarname	=	17	;
else	if	3660	<=	&sic	<=	3660	then	&newvarname	=	17	;
else	if	3661	<=	&sic	<=	3661	then	&newvarname	=	17	;
else	if	3662	<=	&sic	<=	3662	then	&newvarname	=	17	;
else	if	3663	<=	&sic	<=	3663	then	&newvarname	=	17	;
else	if	3664	<=	&sic	<=	3664	then	&newvarname	=	17	;
else	if	3665	<=	&sic	<=	3665	then	&newvarname	=	17	;
else	if	3666	<=	&sic	<=	3666	then	&newvarname	=	17	;
else	if	3669	<=	&sic	<=	3669	then	&newvarname	=	17	;
else	if	3840	<=	&sic	<=	3849	then	&newvarname	=	17	;
else	if	3850	<=	&sic	<=	3851	then	&newvarname	=	17	;
else	if	3991	<=	&sic	<=	3991	then	&newvarname	=	17	;
else	if	3993	<=	&sic	<=	3993	then	&newvarname	=	17	;
else	if	3995	<=	&sic	<=	3995	then	&newvarname	=	17	;
else	if	3996	<=	&sic	<=	3996	then	&newvarname	=	17	;
else	if	4810	<=	&sic	<=	4813	then	&newvarname	=	17	;
else	if	4820	<=	&sic	<=	4822	then	&newvarname	=	17	;
else	if	4830	<=	&sic	<=	4839	then	&newvarname	=	17	;
else	if	4840	<=	&sic	<=	4841	then	&newvarname	=	17	;
else	if	4890	<=	&sic	<=	4890	then	&newvarname	=	17	;
else	if	4891	<=	&sic	<=	4891	then	&newvarname	=	17	;
else	if	4892	<=	&sic	<=	4892	then	&newvarname	=	17	;
else	if	4899	<=	&sic	<=	4899	then	&newvarname	=	17	;
else	if	4950	<=	&sic	<=	4959	then	&newvarname	=	17	;
else	if	4960	<=	&sic	<=	4961	then	&newvarname	=	17	;
else	if	4970	<=	&sic	<=	4971	then	&newvarname	=	17	;
else	if	4991	<=	&sic	<=	4991	then	&newvarname	=	17	;
else	if	5040	<=	&sic	<=	5042	then	&newvarname	=	17	;
else	if	5043	<=	&sic	<=	5043	then	&newvarname	=	17	;
else	if	5044	<=	&sic	<=	5044	then	&newvarname	=	17	;
else	if	5045	<=	&sic	<=	5045	then	&newvarname	=	17	;
else	if	5046	<=	&sic	<=	5046	then	&newvarname	=	17	;
else	if	5047	<=	&sic	<=	5047	then	&newvarname	=	17	;
else	if	5048	<=	&sic	<=	5048	then	&newvarname	=	17	;
else	if	5049	<=	&sic	<=	5049	then	&newvarname	=	17	;
else	if	5082	<=	&sic	<=	5082	then	&newvarname	=	17	;
else	if	5083	<=	&sic	<=	5083	then	&newvarname	=	17	;
else	if	5084	<=	&sic	<=	5084	then	&newvarname	=	17	;
else	if	5085	<=	&sic	<=	5085	then	&newvarname	=	17	;
else	if	5086	<=	&sic	<=	5087	then	&newvarname	=	17	;
else	if	5088	<=	&sic	<=	5088	then	&newvarname	=	17	;
else	if	5090	<=	&sic	<=	5090	then	&newvarname	=	17	;
else	if	5091	<=	&sic	<=	5092	then	&newvarname	=	17	;
else	if	5093	<=	&sic	<=	5093	then	&newvarname	=	17	;
else	if	5100	<=	&sic	<=	5100	then	&newvarname	=	17	;
else	if	5110	<=	&sic	<=	5113	then	&newvarname	=	17	;
else	if	5199	<=	&sic	<=	5199	then	&newvarname	=	17	;
else	if	7000	<=	&sic	<=	7000	then	&newvarname	=	17	;
else	if	7010	<=	&sic	<=	7011	then	&newvarname	=	17	;
else	if	7020	<=	&sic	<=	7021	then	&newvarname	=	17	;
else	if	7030	<=	&sic	<=	7033	then	&newvarname	=	17	;
else	if	7040	<=	&sic	<=	7041	then	&newvarname	=	17	;
else	if	7200	<=	&sic	<=	7200	then	&newvarname	=	17	;
else	if	7210	<=	&sic	<=	7212	then	&newvarname	=	17	;
else	if	7213	<=	&sic	<=	7213	then	&newvarname	=	17	;
else	if	7215	<=	&sic	<=	7216	then	&newvarname	=	17	;
else	if	7217	<=	&sic	<=	7217	then	&newvarname	=	17	;
else	if	7218	<=	&sic	<=	7218	then	&newvarname	=	17	;
else	if	7219	<=	&sic	<=	7219	then	&newvarname	=	17	;
else	if	7220	<=	&sic	<=	7221	then	&newvarname	=	17	;
else	if	7230	<=	&sic	<=	7231	then	&newvarname	=	17	;
else	if	7240	<=	&sic	<=	7241	then	&newvarname	=	17	;
else	if	7250	<=	&sic	<=	7251	then	&newvarname	=	17	;
else	if	7260	<=	&sic	<=	7269	then	&newvarname	=	17	;
else	if	7290	<=	&sic	<=	7290	then	&newvarname	=	17	;
else	if	7291	<=	&sic	<=	7291	then	&newvarname	=	17	;
else	if	7299	<=	&sic	<=	7299	then	&newvarname	=	17	;
else	if	7300	<=	&sic	<=	7300	then	&newvarname	=	17	;
else	if	7310	<=	&sic	<=	7319	then	&newvarname	=	17	;
else	if	7320	<=	&sic	<=	7323	then	&newvarname	=	17	;
else	if	7330	<=	&sic	<=	7338	then	&newvarname	=	17	;
else	if	7340	<=	&sic	<=	7342	then	&newvarname	=	17	;
else	if	7349	<=	&sic	<=	7349	then	&newvarname	=	17	;
else	if	7350	<=	&sic	<=	7351	then	&newvarname	=	17	;
else	if	7352	<=	&sic	<=	7352	then	&newvarname	=	17	;
else	if	7353	<=	&sic	<=	7353	then	&newvarname	=	17	;
else	if	7359	<=	&sic	<=	7359	then	&newvarname	=	17	;
else	if	7360	<=	&sic	<=	7369	then	&newvarname	=	17	;
else	if	7370	<=	&sic	<=	7372	then	&newvarname	=	17	;
else	if	7373	<=	&sic	<=	7373	then	&newvarname	=	17	;
else	if	7374	<=	&sic	<=	7374	then	&newvarname	=	17	;
else	if	7375	<=	&sic	<=	7375	then	&newvarname	=	17	;
else	if	7376	<=	&sic	<=	7376	then	&newvarname	=	17	;
else	if	7377	<=	&sic	<=	7377	then	&newvarname	=	17	;
else	if	7378	<=	&sic	<=	7378	then	&newvarname	=	17	;
else	if	7379	<=	&sic	<=	7379	then	&newvarname	=	17	;
else	if	7380	<=	&sic	<=	7380	then	&newvarname	=	17	;
else	if	7381	<=	&sic	<=	7382	then	&newvarname	=	17	;
else	if	7383	<=	&sic	<=	7383	then	&newvarname	=	17	;
else	if	7384	<=	&sic	<=	7384	then	&newvarname	=	17	;
else	if	7385	<=	&sic	<=	7385	then	&newvarname	=	17	;
else	if	7389	<=	&sic	<=	7390	then	&newvarname	=	17	;
else	if	7391	<=	&sic	<=	7391	then	&newvarname	=	17	;
else	if	7392	<=	&sic	<=	7392	then	&newvarname	=	17	;
else	if	7393	<=	&sic	<=	7393	then	&newvarname	=	17	;
else	if	7394	<=	&sic	<=	7394	then	&newvarname	=	17	;
else	if	7395	<=	&sic	<=	7395	then	&newvarname	=	17	;
else	if	7397	<=	&sic	<=	7397	then	&newvarname	=	17	;
else	if	7399	<=	&sic	<=	7399	then	&newvarname	=	17	;
else	if	7500	<=	&sic	<=	7500	then	&newvarname	=	17	;
else	if	7510	<=	&sic	<=	7519	then	&newvarname	=	17	;
else	if	7520	<=	&sic	<=	7523	then	&newvarname	=	17	;
else	if	7530	<=	&sic	<=	7539	then	&newvarname	=	17	;
else	if	7540	<=	&sic	<=	7549	then	&newvarname	=	17	;
else	if	7600	<=	&sic	<=	7600	then	&newvarname	=	17	;
else	if	7620	<=	&sic	<=	7620	then	&newvarname	=	17	;
else	if	7622	<=	&sic	<=	7622	then	&newvarname	=	17	;
else	if	7623	<=	&sic	<=	7623	then	&newvarname	=	17	;
else	if	7629	<=	&sic	<=	7629	then	&newvarname	=	17	;
else	if	7630	<=	&sic	<=	7631	then	&newvarname	=	17	;
else	if	7640	<=	&sic	<=	7641	then	&newvarname	=	17	;
else	if	7690	<=	&sic	<=	7699	then	&newvarname	=	17	;
else	if	7800	<=	&sic	<=	7829	then	&newvarname	=	17	;
else	if	7830	<=	&sic	<=	7833	then	&newvarname	=	17	;
else	if	7840	<=	&sic	<=	7841	then	&newvarname	=	17	;
else	if	7900	<=	&sic	<=	7900	then	&newvarname	=	17	;
else	if	7910	<=	&sic	<=	7911	then	&newvarname	=	17	;
else	if	7920	<=	&sic	<=	7929	then	&newvarname	=	17	;
else	if	7930	<=	&sic	<=	7933	then	&newvarname	=	17	;
else	if	7940	<=	&sic	<=	7949	then	&newvarname	=	17	;
else	if	7980	<=	&sic	<=	7980	then	&newvarname	=	17	;
else	if	7990	<=	&sic	<=	7999	then	&newvarname	=	17	;
else	if	8000	<=	&sic	<=	8099	then	&newvarname	=	17	;
else	if	8100	<=	&sic	<=	8199	then	&newvarname	=	17	;
else	if	8200	<=	&sic	<=	8299	then	&newvarname	=	17	;
else	if	8300	<=	&sic	<=	8399	then	&newvarname	=	17	;
else	if	8400	<=	&sic	<=	8499	then	&newvarname	=	17	;
else	if	8600	<=	&sic	<=	8699	then	&newvarname	=	17	;
else	if	8700	<=	&sic	<=	8700	then	&newvarname	=	17	;
else	if	8710	<=	&sic	<=	8713	then	&newvarname	=	17	;
else	if	8720	<=	&sic	<=	8721	then	&newvarname	=	17	;
else	if	8730	<=	&sic	<=	8734	then	&newvarname	=	17	;
else	if	8740	<=	&sic	<=	8748	then	&newvarname	=	17	;
else	if	8800	<=	&sic	<=	8899	then	&newvarname	=	17	;
else	if	8900	<=	&sic	<=	8910	then	&newvarname	=	17	;
else	if	8911	<=	&sic	<=	8911	then	&newvarname	=	17	;
else	if	8920	<=	&sic	<=	8999	then	&newvarname	=	17	;
else &newvarname = 17;
run;

proc format;
   value ff
 1	="Food"
 2	="Mining and Minerals"
 3	="Oil and Petroleum Products"
 4	="Textiles, Apparel & Footwear"
 5	="Consumer Durables"
 6	="Chemicals"
 7	="Drugs, Soap, Perfumes, Tobacco"
 8	="Construction"
 9	="Steel"
10	="Fabricated Products"
11	="Machinery and Business Equipment"
12	="Automobiles"
13	="Transportation"
14	="Utilities"
15	="Retail Stores"
16	="Financial Institutions"
17	="Other";
run;

%mend;


*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;
*CREATES FAMA-FRENCH 30 INDUSTRY CLASSIFICATIONS
*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;

%macro ff30(data=,newvarname=industry,sic=sic,out=&data);

data &out;
    set &data;
    &newvarname = 30;
if	100	<= &sic <=	199	then &newvarname = 	1	;
else if	200	<= &sic <=	299	then &newvarname = 	1	;
else if	700	<= &sic <=	799	then &newvarname = 	1	;
else if	910	<= &sic <=	919	then &newvarname = 	1	;
else if	2000	<= &sic <=	2009	then &newvarname = 	1	;
else if	2010	<= &sic <=	2019	then &newvarname = 	1	;
else if	2020	<= &sic <=	2029	then &newvarname = 	1	;
else if	2030	<= &sic <=	2039	then &newvarname = 	1	;
else if	2040	<= &sic <=	2046	then &newvarname = 	1	;
else if	2048	<= &sic <=	2048	then &newvarname = 	1	;
else if	2050	<= &sic <=	2059	then &newvarname = 	1	;
else if	2060	<= &sic <=	2063	then &newvarname = 	1	;
else if	2064	<= &sic <=	2068	then &newvarname = 	1	;
else if	2070	<= &sic <=	2079	then &newvarname = 	1	;
else if	2086	<= &sic <=	2086	then &newvarname = 	1	;
else if	2087	<= &sic <=	2087	then &newvarname = 	1	;
else if	2090	<= &sic <=	2092	then &newvarname = 	1	;
else if	2095	<= &sic <=	2095	then &newvarname = 	1	;
else if	2096	<= &sic <=	2096	then &newvarname = 	1	;
else if	2097	<= &sic <=	2097	then &newvarname = 	1	;
else if	2098	<= &sic <=	2099	then &newvarname = 	1	;
else if	2080	<= &sic <=	2080	then &newvarname = 	2	;
else if	2082	<= &sic <=	2082	then &newvarname = 	2	;
else if	2083	<= &sic <=	2083	then &newvarname = 	2	;
else if	2084	<= &sic <=	2084	then &newvarname = 	2	;
else if	2085	<= &sic <=	2085	then &newvarname = 	2	;
else if	2100	<= &sic <=	2199	then &newvarname = 	3	;
else if	920	<= &sic <=	999	then &newvarname = 	4	;
else if	3650	<= &sic <=	3651	then &newvarname = 	4	;
else if	3652	<= &sic <=	3652	then &newvarname = 	4	;
else if	3732	<= &sic <=	3732	then &newvarname = 	4	;
else if	3930	<= &sic <=	3931	then &newvarname = 	4	;
else if	3940	<= &sic <=	3949	then &newvarname = 	4	;
else if	7800	<= &sic <=	7829	then &newvarname = 	4	;
else if	7830	<= &sic <=	7833	then &newvarname = 	4	;
else if	7840	<= &sic <=	7841	then &newvarname = 	4	;
else if	7900	<= &sic <=	7900	then &newvarname = 	4	;
else if	7910	<= &sic <=	7911	then &newvarname = 	4	;
else if	7920	<= &sic <=	7929	then &newvarname = 	4	;
else if	7930	<= &sic <=	7933	then &newvarname = 	4	;
else if	7940	<= &sic <=	7949	then &newvarname = 	4	;
else if	7980	<= &sic <=	7980	then &newvarname = 	4	;
else if	7990	<= &sic <=	7999	then &newvarname = 	4	;
else if	2700	<= &sic <=	2709	then &newvarname = 	5	;
else if	2710	<= &sic <=	2719	then &newvarname = 	5	;
else if	2720	<= &sic <=	2729	then &newvarname = 	5	;
else if	2730	<= &sic <=	2739	then &newvarname = 	5	;
else if	2740	<= &sic <=	2749	then &newvarname = 	5	;
else if	2750	<= &sic <=	2759	then &newvarname = 	5	;
else if	2770	<= &sic <=	2771	then &newvarname = 	5	;
else if	2780	<= &sic <=	2789	then &newvarname = 	5	;
else if	2790	<= &sic <=	2799	then &newvarname = 	5	;
else if	3993	<= &sic <=	3993	then &newvarname = 	5	;
else if	2047	<= &sic <=	2047	then &newvarname = 	6	;
else if	2391	<= &sic <=	2392	then &newvarname = 	6	;
else if	2510	<= &sic <=	2519	then &newvarname = 	6	;
else if	2590	<= &sic <=	2599	then &newvarname = 	6	;
else if	2840	<= &sic <=	2843	then &newvarname = 	6	;
else if	2844	<= &sic <=	2844	then &newvarname = 	6	;
else if	3160	<= &sic <=	3161	then &newvarname = 	6	;
else if	3170	<= &sic <=	3171	then &newvarname = 	6	;
else if	3172	<= &sic <=	3172	then &newvarname = 	6	;
else if	3190	<= &sic <=	3199	then &newvarname = 	6	;
else if	3229	<= &sic <=	3229	then &newvarname = 	6	;
else if	3260	<= &sic <=	3260	then &newvarname = 	6	;
else if	3262	<= &sic <=	3263	then &newvarname = 	6	;
else if	3269	<= &sic <=	3269	then &newvarname = 	6	;
else if	3230	<= &sic <=	3231	then &newvarname = 	6	;
else if	3630	<= &sic <=	3639	then &newvarname = 	6	;
else if	3750	<= &sic <=	3751	then &newvarname = 	6	;
else if	3800	<= &sic <=	3800	then &newvarname = 	6	;
else if	3860	<= &sic <=	3861	then &newvarname = 	6	;
else if	3870	<= &sic <=	3873	then &newvarname = 	6	;
else if	3910	<= &sic <=	3911	then &newvarname = 	6	;
else if	3914	<= &sic <=	3914	then &newvarname = 	6	;
else if	3915	<= &sic <=	3915	then &newvarname = 	6	;
else if	3960	<= &sic <=	3962	then &newvarname = 	6	;
else if	3991	<= &sic <=	3991	then &newvarname = 	6	;
else if	3995	<= &sic <=	3995	then &newvarname = 	6	;
else if	2300	<= &sic <=	2390	then &newvarname = 	7	;
else if	3020	<= &sic <=	3021	then &newvarname = 	7	;
else if	3100	<= &sic <=	3111	then &newvarname = 	7	;
else if	3130	<= &sic <=	3131	then &newvarname = 	7	;
else if	3140	<= &sic <=	3149	then &newvarname = 	7	;
else if	3150	<= &sic <=	3151	then &newvarname = 	7	;
else if	3963	<= &sic <=	3965	then &newvarname = 	7	;
else if	2830	<= &sic <=	2830	then &newvarname = 	8	;
else if	2831	<= &sic <=	2831	then &newvarname = 	8	;
else if	2833	<= &sic <=	2833	then &newvarname = 	8	;
else if	2834	<= &sic <=	2834	then &newvarname = 	8	;
else if	2835	<= &sic <=	2835	then &newvarname = 	8	;
else if	2836	<= &sic <=	2836	then &newvarname = 	8	;
else if	3693	<= &sic <=	3693	then &newvarname = 	8	;
else if	3840	<= &sic <=	3849	then &newvarname = 	8	;
else if	3850	<= &sic <=	3851	then &newvarname = 	8	;
else if	8000	<= &sic <=	8099	then &newvarname = 	8	;
else if	2800	<= &sic <=	2809	then &newvarname = 	9	;
else if	2810	<= &sic <=	2819	then &newvarname = 	9	;
else if	2820	<= &sic <=	2829	then &newvarname = 	9	;
else if	2850	<= &sic <=	2859	then &newvarname = 	9	;
else if	2860	<= &sic <=	2869	then &newvarname = 	9	;
else if	2870	<= &sic <=	2879	then &newvarname = 	9	;
else if	2890	<= &sic <=	2899	then &newvarname = 	9	;
else if	2200	<= &sic <=	2269	then &newvarname = 	10	;
else if	2270	<= &sic <=	2279	then &newvarname = 	10	;
else if	2280	<= &sic <=	2284	then &newvarname = 	10	;
else if	2290	<= &sic <=	2295	then &newvarname = 	10	;
else if	2297	<= &sic <=	2297	then &newvarname = 	10	;
else if	2298	<= &sic <=	2298	then &newvarname = 	10	;
else if	2299	<= &sic <=	2299	then &newvarname = 	10	;
else if	2393	<= &sic <=	2395	then &newvarname = 	10	;
else if	2397	<= &sic <=	2399	then &newvarname = 	10	;
else if	800	<= &sic <=	899	then &newvarname = 	11	;
else if	1500	<= &sic <=	1511	then &newvarname = 	11	;
else if	1520	<= &sic <=	1529	then &newvarname = 	11	;
else if	1530	<= &sic <=	1539	then &newvarname = 	11	;
else if	1540	<= &sic <=	1549	then &newvarname = 	11	;
else if	1600	<= &sic <=	1699	then &newvarname = 	11	;
else if	1700	<= &sic <=	1799	then &newvarname = 	11	;
else if	2400	<= &sic <=	2439	then &newvarname = 	11	;
else if	2450	<= &sic <=	2459	then &newvarname = 	11	;
else if	2490	<= &sic <=	2499	then &newvarname = 	11	;
else if	2660	<= &sic <=	2661	then &newvarname = 	11	;
else if	2950	<= &sic <=	2952	then &newvarname = 	11	;
else if	3200	<= &sic <=	3200	then &newvarname = 	11	;
else if	3210	<= &sic <=	3211	then &newvarname = 	11	;
else if	3240	<= &sic <=	3241	then &newvarname = 	11	;
else if	3250	<= &sic <=	3259	then &newvarname = 	11	;
else if	3261	<= &sic <=	3261	then &newvarname = 	11	;
else if	3264	<= &sic <=	3264	then &newvarname = 	11	;
else if	3270	<= &sic <=	3275	then &newvarname = 	11	;
else if	3280	<= &sic <=	3281	then &newvarname = 	11	;
else if	3290	<= &sic <=	3293	then &newvarname = 	11	;
else if	3295	<= &sic <=	3299	then &newvarname = 	11	;
else if	3420	<= &sic <=	3429	then &newvarname = 	11	;
else if	3430	<= &sic <=	3433	then &newvarname = 	11	;
else if	3440	<= &sic <=	3441	then &newvarname = 	11	;
else if	3442	<= &sic <=	3442	then &newvarname = 	11	;
else if	3446	<= &sic <=	3446	then &newvarname = 	11	;
else if	3448	<= &sic <=	3448	then &newvarname = 	11	;
else if	3449	<= &sic <=	3449	then &newvarname = 	11	;
else if	3450	<= &sic <=	3451	then &newvarname = 	11	;
else if	3452	<= &sic <=	3452	then &newvarname = 	11	;
else if	3490	<= &sic <=	3499	then &newvarname = 	11	;
else if	3996	<= &sic <=	3996	then &newvarname = 	11	;
else if	3300	<= &sic <=	3300	then &newvarname = 	12	;
else if	3310	<= &sic <=	3317	then &newvarname = 	12	;
else if	3320	<= &sic <=	3325	then &newvarname = 	12	;
else if	3330	<= &sic <=	3339	then &newvarname = 	12	;
else if	3340	<= &sic <=	3341	then &newvarname = 	12	;
else if	3350	<= &sic <=	3357	then &newvarname = 	12	;
else if	3360	<= &sic <=	3369	then &newvarname = 	12	;
else if	3370	<= &sic <=	3379	then &newvarname = 	12	;
else if	3390	<= &sic <=	3399	then &newvarname = 	12	;
else if	3400	<= &sic <=	3400	then &newvarname = 	13	;
else if	3443	<= &sic <=	3443	then &newvarname = 	13	;
else if	3444	<= &sic <=	3444	then &newvarname = 	13	;
else if	3460	<= &sic <=	3469	then &newvarname = 	13	;
else if	3470	<= &sic <=	3479	then &newvarname = 	13	;
else if	3510	<= &sic <=	3519	then &newvarname = 	13	;
else if	3520	<= &sic <=	3529	then &newvarname = 	13	;
else if	3530	<= &sic <=	3530	then &newvarname = 	13	;
else if	3531	<= &sic <=	3531	then &newvarname = 	13	;
else if	3532	<= &sic <=	3532	then &newvarname = 	13	;
else if	3533	<= &sic <=	3533	then &newvarname = 	13	;
else if	3534	<= &sic <=	3534	then &newvarname = 	13	;
else if	3535	<= &sic <=	3535	then &newvarname = 	13	;
else if	3536	<= &sic <=	3536	then &newvarname = 	13	;
else if	3538	<= &sic <=	3538	then &newvarname = 	13	;
else if	3540	<= &sic <=	3549	then &newvarname = 	13	;
else if	3550	<= &sic <=	3559	then &newvarname = 	13	;
else if	3560	<= &sic <=	3569	then &newvarname = 	13	;
else if	3580	<= &sic <=	3580	then &newvarname = 	13	;
else if	3581	<= &sic <=	3581	then &newvarname = 	13	;
else if	3582	<= &sic <=	3582	then &newvarname = 	13	;
else if	3585	<= &sic <=	3585	then &newvarname = 	13	;
else if	3586	<= &sic <=	3586	then &newvarname = 	13	;
else if	3589	<= &sic <=	3589	then &newvarname = 	13	;
else if	3590	<= &sic <=	3599	then &newvarname = 	13	;
else if	3600	<= &sic <=	3600	then &newvarname = 	14	;
else if	3610	<= &sic <=	3613	then &newvarname = 	14	;
else if	3620	<= &sic <=	3621	then &newvarname = 	14	;
else if	3623	<= &sic <=	3629	then &newvarname = 	14	;
else if	3640	<= &sic <=	3644	then &newvarname = 	14	;
else if	3645	<= &sic <=	3645	then &newvarname = 	14	;
else if	3646	<= &sic <=	3646	then &newvarname = 	14	;
else if	3648	<= &sic <=	3649	then &newvarname = 	14	;
else if	3660	<= &sic <=	3660	then &newvarname = 	14	;
else if	3690	<= &sic <=	3690	then &newvarname = 	14	;
else if	3691	<= &sic <=	3692	then &newvarname = 	14	;
else if	3699	<= &sic <=	3699	then &newvarname = 	14	;
else if	2296	<= &sic <=	2296	then &newvarname = 	15	;
else if	2396	<= &sic <=	2396	then &newvarname = 	15	;
else if	3010	<= &sic <=	3011	then &newvarname = 	15	;
else if	3537	<= &sic <=	3537	then &newvarname = 	15	;
else if	3647	<= &sic <=	3647	then &newvarname = 	15	;
else if	3694	<= &sic <=	3694	then &newvarname = 	15	;
else if	3700	<= &sic <=	3700	then &newvarname = 	15	;
else if	3710	<= &sic <=	3710	then &newvarname = 	15	;
else if	3711	<= &sic <=	3711	then &newvarname = 	15	;
else if	3713	<= &sic <=	3713	then &newvarname = 	15	;
else if	3714	<= &sic <=	3714	then &newvarname = 	15	;
else if	3715	<= &sic <=	3715	then &newvarname = 	15	;
else if	3716	<= &sic <=	3716	then &newvarname = 	15	;
else if	3792	<= &sic <=	3792	then &newvarname = 	15	;
else if	3790	<= &sic <=	3791	then &newvarname = 	15	;
else if	3799	<= &sic <=	3799	then &newvarname = 	15	;
else if	3720	<= &sic <=	3720	then &newvarname = 	16	;
else if	3721	<= &sic <=	3721	then &newvarname = 	16	;
else if	3723	<= &sic <=	3724	then &newvarname = 	16	;
else if	3725	<= &sic <=	3725	then &newvarname = 	16	;
else if	3728	<= &sic <=	3729	then &newvarname = 	16	;
else if	3730	<= &sic <=	3731	then &newvarname = 	16	;
else if	3740	<= &sic <=	3743	then &newvarname = 	16	;
else if	1000	<= &sic <=	1009	then &newvarname = 	17	;
else if	1010	<= &sic <=	1019	then &newvarname = 	17	;
else if	1020	<= &sic <=	1029	then &newvarname = 	17	;
else if	1030	<= &sic <=	1039	then &newvarname = 	17	;
else if	1040	<= &sic <=	1049	then &newvarname = 	17	;
else if	1050	<= &sic <=	1059	then &newvarname = 	17	;
else if	1060	<= &sic <=	1069	then &newvarname = 	17	;
else if	1070	<= &sic <=	1079	then &newvarname = 	17	;
else if	1080	<= &sic <=	1089	then &newvarname = 	17	;
else if	1090	<= &sic <=	1099	then &newvarname = 	17	;
else if	1100	<= &sic <=	1119	then &newvarname = 	17	;
else if	1400	<= &sic <=	1499	then &newvarname = 	17	;
else if	1200	<= &sic <=	1299	then &newvarname = 	18	;
else if	1300	<= &sic <=	1300	then &newvarname = 	19	;
else if	1310	<= &sic <=	1319	then &newvarname = 	19	;
else if	1320	<= &sic <=	1329	then &newvarname = 	19	;
else if	1330	<= &sic <=	1339	then &newvarname = 	19	;
else if	1370	<= &sic <=	1379	then &newvarname = 	19	;
else if	1380	<= &sic <=	1380	then &newvarname = 	19	;
else if	1381	<= &sic <=	1381	then &newvarname = 	19	;
else if	1382	<= &sic <=	1382	then &newvarname = 	19	;
else if	1389	<= &sic <=	1389	then &newvarname = 	19	;
else if	2900	<= &sic <=	2912	then &newvarname = 	19	;
else if	2990	<= &sic <=	2999	then &newvarname = 	19	;
else if	4900	<= &sic <=	4900	then &newvarname = 	20	;
else if	4910	<= &sic <=	4911	then &newvarname = 	20	;
else if	4920	<= &sic <=	4922	then &newvarname = 	20	;
else if	4923	<= &sic <=	4923	then &newvarname = 	20	;
else if	4924	<= &sic <=	4925	then &newvarname = 	20	;
else if	4930	<= &sic <=	4931	then &newvarname = 	20	;
else if	4932	<= &sic <=	4932	then &newvarname = 	20	;
else if	4939	<= &sic <=	4939	then &newvarname = 	20	;
else if	4940	<= &sic <=	4942	then &newvarname = 	20	;
else if	4800	<= &sic <=	4800	then &newvarname = 	21	;
else if	4810	<= &sic <=	4813	then &newvarname = 	21	;
else if	4820	<= &sic <=	4822	then &newvarname = 	21	;
else if	4830	<= &sic <=	4839	then &newvarname = 	21	;
else if	4840	<= &sic <=	4841	then &newvarname = 	21	;
else if	4880	<= &sic <=	4889	then &newvarname = 	21	;
else if	4890	<= &sic <=	4890	then &newvarname = 	21	;
else if	4891	<= &sic <=	4891	then &newvarname = 	21	;
else if	4892	<= &sic <=	4892	then &newvarname = 	21	;
else if	4899	<= &sic <=	4899	then &newvarname = 	21	;
else if	7020	<= &sic <=	7021	then &newvarname = 	22	;
else if	7030	<= &sic <=	7033	then &newvarname = 	22	;
else if	7200	<= &sic <=	7200	then &newvarname = 	22	;
else if	7210	<= &sic <=	7212	then &newvarname = 	22	;
else if	7214	<= &sic <=	7214	then &newvarname = 	22	;
else if	7215	<= &sic <=	7216	then &newvarname = 	22	;
else if	7217	<= &sic <=	7217	then &newvarname = 	22	;
else if	7218	<= &sic <=	7218	then &newvarname = 	22	;
else if	7219	<= &sic <=	7219	then &newvarname = 	22	;
else if	7220	<= &sic <=	7221	then &newvarname = 	22	;
else if	7230	<= &sic <=	7231	then &newvarname = 	22	;
else if	7240	<= &sic <=	7241	then &newvarname = 	22	;
else if	7250	<= &sic <=	7251	then &newvarname = 	22	;
else if	7260	<= &sic <=	7269	then &newvarname = 	22	;
else if	7270	<= &sic <=	7290	then &newvarname = 	22	;
else if	7291	<= &sic <=	7291	then &newvarname = 	22	;
else if	7292	<= &sic <=	7299	then &newvarname = 	22	;
else if	7300	<= &sic <=	7300	then &newvarname = 	22	;
else if	7310	<= &sic <=	7319	then &newvarname = 	22	;
else if	7320	<= &sic <=	7329	then &newvarname = 	22	;
else if	7330	<= &sic <=	7339	then &newvarname = 	22	;
else if	7340	<= &sic <=	7342	then &newvarname = 	22	;
else if	7349	<= &sic <=	7349	then &newvarname = 	22	;
else if	7350	<= &sic <=	7351	then &newvarname = 	22	;
else if	7352	<= &sic <=	7352	then &newvarname = 	22	;
else if	7353	<= &sic <=	7353	then &newvarname = 	22	;
else if	7359	<= &sic <=	7359	then &newvarname = 	22	;
else if	7360	<= &sic <=	7369	then &newvarname = 	22	;
else if	7370	<= &sic <=	7372	then &newvarname = 	22	;
else if	7374	<= &sic <=	7374	then &newvarname = 	22	;
else if	7375	<= &sic <=	7375	then &newvarname = 	22	;
else if	7376	<= &sic <=	7376	then &newvarname = 	22	;
else if	7377	<= &sic <=	7377	then &newvarname = 	22	;
else if	7378	<= &sic <=	7378	then &newvarname = 	22	;
else if	7379	<= &sic <=	7379	then &newvarname = 	22	;
else if	7380	<= &sic <=	7380	then &newvarname = 	22	;
else if	7381	<= &sic <=	7382	then &newvarname = 	22	;
else if	7383	<= &sic <=	7383	then &newvarname = 	22	;
else if	7384	<= &sic <=	7384	then &newvarname = 	22	;
else if	7385	<= &sic <=	7385	then &newvarname = 	22	;
else if	7389	<= &sic <=	7390	then &newvarname = 	22	;
else if	7391	<= &sic <=	7391	then &newvarname = 	22	;
else if	7392	<= &sic <=	7392	then &newvarname = 	22	;
else if	7393	<= &sic <=	7393	then &newvarname = 	22	;
else if	7394	<= &sic <=	7394	then &newvarname = 	22	;
else if	7395	<= &sic <=	7395	then &newvarname = 	22	;
else if	7396	<= &sic <=	7396	then &newvarname = 	22	;
else if	7397	<= &sic <=	7397	then &newvarname = 	22	;
else if	7399	<= &sic <=	7399	then &newvarname = 	22	;
else if	7500	<= &sic <=	7500	then &newvarname = 	22	;
else if	7510	<= &sic <=	7519	then &newvarname = 	22	;
else if	7520	<= &sic <=	7529	then &newvarname = 	22	;
else if	7530	<= &sic <=	7539	then &newvarname = 	22	;
else if	7540	<= &sic <=	7549	then &newvarname = 	22	;
else if	7600	<= &sic <=	7600	then &newvarname = 	22	;
else if	7620	<= &sic <=	7620	then &newvarname = 	22	;
else if	7622	<= &sic <=	7622	then &newvarname = 	22	;
else if	7623	<= &sic <=	7623	then &newvarname = 	22	;
else if	7629	<= &sic <=	7629	then &newvarname = 	22	;
else if	7630	<= &sic <=	7631	then &newvarname = 	22	;
else if	7640	<= &sic <=	7641	then &newvarname = 	22	;
else if	7690	<= &sic <=	7699	then &newvarname = 	22	;
else if	8100	<= &sic <=	8199	then &newvarname = 	22	;
else if	8200	<= &sic <=	8299	then &newvarname = 	22	;
else if	8300	<= &sic <=	8399	then &newvarname = 	22	;
else if	8400	<= &sic <=	8499	then &newvarname = 	22	;
else if	8600	<= &sic <=	8699	then &newvarname = 	22	;
else if	8700	<= &sic <=	8700	then &newvarname = 	22	;
else if	8710	<= &sic <=	8713	then &newvarname = 	22	;
else if	8720	<= &sic <=	8721	then &newvarname = 	22	;
else if	8730	<= &sic <=	8734	then &newvarname = 	22	;
else if	8740	<= &sic <=	8748	then &newvarname = 	22	;
else if	8800	<= &sic <=	8899	then &newvarname = 	22	;
else if	8900	<= &sic <=	8910	then &newvarname = 	22	;
else if	8911	<= &sic <=	8911	then &newvarname = 	22	;
else if	8920	<= &sic <=	8999	then &newvarname = 	22	;
else if	3570	<= &sic <=	3579	then &newvarname = 	23	;
else if	3622	<= &sic <=	3622	then &newvarname = 	23	;
else if	3661	<= &sic <=	3661	then &newvarname = 	23	;
else if	3662	<= &sic <=	3662	then &newvarname = 	23	;
else if	3663	<= &sic <=	3663	then &newvarname = 	23	;
else if	3664	<= &sic <=	3664	then &newvarname = 	23	;
else if	3665	<= &sic <=	3665	then &newvarname = 	23	;
else if	3666	<= &sic <=	3666	then &newvarname = 	23	;
else if	3669	<= &sic <=	3669	then &newvarname = 	23	;
else if	3670	<= &sic <=	3679	then &newvarname = 	23	;
else if	3680	<= &sic <=	3680	then &newvarname = 	23	;
else if	3681	<= &sic <=	3681	then &newvarname = 	23	;
else if	3682	<= &sic <=	3682	then &newvarname = 	23	;
else if	3683	<= &sic <=	3683	then &newvarname = 	23	;
else if	3684	<= &sic <=	3684	then &newvarname = 	23	;
else if	3685	<= &sic <=	3685	then &newvarname = 	23	;
else if	3686	<= &sic <=	3686	then &newvarname = 	23	;
else if	3687	<= &sic <=	3687	then &newvarname = 	23	;
else if	3688	<= &sic <=	3688	then &newvarname = 	23	;
else if	3689	<= &sic <=	3689	then &newvarname = 	23	;
else if	3695	<= &sic <=	3695	then &newvarname = 	23	;
else if	3810	<= &sic <=	3810	then &newvarname = 	23	;
else if	3811	<= &sic <=	3811	then &newvarname = 	23	;
else if	3812	<= &sic <=	3812	then &newvarname = 	23	;
else if	3820	<= &sic <=	3820	then &newvarname = 	23	;
else if	3821	<= &sic <=	3821	then &newvarname = 	23	;
else if	3822	<= &sic <=	3822	then &newvarname = 	23	;
else if	3823	<= &sic <=	3823	then &newvarname = 	23	;
else if	3824	<= &sic <=	3824	then &newvarname = 	23	;
else if	3825	<= &sic <=	3825	then &newvarname = 	23	;
else if	3826	<= &sic <=	3826	then &newvarname = 	23	;
else if	3827	<= &sic <=	3827	then &newvarname = 	23	;
else if	3829	<= &sic <=	3829	then &newvarname = 	23	;
else if	3830	<= &sic <=	3839	then &newvarname = 	23	;
else if	7373	<= &sic <=	7373	then &newvarname = 	23	;
else if	2440	<= &sic <=	2449	then &newvarname = 	24	;
else if	2520	<= &sic <=	2549	then &newvarname = 	24	;
else if	2600	<= &sic <=	2639	then &newvarname = 	24	;
else if	2640	<= &sic <=	2659	then &newvarname = 	24	;
else if	2670	<= &sic <=	2699	then &newvarname = 	24	;
else if	2760	<= &sic <=	2761	then &newvarname = 	24	;
else if	3220	<= &sic <=	3221	then &newvarname = 	24	;
else if	3410	<= &sic <=	3412	then &newvarname = 	24	;
else if	3950	<= &sic <=	3955	then &newvarname = 	24	;
else if	4000	<= &sic <=	4013	then &newvarname = 	25	;
else if	4040	<= &sic <=	4049	then &newvarname = 	25	;
else if	4100	<= &sic <=	4100	then &newvarname = 	25	;
else if	4110	<= &sic <=	4119	then &newvarname = 	25	;
else if	4120	<= &sic <=	4121	then &newvarname = 	25	;
else if	4130	<= &sic <=	4131	then &newvarname = 	25	;
else if	4140	<= &sic <=	4142	then &newvarname = 	25	;
else if	4150	<= &sic <=	4151	then &newvarname = 	25	;
else if	4170	<= &sic <=	4173	then &newvarname = 	25	;
else if	4190	<= &sic <=	4199	then &newvarname = 	25	;
else if	4200	<= &sic <=	4200	then &newvarname = 	25	;
else if	4210	<= &sic <=	4219	then &newvarname = 	25	;
else if	4220	<= &sic <=	4229	then &newvarname = 	25	;
else if	4230	<= &sic <=	4231	then &newvarname = 	25	;
else if	4240	<= &sic <=	4249	then &newvarname = 	25	;
else if	4400	<= &sic <=	4499	then &newvarname = 	25	;
else if	4500	<= &sic <=	4599	then &newvarname = 	25	;
else if	4600	<= &sic <=	4699	then &newvarname = 	25	;
else if	4700	<= &sic <=	4700	then &newvarname = 	25	;
else if	4710	<= &sic <=	4712	then &newvarname = 	25	;
else if	4720	<= &sic <=	4729	then &newvarname = 	25	;
else if	4730	<= &sic <=	4739	then &newvarname = 	25	;
else if	4740	<= &sic <=	4749	then &newvarname = 	25	;
else if	4780	<= &sic <=	4780	then &newvarname = 	25	;
else if	4782	<= &sic <=	4782	then &newvarname = 	25	;
else if	4783	<= &sic <=	4783	then &newvarname = 	25	;
else if	4784	<= &sic <=	4784	then &newvarname = 	25	;
else if	4785	<= &sic <=	4785	then &newvarname = 	25	;
else if	4789	<= &sic <=	4789	then &newvarname = 	25	;
else if	5000	<= &sic <=	5000	then &newvarname = 	26	;
else if	5010	<= &sic <=	5015	then &newvarname = 	26	;
else if	5020	<= &sic <=	5023	then &newvarname = 	26	;
else if	5030	<= &sic <=	5039	then &newvarname = 	26	;
else if	5040	<= &sic <=	5042	then &newvarname = 	26	;
else if	5043	<= &sic <=	5043	then &newvarname = 	26	;
else if	5044	<= &sic <=	5044	then &newvarname = 	26	;
else if	5045	<= &sic <=	5045	then &newvarname = 	26	;
else if	5046	<= &sic <=	5046	then &newvarname = 	26	;
else if	5047	<= &sic <=	5047	then &newvarname = 	26	;
else if	5048	<= &sic <=	5048	then &newvarname = 	26	;
else if	5049	<= &sic <=	5049	then &newvarname = 	26	;
else if	5050	<= &sic <=	5059	then &newvarname = 	26	;
else if	5060	<= &sic <=	5060	then &newvarname = 	26	;
else if	5063	<= &sic <=	5063	then &newvarname = 	26	;
else if	5064	<= &sic <=	5064	then &newvarname = 	26	;
else if	5065	<= &sic <=	5065	then &newvarname = 	26	;
else if	5070	<= &sic <=	5078	then &newvarname = 	26	;
else if	5080	<= &sic <=	5080	then &newvarname = 	26	;
else if	5081	<= &sic <=	5081	then &newvarname = 	26	;
else if	5082	<= &sic <=	5082	then &newvarname = 	26	;
else if	5083	<= &sic <=	5083	then &newvarname = 	26	;
else if	5084	<= &sic <=	5084	then &newvarname = 	26	;
else if	5085	<= &sic <=	5085	then &newvarname = 	26	;
else if	5086	<= &sic <=	5087	then &newvarname = 	26	;
else if	5088	<= &sic <=	5088	then &newvarname = 	26	;
else if	5090	<= &sic <=	5090	then &newvarname = 	26	;
else if	5091	<= &sic <=	5092	then &newvarname = 	26	;
else if	5093	<= &sic <=	5093	then &newvarname = 	26	;
else if	5094	<= &sic <=	5094	then &newvarname = 	26	;
else if	5099	<= &sic <=	5099	then &newvarname = 	26	;
else if	5100	<= &sic <=	5100	then &newvarname = 	26	;
else if	5110	<= &sic <=	5113	then &newvarname = 	26	;
else if	5120	<= &sic <=	5122	then &newvarname = 	26	;
else if	5130	<= &sic <=	5139	then &newvarname = 	26	;
else if	5140	<= &sic <=	5149	then &newvarname = 	26	;
else if	5150	<= &sic <=	5159	then &newvarname = 	26	;
else if	5160	<= &sic <=	5169	then &newvarname = 	26	;
else if	5170	<= &sic <=	5172	then &newvarname = 	26	;
else if	5180	<= &sic <=	5182	then &newvarname = 	26	;
else if	5190	<= &sic <=	5199	then &newvarname = 	26	;
else if	5200	<= &sic <=	5200	then &newvarname = 	27	;
else if	5210	<= &sic <=	5219	then &newvarname = 	27	;
else if	5220	<= &sic <=	5229	then &newvarname = 	27	;
else if	5230	<= &sic <=	5231	then &newvarname = 	27	;
else if	5250	<= &sic <=	5251	then &newvarname = 	27	;
else if	5260	<= &sic <=	5261	then &newvarname = 	27	;
else if	5270	<= &sic <=	5271	then &newvarname = 	27	;
else if	5300	<= &sic <=	5300	then &newvarname = 	27	;
else if	5310	<= &sic <=	5311	then &newvarname = 	27	;
else if	5320	<= &sic <=	5320	then &newvarname = 	27	;
else if	5330	<= &sic <=	5331	then &newvarname = 	27	;
else if	5334	<= &sic <=	5334	then &newvarname = 	27	;
else if	5340	<= &sic <=	5349	then &newvarname = 	27	;
else if	5390	<= &sic <=	5399	then &newvarname = 	27	;
else if	5400	<= &sic <=	5400	then &newvarname = 	27	;
else if	5410	<= &sic <=	5411	then &newvarname = 	27	;
else if	5412	<= &sic <=	5412	then &newvarname = 	27	;
else if	5420	<= &sic <=	5429	then &newvarname = 	27	;
else if	5430	<= &sic <=	5439	then &newvarname = 	27	;
else if	5440	<= &sic <=	5449	then &newvarname = 	27	;
else if	5450	<= &sic <=	5459	then &newvarname = 	27	;
else if	5460	<= &sic <=	5469	then &newvarname = 	27	;
else if	5490	<= &sic <=	5499	then &newvarname = 	27	;
else if	5500	<= &sic <=	5500	then &newvarname = 	27	;
else if	5510	<= &sic <=	5529	then &newvarname = 	27	;
else if	5530	<= &sic <=	5539	then &newvarname = 	27	;
else if	5540	<= &sic <=	5549	then &newvarname = 	27	;
else if	5550	<= &sic <=	5559	then &newvarname = 	27	;
else if	5560	<= &sic <=	5569	then &newvarname = 	27	;
else if	5570	<= &sic <=	5579	then &newvarname = 	27	;
else if	5590	<= &sic <=	5599	then &newvarname = 	27	;
else if	5600	<= &sic <=	5699	then &newvarname = 	27	;
else if	5700	<= &sic <=	5700	then &newvarname = 	27	;
else if	5710	<= &sic <=	5719	then &newvarname = 	27	;
else if	5720	<= &sic <=	5722	then &newvarname = 	27	;
else if	5730	<= &sic <=	5733	then &newvarname = 	27	;
else if	5734	<= &sic <=	5734	then &newvarname = 	27	;
else if	5735	<= &sic <=	5735	then &newvarname = 	27	;
else if	5736	<= &sic <=	5736	then &newvarname = 	27	;
else if	5750	<= &sic <=	5799	then &newvarname = 	27	;
else if	5900	<= &sic <=	5900	then &newvarname = 	27	;
else if	5910	<= &sic <=	5912	then &newvarname = 	27	;
else if	5920	<= &sic <=	5929	then &newvarname = 	27	;
else if	5930	<= &sic <=	5932	then &newvarname = 	27	;
else if	5940	<= &sic <=	5940	then &newvarname = 	27	;
else if	5941	<= &sic <=	5941	then &newvarname = 	27	;
else if	5942	<= &sic <=	5942	then &newvarname = 	27	;
else if	5943	<= &sic <=	5943	then &newvarname = 	27	;
else if	5944	<= &sic <=	5944	then &newvarname = 	27	;
else if	5945	<= &sic <=	5945	then &newvarname = 	27	;
else if	5946	<= &sic <=	5946	then &newvarname = 	27	;
else if	5947	<= &sic <=	5947	then &newvarname = 	27	;
else if	5948	<= &sic <=	5948	then &newvarname = 	27	;
else if	5949	<= &sic <=	5949	then &newvarname = 	27	;
else if	5950	<= &sic <=	5959	then &newvarname = 	27	;
else if	5960	<= &sic <=	5969	then &newvarname = 	27	;
else if	5970	<= &sic <=	5979	then &newvarname = 	27	;
else if	5980	<= &sic <=	5989	then &newvarname = 	27	;
else if	5990	<= &sic <=	5990	then &newvarname = 	27	;
else if	5992	<= &sic <=	5992	then &newvarname = 	27	;
else if	5993	<= &sic <=	5993	then &newvarname = 	27	;
else if	5994	<= &sic <=	5994	then &newvarname = 	27	;
else if	5995	<= &sic <=	5995	then &newvarname = 	27	;
else if	5999	<= &sic <=	5999	then &newvarname = 	27	;
else if	5800	<= &sic <=	5819	then &newvarname = 	28	;
else if	5820	<= &sic <=	5829	then &newvarname = 	28	;
else if	5890	<= &sic <=	5899	then &newvarname = 	28	;
else if	7000	<= &sic <=	7000	then &newvarname = 	28	;
else if	7010	<= &sic <=	7019	then &newvarname = 	28	;
else if	7040	<= &sic <=	7049	then &newvarname = 	28	;
else if	7213	<= &sic <=	7213	then &newvarname = 	28	;
else if	6000	<= &sic <=	6000	then &newvarname = 	29	;
else if	6010	<= &sic <=	6019	then &newvarname = 	29	;
else if	6020	<= &sic <=	6020	then &newvarname = 	29	;
else if	6021	<= &sic <=	6021	then &newvarname = 	29	;
else if	6022	<= &sic <=	6022	then &newvarname = 	29	;
else if	6023	<= &sic <=	6024	then &newvarname = 	29	;
else if	6025	<= &sic <=	6025	then &newvarname = 	29	;
else if	6026	<= &sic <=	6026	then &newvarname = 	29	;
else if	6027	<= &sic <=	6027	then &newvarname = 	29	;
else if	6028	<= &sic <=	6029	then &newvarname = 	29	;
else if	6030	<= &sic <=	6036	then &newvarname = 	29	;
else if	6040	<= &sic <=	6059	then &newvarname = 	29	;
else if	6060	<= &sic <=	6062	then &newvarname = 	29	;
else if	6080	<= &sic <=	6082	then &newvarname = 	29	;
else if	6090	<= &sic <=	6099	then &newvarname = 	29	;
else if	6100	<= &sic <=	6100	then &newvarname = 	29	;
else if	6110	<= &sic <=	6111	then &newvarname = 	29	;
else if	6112	<= &sic <=	6113	then &newvarname = 	29	;
else if	6120	<= &sic <=	6129	then &newvarname = 	29	;
else if	6130	<= &sic <=	6139	then &newvarname = 	29	;
else if	6140	<= &sic <=	6149	then &newvarname = 	29	;
else if	6150	<= &sic <=	6159	then &newvarname = 	29	;
else if	6160	<= &sic <=	6169	then &newvarname = 	29	;
else if	6170	<= &sic <=	6179	then &newvarname = 	29	;
else if	6190	<= &sic <=	6199	then &newvarname = 	29	;
else if	6200	<= &sic <=	6299	then &newvarname = 	29	;
else if	6300	<= &sic <=	6300	then &newvarname = 	29	;
else if	6310	<= &sic <=	6319	then &newvarname = 	29	;
else if	6320	<= &sic <=	6329	then &newvarname = 	29	;
else if	6330	<= &sic <=	6331	then &newvarname = 	29	;
else if	6350	<= &sic <=	6351	then &newvarname = 	29	;
else if	6360	<= &sic <=	6361	then &newvarname = 	29	;
else if	6370	<= &sic <=	6379	then &newvarname = 	29	;
else if	6390	<= &sic <=	6399	then &newvarname = 	29	;
else if	6400	<= &sic <=	6411	then &newvarname = 	29	;
else if	6500	<= &sic <=	6500	then &newvarname = 	29	;
else if	6510	<= &sic <=	6510	then &newvarname = 	29	;
else if	6512	<= &sic <=	6512	then &newvarname = 	29	;
else if	6513	<= &sic <=	6513	then &newvarname = 	29	;
else if	6514	<= &sic <=	6514	then &newvarname = 	29	;
else if	6515	<= &sic <=	6515	then &newvarname = 	29	;
else if	6517	<= &sic <=	6519	then &newvarname = 	29	;
else if	6520	<= &sic <=	6529	then &newvarname = 	29	;
else if	6530	<= &sic <=	6531	then &newvarname = 	29	;
else if	6532	<= &sic <=	6532	then &newvarname = 	29	;
else if	6540	<= &sic <=	6541	then &newvarname = 	29	;
else if	6550	<= &sic <=	6553	then &newvarname = 	29	;
else if	6590	<= &sic <=	6599	then &newvarname = 	29	;
else if	6610	<= &sic <=	6611	then &newvarname = 	29	;
else if	6700	<= &sic <=	6700	then &newvarname = 	29	;
else if	6710	<= &sic <=	6719	then &newvarname = 	29	;
else if	6720	<= &sic <=	6722	then &newvarname = 	29	;
else if	6723	<= &sic <=	6723	then &newvarname = 	29	;
else if	6724	<= &sic <=	6724	then &newvarname = 	29	;
else if	6725	<= &sic <=	6725	then &newvarname = 	29	;
else if	6726	<= &sic <=	6726	then &newvarname = 	29	;
else if	6730	<= &sic <=	6733	then &newvarname = 	29	;
else if	6740	<= &sic <=	6779	then &newvarname = 	29	;
else if	6790	<= &sic <=	6791	then &newvarname = 	29	;
else if	6792	<= &sic <=	6792	then &newvarname = 	29	;
else if	6793	<= &sic <=	6793	then &newvarname = 	29	;
else if	6794	<= &sic <=	6794	then &newvarname = 	29	;
else if	6795	<= &sic <=	6795	then &newvarname = 	29	;
else if	6798	<= &sic <=	6798	then &newvarname = 	29	;
else if	6799	<= &sic <=	6799	then &newvarname = 	29	;
else if	4950	<= &sic <=	4959	then &newvarname = 	30	;
else if	4960	<= &sic <=	4961	then &newvarname = 	30	;
else if	4970	<= &sic <=	4971	then &newvarname = 	30	;
else if	4990	<= &sic <=	4991	then &newvarname = 	30	;
else &newvarname =30;
run;



proc format;
   value ff
	1="Food Products"
2="Beer and Liquor"
3="Tobacco Products"
4="Recreation"
5="Printing and Publishing"
6="Consumer Goods"
7="Apparel"
8="Healthcare, Medical Equip and Pharmaceuticals"
9="Chemicals"
10="Textiles"
11="Construction and Const. Materials"
12="Steel Works"
13="Fabricated Products"
14="Electrical Equipment"
15="Automobiles"
16="Aircraft, Ships and Equipment"
17="Precious Metals and Industrial Metal Mining"
18="Coal"
19="Petroleum and Natural Gas"
20="Utilities"
21="Communication"
22="Personal and Business Services"
23="Business Equipment"
24="Business Supplies and Shipping Containers"
25="Transportation"
26="Wholesale"
27="Retail"
28="Restaurants, Hotels & Motels"
29="Financial Institutions"
30="Other";
run;
%mend;


*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;
*CREATES FAMA-FRENCH 48 INDUSTRY CLASSIFICATIONS
*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;

%macro ff48(data=,newvarname=industry,sic=sic,out=&data);

data &out;
    set &data;
    &newvarname = 48;
if	100	<= &sic <=	199	then &newvarname = 	1	;
else	if		200	<=	&sic	<=	299	then	&newvarname	=	1	;
else	if		700	<=	&sic	<=	799	then	&newvarname	=	1	;
else	if		910	<=	&sic	<=	919	then	&newvarname	=	1	;
else	if		2048	<=	&sic	<=	2048	then	&newvarname	=	1	;
else	if		2000	<=	&sic	<=	2009	then	&newvarname	=	2	;
else	if		2010	<=	&sic	<=	2019	then	&newvarname	=	2	;
else	if		2020	<=	&sic	<=	2029	then	&newvarname	=	2	;
else	if		2030	<=	&sic	<=	2039	then	&newvarname	=	2	;
else	if		2040	<=	&sic	<=	2046	then	&newvarname	=	2	;
else	if		2050	<=	&sic	<=	2059	then	&newvarname	=	2	;
else	if		2060	<=	&sic	<=	2063	then	&newvarname	=	2	;
else	if		2070	<=	&sic	<=	2079	then	&newvarname	=	2	;
else	if		2090	<=	&sic	<=	2092	then	&newvarname	=	2	;
else	if		2095	<=	&sic	<=	2095	then	&newvarname	=	2	;
else	if		2098	<=	&sic	<=	2099	then	&newvarname	=	2	;
else	if		2064	<=	&sic	<=	2068	then	&newvarname	=	3	;
else	if		2086	<=	&sic	<=	2086	then	&newvarname	=	3	;
else	if		2087	<=	&sic	<=	2087	then	&newvarname	=	3	;
else	if		2096	<=	&sic	<=	2096	then	&newvarname	=	3	;
else	if		2097	<=	&sic	<=	2097	then	&newvarname	=	3	;
else	if		2080	<=	&sic	<=	2080	then	&newvarname	=	4	;
else	if		2082	<=	&sic	<=	2082	then	&newvarname	=	4	;
else	if		2083	<=	&sic	<=	2083	then	&newvarname	=	4	;
else	if		2084	<=	&sic	<=	2084	then	&newvarname	=	4	;
else	if		2085	<=	&sic	<=	2085	then	&newvarname	=	4	;
else	if		2100	<=	&sic	<=	2199	then	&newvarname	=	5	;
else	if		920	<=	&sic	<=	999	then	&newvarname	=	6	;
else	if		3650	<=	&sic	<=	3651	then	&newvarname	=	6	;
else	if		3652	<=	&sic	<=	3652	then	&newvarname	=	6	;
else	if		3732	<=	&sic	<=	3732	then	&newvarname	=	6	;
else	if		3930	<=	&sic	<=	3931	then	&newvarname	=	6	;
else	if		3940	<=	&sic	<=	3949	then	&newvarname	=	6	;
else	if		7800	<=	&sic	<=	7829	then	&newvarname	=	7	;
else	if		7830	<=	&sic	<=	7833	then	&newvarname	=	7	;
else	if		7840	<=	&sic	<=	7841	then	&newvarname	=	7	;
else	if		7900	<=	&sic	<=	7900	then	&newvarname	=	7	;
else	if		7910	<=	&sic	<=	7911	then	&newvarname	=	7	;
else	if		7920	<=	&sic	<=	7929	then	&newvarname	=	7	;
else	if		7930	<=	&sic	<=	7933	then	&newvarname	=	7	;
else	if		7940	<=	&sic	<=	7949	then	&newvarname	=	7	;
else	if		7980	<=	&sic	<=	7980	then	&newvarname	=	7	;
else	if		7990	<=	&sic	<=	7999	then	&newvarname	=	7	;
else	if		2700	<=	&sic	<=	2709	then	&newvarname	=	8	;
else	if		2710	<=	&sic	<=	2719	then	&newvarname	=	8	;
else	if		2720	<=	&sic	<=	2729	then	&newvarname	=	8	;
else	if		2730	<=	&sic	<=	2739	then	&newvarname	=	8	;
else	if		2740	<=	&sic	<=	2749	then	&newvarname	=	8	;
else	if		2770	<=	&sic	<=	2771	then	&newvarname	=	8	;
else	if		2780	<=	&sic	<=	2789	then	&newvarname	=	8	;
else	if		2790	<=	&sic	<=	2799	then	&newvarname	=	8	;
else	if		2047	<=	&sic	<=	2047	then	&newvarname	=	9	;
else	if		2391	<=	&sic	<=	2392	then	&newvarname	=	9	;
else	if		2510	<=	&sic	<=	2519	then	&newvarname	=	9	;
else	if		2590	<=	&sic	<=	2599	then	&newvarname	=	9	;
else	if		2840	<=	&sic	<=	2843	then	&newvarname	=	9	;
else	if		2844	<=	&sic	<=	2844	then	&newvarname	=	9	;
else	if		3160	<=	&sic	<=	3161	then	&newvarname	=	9	;
else	if		3170	<=	&sic	<=	3171	then	&newvarname	=	9	;
else	if		3172	<=	&sic	<=	3172	then	&newvarname	=	9	;
else	if		3190	<=	&sic	<=	3199	then	&newvarname	=	9	;
else	if		3229	<=	&sic	<=	3229	then	&newvarname	=	9	;
else	if		3260	<=	&sic	<=	3260	then	&newvarname	=	9	;
else	if		3262	<=	&sic	<=	3263	then	&newvarname	=	9	;
else	if		3269	<=	&sic	<=	3269	then	&newvarname	=	9	;
else	if		3230	<=	&sic	<=	3231	then	&newvarname	=	9	;
else	if		3630	<=	&sic	<=	3639	then	&newvarname	=	9	;
else	if		3750	<=	&sic	<=	3751	then	&newvarname	=	9	;
else	if		3800	<=	&sic	<=	3800	then	&newvarname	=	9	;
else	if		3860	<=	&sic	<=	3861	then	&newvarname	=	9	;
else	if		3870	<=	&sic	<=	3873	then	&newvarname	=	9	;
else	if		3910	<=	&sic	<=	3911	then	&newvarname	=	9	;
else	if		3914	<=	&sic	<=	3914	then	&newvarname	=	9	;
else	if		3915	<=	&sic	<=	3915	then	&newvarname	=	9	;
else	if		3960	<=	&sic	<=	3962	then	&newvarname	=	9	;
else	if		3991	<=	&sic	<=	3991	then	&newvarname	=	9	;
else	if		3995	<=	&sic	<=	3995	then	&newvarname	=	9	;
else	if		2300	<=	&sic	<=	2390	then	&newvarname	=	10	;
else	if		3020	<=	&sic	<=	3021	then	&newvarname	=	10	;
else	if		3100	<=	&sic	<=	3111	then	&newvarname	=	10	;
else	if		3130	<=	&sic	<=	3131	then	&newvarname	=	10	;
else	if		3140	<=	&sic	<=	3149	then	&newvarname	=	10	;
else	if		3150	<=	&sic	<=	3151	then	&newvarname	=	10	;
else	if		3963	<=	&sic	<=	3965	then	&newvarname	=	10	;
else	if		8000	<=	&sic	<=	8099	then	&newvarname	=	11	;
else	if		3693	<=	&sic	<=	3693	then	&newvarname	=	12	;
else	if		3840	<=	&sic	<=	3849	then	&newvarname	=	12	;
else	if		3850	<=	&sic	<=	3851	then	&newvarname	=	12	;
else	if		2830	<=	&sic	<=	2830	then	&newvarname	=	13	;
else	if		2831	<=	&sic	<=	2831	then	&newvarname	=	13	;
else	if		2833	<=	&sic	<=	2833	then	&newvarname	=	13	;
else	if		2834	<=	&sic	<=	2834	then	&newvarname	=	13	;
else	if		2835	<=	&sic	<=	2835	then	&newvarname	=	13	;
else	if		2836	<=	&sic	<=	2836	then	&newvarname	=	13	;
else	if		2800	<=	&sic	<=	2809	then	&newvarname	=	14	;
else	if		2810	<=	&sic	<=	2819	then	&newvarname	=	14	;
else	if		2820	<=	&sic	<=	2829	then	&newvarname	=	14	;
else	if		2850	<=	&sic	<=	2859	then	&newvarname	=	14	;
else	if		2860	<=	&sic	<=	2869	then	&newvarname	=	14	;
else	if		2870	<=	&sic	<=	2879	then	&newvarname	=	14	;
else	if		2890	<=	&sic	<=	2899	then	&newvarname	=	14	;
else	if		3031	<=	&sic	<=	3031	then	&newvarname	=	15	;
else	if		3041	<=	&sic	<=	3041	then	&newvarname	=	15	;
else	if		3050	<=	&sic	<=	3053	then	&newvarname	=	15	;
else	if		3060	<=	&sic	<=	3069	then	&newvarname	=	15	;
else	if		3070	<=	&sic	<=	3079	then	&newvarname	=	15	;
else	if		3080	<=	&sic	<=	3089	then	&newvarname	=	15	;
else	if		3090	<=	&sic	<=	3099	then	&newvarname	=	15	;
else	if		2200	<=	&sic	<=	2269	then	&newvarname	=	16	;
else	if		2270	<=	&sic	<=	2279	then	&newvarname	=	16	;
else	if		2280	<=	&sic	<=	2284	then	&newvarname	=	16	;
else	if		2290	<=	&sic	<=	2295	then	&newvarname	=	16	;
else	if		2297	<=	&sic	<=	2297	then	&newvarname	=	16	;
else	if		2298	<=	&sic	<=	2298	then	&newvarname	=	16	;
else	if		2299	<=	&sic	<=	2299	then	&newvarname	=	16	;
else	if		2393	<=	&sic	<=	2395	then	&newvarname	=	16	;
else	if		2397	<=	&sic	<=	2399	then	&newvarname	=	16	;
else	if		800	<=	&sic	<=	899	then	&newvarname	=	17	;
else	if		2400	<=	&sic	<=	2439	then	&newvarname	=	17	;
else	if		2450	<=	&sic	<=	2459	then	&newvarname	=	17	;
else	if		2490	<=	&sic	<=	2499	then	&newvarname	=	17	;
else	if		2660	<=	&sic	<=	2661	then	&newvarname	=	17	;
else	if		2950	<=	&sic	<=	2952	then	&newvarname	=	17	;
else	if		3200	<=	&sic	<=	3200	then	&newvarname	=	17	;
else	if		3210	<=	&sic	<=	3211	then	&newvarname	=	17	;
else	if		3240	<=	&sic	<=	3241	then	&newvarname	=	17	;
else	if		3250	<=	&sic	<=	3259	then	&newvarname	=	17	;
else	if		3261	<=	&sic	<=	3261	then	&newvarname	=	17	;
else	if		3264	<=	&sic	<=	3264	then	&newvarname	=	17	;
else	if		3270	<=	&sic	<=	3275	then	&newvarname	=	17	;
else	if		3280	<=	&sic	<=	3281	then	&newvarname	=	17	;
else	if		3290	<=	&sic	<=	3293	then	&newvarname	=	17	;
else	if		3295	<=	&sic	<=	3299	then	&newvarname	=	17	;
else	if		3420	<=	&sic	<=	3429	then	&newvarname	=	17	;
else	if		3430	<=	&sic	<=	3433	then	&newvarname	=	17	;
else	if		3440	<=	&sic	<=	3441	then	&newvarname	=	17	;
else	if		3442	<=	&sic	<=	3442	then	&newvarname	=	17	;
else	if		3446	<=	&sic	<=	3446	then	&newvarname	=	17	;
else	if		3448	<=	&sic	<=	3448	then	&newvarname	=	17	;
else	if		3449	<=	&sic	<=	3449	then	&newvarname	=	17	;
else	if		3450	<=	&sic	<=	3451	then	&newvarname	=	17	;
else	if		3452	<=	&sic	<=	3452	then	&newvarname	=	17	;
else	if		3490	<=	&sic	<=	3499	then	&newvarname	=	17	;
else	if		3996	<=	&sic	<=	3996	then	&newvarname	=	17	;
else	if		1500	<=	&sic	<=	1511	then	&newvarname	=	18	;
else	if		1520	<=	&sic	<=	1529	then	&newvarname	=	18	;
else	if		1530	<=	&sic	<=	1539	then	&newvarname	=	18	;
else	if		1540	<=	&sic	<=	1549	then	&newvarname	=	18	;
else	if		1600	<=	&sic	<=	1699	then	&newvarname	=	18	;
else	if		1700	<=	&sic	<=	1799	then	&newvarname	=	18	;
else	if		3300	<=	&sic	<=	3300	then	&newvarname	=	19	;
else	if		3310	<=	&sic	<=	3317	then	&newvarname	=	19	;
else	if		3320	<=	&sic	<=	3325	then	&newvarname	=	19	;
else	if		3330	<=	&sic	<=	3339	then	&newvarname	=	19	;
else	if		3340	<=	&sic	<=	3341	then	&newvarname	=	19	;
else	if		3350	<=	&sic	<=	3357	then	&newvarname	=	19	;
else	if		3360	<=	&sic	<=	3369	then	&newvarname	=	19	;
else	if		3370	<=	&sic	<=	3379	then	&newvarname	=	19	;
else	if		3390	<=	&sic	<=	3399	then	&newvarname	=	19	;
else	if		3400	<=	&sic	<=	3400	then	&newvarname	=	20	;
else	if		3443	<=	&sic	<=	3443	then	&newvarname	=	20	;
else	if		3444	<=	&sic	<=	3444	then	&newvarname	=	20	;
else	if		3460	<=	&sic	<=	3469	then	&newvarname	=	20	;
else	if		3470	<=	&sic	<=	3479	then	&newvarname	=	20	;
else	if		3510	<=	&sic	<=	3519	then	&newvarname	=	21	;
else	if		3520	<=	&sic	<=	3529	then	&newvarname	=	21	;
else	if		3530	<=	&sic	<=	3530	then	&newvarname	=	21	;
else	if		3531	<=	&sic	<=	3531	then	&newvarname	=	21	;
else	if		3532	<=	&sic	<=	3532	then	&newvarname	=	21	;
else	if		3533	<=	&sic	<=	3533	then	&newvarname	=	21	;
else	if		3534	<=	&sic	<=	3534	then	&newvarname	=	21	;
else	if		3535	<=	&sic	<=	3535	then	&newvarname	=	21	;
else	if		3536	<=	&sic	<=	3536	then	&newvarname	=	21	;
else	if		3538	<=	&sic	<=	3538	then	&newvarname	=	21	;
else	if		3540	<=	&sic	<=	3549	then	&newvarname	=	21	;
else	if		3550	<=	&sic	<=	3559	then	&newvarname	=	21	;
else	if		3560	<=	&sic	<=	3569	then	&newvarname	=	21	;
else	if		3580	<=	&sic	<=	3580	then	&newvarname	=	21	;
else	if		3581	<=	&sic	<=	3581	then	&newvarname	=	21	;
else	if		3582	<=	&sic	<=	3582	then	&newvarname	=	21	;
else	if		3585	<=	&sic	<=	3585	then	&newvarname	=	21	;
else	if		3586	<=	&sic	<=	3586	then	&newvarname	=	21	;
else	if		3589	<=	&sic	<=	3589	then	&newvarname	=	21	;
else	if		3590	<=	&sic	<=	3599	then	&newvarname	=	21	;
else	if		3600	<=	&sic	<=	3600	then	&newvarname	=	22	;
else	if		3610	<=	&sic	<=	3613	then	&newvarname	=	22	;
else	if		3620	<=	&sic	<=	3621	then	&newvarname	=	22	;
else	if		3623	<=	&sic	<=	3629	then	&newvarname	=	22	;
else	if		3640	<=	&sic	<=	3644	then	&newvarname	=	22	;
else	if		3645	<=	&sic	<=	3645	then	&newvarname	=	22	;
else	if		3646	<=	&sic	<=	3646	then	&newvarname	=	22	;
else	if		3648	<=	&sic	<=	3649	then	&newvarname	=	22	;
else	if		3660	<=	&sic	<=	3660	then	&newvarname	=	22	;
else	if		3690	<=	&sic	<=	3690	then	&newvarname	=	22	;
else	if		3691	<=	&sic	<=	3692	then	&newvarname	=	22	;
else	if		3699	<=	&sic	<=	3699	then	&newvarname	=	22	;
else	if		2296	<=	&sic	<=	2296	then	&newvarname	=	23	;
else	if		2396	<=	&sic	<=	2396	then	&newvarname	=	23	;
else	if		3010	<=	&sic	<=	3011	then	&newvarname	=	23	;
else	if		3537	<=	&sic	<=	3537	then	&newvarname	=	23	;
else	if		3647	<=	&sic	<=	3647	then	&newvarname	=	23	;
else	if		3694	<=	&sic	<=	3694	then	&newvarname	=	23	;
else	if		3700	<=	&sic	<=	3700	then	&newvarname	=	23	;
else	if		3710	<=	&sic	<=	3710	then	&newvarname	=	23	;
else	if		3711	<=	&sic	<=	3711	then	&newvarname	=	23	;
else	if		3713	<=	&sic	<=	3713	then	&newvarname	=	23	;
else	if		3714	<=	&sic	<=	3714	then	&newvarname	=	23	;
else	if		3715	<=	&sic	<=	3715	then	&newvarname	=	23	;
else	if		3716	<=	&sic	<=	3716	then	&newvarname	=	23	;
else	if		3792	<=	&sic	<=	3792	then	&newvarname	=	23	;
else	if		3790	<=	&sic	<=	3791	then	&newvarname	=	23	;
else	if		3799	<=	&sic	<=	3799	then	&newvarname	=	23	;
else	if		3720	<=	&sic	<=	3720	then	&newvarname	=	24	;
else	if		3721	<=	&sic	<=	3721	then	&newvarname	=	24	;
else	if		3723	<=	&sic	<=	3724	then	&newvarname	=	24	;
else	if		3725	<=	&sic	<=	3725	then	&newvarname	=	24	;
else	if		3728	<=	&sic	<=	3729	then	&newvarname	=	24	;
else	if		3730	<=	&sic	<=	3731	then	&newvarname	=	25	;
else	if		3740	<=	&sic	<=	3743	then	&newvarname	=	25	;
else	if		3760	<=	&sic	<=	3769	then	&newvarname	=	26	;
else	if		3795	<=	&sic	<=	3795	then	&newvarname	=	26	;
else	if		3480	<=	&sic	<=	3489	then	&newvarname	=	26	;
else	if		1040	<=	&sic	<=	1049	then	&newvarname	=	27	;
else	if		1000	<=	&sic	<=	1009	then	&newvarname	=	28	;
else	if		1010	<=	&sic	<=	1019	then	&newvarname	=	28	;
else	if		1020	<=	&sic	<=	1029	then	&newvarname	=	28	;
else	if		1030	<=	&sic	<=	1039	then	&newvarname	=	28	;
else	if		1050	<=	&sic	<=	1059	then	&newvarname	=	28	;
else	if		1060	<=	&sic	<=	1069	then	&newvarname	=	28	;
else	if		1070	<=	&sic	<=	1079	then	&newvarname	=	28	;
else	if		1080	<=	&sic	<=	1089	then	&newvarname	=	28	;
else	if		1090	<=	&sic	<=	1099	then	&newvarname	=	28	;
else	if		1100	<=	&sic	<=	1119	then	&newvarname	=	28	;
else	if		1400	<=	&sic	<=	1499	then	&newvarname	=	28	;
else	if		1200	<=	&sic	<=	1299	then	&newvarname	=	29	;
else	if		1300	<=	&sic	<=	1300	then	&newvarname	=	30	;
else	if		1310	<=	&sic	<=	1319	then	&newvarname	=	30	;
else	if		1320	<=	&sic	<=	1329	then	&newvarname	=	30	;
else	if		1330	<=	&sic	<=	1339	then	&newvarname	=	30	;
else	if		1370	<=	&sic	<=	1379	then	&newvarname	=	30	;
else	if		1380	<=	&sic	<=	1380	then	&newvarname	=	30	;
else	if		1381	<=	&sic	<=	1381	then	&newvarname	=	30	;
else	if		1382	<=	&sic	<=	1382	then	&newvarname	=	30	;
else	if		1389	<=	&sic	<=	1389	then	&newvarname	=	30	;
else	if		2900	<=	&sic	<=	2912	then	&newvarname	=	30	;
else	if		2990	<=	&sic	<=	2999	then	&newvarname	=	30	;
else	if		4900	<=	&sic	<=	4900	then	&newvarname	=	31	;
else	if		4910	<=	&sic	<=	4911	then	&newvarname	=	31	;
else	if		4920	<=	&sic	<=	4922	then	&newvarname	=	31	;
else	if		4923	<=	&sic	<=	4923	then	&newvarname	=	31	;
else	if		4924	<=	&sic	<=	4925	then	&newvarname	=	31	;
else	if		4930	<=	&sic	<=	4931	then	&newvarname	=	31	;
else	if		4932	<=	&sic	<=	4932	then	&newvarname	=	31	;
else	if		4939	<=	&sic	<=	4939	then	&newvarname	=	31	;
else	if		4940	<=	&sic	<=	4942	then	&newvarname	=	31	;
else	if		4800	<=	&sic	<=	4800	then	&newvarname	=	32	;
else	if		4810	<=	&sic	<=	4813	then	&newvarname	=	32	;
else	if		4820	<=	&sic	<=	4822	then	&newvarname	=	32	;
else	if		4830	<=	&sic	<=	4839	then	&newvarname	=	32	;
else	if		4840	<=	&sic	<=	4841	then	&newvarname	=	32	;
else	if		4880	<=	&sic	<=	4889	then	&newvarname	=	32	;
else	if		4890	<=	&sic	<=	4890	then	&newvarname	=	32	;
else	if		4891	<=	&sic	<=	4891	then	&newvarname	=	32	;
else	if		4892	<=	&sic	<=	4892	then	&newvarname	=	32	;
else	if		4899	<=	&sic	<=	4899	then	&newvarname	=	32	;
else	if		7020	<=	&sic	<=	7021	then	&newvarname	=	33	;
else	if		7030	<=	&sic	<=	7033	then	&newvarname	=	33	;
else	if		7200	<=	&sic	<=	7200	then	&newvarname	=	33	;
else	if		7210	<=	&sic	<=	7212	then	&newvarname	=	33	;
else	if		7214	<=	&sic	<=	7214	then	&newvarname	=	33	;
else	if		7215	<=	&sic	<=	7216	then	&newvarname	=	33	;
else	if		7217	<=	&sic	<=	7217	then	&newvarname	=	33	;
else	if		7219	<=	&sic	<=	7219	then	&newvarname	=	33	;
else	if		7220	<=	&sic	<=	7221	then	&newvarname	=	33	;
else	if		7230	<=	&sic	<=	7231	then	&newvarname	=	33	;
else	if		7240	<=	&sic	<=	7241	then	&newvarname	=	33	;
else	if		7250	<=	&sic	<=	7251	then	&newvarname	=	33	;
else	if		7260	<=	&sic	<=	7269	then	&newvarname	=	33	;
else	if		7270	<=	&sic	<=	7290	then	&newvarname	=	33	;
else	if		7291	<=	&sic	<=	7291	then	&newvarname	=	33	;
else	if		7292	<=	&sic	<=	7299	then	&newvarname	=	33	;
else	if		7395	<=	&sic	<=	7395	then	&newvarname	=	33	;
else	if		7500	<=	&sic	<=	7500	then	&newvarname	=	33	;
else	if		7520	<=	&sic	<=	7529	then	&newvarname	=	33	;
else	if		7530	<=	&sic	<=	7539	then	&newvarname	=	33	;
else	if		7540	<=	&sic	<=	7549	then	&newvarname	=	33	;
else	if		7600	<=	&sic	<=	7600	then	&newvarname	=	33	;
else	if		7620	<=	&sic	<=	7620	then	&newvarname	=	33	;
else	if		7622	<=	&sic	<=	7622	then	&newvarname	=	33	;
else	if		7623	<=	&sic	<=	7623	then	&newvarname	=	33	;
else	if		7629	<=	&sic	<=	7629	then	&newvarname	=	33	;
else	if		7630	<=	&sic	<=	7631	then	&newvarname	=	33	;
else	if		7640	<=	&sic	<=	7641	then	&newvarname	=	33	;
else	if		7690	<=	&sic	<=	7699	then	&newvarname	=	33	;
else	if		8100	<=	&sic	<=	8199	then	&newvarname	=	33	;
else	if		8200	<=	&sic	<=	8299	then	&newvarname	=	33	;
else	if		8300	<=	&sic	<=	8399	then	&newvarname	=	33	;
else	if		8400	<=	&sic	<=	8499	then	&newvarname	=	33	;
else	if		8600	<=	&sic	<=	8699	then	&newvarname	=	33	;
else	if		8800	<=	&sic	<=	8899	then	&newvarname	=	33	;
else	if		7510	<=	&sic	<=	7515	then	&newvarname	=	33	;
else	if		2750	<=	&sic	<=	2759	then	&newvarname	=	34	;
else	if		3993	<=	&sic	<=	3993	then	&newvarname	=	34	;
else	if		7218	<=	&sic	<=	7218	then	&newvarname	=	34	;
else	if		7300	<=	&sic	<=	7300	then	&newvarname	=	34	;
else	if		7310	<=	&sic	<=	7319	then	&newvarname	=	34	;
else	if		7320	<=	&sic	<=	7329	then	&newvarname	=	34	;
else	if		7330	<=	&sic	<=	7339	then	&newvarname	=	34	;
else	if		7340	<=	&sic	<=	7342	then	&newvarname	=	34	;
else	if		7349	<=	&sic	<=	7349	then	&newvarname	=	34	;
else	if		7350	<=	&sic	<=	7351	then	&newvarname	=	34	;
else	if		7352	<=	&sic	<=	7352	then	&newvarname	=	34	;
else	if		7353	<=	&sic	<=	7353	then	&newvarname	=	34	;
else	if		7359	<=	&sic	<=	7359	then	&newvarname	=	34	;
else	if		7360	<=	&sic	<=	7369	then	&newvarname	=	34	;
else	if		7370	<=	&sic	<=	7372	then	&newvarname	=	34	;
else	if		7374	<=	&sic	<=	7374	then	&newvarname	=	34	;
else	if		7375	<=	&sic	<=	7375	then	&newvarname	=	34	;
else	if		7376	<=	&sic	<=	7376	then	&newvarname	=	34	;
else	if		7377	<=	&sic	<=	7377	then	&newvarname	=	34	;
else	if		7378	<=	&sic	<=	7378	then	&newvarname	=	34	;
else	if		7379	<=	&sic	<=	7379	then	&newvarname	=	34	;
else	if		7380	<=	&sic	<=	7380	then	&newvarname	=	34	;
else	if		7381	<=	&sic	<=	7382	then	&newvarname	=	34	;
else	if		7383	<=	&sic	<=	7383	then	&newvarname	=	34	;
else	if		7384	<=	&sic	<=	7384	then	&newvarname	=	34	;
else	if		7385	<=	&sic	<=	7385	then	&newvarname	=	34	;
else	if		7389	<=	&sic	<=	7390	then	&newvarname	=	34	;
else	if		7391	<=	&sic	<=	7391	then	&newvarname	=	34	;
else	if		7392	<=	&sic	<=	7392	then	&newvarname	=	34	;
else	if		7393	<=	&sic	<=	7393	then	&newvarname	=	34	;
else	if		7394	<=	&sic	<=	7394	then	&newvarname	=	34	;
else	if		7396	<=	&sic	<=	7396	then	&newvarname	=	34	;
else	if		7397	<=	&sic	<=	7397	then	&newvarname	=	34	;
else	if		7399	<=	&sic	<=	7399	then	&newvarname	=	34	;
else	if		7519	<=	&sic	<=	7519	then	&newvarname	=	34	;
else	if		8700	<=	&sic	<=	8700	then	&newvarname	=	34	;
else	if		8710	<=	&sic	<=	8713	then	&newvarname	=	34	;
else	if		8720	<=	&sic	<=	8721	then	&newvarname	=	34	;
else	if		8730	<=	&sic	<=	8734	then	&newvarname	=	34	;
else	if		8740	<=	&sic	<=	8748	then	&newvarname	=	34	;
else	if		8900	<=	&sic	<=	8910	then	&newvarname	=	34	;
else	if		8911	<=	&sic	<=	8911	then	&newvarname	=	34	;
else	if		8920	<=	&sic	<=	8999	then	&newvarname	=	34	;
else	if		4220	<=	&sic	<=	4229	then	&newvarname	=	34	;
else	if		3570	<=	&sic	<=	3579	then	&newvarname	=	35	;
else	if		3680	<=	&sic	<=	3680	then	&newvarname	=	35	;
else	if		3681	<=	&sic	<=	3681	then	&newvarname	=	35	;
else	if		3682	<=	&sic	<=	3682	then	&newvarname	=	35	;
else	if		3683	<=	&sic	<=	3683	then	&newvarname	=	35	;
else	if		3684	<=	&sic	<=	3684	then	&newvarname	=	35	;
else	if		3685	<=	&sic	<=	3685	then	&newvarname	=	35	;
else	if		3686	<=	&sic	<=	3686	then	&newvarname	=	35	;
else	if		3687	<=	&sic	<=	3687	then	&newvarname	=	35	;
else	if		3688	<=	&sic	<=	3688	then	&newvarname	=	35	;
else	if		3689	<=	&sic	<=	3689	then	&newvarname	=	35	;
else	if		3695	<=	&sic	<=	3695	then	&newvarname	=	35	;
else	if		7373	<=	&sic	<=	7373	then	&newvarname	=	35	;
else	if		3622	<=	&sic	<=	3622	then	&newvarname	=	36	;
else	if		3661	<=	&sic	<=	3661	then	&newvarname	=	36	;
else	if		3662	<=	&sic	<=	3662	then	&newvarname	=	36	;
else	if		3663	<=	&sic	<=	3663	then	&newvarname	=	36	;
else	if		3664	<=	&sic	<=	3664	then	&newvarname	=	36	;
else	if		3665	<=	&sic	<=	3665	then	&newvarname	=	36	;
else	if		3666	<=	&sic	<=	3666	then	&newvarname	=	36	;
else	if		3669	<=	&sic	<=	3669	then	&newvarname	=	36	;
else	if		3670	<=	&sic	<=	3679	then	&newvarname	=	36	;
else	if		3810	<=	&sic	<=	3810	then	&newvarname	=	36	;
else	if		3812	<=	&sic	<=	3812	then	&newvarname	=	36	;
else	if		3811	<=	&sic	<=	3811	then	&newvarname	=	37	;
else	if		3820	<=	&sic	<=	3820	then	&newvarname	=	37	;
else	if		3821	<=	&sic	<=	3821	then	&newvarname	=	37	;
else	if		3822	<=	&sic	<=	3822	then	&newvarname	=	37	;
else	if		3823	<=	&sic	<=	3823	then	&newvarname	=	37	;
else	if		3824	<=	&sic	<=	3824	then	&newvarname	=	37	;
else	if		3825	<=	&sic	<=	3825	then	&newvarname	=	37	;
else	if		3826	<=	&sic	<=	3826	then	&newvarname	=	37	;
else	if		3827	<=	&sic	<=	3827	then	&newvarname	=	37	;
else	if		3829	<=	&sic	<=	3829	then	&newvarname	=	37	;
else	if		3830	<=	&sic	<=	3839	then	&newvarname	=	37	;
else	if		2520	<=	&sic	<=	2549	then	&newvarname	=	38	;
else	if		2600	<=	&sic	<=	2639	then	&newvarname	=	38	;
else	if		2670	<=	&sic	<=	2699	then	&newvarname	=	38	;
else	if		2760	<=	&sic	<=	2761	then	&newvarname	=	38	;
else	if		3950	<=	&sic	<=	3955	then	&newvarname	=	38	;
else	if		2440	<=	&sic	<=	2449	then	&newvarname	=	39	;
else	if		2640	<=	&sic	<=	2659	then	&newvarname	=	39	;
else	if		3220	<=	&sic	<=	3221	then	&newvarname	=	39	;
else	if		3410	<=	&sic	<=	3412	then	&newvarname	=	39	;
else	if		4000	<=	&sic	<=	4013	then	&newvarname	=	40	;
else	if		4040	<=	&sic	<=	4049	then	&newvarname	=	40	;
else	if		4100	<=	&sic	<=	4100	then	&newvarname	=	40	;
else	if		4110	<=	&sic	<=	4119	then	&newvarname	=	40	;
else	if		4120	<=	&sic	<=	4121	then	&newvarname	=	40	;
else	if		4130	<=	&sic	<=	4131	then	&newvarname	=	40	;
else	if		4140	<=	&sic	<=	4142	then	&newvarname	=	40	;
else	if		4150	<=	&sic	<=	4151	then	&newvarname	=	40	;
else	if		4170	<=	&sic	<=	4173	then	&newvarname	=	40	;
else	if		4190	<=	&sic	<=	4199	then	&newvarname	=	40	;
else	if		4200	<=	&sic	<=	4200	then	&newvarname	=	40	;
else	if		4210	<=	&sic	<=	4219	then	&newvarname	=	40	;
else	if		4230	<=	&sic	<=	4231	then	&newvarname	=	40	;
else	if		4240	<=	&sic	<=	4249	then	&newvarname	=	40	;
else	if		4400	<=	&sic	<=	4499	then	&newvarname	=	40	;
else	if		4500	<=	&sic	<=	4599	then	&newvarname	=	40	;
else	if		4600	<=	&sic	<=	4699	then	&newvarname	=	40	;
else	if		4700	<=	&sic	<=	4700	then	&newvarname	=	40	;
else	if		4710	<=	&sic	<=	4712	then	&newvarname	=	40	;
else	if		4720	<=	&sic	<=	4729	then	&newvarname	=	40	;
else	if		4730	<=	&sic	<=	4739	then	&newvarname	=	40	;
else	if		4740	<=	&sic	<=	4749	then	&newvarname	=	40	;
else	if		4780	<=	&sic	<=	4780	then	&newvarname	=	40	;
else	if		4782	<=	&sic	<=	4782	then	&newvarname	=	40	;
else	if		4783	<=	&sic	<=	4783	then	&newvarname	=	40	;
else	if		4784	<=	&sic	<=	4784	then	&newvarname	=	40	;
else	if		4785	<=	&sic	<=	4785	then	&newvarname	=	40	;
else	if		4789	<=	&sic	<=	4789	then	&newvarname	=	40	;
else	if		5000	<=	&sic	<=	5000	then	&newvarname	=	41	;
else	if		5010	<=	&sic	<=	5015	then	&newvarname	=	41	;
else	if		5020	<=	&sic	<=	5023	then	&newvarname	=	41	;
else	if		5030	<=	&sic	<=	5039	then	&newvarname	=	41	;
else	if		5040	<=	&sic	<=	5042	then	&newvarname	=	41	;
else	if		5043	<=	&sic	<=	5043	then	&newvarname	=	41	;
else	if		5044	<=	&sic	<=	5044	then	&newvarname	=	41	;
else	if		5045	<=	&sic	<=	5045	then	&newvarname	=	41	;
else	if		5046	<=	&sic	<=	5046	then	&newvarname	=	41	;
else	if		5047	<=	&sic	<=	5047	then	&newvarname	=	41	;
else	if		5048	<=	&sic	<=	5048	then	&newvarname	=	41	;
else	if		5049	<=	&sic	<=	5049	then	&newvarname	=	41	;
else	if		5050	<=	&sic	<=	5059	then	&newvarname	=	41	;
else	if		5060	<=	&sic	<=	5060	then	&newvarname	=	41	;
else	if		5063	<=	&sic	<=	5063	then	&newvarname	=	41	;
else	if		5064	<=	&sic	<=	5064	then	&newvarname	=	41	;
else	if		5065	<=	&sic	<=	5065	then	&newvarname	=	41	;
else	if		5070	<=	&sic	<=	5078	then	&newvarname	=	41	;
else	if		5080	<=	&sic	<=	5080	then	&newvarname	=	41	;
else	if		5081	<=	&sic	<=	5081	then	&newvarname	=	41	;
else	if		5082	<=	&sic	<=	5082	then	&newvarname	=	41	;
else	if		5083	<=	&sic	<=	5083	then	&newvarname	=	41	;
else	if		5084	<=	&sic	<=	5084	then	&newvarname	=	41	;
else	if		5085	<=	&sic	<=	5085	then	&newvarname	=	41	;
else	if		5086	<=	&sic	<=	5087	then	&newvarname	=	41	;
else	if		5088	<=	&sic	<=	5088	then	&newvarname	=	41	;
else	if		5090	<=	&sic	<=	5090	then	&newvarname	=	41	;
else	if		5091	<=	&sic	<=	5092	then	&newvarname	=	41	;
else	if		5093	<=	&sic	<=	5093	then	&newvarname	=	41	;
else	if		5094	<=	&sic	<=	5094	then	&newvarname	=	41	;
else	if		5099	<=	&sic	<=	5099	then	&newvarname	=	41	;
else	if		5100	<=	&sic	<=	5100	then	&newvarname	=	41	;
else	if		5110	<=	&sic	<=	5113	then	&newvarname	=	41	;
else	if		5120	<=	&sic	<=	5122	then	&newvarname	=	41	;
else	if		5130	<=	&sic	<=	5139	then	&newvarname	=	41	;
else	if		5140	<=	&sic	<=	5149	then	&newvarname	=	41	;
else	if		5150	<=	&sic	<=	5159	then	&newvarname	=	41	;
else	if		5160	<=	&sic	<=	5169	then	&newvarname	=	41	;
else	if		5170	<=	&sic	<=	5172	then	&newvarname	=	41	;
else	if		5180	<=	&sic	<=	5182	then	&newvarname	=	41	;
else	if		5190	<=	&sic	<=	5199	then	&newvarname	=	41	;
else	if		5200	<=	&sic	<=	5200	then	&newvarname	=	42	;
else	if		5210	<=	&sic	<=	5219	then	&newvarname	=	42	;
else	if		5220	<=	&sic	<=	5229	then	&newvarname	=	42	;
else	if		5230	<=	&sic	<=	5231	then	&newvarname	=	42	;
else	if		5250	<=	&sic	<=	5251	then	&newvarname	=	42	;
else	if		5260	<=	&sic	<=	5261	then	&newvarname	=	42	;
else	if		5270	<=	&sic	<=	5271	then	&newvarname	=	42	;
else	if		5300	<=	&sic	<=	5300	then	&newvarname	=	42	;
else	if		5310	<=	&sic	<=	5311	then	&newvarname	=	42	;
else	if		5320	<=	&sic	<=	5320	then	&newvarname	=	42	;
else	if		5330	<=	&sic	<=	5331	then	&newvarname	=	42	;
else	if		5334	<=	&sic	<=	5334	then	&newvarname	=	42	;
else	if		5340	<=	&sic	<=	5349	then	&newvarname	=	42	;
else	if		5390	<=	&sic	<=	5399	then	&newvarname	=	42	;
else	if		5400	<=	&sic	<=	5400	then	&newvarname	=	42	;
else	if		5410	<=	&sic	<=	5411	then	&newvarname	=	42	;
else	if		5412	<=	&sic	<=	5412	then	&newvarname	=	42	;
else	if		5420	<=	&sic	<=	5429	then	&newvarname	=	42	;
else	if		5430	<=	&sic	<=	5439	then	&newvarname	=	42	;
else	if		5440	<=	&sic	<=	5449	then	&newvarname	=	42	;
else	if		5450	<=	&sic	<=	5459	then	&newvarname	=	42	;
else	if		5460	<=	&sic	<=	5469	then	&newvarname	=	42	;
else	if		5490	<=	&sic	<=	5499	then	&newvarname	=	42	;
else	if		5500	<=	&sic	<=	5500	then	&newvarname	=	42	;
else	if		5510	<=	&sic	<=	5529	then	&newvarname	=	42	;
else	if		5530	<=	&sic	<=	5539	then	&newvarname	=	42	;
else	if		5540	<=	&sic	<=	5549	then	&newvarname	=	42	;
else	if		5550	<=	&sic	<=	5559	then	&newvarname	=	42	;
else	if		5560	<=	&sic	<=	5569	then	&newvarname	=	42	;
else	if		5570	<=	&sic	<=	5579	then	&newvarname	=	42	;
else	if		5590	<=	&sic	<=	5599	then	&newvarname	=	42	;
else	if		5600	<=	&sic	<=	5699	then	&newvarname	=	42	;
else	if		5700	<=	&sic	<=	5700	then	&newvarname	=	42	;
else	if		5710	<=	&sic	<=	5719	then	&newvarname	=	42	;
else	if		5720	<=	&sic	<=	5722	then	&newvarname	=	42	;
else	if		5730	<=	&sic	<=	5733	then	&newvarname	=	42	;
else	if		5734	<=	&sic	<=	5734	then	&newvarname	=	42	;
else	if		5735	<=	&sic	<=	5735	then	&newvarname	=	42	;
else	if		5736	<=	&sic	<=	5736	then	&newvarname	=	42	;
else	if		5750	<=	&sic	<=	5799	then	&newvarname	=	42	;
else	if		5900	<=	&sic	<=	5900	then	&newvarname	=	42	;
else	if		5910	<=	&sic	<=	5912	then	&newvarname	=	42	;
else	if		5920	<=	&sic	<=	5929	then	&newvarname	=	42	;
else	if		5930	<=	&sic	<=	5932	then	&newvarname	=	42	;
else	if		5940	<=	&sic	<=	5940	then	&newvarname	=	42	;
else	if		5941	<=	&sic	<=	5941	then	&newvarname	=	42	;
else	if		5942	<=	&sic	<=	5942	then	&newvarname	=	42	;
else	if		5943	<=	&sic	<=	5943	then	&newvarname	=	42	;
else	if		5944	<=	&sic	<=	5944	then	&newvarname	=	42	;
else	if		5945	<=	&sic	<=	5945	then	&newvarname	=	42	;
else	if		5946	<=	&sic	<=	5946	then	&newvarname	=	42	;
else	if		5947	<=	&sic	<=	5947	then	&newvarname	=	42	;
else	if		5948	<=	&sic	<=	5948	then	&newvarname	=	42	;
else	if		5949	<=	&sic	<=	5949	then	&newvarname	=	42	;
else	if		5950	<=	&sic	<=	5959	then	&newvarname	=	42	;
else	if		5960	<=	&sic	<=	5969	then	&newvarname	=	42	;
else	if		5970	<=	&sic	<=	5979	then	&newvarname	=	42	;
else	if		5980	<=	&sic	<=	5989	then	&newvarname	=	42	;
else	if		5990	<=	&sic	<=	5990	then	&newvarname	=	42	;
else	if		5992	<=	&sic	<=	5992	then	&newvarname	=	42	;
else	if		5993	<=	&sic	<=	5993	then	&newvarname	=	42	;
else	if		5994	<=	&sic	<=	5994	then	&newvarname	=	42	;
else	if		5995	<=	&sic	<=	5995	then	&newvarname	=	42	;
else	if		5999	<=	&sic	<=	5999	then	&newvarname	=	42	;
else	if		5800	<=	&sic	<=	5819	then	&newvarname	=	43	;
else	if		5820	<=	&sic	<=	5829	then	&newvarname	=	43	;
else	if		5890	<=	&sic	<=	5899	then	&newvarname	=	43	;
else	if		7000	<=	&sic	<=	7000	then	&newvarname	=	43	;
else	if		7010	<=	&sic	<=	7019	then	&newvarname	=	43	;
else	if		7040	<=	&sic	<=	7049	then	&newvarname	=	43	;
else	if		7213	<=	&sic	<=	7213	then	&newvarname	=	43	;
else	if		6000	<=	&sic	<=	6000	then	&newvarname	=	44	;
else	if		6010	<=	&sic	<=	6019	then	&newvarname	=	44	;
else	if		6020	<=	&sic	<=	6020	then	&newvarname	=	44	;
else	if		6021	<=	&sic	<=	6021	then	&newvarname	=	44	;
else	if		6022	<=	&sic	<=	6022	then	&newvarname	=	44	;
else	if		6023	<=	&sic	<=	6024	then	&newvarname	=	44	;
else	if		6025	<=	&sic	<=	6025	then	&newvarname	=	44	;
else	if		6026	<=	&sic	<=	6026	then	&newvarname	=	44	;
else	if		6027	<=	&sic	<=	6027	then	&newvarname	=	44	;
else	if		6028	<=	&sic	<=	6029	then	&newvarname	=	44	;
else	if		6030	<=	&sic	<=	6036	then	&newvarname	=	44	;
else	if		6040	<=	&sic	<=	6059	then	&newvarname	=	44	;
else	if		6060	<=	&sic	<=	6062	then	&newvarname	=	44	;
else	if		6080	<=	&sic	<=	6082	then	&newvarname	=	44	;
else	if		6090	<=	&sic	<=	6099	then	&newvarname	=	44	;
else	if		6100	<=	&sic	<=	6100	then	&newvarname	=	44	;
else	if		6110	<=	&sic	<=	6111	then	&newvarname	=	44	;
else	if		6112	<=	&sic	<=	6113	then	&newvarname	=	44	;
else	if		6120	<=	&sic	<=	6129	then	&newvarname	=	44	;
else	if		6130	<=	&sic	<=	6139	then	&newvarname	=	44	;
else	if		6140	<=	&sic	<=	6149	then	&newvarname	=	44	;
else	if		6150	<=	&sic	<=	6159	then	&newvarname	=	44	;
else	if		6160	<=	&sic	<=	6169	then	&newvarname	=	44	;
else	if		6170	<=	&sic	<=	6179	then	&newvarname	=	44	;
else	if		6190	<=	&sic	<=	6199	then	&newvarname	=	44	;
else	if		6300	<=	&sic	<=	6300	then	&newvarname	=	45	;
else	if		6310	<=	&sic	<=	6319	then	&newvarname	=	45	;
else	if		6320	<=	&sic	<=	6329	then	&newvarname	=	45	;
else	if		6330	<=	&sic	<=	6331	then	&newvarname	=	45	;
else	if		6350	<=	&sic	<=	6351	then	&newvarname	=	45	;
else	if		6360	<=	&sic	<=	6361	then	&newvarname	=	45	;
else	if		6370	<=	&sic	<=	6379	then	&newvarname	=	45	;
else	if		6390	<=	&sic	<=	6399	then	&newvarname	=	45	;
else	if		6400	<=	&sic	<=	6411	then	&newvarname	=	45	;
else	if		6500	<=	&sic	<=	6500	then	&newvarname	=	46	;
else	if		6510	<=	&sic	<=	6510	then	&newvarname	=	46	;
else	if		6512	<=	&sic	<=	6512	then	&newvarname	=	46	;
else	if		6513	<=	&sic	<=	6513	then	&newvarname	=	46	;
else	if		6514	<=	&sic	<=	6514	then	&newvarname	=	46	;
else	if		6515	<=	&sic	<=	6515	then	&newvarname	=	46	;
else	if		6517	<=	&sic	<=	6519	then	&newvarname	=	46	;
else	if		6520	<=	&sic	<=	6529	then	&newvarname	=	46	;
else	if		6530	<=	&sic	<=	6531	then	&newvarname	=	46	;
else	if		6532	<=	&sic	<=	6532	then	&newvarname	=	46	;
else	if		6540	<=	&sic	<=	6541	then	&newvarname	=	46	;
else	if		6550	<=	&sic	<=	6553	then	&newvarname	=	46	;
else	if		6590	<=	&sic	<=	6599	then	&newvarname	=	46	;
else	if		6610	<=	&sic	<=	6611	then	&newvarname	=	46	;
else	if		6200	<=	&sic	<=	6299	then	&newvarname	=	47	;
else	if		6700	<=	&sic	<=	6700	then	&newvarname	=	47	;
else	if		6710	<=	&sic	<=	6719	then	&newvarname	=	47	;
else	if		6720	<=	&sic	<=	6722	then	&newvarname	=	47	;
else	if		6723	<=	&sic	<=	6723	then	&newvarname	=	47	;
else	if		6724	<=	&sic	<=	6724	then	&newvarname	=	47	;
else	if		6725	<=	&sic	<=	6725	then	&newvarname	=	47	;
else	if		6726	<=	&sic	<=	6726	then	&newvarname	=	47	;
else	if		6730	<=	&sic	<=	6733	then	&newvarname	=	47	;
else	if		6740	<=	&sic	<=	6779	then	&newvarname	=	47	;
else	if		6790	<=	&sic	<=	6791	then	&newvarname	=	47	;
else	if		6792	<=	&sic	<=	6792	then	&newvarname	=	47	;
else	if		6793	<=	&sic	<=	6793	then	&newvarname	=	47	;
else	if		6794	<=	&sic	<=	6794	then	&newvarname	=	47	;
else	if		6795	<=	&sic	<=	6795	then	&newvarname	=	47	;
else	if		6798	<=	&sic	<=	6798	then	&newvarname	=	47	;
else	if		6799	<=	&sic	<=	6799	then	&newvarname	=	47	;
else	if		4950	<=	&sic	<=	4959	then	&newvarname	=	48	;
else	if		4960	<=	&sic	<=	4961	then	&newvarname	=	48	;
else	if		4970	<=	&sic	<=	4971	then	&newvarname	=	48	;
else	if		4990	<=	&sic	<=	4991	then	&newvarname	=	48	;
else &newvarname =48;
run;

proc format;
value ff
1="Agriculture"
2="Food Products"
3="Candy and Soda"
4="Beer and Liquor"
5="Tobacco Products"
6="Recreation"
7="Entertainment"
8="Printing and Publishing"
9="Consumer Goods"
10="Apparel"
11="Healthcare"
12="Medical Equipment"
13="Pharmaceutical Products"
14="Chemicals"
15="Rubber and Plastic Products"
16="Textiles"
17="Construction Materials"
18="Construction"
19="Steel Works"
20="Fabricated Products"
21="Machinery"
22="Electrical Equipment"
23="Automobiles and Trucks"
24="Aircraft"
25="Shipbuilding, Railroad Equipment"
26="Defense"
27="Precious Metals"
28="Non-Metallic and Industrial Metal Mining"
29="Coal"
30="Petroleum and Natural Gas"
31="Utilities"
32="Communication"
33="Personal Services"
34="Business Services"
35="Computers"
36="Electronic Equipment"
37="Measuring and Control Equipment"
38="Business Supplies"
39="Shipping Containers"
40="Transportation"
41="Wholesale"
42="Retail"
43="Restaurants, Hotels, Motels"
44="Banking"
45="Insurance"
46="Real Estate"
47="Trading"
48="Almost Nothing";
run;

%mend;





*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;
*CREATES FAMA-FRENCH 53 INDUSTRY CLASSIFICATIONS
*---------------------------------------------------------------------------------------------------------------------;
*---------------------------------------------------------------------------------------------------------------------;

%macro ff53(data=,newvarname=industry,sic=sic,out=&data);

data &out;
    set &data;
    &newvarname = 48;
if	100	<= &sic <=	199	then &newvarname = 	1	;
else	if		200	<=	&sic	<=	299	then	&newvarname	=	1	;
else	if		700	<=	&sic	<=	799	then	&newvarname	=	1	;
else	if		910	<=	&sic	<=	919	then	&newvarname	=	1	;
else	if		2048	<=	&sic	<=	2048	then	&newvarname	=	1	;
else	if		2000	<=	&sic	<=	2009	then	&newvarname	=	2	;
else	if		2010	<=	&sic	<=	2019	then	&newvarname	=	2	;
else	if		2020	<=	&sic	<=	2029	then	&newvarname	=	2	;
else	if		2030	<=	&sic	<=	2039	then	&newvarname	=	2	;
else	if		2040	<=	&sic	<=	2046	then	&newvarname	=	2	;
else	if		2050	<=	&sic	<=	2059	then	&newvarname	=	2	;
else	if		2060	<=	&sic	<=	2063	then	&newvarname	=	2	;
else	if		2070	<=	&sic	<=	2079	then	&newvarname	=	2	;
else	if		2090	<=	&sic	<=	2092	then	&newvarname	=	2	;
else	if		2095	<=	&sic	<=	2095	then	&newvarname	=	2	;
else	if		2098	<=	&sic	<=	2099	then	&newvarname	=	2	;
else	if		2064	<=	&sic	<=	2068	then	&newvarname	=	3	;
else	if		2086	<=	&sic	<=	2086	then	&newvarname	=	3	;
else	if		2087	<=	&sic	<=	2087	then	&newvarname	=	3	;
else	if		2096	<=	&sic	<=	2096	then	&newvarname	=	3	;
else	if		2097	<=	&sic	<=	2097	then	&newvarname	=	3	;
else	if		2080	<=	&sic	<=	2080	then	&newvarname	=	4	;
else	if		2082	<=	&sic	<=	2082	then	&newvarname	=	4	;
else	if		2083	<=	&sic	<=	2083	then	&newvarname	=	4	;
else	if		2084	<=	&sic	<=	2084	then	&newvarname	=	4	;
else	if		2085	<=	&sic	<=	2085	then	&newvarname	=	4	;
else	if		2100	<=	&sic	<=	2199	then	&newvarname	=	5	;
else	if		920	<=	&sic	<=	999	then	&newvarname	=	6	;
else	if		3650	<=	&sic	<=	3651	then	&newvarname	=	6	;
else	if		3652	<=	&sic	<=	3652	then	&newvarname	=	6	;
else	if		3732	<=	&sic	<=	3732	then	&newvarname	=	6	;
else	if		3930	<=	&sic	<=	3931	then	&newvarname	=	6	;
else	if		3940	<=	&sic	<=	3949	then	&newvarname	=	6	;
else	if		7800	<=	&sic	<=	7829	then	&newvarname	=	7	;
else	if		7830	<=	&sic	<=	7833	then	&newvarname	=	7	;
else	if		7840	<=	&sic	<=	7841	then	&newvarname	=	7	;
else	if		7900	<=	&sic	<=	7900	then	&newvarname	=	7	;
else	if		7910	<=	&sic	<=	7911	then	&newvarname	=	7	;
else	if		7920	<=	&sic	<=	7929	then	&newvarname	=	7	;
else	if		7930	<=	&sic	<=	7933	then	&newvarname	=	7	;
else	if		7940	<=	&sic	<=	7949	then	&newvarname	=	7	;
else	if		7980	<=	&sic	<=	7980	then	&newvarname	=	7	;
else	if		7990	<=	&sic	<=	7999	then	&newvarname	=	7	;
else	if		2700	<=	&sic	<=	2709	then	&newvarname	=	8	;
else	if		2710	<=	&sic	<=	2719	then	&newvarname	=	8	;
else	if		2720	<=	&sic	<=	2729	then	&newvarname	=	8	;
else	if		2730	<=	&sic	<=	2739	then	&newvarname	=	8	;
else	if		2740	<=	&sic	<=	2749	then	&newvarname	=	8	;
else	if		2770	<=	&sic	<=	2771	then	&newvarname	=	8	;
else	if		2780	<=	&sic	<=	2789	then	&newvarname	=	8	;
else	if		2790	<=	&sic	<=	2799	then	&newvarname	=	8	;
else	if		2047	<=	&sic	<=	2047	then	&newvarname	=	9	;
else	if		2391	<=	&sic	<=	2392	then	&newvarname	=	9	;
else	if		2510	<=	&sic	<=	2519	then	&newvarname	=	9	;
else	if		2590	<=	&sic	<=	2599	then	&newvarname	=	9	;
else	if		2840	<=	&sic	<=	2843	then	&newvarname	=	9	;
else	if		2844	<=	&sic	<=	2844	then	&newvarname	=	9	;
else	if		3160	<=	&sic	<=	3161	then	&newvarname	=	9	;
else	if		3170	<=	&sic	<=	3171	then	&newvarname	=	9	;
else	if		3172	<=	&sic	<=	3172	then	&newvarname	=	9	;
else	if		3190	<=	&sic	<=	3199	then	&newvarname	=	9	;
else	if		3229	<=	&sic	<=	3229	then	&newvarname	=	9	;
else	if		3260	<=	&sic	<=	3260	then	&newvarname	=	9	;
else	if		3262	<=	&sic	<=	3263	then	&newvarname	=	9	;
else	if		3269	<=	&sic	<=	3269	then	&newvarname	=	9	;
else	if		3230	<=	&sic	<=	3231	then	&newvarname	=	9	;
else	if		3630	<=	&sic	<=	3639	then	&newvarname	=	9	;
else	if		3750	<=	&sic	<=	3751	then	&newvarname	=	9	;
else	if		3800	<=	&sic	<=	3800	then	&newvarname	=	9	;
else	if		3860	<=	&sic	<=	3861	then	&newvarname	=	9	;
else	if		3870	<=	&sic	<=	3873	then	&newvarname	=	9	;
else	if		3910	<=	&sic	<=	3911	then	&newvarname	=	9	;
else	if		3914	<=	&sic	<=	3914	then	&newvarname	=	9	;
else	if		3915	<=	&sic	<=	3915	then	&newvarname	=	9	;
else	if		3960	<=	&sic	<=	3962	then	&newvarname	=	9	;
else	if		3991	<=	&sic	<=	3991	then	&newvarname	=	9	;
else	if		3995	<=	&sic	<=	3995	then	&newvarname	=	9	;
else	if		2300	<=	&sic	<=	2390	then	&newvarname	=	10	;
else	if		3020	<=	&sic	<=	3021	then	&newvarname	=	10	;
else	if		3100	<=	&sic	<=	3111	then	&newvarname	=	10	;
else	if		3130	<=	&sic	<=	3131	then	&newvarname	=	10	;
else	if		3140	<=	&sic	<=	3149	then	&newvarname	=	10	;
else	if		3150	<=	&sic	<=	3151	then	&newvarname	=	10	;
else	if		3963	<=	&sic	<=	3965	then	&newvarname	=	10	;
else	if		8000	<=	&sic	<=	8099	then	&newvarname	=	11	;
else	if		3693	<=	&sic	<=	3693	then	&newvarname	=	12	;
else	if		3840	<=	&sic	<=	3849	then	&newvarname	=	12	;
else	if		3850	<=	&sic	<=	3851	then	&newvarname	=	12	;
else	if		2830	<=	&sic	<=	2830	then	&newvarname	=	13	;
else	if		2831	<=	&sic	<=	2831	then	&newvarname	=	13	;
else	if		2833	<=	&sic	<=	2833	then	&newvarname	=	13	;
else	if		2834	<=	&sic	<=	2834	then	&newvarname	=	13	;
else	if		2835	<=	&sic	<=	2835	then	&newvarname	=	13	;
else	if		2836	<=	&sic	<=	2836	then	&newvarname	=	13	;
else	if		2800	<=	&sic	<=	2809	then	&newvarname	=	14	;
else	if		2810	<=	&sic	<=	2819	then	&newvarname	=	14	;
else	if		2820	<=	&sic	<=	2829	then	&newvarname	=	14	;
else	if		2850	<=	&sic	<=	2859	then	&newvarname	=	14	;
else	if		2860	<=	&sic	<=	2869	then	&newvarname	=	14	;
else	if		2870	<=	&sic	<=	2879	then	&newvarname	=	14	;
else	if		2890	<=	&sic	<=	2899	then	&newvarname	=	14	;
else	if		3031	<=	&sic	<=	3031	then	&newvarname	=	15	;
else	if		3041	<=	&sic	<=	3041	then	&newvarname	=	15	;
else	if		3050	<=	&sic	<=	3053	then	&newvarname	=	15	;
else	if		3060	<=	&sic	<=	3069	then	&newvarname	=	15	;
else	if		3070	<=	&sic	<=	3079	then	&newvarname	=	15	;
else	if		3080	<=	&sic	<=	3089	then	&newvarname	=	15	;
else	if		3090	<=	&sic	<=	3099	then	&newvarname	=	15	;
else	if		2200	<=	&sic	<=	2269	then	&newvarname	=	16	;
else	if		2270	<=	&sic	<=	2279	then	&newvarname	=	16	;
else	if		2280	<=	&sic	<=	2284	then	&newvarname	=	16	;
else	if		2290	<=	&sic	<=	2295	then	&newvarname	=	16	;
else	if		2297	<=	&sic	<=	2297	then	&newvarname	=	16	;
else	if		2298	<=	&sic	<=	2298	then	&newvarname	=	16	;
else	if		2299	<=	&sic	<=	2299	then	&newvarname	=	16	;
else	if		2393	<=	&sic	<=	2395	then	&newvarname	=	16	;
else	if		2397	<=	&sic	<=	2399	then	&newvarname	=	16	;
else	if		800	<=	&sic	<=	899	then	&newvarname	=	17	;
else	if		2400	<=	&sic	<=	2439	then	&newvarname	=	17	;
else	if		2450	<=	&sic	<=	2459	then	&newvarname	=	17	;
else	if		2490	<=	&sic	<=	2499	then	&newvarname	=	17	;
else	if		2660	<=	&sic	<=	2661	then	&newvarname	=	17	;
else	if		2950	<=	&sic	<=	2952	then	&newvarname	=	17	;
else	if		3200	<=	&sic	<=	3200	then	&newvarname	=	17	;
else	if		3210	<=	&sic	<=	3211	then	&newvarname	=	17	;
else	if		3240	<=	&sic	<=	3241	then	&newvarname	=	17	;
else	if		3250	<=	&sic	<=	3259	then	&newvarname	=	17	;
else	if		3261	<=	&sic	<=	3261	then	&newvarname	=	17	;
else	if		3264	<=	&sic	<=	3264	then	&newvarname	=	17	;
else	if		3270	<=	&sic	<=	3275	then	&newvarname	=	17	;
else	if		3280	<=	&sic	<=	3281	then	&newvarname	=	17	;
else	if		3290	<=	&sic	<=	3293	then	&newvarname	=	17	;
else	if		3295	<=	&sic	<=	3299	then	&newvarname	=	17	;
else	if		3420	<=	&sic	<=	3429	then	&newvarname	=	17	;
else	if		3430	<=	&sic	<=	3433	then	&newvarname	=	17	;
else	if		3440	<=	&sic	<=	3441	then	&newvarname	=	17	;
else	if		3442	<=	&sic	<=	3442	then	&newvarname	=	17	;
else	if		3446	<=	&sic	<=	3446	then	&newvarname	=	17	;
else	if		3448	<=	&sic	<=	3448	then	&newvarname	=	17	;
else	if		3449	<=	&sic	<=	3449	then	&newvarname	=	17	;
else	if		3450	<=	&sic	<=	3451	then	&newvarname	=	17	;
else	if		3452	<=	&sic	<=	3452	then	&newvarname	=	17	;
else	if		3490	<=	&sic	<=	3499	then	&newvarname	=	17	;
else	if		3996	<=	&sic	<=	3996	then	&newvarname	=	17	;
else	if		1500	<=	&sic	<=	1511	then	&newvarname	=	18	;
else	if		1520	<=	&sic	<=	1529	then	&newvarname	=	18	;
else	if		1530	<=	&sic	<=	1539	then	&newvarname	=	18	;
else	if		1540	<=	&sic	<=	1549	then	&newvarname	=	18	;
else	if		1600	<=	&sic	<=	1699	then	&newvarname	=	18	;
else	if		1700	<=	&sic	<=	1799	then	&newvarname	=	18	;
else	if		3300	<=	&sic	<=	3300	then	&newvarname	=	19	;
else	if		3310	<=	&sic	<=	3317	then	&newvarname	=	19	;
else	if		3320	<=	&sic	<=	3325	then	&newvarname	=	19	;
else	if		3330	<=	&sic	<=	3339	then	&newvarname	=	19	;
else	if		3340	<=	&sic	<=	3341	then	&newvarname	=	19	;
else	if		3350	<=	&sic	<=	3357	then	&newvarname	=	19	;
else	if		3360	<=	&sic	<=	3369	then	&newvarname	=	19	;
else	if		3370	<=	&sic	<=	3379	then	&newvarname	=	19	;
else	if		3390	<=	&sic	<=	3399	then	&newvarname	=	19	;
else	if		3400	<=	&sic	<=	3400	then	&newvarname	=	20	;
else	if		3443	<=	&sic	<=	3443	then	&newvarname	=	20	;
else	if		3444	<=	&sic	<=	3444	then	&newvarname	=	20	;
else	if		3460	<=	&sic	<=	3469	then	&newvarname	=	20	;
else	if		3470	<=	&sic	<=	3479	then	&newvarname	=	20	;
else	if		3510	<=	&sic	<=	3519	then	&newvarname	=	21	;
else	if		3520	<=	&sic	<=	3529	then	&newvarname	=	21	;
else	if		3530	<=	&sic	<=	3530	then	&newvarname	=	21	;
else	if		3531	<=	&sic	<=	3531	then	&newvarname	=	21	;
else	if		3532	<=	&sic	<=	3532	then	&newvarname	=	21	;
else	if		3533	<=	&sic	<=	3533	then	&newvarname	=	21	;
else	if		3534	<=	&sic	<=	3534	then	&newvarname	=	21	;
else	if		3535	<=	&sic	<=	3535	then	&newvarname	=	21	;
else	if		3536	<=	&sic	<=	3536	then	&newvarname	=	21	;
else	if		3538	<=	&sic	<=	3538	then	&newvarname	=	21	;
else	if		3540	<=	&sic	<=	3549	then	&newvarname	=	21	;
else	if		3550	<=	&sic	<=	3559	then	&newvarname	=	21	;
else	if		3560	<=	&sic	<=	3569	then	&newvarname	=	21	;
else	if		3580	<=	&sic	<=	3580	then	&newvarname	=	21	;
else	if		3581	<=	&sic	<=	3581	then	&newvarname	=	21	;
else	if		3582	<=	&sic	<=	3582	then	&newvarname	=	21	;
else	if		3585	<=	&sic	<=	3585	then	&newvarname	=	21	;
else	if		3586	<=	&sic	<=	3586	then	&newvarname	=	21	;
else	if		3589	<=	&sic	<=	3589	then	&newvarname	=	21	;
else	if		3590	<=	&sic	<=	3599	then	&newvarname	=	21	;
else	if		3600	<=	&sic	<=	3600	then	&newvarname	=	22	;
else	if		3610	<=	&sic	<=	3613	then	&newvarname	=	22	;
else	if		3620	<=	&sic	<=	3621	then	&newvarname	=	22	;
else	if		3623	<=	&sic	<=	3629	then	&newvarname	=	22	;
else	if		3640	<=	&sic	<=	3644	then	&newvarname	=	22	;
else	if		3645	<=	&sic	<=	3645	then	&newvarname	=	22	;
else	if		3646	<=	&sic	<=	3646	then	&newvarname	=	22	;
else	if		3648	<=	&sic	<=	3649	then	&newvarname	=	22	;
else	if		3660	<=	&sic	<=	3660	then	&newvarname	=	22	;
else	if		3690	<=	&sic	<=	3690	then	&newvarname	=	22	;
else	if		3691	<=	&sic	<=	3692	then	&newvarname	=	22	;
else	if		3699	<=	&sic	<=	3699	then	&newvarname	=	22	;
else	if		2296	<=	&sic	<=	2296	then	&newvarname	=	23	;
else	if		2396	<=	&sic	<=	2396	then	&newvarname	=	23	;
else	if		3010	<=	&sic	<=	3011	then	&newvarname	=	23	;
else	if		3537	<=	&sic	<=	3537	then	&newvarname	=	23	;
else	if		3647	<=	&sic	<=	3647	then	&newvarname	=	23	;
else	if		3694	<=	&sic	<=	3694	then	&newvarname	=	23	;
else	if		3700	<=	&sic	<=	3700	then	&newvarname	=	23	;
else	if		3710	<=	&sic	<=	3710	then	&newvarname	=	23	;
else	if		3711	<=	&sic	<=	3711	then	&newvarname	=	23	;
else	if		3713	<=	&sic	<=	3713	then	&newvarname	=	23	;
else	if		3714	<=	&sic	<=	3714	then	&newvarname	=	23	;
else	if		3715	<=	&sic	<=	3715	then	&newvarname	=	23	;
else	if		3716	<=	&sic	<=	3716	then	&newvarname	=	23	;
else	if		3792	<=	&sic	<=	3792	then	&newvarname	=	23	;
else	if		3790	<=	&sic	<=	3791	then	&newvarname	=	23	;
else	if		3799	<=	&sic	<=	3799	then	&newvarname	=	23	;
else	if		3720	<=	&sic	<=	3720	then	&newvarname	=	24	;
else	if		3721	<=	&sic	<=	3721	then	&newvarname	=	24	;
else	if		3723	<=	&sic	<=	3724	then	&newvarname	=	24	;
else	if		3725	<=	&sic	<=	3725	then	&newvarname	=	24	;
else	if		3728	<=	&sic	<=	3729	then	&newvarname	=	24	;
else	if		3730	<=	&sic	<=	3731	then	&newvarname	=	25	;
else	if		3740	<=	&sic	<=	3743	then	&newvarname	=	25	;
else	if		3760	<=	&sic	<=	3769	then	&newvarname	=	26	;
else	if		3795	<=	&sic	<=	3795	then	&newvarname	=	26	;
else	if		3480	<=	&sic	<=	3489	then	&newvarname	=	26	;
else	if		1040	<=	&sic	<=	1049	then	&newvarname	=	27	;
else	if		1000	<=	&sic	<=	1009	then	&newvarname	=	28	;
else	if		1010	<=	&sic	<=	1019	then	&newvarname	=	28	;
else	if		1020	<=	&sic	<=	1029	then	&newvarname	=	28	;
else	if		1030	<=	&sic	<=	1039	then	&newvarname	=	28	;
else	if		1050	<=	&sic	<=	1059	then	&newvarname	=	28	;
else	if		1060	<=	&sic	<=	1069	then	&newvarname	=	28	;
else	if		1070	<=	&sic	<=	1079	then	&newvarname	=	28	;
else	if		1080	<=	&sic	<=	1089	then	&newvarname	=	28	;
else	if		1090	<=	&sic	<=	1099	then	&newvarname	=	28	;
else	if		1100	<=	&sic	<=	1119	then	&newvarname	=	28	;
else	if		1400	<=	&sic	<=	1499	then	&newvarname	=	28	;
else	if		1200	<=	&sic	<=	1299	then	&newvarname	=	29	;
else	if		1300	<=	&sic	<=	1300	then	&newvarname	=	30	;
else	if		1310	<=	&sic	<=	1319	then	&newvarname	=	30	;
else	if		1320	<=	&sic	<=	1329	then	&newvarname	=	30	;
else	if		1330	<=	&sic	<=	1339	then	&newvarname	=	30	;
else	if		1370	<=	&sic	<=	1379	then	&newvarname	=	30	;
else	if		1380	<=	&sic	<=	1380	then	&newvarname	=	30	;
else	if		1381	<=	&sic	<=	1381	then	&newvarname	=	30	;
else	if		1382	<=	&sic	<=	1382	then	&newvarname	=	30	;
else	if		1389	<=	&sic	<=	1389	then	&newvarname	=	30	;
else	if		2900	<=	&sic	<=	2912	then	&newvarname	=	30	;
else	if		2990	<=	&sic	<=	2999	then	&newvarname	=	30	;
else	if		4900	<=	&sic	<=	4900	then	&newvarname	=	31	;
else	if		4910	<=	&sic	<=	4911	then	&newvarname	=	31	;
else	if		4920	<=	&sic	<=	4922	then	&newvarname	=	31	;
else	if		4923	<=	&sic	<=	4923	then	&newvarname	=	31	;
else	if		4924	<=	&sic	<=	4925	then	&newvarname	=	31	;
else	if		4930	<=	&sic	<=	4931	then	&newvarname	=	31	;
else	if		4932	<=	&sic	<=	4932	then	&newvarname	=	31	;
else	if		4939	<=	&sic	<=	4939	then	&newvarname	=	31	;
else	if		4940	<=	&sic	<=	4942	then	&newvarname	=	31	;
else	if		4800	<=	&sic	<=	4800	then	&newvarname	=	32	;
else	if		4810	<=	&sic	<=	4813	then	&newvarname	=	32	;
else	if		4820	<=	&sic	<=	4822	then	&newvarname	=	32	;
else	if		4830	<=	&sic	<=	4839	then	&newvarname	=	32	;
else	if		4840	<=	&sic	<=	4841	then	&newvarname	=	32	;
else	if		4880	<=	&sic	<=	4889	then	&newvarname	=	32	;
else	if		4890	<=	&sic	<=	4890	then	&newvarname	=	32	;
else	if		4891	<=	&sic	<=	4891	then	&newvarname	=	32	;
else	if		4892	<=	&sic	<=	4892	then	&newvarname	=	32	;
else	if		4899	<=	&sic	<=	4899	then	&newvarname	=	32	;
else	if		7020	<=	&sic	<=	7021	then	&newvarname	=	33	;
else	if		7030	<=	&sic	<=	7033	then	&newvarname	=	33	;
else	if		7200	<=	&sic	<=	7200	then	&newvarname	=	33	;
else	if		7210	<=	&sic	<=	7212	then	&newvarname	=	33	;
else	if		7214	<=	&sic	<=	7214	then	&newvarname	=	33	;
else	if		7215	<=	&sic	<=	7216	then	&newvarname	=	33	;
else	if		7217	<=	&sic	<=	7217	then	&newvarname	=	33	;
else	if		7219	<=	&sic	<=	7219	then	&newvarname	=	33	;
else	if		7220	<=	&sic	<=	7221	then	&newvarname	=	33	;
else	if		7230	<=	&sic	<=	7231	then	&newvarname	=	33	;
else	if		7240	<=	&sic	<=	7241	then	&newvarname	=	33	;
else	if		7250	<=	&sic	<=	7251	then	&newvarname	=	33	;
else	if		7260	<=	&sic	<=	7269	then	&newvarname	=	33	;
else	if		7270	<=	&sic	<=	7290	then	&newvarname	=	33	;
else	if		7291	<=	&sic	<=	7291	then	&newvarname	=	33	;
else	if		7292	<=	&sic	<=	7299	then	&newvarname	=	33	;
else	if		7395	<=	&sic	<=	7395	then	&newvarname	=	33	;
else	if		7500	<=	&sic	<=	7500	then	&newvarname	=	33	;
else	if		7520	<=	&sic	<=	7529	then	&newvarname	=	33	;
else	if		7530	<=	&sic	<=	7539	then	&newvarname	=	33	;
else	if		7540	<=	&sic	<=	7549	then	&newvarname	=	33	;
else	if		7600	<=	&sic	<=	7600	then	&newvarname	=	33	;
else	if		7620	<=	&sic	<=	7620	then	&newvarname	=	33	;
else	if		7622	<=	&sic	<=	7622	then	&newvarname	=	33	;
else	if		7623	<=	&sic	<=	7623	then	&newvarname	=	33	;
else	if		7629	<=	&sic	<=	7629	then	&newvarname	=	33	;
else	if		7630	<=	&sic	<=	7631	then	&newvarname	=	33	;
else	if		7640	<=	&sic	<=	7641	then	&newvarname	=	33	;
else	if		7690	<=	&sic	<=	7699	then	&newvarname	=	33	;
else	if		8100	<=	&sic	<=	8199	then	&newvarname	=	33	;
else	if		8200	<=	&sic	<=	8299	then	&newvarname	=	33	;
else	if		8300	<=	&sic	<=	8399	then	&newvarname	=	33	;
else	if		8400	<=	&sic	<=	8499	then	&newvarname	=	33	;
else	if		8600	<=	&sic	<=	8699	then	&newvarname	=	33	;
else	if		8800	<=	&sic	<=	8899	then	&newvarname	=	33	;
else	if		7510	<=	&sic	<=	7515	then	&newvarname	=	33	;
else	if		2750	<=	&sic	<=	2759	then	&newvarname	=	34	;
else	if		3993	<=	&sic	<=	3993	then	&newvarname	=	34	;
else	if		7218	<=	&sic	<=	7218	then	&newvarname	=	34	;
else	if		7300	<=	&sic	<=	7300	then	&newvarname	=	34	;
else	if		7310	<=	&sic	<=	7319	then	&newvarname	=	34	;
else	if		7320	<=	&sic	<=	7329	then	&newvarname	=	34	;
else	if		7330	<=	&sic	<=	7339	then	&newvarname	=	34	;
else	if		7340	<=	&sic	<=	7342	then	&newvarname	=	34	;
else	if		7349	<=	&sic	<=	7349	then	&newvarname	=	34	;
else	if		7350	<=	&sic	<=	7351	then	&newvarname	=	34	;
else	if		7352	<=	&sic	<=	7352	then	&newvarname	=	34	;
else	if		7353	<=	&sic	<=	7353	then	&newvarname	=	34	;
else	if		7359	<=	&sic	<=	7359	then	&newvarname	=	34	;
else	if		7360	<=	&sic	<=	7369	then	&newvarname	=	34	;
else	if		7370	<=	&sic	<=	7372	then	&newvarname	=	34	;
else	if		7374	<=	&sic	<=	7374	then	&newvarname	=	34	;
else	if		7375	<=	&sic	<=	7375	then	&newvarname	=	34	;
else	if		7376	<=	&sic	<=	7376	then	&newvarname	=	34	;
else	if		7377	<=	&sic	<=	7377	then	&newvarname	=	34	;
else	if		7378	<=	&sic	<=	7378	then	&newvarname	=	34	;
else	if		7379	<=	&sic	<=	7379	then	&newvarname	=	34	;
else	if		7380	<=	&sic	<=	7380	then	&newvarname	=	34	;
else	if		7381	<=	&sic	<=	7382	then	&newvarname	=	34	;
else	if		7383	<=	&sic	<=	7383	then	&newvarname	=	34	;
else	if		7384	<=	&sic	<=	7384	then	&newvarname	=	34	;
else	if		7385	<=	&sic	<=	7385	then	&newvarname	=	34	;
else	if		7389	<=	&sic	<=	7390	then	&newvarname	=	34	;
else	if		7391	<=	&sic	<=	7391	then	&newvarname	=	34	;
else	if		7392	<=	&sic	<=	7392	then	&newvarname	=	34	;
else	if		7393	<=	&sic	<=	7393	then	&newvarname	=	34	;
else	if		7394	<=	&sic	<=	7394	then	&newvarname	=	34	;
else	if		7396	<=	&sic	<=	7396	then	&newvarname	=	34	;
else	if		7397	<=	&sic	<=	7397	then	&newvarname	=	34	;
else	if		7399	<=	&sic	<=	7399	then	&newvarname	=	34	;
else	if		7519	<=	&sic	<=	7519	then	&newvarname	=	34	;
else	if		8700	<=	&sic	<=	8700	then	&newvarname	=	34	;
else	if		8710	<=	&sic	<=	8713	then	&newvarname	=	34	;
else	if		8720	<=	&sic	<=	8721	then	&newvarname	=	34	;
else	if		8730	<=	&sic	<=	8734	then	&newvarname	=	34	;
else	if		8740	<=	&sic	<=	8748	then	&newvarname	=	34	;
else	if		8900	<=	&sic	<=	8910	then	&newvarname	=	34	;
else	if		8911	<=	&sic	<=	8911	then	&newvarname	=	34	;
else	if		8920	<=	&sic	<=	8999	then	&newvarname	=	34	;
else	if		4220	<=	&sic	<=	4229	then	&newvarname	=	34	;
else	if		3570	<=	&sic	<=	3579	then	&newvarname	=	35	;
else	if		3680	<=	&sic	<=	3680	then	&newvarname	=	35	;
else	if		3681	<=	&sic	<=	3681	then	&newvarname	=	35	;
else	if		3682	<=	&sic	<=	3682	then	&newvarname	=	35	;
else	if		3683	<=	&sic	<=	3683	then	&newvarname	=	35	;
else	if		3684	<=	&sic	<=	3684	then	&newvarname	=	35	;
else	if		3685	<=	&sic	<=	3685	then	&newvarname	=	35	;
else	if		3686	<=	&sic	<=	3686	then	&newvarname	=	35	;
else	if		3687	<=	&sic	<=	3687	then	&newvarname	=	35	;
else	if		3688	<=	&sic	<=	3688	then	&newvarname	=	35	;
else	if		3689	<=	&sic	<=	3689	then	&newvarname	=	35	;
else	if		3695	<=	&sic	<=	3695	then	&newvarname	=	35	;
else	if		7373	<=	&sic	<=	7373	then	&newvarname	=	35	;
else	if		3622	<=	&sic	<=	3622	then	&newvarname	=	36	;
else	if		3661	<=	&sic	<=	3661	then	&newvarname	=	36	;
else	if		3662	<=	&sic	<=	3662	then	&newvarname	=	36	;
else	if		3663	<=	&sic	<=	3663	then	&newvarname	=	36	;
else	if		3664	<=	&sic	<=	3664	then	&newvarname	=	36	;
else	if		3665	<=	&sic	<=	3665	then	&newvarname	=	36	;
else	if		3666	<=	&sic	<=	3666	then	&newvarname	=	36	;
else	if		3669	<=	&sic	<=	3669	then	&newvarname	=	36	;
else	if		3670	<=	&sic	<=	3679	then	&newvarname	=	36	;
else	if		3810	<=	&sic	<=	3810	then	&newvarname	=	36	;
else	if		3812	<=	&sic	<=	3812	then	&newvarname	=	36	;
else	if		3811	<=	&sic	<=	3811	then	&newvarname	=	37	;
else	if		3820	<=	&sic	<=	3820	then	&newvarname	=	37	;
else	if		3821	<=	&sic	<=	3821	then	&newvarname	=	37	;
else	if		3822	<=	&sic	<=	3822	then	&newvarname	=	37	;
else	if		3823	<=	&sic	<=	3823	then	&newvarname	=	37	;
else	if		3824	<=	&sic	<=	3824	then	&newvarname	=	37	;
else	if		3825	<=	&sic	<=	3825	then	&newvarname	=	37	;
else	if		3826	<=	&sic	<=	3826	then	&newvarname	=	37	;
else	if		3827	<=	&sic	<=	3827	then	&newvarname	=	37	;
else	if		3829	<=	&sic	<=	3829	then	&newvarname	=	37	;
else	if		3830	<=	&sic	<=	3839	then	&newvarname	=	37	;
else	if		2520	<=	&sic	<=	2549	then	&newvarname	=	38	;
else	if		2600	<=	&sic	<=	2639	then	&newvarname	=	38	;
else	if		2670	<=	&sic	<=	2699	then	&newvarname	=	38	;
else	if		2760	<=	&sic	<=	2761	then	&newvarname	=	38	;
else	if		3950	<=	&sic	<=	3955	then	&newvarname	=	38	;
else	if		2440	<=	&sic	<=	2449	then	&newvarname	=	39	;
else	if		2640	<=	&sic	<=	2659	then	&newvarname	=	39	;
else	if		3220	<=	&sic	<=	3221	then	&newvarname	=	39	;
else	if		3410	<=	&sic	<=	3412	then	&newvarname	=	39	;
else	if		4000	<=	&sic	<=	4013	then	&newvarname	=	40	;
else	if		4040	<=	&sic	<=	4049	then	&newvarname	=	40	;
else	if		4100	<=	&sic	<=	4100	then	&newvarname	=	40	;
else	if		4110	<=	&sic	<=	4119	then	&newvarname	=	40	;
else	if		4120	<=	&sic	<=	4121	then	&newvarname	=	40	;
else	if		4130	<=	&sic	<=	4131	then	&newvarname	=	40	;
else	if		4140	<=	&sic	<=	4142	then	&newvarname	=	40	;
else	if		4150	<=	&sic	<=	4151	then	&newvarname	=	40	;
else	if		4170	<=	&sic	<=	4173	then	&newvarname	=	40	;
else	if		4190	<=	&sic	<=	4199	then	&newvarname	=	40	;
else	if		4200	<=	&sic	<=	4200	then	&newvarname	=	40	;
else	if		4210	<=	&sic	<=	4219	then	&newvarname	=	40	;
else	if		4230	<=	&sic	<=	4231	then	&newvarname	=	40	;
else	if		4240	<=	&sic	<=	4249	then	&newvarname	=	40	;
else	if		4400	<=	&sic	<=	4499	then	&newvarname	=	40	;
else	if		4500	<=	&sic	<=	4599	then	&newvarname	=	40	;
else	if		4600	<=	&sic	<=	4699	then	&newvarname	=	40	;
else	if		4700	<=	&sic	<=	4700	then	&newvarname	=	40	;
else	if		4710	<=	&sic	<=	4712	then	&newvarname	=	40	;
else	if		4720	<=	&sic	<=	4729	then	&newvarname	=	40	;
else	if		4730	<=	&sic	<=	4739	then	&newvarname	=	40	;
else	if		4740	<=	&sic	<=	4749	then	&newvarname	=	40	;
else	if		4780	<=	&sic	<=	4780	then	&newvarname	=	40	;
else	if		4782	<=	&sic	<=	4782	then	&newvarname	=	40	;
else	if		4783	<=	&sic	<=	4783	then	&newvarname	=	40	;
else	if		4784	<=	&sic	<=	4784	then	&newvarname	=	40	;
else	if		4785	<=	&sic	<=	4785	then	&newvarname	=	40	;
else	if		4789	<=	&sic	<=	4789	then	&newvarname	=	40	;
else	if		5000	<=	&sic	<=	5000	then	&newvarname	=	41	;
else	if		5010	<=	&sic	<=	5015	then	&newvarname	=	41	;
else	if		5020	<=	&sic	<=	5023	then	&newvarname	=	41	;
else	if		5030	<=	&sic	<=	5039	then	&newvarname	=	41	;
else	if		5040	<=	&sic	<=	5042	then	&newvarname	=	41	;
else	if		5043	<=	&sic	<=	5043	then	&newvarname	=	41	;
else	if		5044	<=	&sic	<=	5044	then	&newvarname	=	41	;
else	if		5045	<=	&sic	<=	5045	then	&newvarname	=	41	;
else	if		5046	<=	&sic	<=	5046	then	&newvarname	=	41	;
else	if		5047	<=	&sic	<=	5047	then	&newvarname	=	41	;
else	if		5048	<=	&sic	<=	5048	then	&newvarname	=	41	;
else	if		5049	<=	&sic	<=	5049	then	&newvarname	=	41	;
else	if		5050	<=	&sic	<=	5059	then	&newvarname	=	41	;
else	if		5060	<=	&sic	<=	5060	then	&newvarname	=	41	;
else	if		5063	<=	&sic	<=	5063	then	&newvarname	=	41	;
else	if		5064	<=	&sic	<=	5064	then	&newvarname	=	41	;
else	if		5065	<=	&sic	<=	5065	then	&newvarname	=	41	;
else	if		5070	<=	&sic	<=	5078	then	&newvarname	=	41	;
else	if		5080	<=	&sic	<=	5080	then	&newvarname	=	41	;
else	if		5081	<=	&sic	<=	5081	then	&newvarname	=	41	;
else	if		5082	<=	&sic	<=	5082	then	&newvarname	=	41	;
else	if		5083	<=	&sic	<=	5083	then	&newvarname	=	41	;
else	if		5084	<=	&sic	<=	5084	then	&newvarname	=	41	;
else	if		5085	<=	&sic	<=	5085	then	&newvarname	=	41	;
else	if		5086	<=	&sic	<=	5087	then	&newvarname	=	41	;
else	if		5088	<=	&sic	<=	5088	then	&newvarname	=	41	;
else	if		5090	<=	&sic	<=	5090	then	&newvarname	=	41	;
else	if		5091	<=	&sic	<=	5092	then	&newvarname	=	41	;
else	if		5093	<=	&sic	<=	5093	then	&newvarname	=	41	;
else	if		5094	<=	&sic	<=	5094	then	&newvarname	=	41	;
else	if		5099	<=	&sic	<=	5099	then	&newvarname	=	41	;
else	if		5100	<=	&sic	<=	5100	then	&newvarname	=	41	;
else	if		5110	<=	&sic	<=	5113	then	&newvarname	=	41	;
else	if		5120	<=	&sic	<=	5122	then	&newvarname	=	41	;
else	if		5130	<=	&sic	<=	5139	then	&newvarname	=	41	;
else	if		5140	<=	&sic	<=	5149	then	&newvarname	=	41	;
else	if		5150	<=	&sic	<=	5159	then	&newvarname	=	41	;
else	if		5160	<=	&sic	<=	5169	then	&newvarname	=	41	;
else	if		5170	<=	&sic	<=	5172	then	&newvarname	=	41	;
else	if		5180	<=	&sic	<=	5182	then	&newvarname	=	41	;
else	if		5190	<=	&sic	<=	5199	then	&newvarname	=	41	;
else	if		5200	<=	&sic	<=	5200	then	&newvarname	=	42	;
else	if		5210	<=	&sic	<=	5219	then	&newvarname	=	42	;
else	if		5220	<=	&sic	<=	5229	then	&newvarname	=	42	;
else	if		5230	<=	&sic	<=	5231	then	&newvarname	=	42	;
else	if		5250	<=	&sic	<=	5251	then	&newvarname	=	42	;
else	if		5260	<=	&sic	<=	5261	then	&newvarname	=	42	;
else	if		5270	<=	&sic	<=	5271	then	&newvarname	=	42	;
else	if		5300	<=	&sic	<=	5300	then	&newvarname	=	42	;
else	if		5310	<=	&sic	<=	5311	then	&newvarname	=	42	;
else	if		5320	<=	&sic	<=	5320	then	&newvarname	=	42	;
else	if		5330	<=	&sic	<=	5331	then	&newvarname	=	42	;
else	if		5334	<=	&sic	<=	5334	then	&newvarname	=	42	;
else	if		5340	<=	&sic	<=	5349	then	&newvarname	=	42	;
else	if		5390	<=	&sic	<=	5399	then	&newvarname	=	42	;
else	if		5400	<=	&sic	<=	5400	then	&newvarname	=	42	;
else	if		5410	<=	&sic	<=	5411	then	&newvarname	=	42	;
else	if		5412	<=	&sic	<=	5412	then	&newvarname	=	42	;
else	if		5420	<=	&sic	<=	5429	then	&newvarname	=	42	;
else	if		5430	<=	&sic	<=	5439	then	&newvarname	=	42	;
else	if		5440	<=	&sic	<=	5449	then	&newvarname	=	42	;
else	if		5450	<=	&sic	<=	5459	then	&newvarname	=	42	;
else	if		5460	<=	&sic	<=	5469	then	&newvarname	=	42	;
else	if		5490	<=	&sic	<=	5499	then	&newvarname	=	42	;
else	if		5500	<=	&sic	<=	5500	then	&newvarname	=	42	;
else	if		5510	<=	&sic	<=	5529	then	&newvarname	=	42	;
else	if		5530	<=	&sic	<=	5539	then	&newvarname	=	42	;
else	if		5540	<=	&sic	<=	5549	then	&newvarname	=	42	;
else	if		5550	<=	&sic	<=	5559	then	&newvarname	=	42	;
else	if		5560	<=	&sic	<=	5569	then	&newvarname	=	42	;
else	if		5570	<=	&sic	<=	5579	then	&newvarname	=	42	;
else	if		5590	<=	&sic	<=	5599	then	&newvarname	=	42	;
else	if		5600	<=	&sic	<=	5699	then	&newvarname	=	42	;
else	if		5700	<=	&sic	<=	5700	then	&newvarname	=	42	;
else	if		5710	<=	&sic	<=	5719	then	&newvarname	=	42	;
else	if		5720	<=	&sic	<=	5722	then	&newvarname	=	42	;
else	if		5730	<=	&sic	<=	5733	then	&newvarname	=	42	;
else	if		5734	<=	&sic	<=	5734	then	&newvarname	=	42	;
else	if		5735	<=	&sic	<=	5735	then	&newvarname	=	42	;
else	if		5736	<=	&sic	<=	5736	then	&newvarname	=	42	;
else	if		5750	<=	&sic	<=	5799	then	&newvarname	=	42	;
else	if		5900	<=	&sic	<=	5900	then	&newvarname	=	42	;
else	if		5910	<=	&sic	<=	5912	then	&newvarname	=	42	;
else	if		5920	<=	&sic	<=	5929	then	&newvarname	=	42	;
else	if		5930	<=	&sic	<=	5932	then	&newvarname	=	42	;
else	if		5940	<=	&sic	<=	5940	then	&newvarname	=	42	;
else	if		5941	<=	&sic	<=	5941	then	&newvarname	=	42	;
else	if		5942	<=	&sic	<=	5942	then	&newvarname	=	42	;
else	if		5943	<=	&sic	<=	5943	then	&newvarname	=	42	;
else	if		5944	<=	&sic	<=	5944	then	&newvarname	=	42	;
else	if		5945	<=	&sic	<=	5945	then	&newvarname	=	42	;
else	if		5946	<=	&sic	<=	5946	then	&newvarname	=	42	;
else	if		5947	<=	&sic	<=	5947	then	&newvarname	=	42	;
else	if		5948	<=	&sic	<=	5948	then	&newvarname	=	42	;
else	if		5949	<=	&sic	<=	5949	then	&newvarname	=	42	;
else	if		5950	<=	&sic	<=	5959	then	&newvarname	=	42	;
else	if		5960	<=	&sic	<=	5969	then	&newvarname	=	42	;
else	if		5970	<=	&sic	<=	5979	then	&newvarname	=	42	;
else	if		5980	<=	&sic	<=	5989	then	&newvarname	=	42	;
else	if		5990	<=	&sic	<=	5990	then	&newvarname	=	42	;
else	if		5992	<=	&sic	<=	5992	then	&newvarname	=	42	;
else	if		5993	<=	&sic	<=	5993	then	&newvarname	=	42	;
else	if		5994	<=	&sic	<=	5994	then	&newvarname	=	42	;
else	if		5995	<=	&sic	<=	5995	then	&newvarname	=	42	;
else	if		5999	<=	&sic	<=	5999	then	&newvarname	=	42	;
else	if		5800	<=	&sic	<=	5819	then	&newvarname	=	43	;
else	if		5820	<=	&sic	<=	5829	then	&newvarname	=	43	;
else	if		5890	<=	&sic	<=	5899	then	&newvarname	=	43	;
else	if		7000	<=	&sic	<=	7000	then	&newvarname	=	43	;
else	if		7010	<=	&sic	<=	7019	then	&newvarname	=	43	;
else	if		7040	<=	&sic	<=	7049	then	&newvarname	=	43	;
else	if		7213	<=	&sic	<=	7213	then	&newvarname	=	43	;
else	if		6000	<=	&sic	<=	6000	then	&newvarname	=	44	;
else	if		6010	<=	&sic	<=	6019	then	&newvarname	=	44	;
else	if		6020	<=	&sic	<=	6020	then	&newvarname	=	44	;
else	if		6021	<=	&sic	<=	6021	then	&newvarname	=	44	;
else	if		6022	<=	&sic	<=	6022	then	&newvarname	=	44	;
else	if		6023	<=	&sic	<=	6024	then	&newvarname	=	44	;
else	if		6025	<=	&sic	<=	6025	then	&newvarname	=	44	;
else	if		6026	<=	&sic	<=	6026	then	&newvarname	=	44	;
else	if		6027	<=	&sic	<=	6027	then	&newvarname	=	44	;
else	if		6028	<=	&sic	<=	6029	then	&newvarname	=	44	;
else	if		6030	<=	&sic	<=	6036	then	&newvarname	=	44	;
else	if		6040	<=	&sic	<=	6059	then	&newvarname	=	44	;
else	if		6060	<=	&sic	<=	6062	then	&newvarname	=	44	;
else	if		6080	<=	&sic	<=	6082	then	&newvarname	=	44	;
else	if		6090	<=	&sic	<=	6099	then	&newvarname	=	44	;
else	if		6100	<=	&sic	<=	6100	then	&newvarname	=	44	;
else	if		6110	<=	&sic	<=	6111	then	&newvarname	=	44	;
else	if		6112	<=	&sic	<=	6113	then	&newvarname	=	44	;
else	if		6120	<=	&sic	<=	6129	then	&newvarname	=	44	;
else	if		6130	<=	&sic	<=	6139	then	&newvarname	=	44	;
else	if		6140	<=	&sic	<=	6149	then	&newvarname	=	44	;
else	if		6150	<=	&sic	<=	6159	then	&newvarname	=	44	;
else	if		6160	<=	&sic	<=	6169	then	&newvarname	=	44	;
else	if		6170	<=	&sic	<=	6179	then	&newvarname	=	44	;
else	if		6190	<=	&sic	<=	6199	then	&newvarname	=	44	;
else	if		6300	<=	&sic	<=	6300	then	&newvarname	=	45	;
else	if		6310	<=	&sic	<=	6319	then	&newvarname	=	45	;
else	if		6320	<=	&sic	<=	6329	then	&newvarname	=	45	;
else	if		6330	<=	&sic	<=	6331	then	&newvarname	=	45	;
else	if		6350	<=	&sic	<=	6351	then	&newvarname	=	45	;
else	if		6360	<=	&sic	<=	6361	then	&newvarname	=	45	;
else	if		6370	<=	&sic	<=	6379	then	&newvarname	=	45	;
else	if		6390	<=	&sic	<=	6399	then	&newvarname	=	45	;
else	if		6400	<=	&sic	<=	6411	then	&newvarname	=	45	;
else	if		6500	<=	&sic	<=	6500	then	&newvarname	=	46	;
else	if		6510	<=	&sic	<=	6510	then	&newvarname	=	46	;
else	if		6512	<=	&sic	<=	6512	then	&newvarname	=	46	;
else	if		6513	<=	&sic	<=	6513	then	&newvarname	=	46	;
else	if		6514	<=	&sic	<=	6514	then	&newvarname	=	46	;
else	if		6515	<=	&sic	<=	6515	then	&newvarname	=	46	;
else	if		6517	<=	&sic	<=	6519	then	&newvarname	=	46	;
else	if		6520	<=	&sic	<=	6529	then	&newvarname	=	46	;
else	if		6530	<=	&sic	<=	6531	then	&newvarname	=	46	;
else	if		6532	<=	&sic	<=	6532	then	&newvarname	=	46	;
else	if		6540	<=	&sic	<=	6541	then	&newvarname	=	46	;
else	if		6550	<=	&sic	<=	6553	then	&newvarname	=	46	;
else	if		6590	<=	&sic	<=	6599	then	&newvarname	=	46	;
else	if		6610	<=	&sic	<=	6611	then	&newvarname	=	46	;
else	if		6200	<=	&sic	<=	6299	then	&newvarname	=	47	;
else	if		6700	<=	&sic	<=	6700	then	&newvarname	=	47	;
else	if		6710	<=	&sic	<=	6719	then	&newvarname	=	47	;
else	if		6720	<=	&sic	<=	6722	then	&newvarname	=	47	;
else	if		6723	<=	&sic	<=	6723	then	&newvarname	=	47	;
else	if		6724	<=	&sic	<=	6724	then	&newvarname	=	47	;
else	if		6725	<=	&sic	<=	6725	then	&newvarname	=	47	;
else	if		6726	<=	&sic	<=	6726	then	&newvarname	=	47	;
else	if		6730	<=	&sic	<=	6733	then	&newvarname	=	47	;
else	if		6740	<=	&sic	<=	6779	then	&newvarname	=	47	;
else	if		6790	<=	&sic	<=	6791	then	&newvarname	=	47	;
else	if		6792	<=	&sic	<=	6792	then	&newvarname	=	47	;
else	if		6793	<=	&sic	<=	6793	then	&newvarname	=	47	;
else	if		6794	<=	&sic	<=	6794	then	&newvarname	=	47	;
else	if		6795	<=	&sic	<=	6795	then	&newvarname	=	47	;
else	if		6798	<=	&sic	<=	6798	then	&newvarname	=	47	;
else	if		6799	<=	&sic	<=	6799	then	&newvarname	=	47	;
else	if		4950	<=	&sic	<=	4959	then	&newvarname	=	48	;
else	if		4960	<=	&sic	<=	4961	then	&newvarname	=	48	;
else	if		4970	<=	&sic	<=	4971	then	&newvarname	=	48	;
else	if		4990	<=	&sic	<=	4991	then	&newvarname	=	48	;
else &newvarname =48;
run;

*Adjustments for Irvine & Pntiff 2008 industies;
data &out; set &out;
if sic in(4500:4599) then industry=49; /*Airlines*/
else if sic in(1310:1389) then industry=50; /*Natural Gas*/
else if sic in(4210:4219) then industry=51; /*Trucking*/
else if sic in(6000:6036) then industry=52; /*Banking*/
else if sic in(7800:7841) then industry=53; /*Entertainment*/
run;

proc format;
value ff
1="Agriculture"
2="Food Products"
3="Candy & Soda"
4="Beer & Liquor"
5="Tobacco Products"
6="Recreation"
7="Entertainment"
8="Printing & Publishing"
9="Consumer Goods"
10="Apparel"
11="Healthcare"
12="Medical Equipment"
13="Pharmaceutical"
14="Chemicals"
15="Rubber and Plastic"
16="Textiles"
17="Construction Materials"
18="Construction"
19="Steel Works"
20="Fabricated Products"
21="Machinery"
22="Electrical Equipment"
23="Automobiles"
24="Aircraft"
25="Shipbuilding, Railroad"
26="Defense"
27="Precious Metals"
28="Industrial Mining"
29="Coal"
30="Petroleum & Natural Gas"
31="Utilities"
32="Communication"
33="Personal Services"
34="Business Services"
35="Computers"
36="Electronic Equip."
37="Measuring Equip."
38="Business Supplies"
39="Shipping Containers"
40="Transportation"
41="Wholesale"
42="Retail"
43="Restaurants, Hotels"
44="Financial Services"
45="Insurance"
46="Real Estate"
47="Trading"
48="Almost Nothing"
49="Airlines"
50="Natural Gas"
51="Trucking"
52="Banking & Thrift"
53="Entertainment IP";
run;

%mend;

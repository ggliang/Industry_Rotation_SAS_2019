/*设置文件所在路径和用于筛选的最小支持度最小置信度阈值！！！！！*/
/*直接更改宏变量即可，其余无需变动，地址请指向存放数据文件的文件夹*/
%let path = D:\sasnew ;
%let given_sup=0.3;/*设置最小支持度*/
%let given_con=0.6;/*设置最小置信度*/

/*为保证程序运行速度，将日志输出至外部文件，储存在存放数据的文件夹*/
proc printto log="&path.\log.log";
run;


libname data "&path." ;
option noxwait;
x md "&path.\var";
libname var "&path.\var";

proc import out = data.data /*原始股价数据集*/
datafile = "&path.\申万二十八个行业收盘价_NoHeader.xlsx"
dbms = excel replace;
getnames=no;
run;
proc import out = data.SZidx /*上证指数*/
datafile = "&path.\上证指数.xlsx"
dbms = excel replace;
getnames=yes;
run;
proc import out = data.RF_return /*无风险收益*/
datafile = "&path.\无风险收益.xlsx"
dbms = excel replace;
getnames=yes;
run;

/*计算对数收益率*/
data data.data_szidx;
merge data.data data.szidx(rename=(TrdDt=F1 CLPr=F30));
by F1;
run;
data data.log_return;
set data.data_szidx;
array F{ * } _all_;
do i = 2 to 30;
	F(i) = log(F(i))-log(lag(F(i)));
end;
drop i;
if F2;
run;

/*计算行业与市场的相关系数，即为beta值，252个观测为一组计算*/
/*计算各行业之间的协方差矩阵与逆，252个观测为一组计算，存储在iml中，再次调用使用load*/
%macro beta;
%do n=2 %to 29;
beta&n=F&n/F30;
%end;
%mend;
/*划分十九年数据为19个周期*/
%macro slice;
%do i=1 %to 18;
data Y&i;
set data.log_return(firstobs=%eval(1+(&i-1)*252) obs=%eval(&i*252));
run;
proc corr data=Y&i out=cov&i cov noprint;
var _all_;
run;
data Y&i;
set cov&i(drop=_TYPE_ _NAME_ F1);
if _N_=30;
%beta;
drop F2-F30;
run;
data Y&i;
set Y&i;
do i =1 to 252;
output;
drop i;
end;
run;
data var.cov&i;
set cov&i(drop=_TYPE_ _NAME_ F1 F30);
if _N_>1;
if _N_<30;
n=_n_-1;
data cov&i;
set var.cov&i(drop=n);
run;
proc iml;
use cov&i;
read all into cov&i;
invcov&i=inv(cov&i);
store invcov&i;
quit;
run;
%end;
%mend;
%slice;
/*单独处理19年数据*/
data Y19;
set data.log_return(firstobs=4537);
run;
proc corr data=Y19 out=cov19 cov noprint;
var _all_;
run;
data Y19;
set cov19(drop=_TYPE_ _NAME_ F1);
if _N_=30;
%beta;
drop F2-F30;
run;
data Y19;
set Y19;
do i =1 to 251;
output;
drop i;
end;
run;
data var.cov19;
set cov19(drop=_TYPE_ _NAME_ F1 F30);
if _N_>1;
if _N_<30;
n=_n_-1;
run;
data cov19;
set var.cov19(drop=n);
run;
proc iml;
use cov19;
read all into cov19;
invcov19=inv(cov19);
store invcov19;
quit;
%macro period();
%do i=1 %to 19;
data Y&i;
set Y&i;
format period $5.;
informat period $5.;
period="&i";
%end;
%mend;
%period;
data data.beta;
set Y1-Y19;
run;
proc datasets library=work noprint;
delete Y1-Y19 cov1-cov19;
quit;

/*计算ALPHA收益*/
data return_beta;
merge data.log_return data.beta;
run;
data data.return_beta_rf;
merge return_beta data.rf_return(rename=(Date=F1));
by F1;
if F2;
run;
data data.return_beta_rf;
set data.return_beta_rf;
n=_n_;
run;
%macro alpha;
%do i=2 %to 29;
alpha&i=F&i-DRfRet-beta&i*(F30-DRfRet);
%end;
%mend;
data data.alpha(keep=alpha2-alpha29 period);
set data.return_beta_rf;
%alpha;
run;

/*alpha收益转化为逻辑值*/
%macro signal;
%do i=2 %to 29;
if alpha&i>0 then alpha&i=1;
else alpha&i=0;
%end;
%mend;
data data.signal;
set data.alpha;
%signal;
run;

data data.sum_connect1;
run;

/*分析不同行业的关联情况，并放置5阶滞后*/
/*生成代表不同滞后情况的变量插入数据集内*/
%macro varname(n); /*n为滞后阶数*/
%let num0 = %eval(2**&n.);
%let num1 = %eval(2**(&n.+1)-1);
%let num2 = %eval(&n.+1);
%put &num0;
%put &num1;
data varname;
 array v{&num2} v1-v&num2.;
 do i = &num0. to &num1.;
  q = i;
  do j = &num2 to 1 by -1;
   v(j)=mod(q , 2);
   q = floor(q / 2);
  end;
  output;
 end;
 drop i q j;
run;
data data.varname2;
 set varname;
 array v{&num2} v1-v&num2.;
 array nv{&num2} nv1-nv&num2.;
 format sv1-sv&num2. $1.;
 informat  var $50. var2 $50.;
 format var $50. var2 $50.;
 array sv{&num2} sv1-sv&num2.;
 do i = 1 to &num2.;
  if v{i}=1 then nv{i}=i;
  else nv{i}=.;
 end;
 var = "v";
 var2 = "v";
 do i = 1 to &num2.;
  if nv{i} ~= . then
   sv{i} = put(nv{i}, $1.);
  else sv{i} = "";
  var = catx('%',var,sv{i});
  var2=cats(var2,sv{i});
 end;
 var=cats(var,'%');
 keep sv1-sv&num2. var var2; /*var即变量名*/
run;
%mend;
%varname(6);
/*生成用于%include插入滞后的语句*/
data data.varname;
set data.varname2(keep=var var2);
file"&path.\varname.txt";
a='%gen_var(';
c=",";
b=');';
put a $ var $ c $ var2 $ b $;
run;
data data.varname;
set data.varname2( keep=var2);
a='sum(';
b=') as ';
d=cats(a,var2,b);
f=catx("",d,var2);
run;
proc sql noprint;
   select f
   into :list_a separated by ','
   from data.varname;
quit;

/*进行关联情况分析，ComA表示A行业，ComB表示B行业*/
%macro run_all(comA,comB);
data com&comA._&comB.(keep=l1 l2 l3 l4 l5 l6 l7 period);
set data.signal;
l1=lag5(alpha&comA.);
l2=lag5(alpha&comB.);
l3=lag4(alpha&comB.);
l4=lag3(alpha&comB.);
l5=lag2(alpha&comB.);
l6=lag(alpha&comB.);
rename alpha&comB.=l7;
if l1^=.;
run;
data com&comA._&comB._1;
set com&comA._&comB.;
format ll1-ll7 $1.;
if l1=1 then ll1=1;
if l2=1 then ll2=2;
if l3=1 then ll3=3;
if l4=1 then ll4=4;
if l5=1 then ll5=5;
if l6=1 then ll6=6;
if l7=1 then ll7=7;
name=cats(ll1,ll2,ll3,ll4,ll5,ll6,ll7);
keep name period;
run;

data com&comA._&comB.;
set com&comA._&comB._1;
no=_n_;
run;
%macro gen_var(name,name1);
data a;
set com&comA._&comB.;
where name like substr("&name.",2);
&name1.=1;
run;
data com&comA._&comB.;
merge com&comA._&comB. a;
by no;
run;
%mend;
%include "&path.\varname.txt";
data com&comA._&comB._2;
set com&comA._&comB.(drop=no name);
run;
proc sql;
create table com&comA._&comB._3 as
select 
&list_a, period
from com&comA._&comB._2
group by period;
quit;
data com&comA._&comB._2;
set com&comA._&comB._3;
lead=&comA.;
result=&comB.;
run;
data data.sum_connect1;
set data.sum_connect1 com&comA._&comB._2;
run;
proc datasets library=work noprint;
delete com&comA._&comB._2 com&comA._&comB._3 com&comA._&comB._1 com&comA._&comB.;
quit;
%mend;
/*生成两两组合的序列数*/
data b;
do i=2 to 29;
do j=2 to 29;
output;
end;
end;
run;
data b;
set b;
if i~=j then output;
run;
data b2;
set b;
file"&path.\index_num.txt";
a='%run_all(';
c=",";
b=');';
put a $ i $ c $ j $ b $;
run;
%include "&path.\index_num.txt";
/*分析房地产、建材、钢铁行业的轮动效应，对应的变量序号分别为15、19、5*/
/*%run_all(5,19);*/
/*%run_all(15,5);*/
/*%run_all(15,19);*/
/*%run_all(19,15);*/
/*%run_all(19,5);*/
/*%run_all(5,15);*/


data data.varname;
set data.varname2(keep=var var2);
file"&path.\select.txt";
d='%select(';
b=');';
e=",&given_sup";
f=",&given_con";
put d $ var2 $ e $ f $ b $;
run;
data data.sum_judge_total;
run;
/*计算每一组关联情况的支持度和置信度，并筛选满足阈值的关联*/
%macro select(vname,given_sup,given_con);
data data.sum_judge;
set data.sum_connect1(firstobs=2);
sup=&vname./247;
con=&vname./v1;
format connection $10.;
informat connection $10.;
connection=substr("&vname.",2);
run;
data data.sum_judge_total;
format connection $10.;
informat connection $10.;
set data.sum_judge_total data.sum_judge;
run;
data data.sum_judge_total;
set data.sum_judge_total;
format connection $10.;
informat connection $10.;
where sup>=&given_sup and con>=&given_con;
keep lead result sup con connection period;
run;
%mend;
%include "&path.\select.txt";
data data.sum_judge_total;
set data.sum_judge_total;
if connection~=1;
run;


proc sort data=data.sum_judge_total out=data.sum_judge_total;
by descending con;
quit;




%macro base_dataset(sum,lr);
data sum_judge_total_2;
	set &sum;
run;

data Log_return;
	set &lr;
run;
%mend;
%base_dataset(data.sum_judge_total_2,data.Log_return);

%macro split_Sum_judge_total_2; /* 各年关联关系情况 */
	%do i = 1 %to 19;
	data t&i.;
		set Sum_judge_total_2;
		if period = &i.;
	run;
	%end;
%mend;

%macro F_lag5_all; /* 各行业0天到-5天的数据 */
	%do i = 2 %to 29;
		data industry&i.;
			set Log_return_for_reg(keep = F&i.);
			/* 此处为 &date_year.年对数收益率数据 */
			rename F&i. = F&i._lag_0;
			%do j = 1 %to 5;
				F&i._lag_&j. = lag&j.(F&i.);
			%end;
			if F&i._lag_5;
		run;
	%end;
%mend;

%macro merge_industries_lag05_;
	data merge_industries_lag05;
		set industry2;
	run;
	%do i = 3 %to 29;
		data merge_industries_lag05;
			merge merge_industries_lag05 industry&i.;
		run;
	%end;
%mend;
%macro reg_rawdata_nodrop;*(target_date);
%split_Sum_judge_total_2;
%F_lag5_all;
%merge_industries_lag05_;
%mend;

%macro dropF_0(indst);
	%do i = 2 %to %eval(&indst.-1);
		drop F&i._lag_0;
	%end;
	%do i = %eval(&indst.+1) %to 29;
		drop F&i._lag_0;
	%end;
%mend;

%macro sql_selectVar;
	%do i = 2 %to 28;
		%do j = 1 %to 5;
			(a.p_F&i._lag_&j.*b.F&i._lag_&j.) as new_F&i._lag_&j.,
		%end;
	%end;
	%do j = 1 %to 4;
		(a.p_F29_lag_&j.*b.F29_lag_&j.) as new_F29_lag_&j.,
	%end;
	a.p_F29_lag_5*b.F29_lag_5 as new_F29_lag_5
%mend;

%macro get_predictdatax;
	%do i = 2 %to 29;
		%do j = 1 %to 5;
			new_F&i._lag_&j. = lag&j.(F&i.);
		%end;
	%end;
	%do i = 2 %to 29;
		%do j = 1 %to 5;
			if new_F&i._lag_&j. ~= .;
		%end;
	%end;
	%do i = 2 %to 29;
		drop F&i.;
	%end;
%mend;

%macro reg_Findst(indst);
data reg_F&indst;
	set merge_industries_lag05;
	%dropF_0(&indst.);
run;
%mend;

%macro predict(target_date, indst);
data null;
	set Log_return;
	yr = year(F1);
	Format yr 4.;
	if F1 = "&target_date."d then do;
		call symput('obs_', _n_);
		call symput('date_year',yr);
		stop;
	end;
run;

%put &obs_.;
%put &date_year.;

%let fstobs = %eval(&obs_. - 20);
data Log_return_for_reg;
	set log_return(firstobs=&fstobs. obs=&obs_.);
run;
%reg_rawdata_nodrop;

%reg_Findst(&indst.);

%let datasetnm = %sysfunc(compress(F&indst._suf01_&date_year.));
%put &datasetnm.;

data &datasetnm.; /*某年某行业受哪些行业影响*/
	set sum_judge_total_2;
	if result = "&indst.";
	if period = substr(%sysfunc(compress("&date_year.")),3);
	/*put substr(%sysfunc(compress("&date_year.")),3);*/
	if length(connection)=2;
	if suf>0.15;
	if connection ~= '12'; 
	if con>0.5;
	p = 1;
run;

proc sort data = &datasetnm. out = &datasetnm._sortbycon_suf;
	by descending con descending suf;
run;

data &datasetnm._maininds;
	set &datasetnm._sortbycon_suf(obs = 10);
run;

proc sort data = &datasetnm._maininds;
	by lead;
run;

data varnm;
	do i = 2 to 29;
		do j = 1 to 5;
			varnames = compress('p_F'||i||'_lag_'||j);
			m + 1;
			output;
		end;
	end;
	keep varnames m;
run;
proc sort data = varnm; by varnames; run;

data &datasetnm._forTrnsps;
	set &datasetnm._maininds;
	informat varnames $32.;
	format varnames $32.;
	varnames = compress('p_F'||lead||'_lag_'||(substr(connection, 2,2)-2));
	*n = _n_;
run;

proc sort data = &datasetnm._forTrnsps; by varnames; run;

data &datasetnm._forTrnsps_new;
	merge &datasetnm._forTrnsps varnm;
	by varnames;
run;

proc sort data = &datasetnm._forTrnsps_new; by m; run;

data &datasetnm._forTrnsps_new;
	set &datasetnm._forTrnsps_new;
	if p = . then p = 0 ;
	if find(varnames,"_F&indst._") then p = 1;
	keep varnames p;
run;

proc transpose data=&datasetnm._forTrnsps_new
				out=&datasetnm._Trnsps;
	ID varnames;
run;

data &datasetnm._Trnspsed;
	set &datasetnm._Trnsps;
	drop _name_;
run;

proc sql;
	create table F&indst._reg_pre as
		select %sql_selectVar 
		from &datasetnm._trnspsed as a, reg_F&indst. as b;
quit;

data F&indst._reg_pre_firstrow;
	set F&indst._reg_pre(obs = 1);
run;	

proc transpose data = F&indst._reg_pre_firstrow
	out = F&indst._reg_pre_firstrow_trpsd;
run;	

data F&indst._reg_pre_firstrow_trpsd;
	set F&indst._reg_pre_firstrow_trpsd;
	if COL1 ~= 0;
	drop COL1;
	m + 1;
	call symput('xvar_num', m);
run;

%put &xvar_num.;

proc transpose data = F&indst._reg_pre
	out = F&indst._reg_pre_trpsd;
run;

proc sql noprint;
	create table F&indst._reg_pre_trpsd_droped as
	select * from F&indst._reg_pre_trpsd a right join F&indst._reg_pre_firstrow_trpsd b
		on a._NAME_ = b._NAME_;
quit;

proc sort data=F&indst._reg_pre_trpsd_droped; by m; run;

data F&indst._reg_pre_trpsd_droped; set F&indst._reg_pre_trpsd_droped; drop m; run;

proc transpose data = F&indst._reg_pre_trpsd_droped out = F&indst._reg_pre_droped;
	ID _NAME_;
run;

data F&indst._reg_pre_droped; set F&indst._reg_pre_droped; drop _NAME_; run;

data F&indst._Y;
	set reg_f&indst.(keep = F&indst._lag_0);
run;

data F&indst._regdata;
	merge F&indst._Y F&indst._reg_pre_droped;
run;

proc sql noprint;
	select _NAME_ into : xvar_nm separated by " "
	from F&indst._reg_pre_firstrow_trpsd;
quit;
%put &xvar_nm.;

proc reg data=F&indst._regdata outest=F&indst._&target_date._para noprint;
	model F&indst._lag_0 = &xvar_nm.
		/ noint collinoint;
run;

data F&indst._&target_date._para_droped;
	set F&indst._&target_date._para;
	drop _MODEL_ _TYPE_ _DEPVAR_ _RMSE_ F&indst._lag_0;
run;/*回归结束*/

data F&indst._&target_date._predictdata;
	set Log_return(firstobs = %eval(&obs_.-4) obs = %eval(&obs_.+1));/*logreturn*/
	%get_predictdatax;
	drop F1;
	keep &xvar_nm.;
run;

data F&indst._&target_date._predict_pre;
	set F&indst._&target_date._predictdata F&indst._&target_date._para_droped;
run;

proc transpose data = F&indst._&target_date._predict_pre
	out = F&indst._&target_date._predict_trpsd;
run;

data F&indst._&target_date._predict_trpsd;
	set F&indst._&target_date._predict_trpsd;
	COL3 = COL1*COL2;
	COL4+COL3;
	call symput("result", COL4);
run;
%put &result.;

data pred_rslt_F&indst._&target_date.;
	date = "&target_date."d;
	format date yymmdd10.;
	pred_var = "F&indst.";
	pred_rslt = &result.;
run;
%mend;

*%let target_date = 30AUG2017;
*%predict(&target_date., 5);


%macro all_indst_pred(target_date);
	%do indst = 2 %to 29;
		%predict(&target_date., &indst.);
	%end;
%mend;
*%all_indst_pred(30AUG2017);*&target_date.);

%macro set_all_pred(target_date);
	%do i = 2 %to 29;
		pred_rslt_F&i._&target_date.
	%end;
%mend;
*%put %set_all_pred(&target_date);

%macro get_est_r(target_date); 
/*得到某一天各行业的预期收益率 结果储存在 data.est_r_&target_date.数据集中*/
%all_indst_pred(&target_date.);
data pred_&target_date;
	set %set_all_pred(&target_date); 
run;


data data.est_r_&target_date.;
	set pred_&target_date;
	rename pred_rslt = e_r;
	keep pred_rslt;
run;

*proc datasets library=work kill nolist;  /*清除work中的数据集*/
*quit;
*run;
%mend;


%macro get_weight(target_date);
data _null_;
	set data.return_beta_rf;
	if F1="&target_date"d;
	call symput('period',input(period,3.));
run;

/*p1为optmodel过程调用，p2为iml过程调用*/
data _null_;
	p1=cats('var.cov',&period);
	p2=cats('invcov',&period);
	call symputx("p1",p1);
	call symputx("p2",p2);
run;

proc iml;
	use data.est_r_&target_date.;
	read all into e;
	store e;
quit;

data rf;
	set data.rf_return;
	if Date="&target_date"d;
	drop Date;
run;

proc iml;
	use rf;
	read all into rf;
	store rf;
quit;

proc iml;
	load rf;
	load e;
	load &p2;
	one=j(28,1,1);
	store one;
	A=t(e)*&p2*one;
	B=t(e)*&p2*e;
	C=t(one)*&p2*one;
	D=B*C-A*A;
	/*E=A/C+D/(A*C-rf*C*C);*/
	E=A/C;
	create data.Er_&target_date. from E;
	append from E;
quit;

proc optmodel printlevel=0;
	var w{1..28}>=0 <=1;
	set l=1..28;
	set c;
	set est_r;
	set e_er;
	number e{est_r};
	number er{e_er};
	number var{c,l} ;
	read data &p1 into c=[n] {i in l} <var[n,i]=col("F"||i+1)>;
	read data data.est_r_&target_date. into est_r=[_n_] e=e_r;
	read data data.Er_&target_date. into e_er=[_n_] er=col1;
	min sigma2=sum{i in c,j in l}var[i,j]*w[i]*w[j];
	con one:sum{i in 1..28}w[i]=1;
	con return:sum{i in 1..28}w[i]*e[i]=er[1];
	solve with qp;
	create data data.w_&target_date. from [i] w=w;
quit;

data data.w_&target_date.;
	set data.w_&target_date.;
	drop i;
run;
%mend;



%macro er(est_r,n_indu);/*date为预测日的日期使用date7.的格式，est_r为预测各行业的收益，n为变化的行业取2-29*/
data _null_;
set data.return_beta_rf;
if F1=&date;
call symput('period',input(period,3.));
run;

/*p1为optmodel过程调用，p2为iml过程调用*/
data _null_;
p1=cats('var.cov',&period);
p2=cats('invcov',&period);
call symputx("p1",p1);
call symputx("p2",p2);
run;
proc iml;
use &est_r;
read all into e;
store e;
quit;
data rf;
set data.rf_return;
if Date=&date;
drop Date;
run;
proc iml;
use rf;
read all into rf;
store rf;
quit;
proc iml;
load rf;
load e;
load &p2;
one=j(28,1,1);
store one;
A=t(e)*&p2*one;
B=t(e)*&p2*e;
C=t(one)*&p2*one;
D=B*C-A*A;
E=A/C;
create Er&n_indu from E;
append from E;
quit;
data Er&n_indu;
set Er&n_indu;
rename col1=E&n_indu;
run;
%mend;

%macro sensibility;
%do j_indu=2 %to 29;
/*考虑行业n-1的敏感性*/
data data.new_lr;
set data.log_return;
F&j_indu=F&j_indu+0.0001;
run;

/*预测收益率的变化得出数据集est_r_j*/
%base_dataset(data.sum_judge_total_2,data.new_lr);
%predict(&target_date., &j_indu.);
data data.new_est_r_&target_date.;
set pred_rslt_F&j_indu._&target_date.(keep=pred_rslt rename=(pred_rslt=e_r));
F=%eval(&j_indu.);
run;

data data.est_r_&target_date.;
set data.est_r_&target_date.;
F=_n_+1;
run;

data data.new_est_r_&target_date.;
merge data.est_r_&target_date. data.new_est_r_&target_date.;
by F;
drop F;
run;
/*算出行业变化后的Er&j*/
%er(data.new_est_r_&target_date.,&j_indu);

%end;
%mend;

%macro delta_e;
%do i=2 %to 29;
dE&i=E&i-col1;
%end;
%mend;
%macro proportion;
%do i=2 %to 29;
pro&i=dE&i./sum;
%end;
%mend;

%macro contribution(d);
/*设置预测日期*/
%let date="&d"d;
%sensibility;

data data.delta_e_&target_date.;
merge data.er_&target_date. er2-er29;
%delta_e;
sum=sum(of dE2-dE29);
%proportion;
keep pro2-pro29;
run;
proc datasets library=work noprint;
delete er2-er29;
quit;
%mend;



/*获取大盘收益率*/
data data.Market_yield;
	set data.Log_return(keep = F1 F30);
	informat F1 date9.;
	format F1 date9.;
	date_F1 = put(F1, date9.);
run;

%let target_date = 11SEP2019;
%base_dataset(data.sum_judge_total_2,data.Log_return);
%get_est_r(&target_date.);
%get_weight(&target_date.);
%let target_date = 12SEP2019;
%base_dataset(data.sum_judge_total_2,data.Log_return);
%get_est_r(&target_date.);
%get_weight(&target_date.);
%contribution(&target_date.);
%let target_date = 16SEP2019;
%base_dataset(data.sum_judge_total_2,data.Log_return);
%get_est_r(&target_date.);
%get_weight(&target_date.);
%contribution(&target_date.);
%let target_date = 17SEP2019;
%base_dataset(data.sum_judge_total_2,data.Log_return);
%get_est_r(&target_date.);
%get_weight(&target_date.);
%contribution(&target_date.);

data data.pred_er;
	set 
		data.Er_11sep2019 
		data.Er_12sep2019 
		data.Er_16sep2019 
		data.Er_17sep2019;
	rename COL1 = pred_E_r;
run;
	
data data.real_er;
	set data.Market_yield(firstobs = 4771 obs = 4774);
	rename F1 = date F30 = market_yield;
	keep F1 F30;
run;

data data.er;
	merge data.pred_er data.real_er;
	epsilon = pred_E_r - market_yield;
run;

data data.er;
	set data.er;
	sum_pred + pred_E_r;
	sum_mkt + market_yield;
run;

data data.weight;
	merge 
		data.W_11sep2019(rename=(W = W_11sep2019))
		data.W_12sep2019(rename=(W = W_12sep2019))
		data.W_16sep2019(rename=(W = W_16sep2019))
		data.W_17sep2019(rename=(W = W_17sep2019));
run;

data data.delta_e;
set data.delta_e_12SEP2019 data.delta_e_16SEP2019 data.delta_e_17SEP2019;
run;
PROC EXPORT DATA= data.delta_e 
            OUTFILE= "&path.\delta_e.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="Sheet1"; 
RUN;

PROC EXPORT DATA= data.er 
            OUTFILE= "&path.\Er.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="Sheet1"; 
RUN; 


PROC EXPORT DATA= data.weight 
            OUTFILE= "&path.\weight.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="Sheet1"; 
RUN;


proc import out = data.dqsb_day /*日流通数据*/
datafile = "&path.\TRD_Dalyr.xlsx"
dbms = excel replace;
getnames=yes;
proc import out = data.dqsb_profit /*财务报表*/
datafile = "&path.\FS_Comins.xlsx"
dbms = excel replace;
getnames=yes;
run;
data data.index_design;
run;
data dqsb_day_weight;
run;
data dqsb_profit;
set data.dqsb_profit;
keep stkcd percentage;
run;
data dqsb_day_weight;
merge data.dqsb_day dqsb_profit;
by stkcd;
run;
data dqsb_day_weight;
set dqsb_day_weight;
dsmvosd_weight=dsmvosd*percentage;
where percentage~=.;
run;

proc sql;
create table q5_c as 
select trddt,sum(dsmvosd) as sum from data.dqsb_day
group by trddt;
quit;
data q5_d;
set q5_c;
dt = input(Trddt, yymmdd10.);
format dt date9.;
lag_dsmvosd=lag(sum);
change=sum/lag_dsmvosd-1;
if change~=.;
drop trddt;
run;

data q5_b;
set q5_d(rename=(dt=trddt));
retain index_design 4154.113;
index_design=index_design*(1+change);
run;
data data.index_design;
merge data.data(rename=(F1=trddt F21=index_actual)) q5_b ;
by trddt;
keep trddt index_actual index_design;
if index_design~=.;
label index_actual=index_actual;
run;


AXIS1 MINOR=(COLOR=BLUE NUMBER=1);
AXIS2 MINOR=(COLOR=BLUE HEIGHT=0.25 NUMBER=1);
SYMBOL1 INTERPOL=JOIN VALUE=DOT C=BLUE;
SYMBOL2 INTERPOL=JOIN VALUE=# C=GREEN;
PROC GPLOT DATA=data.index_design;
    TITLE 'Designed_index compared with Actual_index';
    PLOT index_actual*trddt index_design*trddt/OVERLAY LEGEND HAXIS=AXIS1 VAXIS=AXIS2 ;
RUN;
QUIT;


proc printto ;
run;

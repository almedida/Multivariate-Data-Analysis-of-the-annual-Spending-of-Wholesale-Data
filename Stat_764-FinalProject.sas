

ods rtf file = 'C:\Users\Nazia Khan\Documents\Fall 2020\Multivariate Statistics\Project\Wholesale.rtf';
 
Data new;
infile 'C:\Users\Nazia Khan\Documents\Fall 2020\Multivariate Statistics\Project\wholesale.csv' dlm=',' firstobs=2;
input Channel $	Region $	Fresh	Milk	Grocery	Frozen	Detergents_Paper	Delicassen;run;
Proc sort data=new;
by Channel;
run;
*proc print;*run;


proc sgscatter data=new;
 plot Channel*(Fresh	Milk	Grocery	Frozen	Detergents_Paper	Delicassen) / rows=6 columns=2;
run;

*******Testing of Means**********;
PROC GLM;
  CLASS Channel;
  MODEL Fresh	Milk	Grocery	Frozen	Detergents_Paper	Delicassen = Channel;
  MANOVA H=Channel/PRINTE PRINTH MSTAT=EXACT;
Title 'Multivariate Testing of Means';
RUN;

proc corr data=new plots=matrix plots(maxpoints =50000) ;
var Fresh	Milk	Grocery	Frozen	Detergents_Paper	Delicassen;
by Channel;
run;
quit; 

*proc print data=new;
*run;

********Testing of Covariance*******;

Title 'Multivariate testing of Covariance';

PROC IML;
USE new;
READ ALL VAR {Fresh	Milk Grocery	Frozen	Detergents_Paper Delicassen} INTO Y;
 n = nrow(Y);
 p = ncol(Y);
 Y1 = Y[1:298,];
 Y2 = Y[299:440,];
 n1 = nrow(Y1);
 y1b = 1/n1 * t(Y1)*J(n1,1);
 S1 = 1/(n1-1) * t(Y1) * (I(n1) - 1/n1*j(n1)) * Y1;

 n2 = nrow(Y2);
 y2b = 1/n2 * t(Y2)*J(n2,1);
 S2 = 1/(n2-1) * t(Y2) * (I(n2) - 1/n2*j(n2)) * Y2;

 Spl = 1/(n1+n2-2) * ((n1-1)*S1 + (n2-1)*S2);

 v1 = n1 -1;
 v2 = n2-1;
 v = v1+v2;
 detS1 = det(S1);
 detS2 = det(S2);
 detSpl = det(Spl);

 logM =(1/2) * (v1*log(detS1) + v2*log(detS2) - v*log(detSpl));
 exactu = -2 * logM;

 k = 2;
 c1 = (1/v1 + 1/v2 - 1/v) * (2*p**2 + 3*p - 1) / (6 * (p+1) * (k-1));
 u = -2*(1-c1)*logM;
 dfX2 = (1/2) * (k-1) * p * (p+1);
 X2crit = quantile('CHISQUARE', 0.95, dfX2);
 X2pval = 1 - cdf('CHISQUARE', u, dfX2);
PRINT S1;
PRINT S2;
PRINT Spl;
PRINT v1 v2 detS1 detS2 detSpl, logM;
PRINT 'EXACT TEST STATISTIC';
PRINT exactu;
PRINT 'CHI-SQUARE APPROXIMATION';
PRINT c1 u X2crit X2pval;

*******************Discriminant Analysis***************;


TITLE 'BARTLETTS TEST';
 PROC DISCRIM DATA=new POOL=TEST;
 CLASS Channel;
 run;

PROC DISCRIM LIST CROSSVALIDATE POOL=YES;
CLASS PLAYER;
VAR Fresh	Milk Grocery	Frozen	Detergents_Paper Delicassen;
RUN;

 ******************Factor Analysis*********************;

 PROC FACTOR METHOD=PRIN COV PROPORTION=0.8 SCREE EV RESIDUALS PLOT;  /*Use COV option if you would like to perform*/
 VAR Fresh	Milk Grocery	Frozen	Detergents_Paper Delicassen;	  /*factor analysis on covariance matrix */
 RUN;

 *****************Principle Component******************;

PROC PRINCOMP COV OUT=RESULTS;
VAR Fresh	Milk Grocery	Frozen	Detergents_Paper Delicassen;
proc print;

ods rtf close;















































**********************************************************************************;

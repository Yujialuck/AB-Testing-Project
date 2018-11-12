p_enroll_click<-0.20625
p_pay_enroll<-0.53
p_pay_click<-0.1093125
ctr_trial<-0.08
cookie_overview<-40000
cookie_trial<-3200
enroll<-660

#Standard Deviation
#gross conversion
sd_gross<-sqrt(p_enroll_click*(1-p_enroll_click)/(5000*ctr_trial))
sd_gross
#retention
sd_retention<-sqrt(p_pay_enroll*(1-p_pay_enroll)/(5000*ctr_trial*p_enroll_click))
sd_retention
#net conversion
sd_netconv<-sqrt(p_pay_click*(1-p_pay_click)/(5000*ctr_trial))
sd_netconv

#sample size
#gross conversion
p0_gc<- p_enroll_click
p11_gc<-p_enroll_click-0.01
p12_gc<-p_enroll_click+0.01
numclick_gc<-(qnorm(0.2)+qnorm(0.05/2)*sqrt(p0_gc*(1-p0_gc)/(p11_gc*(1-p11_gc))))^2*p11_gc*(1-p11_gc)/(p0_gc-p11_gc)^2+(qnorm(0.2)+qnorm(0.05/2)*sqrt(p0_gc*(1-p0_gc)/(p12_gc*(1-p12_gc))))^2*p12_gc*(1-p12_gc)/(p0_gc-p12_gc)^2
num_gc<-numclick_gc/0.08*2
num_gc

#retention
p0_r<-p_pay_enroll
p11_r<-p_pay_enroll-0.01
p12_r<-p_pay_enroll+0.01
numenroll_r<-(qnorm(0.2)+qnorm(0.05/2)*sqrt(p0_r*(1-p0_r)/(p11_r*(1-p11_r))))^2*p11_r*(1-p11_r)/(p0_r-p11_r)^2+(qnorm(0.2)+qnorm(0.05/2)*sqrt(p0_r*(1-p0_r)/(p12_r*(1-p12_r))))^2*p12_r*(1-p12_r)/(p0_r-p12_r)^2
num_r<-numenroll_r/(0.08*0.20625)*2
num_r

#net conversion
p0_nc<- p_pay_click
p11_nc<-p_pay_click-0.0075
p12_nc<-p_pay_click+0.0075
numclick_nc<-(qnorm(0.2)+qnorm(0.05/2)*sqrt(p0_nc*(1-p0_nc)/(p11_nc*(1-p11_nc))))^2*p11_nc*(1-p11_nc)/(p0_nc-p11_nc)^2+(qnorm(0.2)+qnorm(0.05/2)*sqrt(p0_nc*(1-p0_nc)/(p12_nc*(1-p12_nc))))^2*p12_nc*(1-p12_nc)/(p0_nc-p12_nc)^2
num_nc<-numclick_nc/0.08*2
num_nc

#Duration
num_nc/(cookie_overview*0.5)


#Sanity Check
Myexp<- read.csv(file="C:/Users/arash/Desktop/finalproj_exp.csv", header=TRUE, sep=",")
Mycont<- read.csv(file="C:/Users/arash/Desktop/finalproj_cont.csv", header=TRUE, sep=",")
Myexp<-data.frame(Myexp)
Mycont<-data.frame(Mycont)

#pageview
#method1
p_page<-sum(Mycont$Pageviews)/(sum(Myexp$Pageviews)+sum(Mycont$Pageviews))
p0_page<-0.5
varoneprop_page<-p0_page*(1-p0_page)
n1<-sum(Myexp$Pageviews)
n2<-sum(Mycont$Pageviews)
CIloweroneprop<-p0_page-qnorm(0.975)*sqrt(varoneprop_page/(n1+n2))
CIupperoneprop<-p0_page+qnorm(0.975)*sqrt(varoneprop_page/(n1+n2))
paste0("For the metric number of cookies, 95% Confidence Interval of fraction assgined 
       to control group is [ ", CIloweroneprop,', ',CIupperoneprop, '].')
paste0("The actual fraction is ", p_page,' which is in the confidence interval so we do not reject
       Hnull and conclude that there is no significant difference between the fractions of two groups.')

#method2 compare two sample mean
varexp_page<-var(Myexp$Pageviews)
varcont_page<-var(Mycont$Pageviews)
meanexp_page<-mean(Myexp$Pageviews)
meancont_page<-mean(Mycont$Pageviews)
vartwomean_page<-(varexp_page/length(Myexp$Pageviews)+varcont_page/length(Mycont$Pageviews))
CIlowertwomean_page<-0-qnorm(0.975)*sqrt(vartwomean_page)
CIuppertwomean_page<-0+qnorm(0.975)*sqrt(vartwomean_page)
paste0("For the metric number of cookies, 95% Confidence Interval of difference between 
       the mean of control group and the mean of experimental group is 
       [ ", CIlowertwomean_page,', ',CIuppertwomean_page, '].')
paste0("The actual difference between two means is ", meanexp_page-meancont_page,' 
       which is in the confidence interval so we do not reject Hnull and conclude that there is 
       no significant difference between two means.')

#click
#method1
p_click<-sum(Mycont$Clicks)/(sum(Mycont$Clicks)+sum(Myexp$Clicks))
p0_click<-0.5
n1_click<-sum(Mycont$Clicks)
n2_click<-sum(Myexp$Clicks)
var_click<-p0_click*(1-p0_click)
CIlower_click<-p0_click-qnorm(0.975)*sqrt(var_click/(n1_click+n2_click))
CIupper_click<-p0_click+qnorm(0.975)*sqrt(var_click/(n1_click+n2_click))
paste0('For number of click, 95% confidence interval for the expected fraction assgined to 
       control group is [', CIlower_click, ', ', CIupper_click, '].')
paste0('The actual fraction is ', p_click, ' which drops in the CI. So we do not reject Hnull
       and conclude there is no significant difference in control and experiment group. Therefore number 
       of clicks pass the sanity check as well.')


#CTR mwthod1 using diff betwwen two proportions and H0 is difference=0
ctpcont<-sum(Mycont$Clicks)/sum(Mycont$Pageviews)
ctpexp<-sum(Myexp$Clicks)/sum(Myexp$Pageviews)
var_ctp<-ctpcont*(1-ctpcont)/sum(Mycont$Pageviews)+ctpexp*(1-ctpexp)/sum(Myexp$Pageviews)
CIlower_ctp<-0-qnorm(0.975)*sqrt(var_ctp)
CIupper_ctp<-0+qnorm(0.975)*sqrt(var_ctp)
paste0('For Click-through-probability, 95% confidence interval for the probability of control group 
       is [', CIlower_ctp, ', ', CIupper_ctp, '].')
paste0('The actual probability is ', ctpcont-ctpexp, ' which drops in the CI. So we do not reject Hnull
       and conclude there is no significant difference in control and experiment group. Therefore number 
       of clicks pass the sanity check as well.')

#CTR method2 using one proportion

# Effect Size (Compare with exp and cont)
#gross conversion
cont<-Mycont[complete.cases(Mycont),]
exp<-Myexp[complete.cases(Myexp),]
gccont<-sum(cont$Enrollments)/sum(cont$Clicks)
gcexp<-sum(exp$Enrollments)/sum(exp$Clicks)
vargc<-gccont*(1-gccont)/sum(cont$Clicks)+gcexp*(1-gcexp)/sum(exp$Clicks)
CIlower_gc<-(gccont-gcexp)-qnorm(0.975)*sqrt(vargc)
CIupper_gc<-(gccont-gcexp)+qnorm(0.975)*sqrt(vargc)

paste0('Since 0 is not in the confidence interval [', CIlower_gc,' ', CIupper_gc,'] of the 
samples difference in gross conversion between control group and experiment group. 
Therefore we should reject Hnull and conclude there exists statistical significant difference 
between two groups. And the CI does not contain dmin=0.01 so it is also practical siginificant.')

#net conversion
nccont<-sum(cont$Payments)/sum(cont$Clicks)
ncexp<-sum(exp$Payments)/sum(exp$Clicks)
ncvar<-nccont*(1-nccont)/sum(cont$Clicks)+ncexp*(1-ncexp)/sum(exp$Clicks)
CIlower_nc<-(nccont-ncexp)-qnorm(0.975)*sqrt(ncvar)
CIupper_nc<-(nccont-ncexp)+qnorm(0.975)*sqrt(ncvar)
paste0('Since 0 is in the confidence interval [', CIlower_nc,' ', CIupper_nc,'] of the samples difference in net conversion 
       between control group and experiment group. Therefore we should not reject Hnull and conclude there 
       is no significant difference between two groups.')


#Sign Tests
#gross conversion
gccont_sign<-cont$Enrollments/cont$Clicks
gcexp_sign<-exp$Enrollments/exp$Clicks
gcsign_pos<-length(which(gcexp_sign-gccont_sign>0))
gcsign_pvalue<-pbinom(gcsign_pos,length(gcexp_sign-gccont_sign),0.5)*2
paste0('The P-value of sign test is ', gcsign_pvalue,'. We can reject Hnull and
conclude there exists statistical significant difference in gross converesion 
of control and experiment groups if siginificant level larger than P-value.')


#net conversion
nccont<-cont$Payments/cont$Clicks
ncexp<-exp$Payments/exp$Clicks
ncsign_pos<-length(which(ncexp-nccont>0))
ncsign_pvalue<-pbinom(ncsign_pos, length(ncexp-nccont),0.5)*2
paste0('Since the P-value is ', gcsign_pvalue,'. Which is very large, so we 
       reject Hnull and conclude there is no statistical significant difference 
       in net converesion of control and experiment groups.')

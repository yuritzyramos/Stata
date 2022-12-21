generate time = m(1939m1) + _n-1
format time %tm
tsset time
// Taking a look at the data 
tsline employment
//Possible breaks in trend in due to recessions
tsline(employment) if time>=m(2001m1)

//Trying out basic models
//Linear model 
regress employment time
predict yhat
//Check AIC and BIC Scores
estat ic

//Quadratic model 
gen time_sq = time^2
regress employment time time_sq
predict yhat_sq
twoway (tsline employment) (tsline yhat_sq, lpattern(dash))
//Check AIC and BIC Scores
estat ic

//Cubic model 
gen time_cub = time^3
regress employment time time_sq time_cub
predict yhat_cub
twoway (tsline employment) (tsline yhat_cub, lpattern(dash))
//Check AIC and BIC Scores
estat ic

//Quartic trend
gen time_quad = time^4
regress employment time time_sq time_cub time_quad
predict yhat_quad 
twoway (tsline employment) (tsline yhat_cub, lpattern(dash))
//Check AIC and BIC Scores
estat ic

//Time^5
gen time_quint = time^5
regress employment time time_sq time_cub time_quad time_quint
//Check AIC and BIC Scores
estat ic

//Time^6
gen time_6 = time^6
regress employment time time_sq time_cub time_quad time_quint time_6
//Check AIC and BIC Scores
estat ic

//Time^7
gen time_7 = time^7
regress employment time time_sq time_cub time_quad time_quint time_6 time_7
//Check AIC and BIC Scores
estat ic

//Time^8
gen time_8 = time^8
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8
//Check AIC and BIC Scores
estat ic

//Visualizing regression model time^8 
predict yhat8
twoway (tsline employment) (tsline yhat8,lpattern(dash))

//Drop for next regression calculations
drop yhat yhat_sq yhat_cub yhat_quad yhat8

//Basic models with ln of employment 
gen ln_emp = ln(employment) 

//Linear model 
regress ln_emp time
predict yhat
//Check AIC and BIC Scores
estat ic

//Quadratic model 
regress ln_emp time time_sq
predict yhat_sq
//Check AIC and BIC Scores
estat ic

//Cubic model 
regress ln_emp time time_sq time_cub
predict yhat_cub
twoway (tsline ln_emp) (tsline yhat_cub, lpattern(dash))
//Check AIC and BIC Scores
estat ic

//Quartic trend
regress ln_emp time time_sq time_cub time_quad
predict yhat_quad 
gen yhat_quad_exp = exp(yhat_quad)
twoway (tsline employment) (tsline yhat_quad_exp, lpattern(dash))
//Check AIC and BIC Scores
estat ic

//Time^5
regress ln_emp time time_sq time_cub time_quad time_quint
//Check AIC and BIC Scores
estat ic

//Time^6
regress ln_emp time time_sq time_cub time_quad time_quint time_6
//Check AIC and BIC Scores
estat ic

//Time^7
regress ln_emp time time_sq time_cub time_quad time_quint time_6 time_7
//Check AIC and BIC Scores
estat ic


//Time^8
regress ln_emp time time_sq time_cub time_quad time_quint time_6 time_7 time_8
//Check AIC and BIC Scores
estat ic

//Visualizing ln regression model time^8 
predict yhat8
gen yhat_8_exp = exp(yhat8)
twoway (tsline employment) (tsline yhat_8_exp,lpattern(dash))

//Deciding whether to use growth rates (not convincing enough)
gen diff_emp = D.ln_emp
regress diff_emp time 
drop yhat
predict yhat
estat ic 
twoway (tsline diff_emp) (tsline yhat)
//quadratic 
regress diff_emp time time_sq
estat ic 

//Looking at breaks in the trend in Level form 
//Recession in 2001
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 if tin(2001m1,2004m12)
predict yhatm01_04 if time <=m(2004m12)
label variable yhatm01_04 "Fitted Trend 2001-2004"
twoway (tsline employment) (tsline yhatm01_04,lpattern(dash)) if time>=m(2001m1)
//Run regression on break 
generate D=(time>=m(2001m1))
generate timexD=time*D
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD
//Check AIC and BIC 
estat ic 
//Visualize 
drop yhatm01_04
predict yhatm01_04
label variable yhatm01_04 "Fitted Trend 2001-2004"
twoway (tsline employment) (tsline yhatm01_04,lpattern(dash))

//Recession in 2008 
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 if tin(2008m1,2010m12)
predict yhatm08_10 if time <=m(2010m12)
twoway (tsline employment) (tsline yhatm08_10,lpattern(dash)) if time>=m(2008m1)
//Run regression on breaks 
gen D2 =(time>=m(2008m1))
generate timexD2=time*D2
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD D2 timexD2 
//Visualize
drop yhatm08_10
predict yhatm08_10
//Check AIC and BIC 
estat ic
twoway (tsline employment) (tsline yhatm08_10,lpattern(dash))

//COVID 
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 if tin(2020m4,2021m12)
predict yhatm20_21 if time<=m(2021m12)
twoway (tsline employment) (tsline yhatm20_21,lpattern(dash)) if time>=m(2020m4)
//Run regression on breaks 
gen D3 =(time>=m(2020m4))
generate timexD3=time*D3
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD D2 timexD2 D3 time*D3
//Visualize
drop yhatm20_21
predict yhatm20_21
//Check AIC and BIC 
estat ic
twoway (tsline employment) (tsline yhatm20_21,lpattern(dash))


//Lags plus Autocorrelations and Partial autocorrelations 
//Lagged regression
regress employment L.employment
estat ic
drop yhat
predict yhat
twoway (tsline employment) (tsline yhat, lpattern(dash))
list time employment L.employment yhat if time>=m(2021m1)

//AC of employment data 
ac employment

//Extending lags to look for decay
ac employment, lags(100)

//Partial autocorrelations
pac employment, yw

//Extending lags to view pattern
pac employment, yw lags(100)

//Checking autocorrelations of residuals
reg employment L(1/1).employment
estimates stats
predict yhat1
predict ehat1, res
ac ehat1
pac ehat1, yw
wntestq ehat1
//AR(4) model 
reg employment L(1/4).employment
estimates stats
predict yhat2
predict ehat2, res
ac ehat2
pac ehat2, yw
wntestq ehat2

//AR(6) model 
reg employment L(1/6).employment
estat ic
predict yhat3
predict ehat3, res
ac ehat3
pac ehat3, yw
wntestq ehat3
//Plotting residuals
tsline ehat3

//Model selection  attempts
//AIC and BIC scores worsened not good model 
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 L.employment
estat ic 
predict yhat_l
predict ehatl, res
wntestq ehatl

//Model with breaks and time trends
//Model with break in 2001m1
//Model with best AIC BIC so far 
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(1/6).employment
estat ic
predict yhat_D
predict ehatD, res
wntestq ehatD

//Model with break in 2001m1 and 2008m1
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD D2 time*D2 L(1/6).employment
estat ic
predict yhat_D2
predict ehatD2, res
wntestq ehatD2

//Model with break in 2001m1 and 2020m4
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD D3 time*D3 L(1/6).employment
estat ic
predict yhat_D3
predict ehatD3, res
wntestq ehatD3

//Model with all breaks
regress employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD D2 time*D2 D3 time*D3 L(1/6).employment
estat ic
predict yhat_D4
predict ehatD4, res
wntestq ehatD4

//For best model so far!
//Joint significance at 10% level
test time_sq time_quad time_8 D 
//Testing joint significance for lag 3 and lag 4
//Jointly significant with other lags in the model
test L1.employment L2.employment L3.employment L4.employment L5.employment L6.employment
//dfuller test -- significance at 10% no unit root problem
dfuller employment, lags(6) trend regress

//seeing how well the model fits
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(1/6).employment
predict compare_yhat
twoway (tsline employment) (tsline compare_yhat, lpattern(dash))


drop yhat1 yhat2 yhat3 yhat8
drop time_sq time_cub time_quad time_quint time_6 time_7 time_8 D time*D
//Forecasting using Direct Forecast
tsappend, add(12)
gen time_sq = time^2
gen time_cub = time^3
gen time_quad = time^4
gen time_quint = time^5
gen time_6 = time^6
gen time_7 = time^7
gen time_8 = time^8
generate D=(time>=m(2001m1))
generate timexD=time*D
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(1/6).employment
predict yhat1
predict seoff1, stdf
generate yhat1L=yhat1-1.96*seoff1
generate yhat1U=yhat1+1.96*seoff1
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(2/7).employment
predict yhat2
predict seoff2, stdf
generate yhat2L=yhat2-1.96*seoff2
generate yhat2U=yhat2+1.96*seoff2
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD  L(3/8).employment
predict yhat3
predict seoff3, stdf
generate yhat3L=yhat3-1.96*seoff3
generate yhat3U=yhat3+1.96*seoff3
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(4/9).employment
predict yhat4
predict seoff4, stdf
generate yhat4L=yhat4-1.96*seoff4
generate yhat4U=yhat4+1.96*seoff4
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(5/10).employment
predict yhat5
predict seoff5, stdf
generate yhat5L=yhat5-1.96*seoff5
generate yhat5U=yhat5+1.96*seoff5
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(6/11).employment
predict yhat6
predict seoff6, stdf
generate yhat6L=yhat6-1.96*seoff6
generate yhat6U=yhat6+1.96*seoff6
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD  L(7/12).employment
predict yhat7
predict seoff7, stdf
generate yhat7L=yhat7-1.96*seoff7
generate yhat7U=yhat7+1.96*seoff7
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(8/13).employment
predict yhat8
predict seoff8, stdf
generate yhat8L=yhat8-1.96*seoff8
generate yhat8U=yhat8+1.96*seoff8
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(9/14).employment
predict yhat9
predict seoff9, stdf
generate yhat9L=yhat9-1.96*seoff9
generate yhat9U=yhat9+1.96*seoff9
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(10/15).employment
predict yhat10
predict seoff10, stdf
generate yhat10L=yhat10-1.96*seoff10
generate yhat10U=yhat10+1.96*seoff10
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(11/16).employment
predict yhat11
predict seoff11, stdf
generate yhat11L=yhat11-1.96*seoff11
generate yhat11U=yhat11+1.96*seoff11
reg employment time time_sq time_cub time_quad time_quint time_6 time_7 time_8 D timexD L(12/17).employment
predict yhat12
predict seoff12, stdf
generate yhat12L=yhat12-1.96*seoff12
generate yhat12U=yhat12+1.96*seoff12
egen forecast=rowfirst(yhat1 yhat2 yhat3 yhat4 yhat5 yhat6 yhat7 yhat8 yhat9 yhat10 yhat11 yhat12) if time>m(2022m10)
egen forecastL=rowfirst(yhat1L yhat2L yhat3L yhat4L yhat5L yhat6L yhat7L yhat8L yhat9L yhat10L yhat11L yhat12L) if time>m(2022m10)
egen forecastU=rowfirst(yhat1U yhat2U yhat3U yhat4U yhat5U yhat6U yhat7U yhat8U yhat9U yhat10U yhat11U yhat12U) if time>m(2022m10)
label variable forecast "Direct Point Forecast"
label variable forecastL "Direct Forecast Lower Limit"
label variable forecastU "Direct Forecast Upper Limit"
tsline employment forecast forecastL forecastU if time>m(2000m12), title(Employment in Manufacturing) lpattern(solid dash shortdash shortdash)
//Close up
tsline employment forecast forecastL forecastU if time>m(2019m1), title(Employment in Manufacturing) lpattern(solid dash shortdash shortdash)

//List of forecast values 
list forecast forecastL forecastU in -12/l, noobs

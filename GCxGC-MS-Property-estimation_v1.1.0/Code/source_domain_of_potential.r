###ABP
ABP <-
"{5 < logKo_w.contour &
logKo_w.contour < 7 &
-4 < logKa_w.contour &
logKa_w.contour < -1}"


###LRTP
LRTP <- 
"{(-5 < logKa_w.contour &
logKa_w.contour < -1) | 
(-1 < logKa_w.contour & 
5 < logKo_a.contour)}"


###ACP
ACP <- 
"{(-0.5 < logKa_w.contour &
logKa_w.contour < 4) & logKo_a.contour < 9 |
(-3 < logKa_w.contour &
6.5 < logKo_a.contour &
logKo_a.contour < 10)}"


###TBP
TBP <- 
"{2 < logKo_w.contour &
logKo_w.contour < 5 &
6 <= logKo_a.contour}"


###ATBP
ATBP <- 
"{2 < logKo_w.contour &
6 <= logKo_a.contour}"



###BAP
BAP <- 
"{(2 < logKo_w.contour &
logKo_w.contour < 11) & 
(6 < logKo_a.contour & 
logKo_a.contour < 12)}"



###For multiple condition
domain.element <- strsplit(domain.name, "&")
d.length <- length(domain.element[[1]])
if ( d.length > 1 ){
	condi.text.bind <- 0
	for (ii in 1:d.length){
		condi.text <- eval(parse(text=(domain.element[[1]][ii])))
		condi.text.bind <- cbind(condi.text.bind,condi.text)
	}
	condi.text.bind <- condi.text.bind[-c(1)]
	remain.condition <- paste(condi.text.bind[1:d.length],collapse="&")
} else {
	remain.condition <- eval(parse(text=domain.name))
}
topic.name <- domain.name

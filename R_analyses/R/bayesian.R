#Bayesian Analysis
mainDir <- getwd()
subDir <- "figs/baye"
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
}
postinf<-MCMCregress(CPC~Position+Search.Volume+Traffic....+Competition+Number.of.Results,data = data,b0 = 0,B0 = 0.1,sigma.mu = 5,sigma.var = 25)
s<-ggs(postinf)
ggmcmc(s,file="figs/baye/model_purina-diag.pdf")
out<-capture.output(raftery.diag(postinf))
cat("rRaftery and Lewis's diagnostic test", out, file="output/bayesian_purina.txt", sep="\n", append=F)
out<-capture.output(summary(postinf))
cat("Bayesian analysis on purina data", out, file="output/bayesian_purina_summary.txt", sep="\n", append=F)

postinf<-MCMCregress(CPC~Position+Search.Volume+Traffic....+Competition+Number.of.Results,data = data1,b0 = 0,B0 = 0.1,sigma.mu = 5,sigma.var = 25)
s<-ggs(postinf)
ggmcmc(s,file="figs/baye/model_whiskas-diag.pdf")
out<-capture.output(raftery.diag(postinf))
cat("rRaftery and Lewis's diagnostic test", out, file="output/bayesian_whiskas.txt", sep="\n", append=TRUE)
out<-capture.output(summary(postinf))
cat("Bayesian analysis on purina data", out, file="output/bayesian_whiskas_summary.txt", sep="\n", append=TRUE)


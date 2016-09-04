

## regression analysis
key_parameters<-function(arg1){
temp.lm <- lm(Position ~. ,data = arg1)#whiskas[ ,-c(1, 7, 12, 13, 14)])
temp.new.lm <- step(temp.lm, trace = FALSE)
important_parameters <- names(temp.new.lm$coefficients)
important_parameters
}



#print(summary(whiskas.new.lm))
#print("Unimportant_parameters in scope of position in whiskas data:")## i.e. position does not depends on them
#print(unimportant_parameters_whiskas)


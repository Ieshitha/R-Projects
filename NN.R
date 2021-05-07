#installation
install.packages("neuralnet")

#create training data
TKS =c(20,10,30,67,34,34,78)
CSS =c(70,10,80,28,50,55,60)
placed = c(1,0,0,0,1,1,1)
df = data.frame(TKS,CSS,placed)


#load library
df
require(neuralnet)

# fit neural network
nn=neuralnet(placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

plot(nn)
df




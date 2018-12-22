#neural network
library("neuralnet")
formula.bpn <- payback_rate ~ int_rate+home_ownership_RENT+purpose_medical +
        dti+delinq_2yrs+purpose_small_business+home_ownership_ANY+
        purpose_car+ purpose_car+inq_last_6mths+purpose_renewable_energy
net.sqrt <- neuralnet(formula = formula.bpn ,err.fct = "sse",act.fct = "logistic",
                      m_L_train, hidden=3, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)
net.results <- compute(net.sqrt, testdata)
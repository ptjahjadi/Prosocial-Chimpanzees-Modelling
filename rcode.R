data = read.table("assign2.txt", header = TRUE)

#Only 2 levels: 0 or 1. Use binomial regression.
null_model = glm(data$pulled_left ~ 1, family = binomial)
full_model = glm(data$pulled_left ~ data$actor + data$condition + data$prosoc_left, family = binomial)
model2 = step(model, scope = ~.)

# Separate the dataset by partner condition.
actor2 = c()
prosoc_left2 = c()
pulled_left2 = c()
condition2 = c()
actor3 = c()
prosoc_left3 = c()
pulled_left3 = c()
condition3 = c()
for (i in seq(1,504))
{
  if(data$condition[i] == 0) {
    actor2 = c(actor2, data$actor[i])
    prosoc_left2 = c(prosoc_left2, data$prosoc_left[i])
    pulled_left2 = c(pulled_left2, data$pulled_left[i])
    condition2 = c(condition2, data$condition[i])
  }
  else {
    actor3 = c(actor3, data$actor[i])
    prosoc_left3 = c(prosoc_left3, data$prosoc_left[i])
    pulled_left3 = c(pulled_left3, data$pulled_left[i])
    condition3 = c(condition3, data$condition[i])
  }
}

data_control = data.frame(actor2, condition2, prosoc_left2, pulled_left2)
data_partner = data.frame(actor3, condition3, prosoc_left3, pulled_left3)

#Is the chimpanzee being prosocial?
prosocial = c()
for (i in seq(1,504))
{
  if((data$prosoc_left[i] == data$pulled_left[i]) && (data$condition[i] == 1))
  {
    prosocial = c(prosocial, 1)
  }
  else
  {
    prosocial = c(prosocial, 0)
  }
}
actor = data$actor
condition = data$condition 
prosoc_left = data$prosoc_left
pulled_left = data$pulled_left
data = data.frame(actor, condition, prosoc_left, pulled_left, prosocial)

data_control = data.frame(actor2, condition2, prosoc_left2, pulled_left2, c(rep(0,252)))

# Alternate condition where the only considered dataset is where condition = 1.
data_partner = data.frame(actor3, condition3, prosoc_left3, pulled_left3, prosocial)


#Prosocial model
prosocial_model = glm(data_partner$prosocial ~ (data_partner$prosoc_left3 + data_partner$actor3 
                      + data_partner$pulled_left3), family = binomial(link = "probit"))
summary(prosocial_model)

prosocial_model2 = step(prosocial_model, scope = ~.)
summary(prosocial_model2)

# Test for overdispersion
phihat = sum(residuals(prosocial_model2, type="pearson")^2 / 250)

# Test for GOF
pchisq(deviance(prosocial_model2), 250, lower.tail = FALSE)

# Check residuals
plot(residuals(prosocial_model2) ~ predict(prosocial_model2, type="link"), xlab=expression(hat(eta)), ylab = "Deviance residuals")



# Contingency Table
con_table = table(data_partner$pulled_left3, data_partner$prosoc_left3)
con_table
frac_con_table = prop.table(con_table)
frac_con_table

# Check for independence
chisq.test(data_partner$pulled_left3, data_partner$prosoc_left3)

# Correlation test
library("ggpubr")
cor.test(data_partner$pulled_left3, data_partner$prosoc_left3,  method = "pearson")


# GLM for data
full_model = glm(prosocial ~ (data$prosoc_left + data$actor 
                                                + data$pulled_left), family = binomial(link = "probit"))
summary(full_model)

full_model2 = step(full_model, scope = ~.)
summary(full_model2)

# Test for overdispersion
phihat = sum(residuals(prosocial_model2, type="pearson")^2 / 250)

# Test for GOF
pchisq(deviance(prosocial_model2), 250, lower.tail = FALSE)

# Check residuals
plot(residuals(prosocial_model2) ~ predict(prosocial_model2, type="link"), xlab=expression(hat(eta)), ylab = "Deviance residuals")
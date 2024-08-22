#Leah Maciel HW 4- problem 2 code adapted from Canvas example

#Problem 1 method 2- testing different error rate and n values
#Prove 1 - e <= (1 - (e/n))^n
# e within (0,1), n is a positive integer
#proved using different test/edge cases

#test 1- small e and n values
e <- 0.05
n<- 1
1 - e <= (1 - (e/n))^n
#[1] TRUE

#test 2- small e and large n values
e <- 0.05
n<- 1000
1 - e <= (1 - (e/n))^n
#[1] TRUE

#test 3- large e and n values
e <- 0.99
n<- 1000
1 - e <= (1 - (e/n))^n
#[1] TRUE

#test 4- large e and small n values
e <- 0.99
n<- 1
1 - e <= (1 - (e/n))^n
#[1] TRUE

#test 5- medium e and n values
e <- 0.5
n<- 30 #not sure what a "medium" n value would be
1 - e <= (1 - (e/n))^n
#[1] TRUE

#Problem 2
?mtcars; #built in data
plot(mtcars$vs, cars$hp);
#2a
#Test if hp and cyl are associated at significance level 0.05

summary(lm(hp ~ cyl, data=mtcars));
# p-value: 3.478e-09 definitely less than 0.05 -> significant


#Find estimated (model-fitted) hp values for 4,6, and 8 cyl cars
hp_4 <- predict((lm(hp ~ cyl, data=mtcars)), data.frame(cyl=4))
#76.77876 
hp_6 <- predict((lm(hp ~ cyl, data=mtcars)), data.frame(cyl=6))
#140.6953 
hp_8 <- predict((lm(hp ~ cyl, data=mtcars)), data.frame(cyl=8))
#204.6119

#2b
#Are the engine shape (vs) and the gross horsepower (hp) associated? 

model <- glm(vs ~ hp, data=mtcars, family=binomial);
summary(model);
#p-value = 0.01234 -> less than 0.05 so it is significant


#If a car has 150hp, what is the estimated chance that the car’s engine is V-shaped?
predict(model, data.frame(hp=150), type = "response")
#0.1294217 

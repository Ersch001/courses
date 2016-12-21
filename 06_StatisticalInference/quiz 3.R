#question 1
qt(0.975,8)*(30/sqrt(9))+1100

#question 2
-2* c(-1,1) +qt(0.975,8)*2.6/sqrt(9)


# question 4
sp <- sqrt((9*0.6+9*0.68)/(10+10-2))
md=3-5
semd <- sp*sqrt(1/10+1/10)^(1/2)

md+c(-1,1)*qt(0.975,(10+10-2))*semd

qbinom(3,5,0.1, lower.tail = T)

pbinom(3,5, 0.1, lower.tail = T)
#----
p = (c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
dbinom(3, 5, p)
(1/11)*dbinom(3, 5, p)
(1/11)*dbinom(3, 5, p)/sum(1/11*dbinom(3, 5, p))



#----
A <- c(9,12,13,15,15,17,24)
var(A)
aad(A) #lsr package

B <- c(7,11,15,15,17,19,21)
var(B)
aad(B)

C <- c(11,11,15,15,15,18,20)
var(C)
aad(C)
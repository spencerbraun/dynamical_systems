#############
#IV: college, Card 1995
#############

library(AER)
library(haven)
library(tidyverse)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/",
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

card <- read_data("card.dta")

#Define variable
#(Y1 = Dependent Variable, Y2 = endogenous variable, X1 = exogenous variable, X2 = Instrument)

attach(card)

Y1 <- lwage
Y2 <- educ
X1 <- cbind(exper, black, south, married, smsa)
X2 <- nearc4

#OLS
ols_reg <- lm(Y1 ~ Y2 + X1)
summary(ols_reg)

#2SLS
iv_reg = ivreg(Y1 ~ Y2 + X1 | X1 + X2)
summary(iv_reg)



B <- t(matrix(
  c(c(2,0),
  c(0.5,1)),
  2,2
))
B

B %^% 4



A <- matrix(
  c(
    c(0,1,1,0,0),
    c(0,1,0,0,1),
    c(1,0,0,0,0),
    c(0,1,0,1,1),
    c(1,0,1,0,0)
    ),
  5,5
)


A

library(expm)



A %^% 9
sum(A %^% 9)

5^10

c(1,2) %*% t(c(3,1))


mat <- (t(matrix(c(
  c(7/2,5/2,3/2,1/2),
  c(1,1,1,1)),
  4,2) ))




library(matlib)
echelon(mat, c(1,0))


library(tidyverse)

test270 <- matrix(NA, 5, 5)
implode <- function(..., sep='') {
  paste(..., collapse=sep)
}

for (i in 1:5) {
  for (j in 1:5) {
    if ((i == 1) & (j ==1)) {
      test270[i, j] <- str_interp("C(${i-1})")
    } else if (j == i + 1) {
      test270[i, j] <- str_interp("D(${i-1})")
    } else if (j == i) {
      test270[i, j] <- str_interp("C(${i-1})B(${j-2})")
    } else if ((i > 1) & (j < i)) {
      A_vec <- sapply(seq(j-1,i-2), function(x) str_interp("A(${x})"))
      B_vec <- ifelse(j-2 >=0, str_interp("B(${j-2})"), "")
      total_vec <- c(
        str_interp("C(${i-1})"),
        A_vec,
        B_vec
      )

      test270[i, j] <- implode(total_vec)
    } else if (j> i + 1) {
      test270[i, j] <- 0
    }
  }
}

test270


norm_vec <- function(x) sqrt(sum(x^2))

a <- c(7,7)
b <- c(-10, -11)



norm_vec(a) + norm_vec(b)
norm_vec(b-a)

a <- c(1,10)
b <- c(10,2)

# a <- c(1,10)
# b <- c(10,1)

x <- c(a[1], b[1])
y <- c(a[2], b[2])


sqnorm <- function(x) (sum(x^2))
a_norm2 = sqnorm(a)
b_norm2 = sqnorm(b)
d = (b_norm2 - a_norm2) * 0.5
c = b-a
slope = (d-c[1])/(c[2])

par(pty="s")
plot(x,y, xlim = c(0,max(x, y) + 1), ylim = c(0,max(x, y) + 1))
arrows(x[2],y[2], x[1],y[1])
arrows(0,0,x[2],y[2])
arrows(0,0,x[1],y[1])
abline(a=0, b=slope)
text(a[1]+0.5, a[2]+0.5, "a")
text(b[1]+0.5, b[2]+0.5, "b")
text(4, 6, "c")
polygon(c(-1, -1, 100, 100),c(-1 * slope, 100, 100, slope*100), density = 20, angle = 0,col="gray", alpha=0.3)

asin(0.5/10)
asin(0.5)


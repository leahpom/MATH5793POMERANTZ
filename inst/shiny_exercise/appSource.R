# source file for shiny app - class exercise
# February 16, 2021

# x-tilde function - from Dr. Stewart's shiny dashboard example
myxtilde = function(X,theta){ # X is the data as a matrix, theta is the rotation angle, use slider input
  cth = cos(theta)
  sth = sin(theta)
  mv = c(cth,-sth,sth,cth) # vector of the values
  Rt = matrix(mv,nr=2,nc=2, byrow=FALSE) # rotation matrix
  Xt = Rt%*%t(X) # multiply the rotation matrix by the data
  t(Xt)
  #list(x1t = Xt[1,], x2t = Xt[2,])
}

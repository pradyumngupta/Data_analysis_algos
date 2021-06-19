JaccardInd<-function(a,b){
  a<-a!=0;
  b<-b!=0;
  andsum<-sum(a&b);
  orsum<-sum(a|b);
  1-(andsum/orsum);
}


euclDis<-function(a,b){
  sqrt(sum(abs(a-b))^2)
}

mandis<-function(a,b){
  sum(abs(a-b))
}

chewdis<-function(a,b){
  max(abs(a-b));
}

mindis<-function(a,b,dim){
  (sum(abs(a-b)^dim))^(1/dim);
}
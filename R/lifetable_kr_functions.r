x<-lifetable.kr$lx
indx<-1:length(x)
dx<-x-x[indx+1]
dx[length(dx)]<-x[length(x)]
qx<-dx/x
#mux
#e0x
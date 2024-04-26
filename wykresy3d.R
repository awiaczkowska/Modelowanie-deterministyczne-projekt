#### wykresy 3D

h=0.1
n = 50/h

# wykres całki pierwszej 

calka_pierwsza_LV<-function(V,P,a=0.7,b=0.5,c=0.4,d=0.7){
  (V^d * P^a )/ ( exp(c*V+b*P) )
}

V <- P <- seq(0, 7, length = 50)
C<-outer(V,P,calka_pierwsza_LV)
persp(V,P,C)

persp(V,P,C, theta = 40, phi = 40,
      col = "lightgreen", shade = 0.3, xlab = "V - ofiary",
      ylab = "P - drapieżniki", zlab = "U(V,P)",
      main = "Wykres całki pierwszej U dla V,P z przedziału [0,7]" ,
      contour=list(z="bottom",col="colors"))


## wykresy ruchome, 3 populacje
library(plotly)

RK4_3 <- function(v10,v20, p0, a1,a2, b1,b2, c1,c2, d){
  
  f = function(v1,v2, p){
    return(a1*v1 - b1*v1*p)  
  }
  
  g = function(v1,v2, p){
    return(a2*v2 - b2*v2*p)
  }
  
  H = function(v1,v2, p){
    return(c1*v1*p + c2*v2*p - d*p)
  }
  
  #Tworzymy wektory v i w:
  v1 = numeric(n)
  v2 = numeric(n)
  p = numeric(n)
  
  #Przypisujemy wartości początkowe:
  v1[1] = v10
  v2[1] = v20
  p[1] = p0
  
  for(i in 2:n){
    k1 = f(v1[i-1],v2[i-1], p[i-1])
    l1 = g(v1[i-1],v2[i-1], p[i-1])
    m1 = H(v1[i-1],v2[i-1], p[i-1])
    
    k2 = f( v1[i-1] + h/2*k1, v2[i-1] + h/2*l1, p[i-1] + h/2*m1 )
    l2 = g( v1[i-1] + h/2*k1, v2[i-1] + h/2*l1, p[i-1] + h/2*m1 )
    m2 = H( v1[i-1] + h/2*k1, v2[i-1] + h/2*l1, p[i-1] + h/2*m1 )
    
    
    k3 = f( v1[i-1] + h/2*k2, v2[i-1] + h/2*l2, p[i-1] + h/2*m2 )
    l3 = g( v1[i-1] + h/2*k2, v2[i-1] + h/2*l2, p[i-1] + h/2*m2 )
    m3 = H( v1[i-1] + h/2*k2, v2[i-1] + h/2*l2, p[i-1] + h/2*m2 )
    
    k4 = f( v1[i-1] + h*k3, v2[i-1] + h*l3, p[i-1] + h*m3 )
    l4 = g( v1[i-1] + h*k3, v2[i-1] + h*l3, p[i-1] + h*m3 )
    m4 = H( v1[i-1] + h*k3, v2[i-1] + h*l3, p[i-1] + h*m3 )
    
    v1[i] = v1[i-1] + h/6*( k1 + 2*k2 + 2*k3 + k4 )
    v2[i] = v2[i-1] + h/6*( l1 + 2*l2 + 2*l3 + l4 )
    p[i] =  p[i-1] + h/6*( m1 + 2*m2 + 2*m3 + m4 )
  }
  
  wynik = matrix(c(v1,v2, p), nrow=3, ncol=length(v1), byrow=T)
  rownames(wynik)<-c('V1','V2', 'P' )
  return(wynik)
}


plot_3pop<-function(v10,v20, p0, a1,a2, b1,b2, c1,c2, d, step=0.5, N=5){
  # N- liczba rysowanych trajektorii
  # step - krok dla każdej ze współrzędnych
  pkt<-RK4_3(v10,v20, p0, a1,a2, b1,b2, c1,c2, d)
  
  
  fig <- plot_ly( x = pkt[1,], y = pkt[2,], z = pkt[3,], type = 'scatter3d', mode = 'lines',
                  line = list(color = '#1f77b4', width = 1), name=paste0('V1(0)=V2(0)=P(0)=',v10))
  
  for(i in 1:(N-1)){
    #te warunki podlegają modyfikacji
    v10=v10*step
    v20=v20*step
    p0=p0*step
    
    pkt<-RK4_3(v10,v20, p0, a1,a2, b1,b2, c1,c2, d)
    x<-pkt[1,]
    y<-pkt[2,]
    z<-pkt[3,]
    
    fig <- fig %>% add_trace(  x = pkt[1,], y = pkt[2,], z = pkt[3,], type = 'scatter3d', mode = 'lines',
                               line = list(color = i, width = 1), name=paste0('V1(0)=V2(0)=P(0)=',v10))
    
  }
  x=c(0, d/c1,   0 )
  y=c(0, 0,   d/c2 )
  z=c(0,a1/b1,a2/b2)
  
  fig <- fig %>% add_trace( x=0,y=0,z=0,
                            type = 'scatter3d', mode = 'markers',
                            
                            line = list(color = 'red', size = 0.2 ,width = 0 ), name='(0,0,0)')
  
  #fig <- fig %>% add_trace( x=d/c1, y=0,z=a1/b1,type = 'scatter3d', mode = 'markers', line = list(color = 'red3', size = 0.2 ,width = 0 ), name="(d/c1, 0, a1/b1)")
  
  #fig <- fig %>% add_trace( x=0, y=d/c2 ,z=a2/b2,type = 'scatter3d', mode = 'markers',line = list(color = 'red2', size = 0.1 ,width = 0 ), name="(0, d/c2, a2/b2)")  
  
  axx <- list(title = "V_1")
  axy <- list(title = "V_2")
  axz <- list(title = "P")
  fig<- fig  %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  
  return(fig)  
}

a1=0.7; a2=0.5 ; b1=0.8 ; b2=0.5 ; c1= 0.3; c2=0.4; d=0.3
v10<-v20<-p0<-1

fig0<-plot_3pop(v10,v20, p0, a1,a2, b1,b2, c1,c2, d, step=0.5, N=5)


plot_3pop_v1<-function(v10,v20, p0, a1,a2, b1,b2, c1,c2, d, step=0.5, N=5){
  # N- liczba rysowanych trajektorii
  # step - krok dla każdej ze współrzędnych
  # badamy stabilność V1=0 
  
  pkt<-RK4_3(v10,v20, p0, a1,a2, b1,b2, c1,c2, d)
  
  fig <- plot_ly( x = pkt[1,], y = pkt[2,], z = pkt[3,], type = 'scatter3d', mode = 'lines',
                  line = list(color = '#1f77b4', width = 1), name=paste0('V1(0)=',v10))
  
  for(i in 1:(N-1)){
    #te warunki podlegają modyfikacji
    v10=v10*step
    
    pkt<-RK4_3(v10,v20, p0, a1,a2, b1,b2, c1,c2, d)
    x<-pkt[1,]
    y<-pkt[2,]
    z<-pkt[3,]
    
    fig <- fig %>% add_trace(  x = pkt[1,], y = pkt[2,], z = pkt[3,], type = 'scatter3d', mode = 'lines',
                               line = list(color = i, width = 1), name=paste0('V1(0)=',v10) )
    
  }
  x=c(0, d/c1,   0 )
  y=c(0, 0,   d/c2 )
  z=c(0,a1/b1,a2/b2)
  
  fig <- fig %>% add_trace( x=0, y=d/c2 ,z=a2/b2,
                            type = 'scatter3d', mode = 'markers',
                            line = list(color = 'red', size = 0.1 ,width = 0 ), name="(0, d/c2, a2/b2)")  
  
  axx <- list(title = "V_1")
  axy <- list(title = "V_2")
  axz <- list(title = "P")
  fig<- fig  %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  
  return(fig)  
}


a1=0.7; a2=0.5 ; b1=0.8 ; b2=0.5 ; c1= 0.3; c2=0.4; d=0.3
v10<-1
v20<-d/c2
p0<-a2/b2

fig1<-plot_3pop_v1(v10,v20, p0, a1,a2, b1,b2, c1,c2, d, step=0.5, N=5)


plot_3pop_v2<-function(v10,v20, p0, a1,a2, b1,b2, c1,c2, d, step=0.5, N=5){
  # N- liczba rysowanych trajektorii
  # step - krok dla każdej ze współrzędnych
  # badamy stabilność V1=0 
  
  pkt<-RK4_3(v10,v20, p0, a1,a2, b1,b2, c1,c2, d)
  
  fig <- plot_ly( x = pkt[1,], y = pkt[2,], z = pkt[3,], type = 'scatter3d', mode = 'lines',
                  line = list(color = '#1f77b4', width = 1), name=paste0('V2(0)=',v20))
  
  for(i in 1:(N-1)){
    #te warunki podlegają modyfikacji
    v20=v20*step
    
    pkt<-RK4_3(v10,v20, p0, a1,a2, b1,b2, c1,c2, d)
    x<-pkt[1,]
    y<-pkt[2,]
    z<-pkt[3,]
    
    fig <- fig %>% add_trace(  x = pkt[1,], y = pkt[2,], z = pkt[3,], type = 'scatter3d', mode = 'lines',
                               line = list(color = i, width = 1), name=paste0('V2(0)=',v20) )
    
  }
  fig <- fig %>% add_trace( x=d/c1, y=0 ,z=a1/b1,
                            type = 'scatter3d', mode = 'markers',
                            line = list(color = 'red', size = 0.1 ,width = 0 ), name="(d/c1, 0, a1/b1)")  
  
  axx <- list(title = "V_1")
  axy <- list(title = "V_2")
  axz <- list(title = "P")
  fig<- fig  %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  
  return(fig)  
}


a1=0.7; a2=0.5 ; b1=0.8 ; b2=0.5 ; c1= 0.3; c2=0.4; d=0.3
v10<-d/c1
v20<-1
p0<-a1/b1

fig2<-plot_3pop_v2(v10,v20, p0, a1,a2, b1,b2, c1,c2, d, step=0.5, N=10)


abcd<- tempdir()
withr::with_dir(abcd,htmlwidgets::saveWidget(fig2, "v2.html"))

withr::with_dir(abcd,htmlwidgets::saveWidget(fig1, "v1=0.html"))

withr::with_dir(abcd,htmlwidgets::saveWidget(fig0, "zero.html"))

-

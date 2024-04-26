import numpy as np
import matplotlib.pyplot as plt

#h = 0.1; #n = 500
h = 0.01
n = 5000

def euler(v0,p0,a,b,c,d):
 # n =50/h
    def eulerV (v,p):
        return v+ h*(a-b*p)*v

    def eulerP(v,p):
        return p+ h*(c*v-d)*p
    v=[v0]
    p=[p0]
    for i in range (1,n):
        v.append( eulerV(v[i-1],p[i-1]) )
        p.append( eulerP(v[i-1],p[i-1]) )
    return [v,p]

if __name__=='__main__':
    a = 0.7
    b = 0.5
    c = 0.4
    d = 0.7

d=euler(0.8,1.2,a,b,c,d)
plt.plot(d[0], d[1])
plt.xlabel('V - liczebność populacji ofiar')
plt.ylabel('P - liczebność populacji drapieżników')
plt.show()

t=[i*0.1 for i in range(0,n)]
plt.plot(t,d[0], color='green')
plt.plot(t,d[1], color='red')
plt.xlabel('t - czas ')

plt.ylabel('liczebność populacji')
plt.legend(['V', 'P'])
plt.show()
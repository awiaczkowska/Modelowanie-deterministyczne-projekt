
import matplotlib.pyplot as plt

h = 0.1
n = 500
def RK4(v0, w0, a, b, c, d):
    # n =50/h
    def f(v,w):
        return(a*v - b*v*w)

    def g(v,w):
        return(c*v*w - d*w)


    v=[v0];  w=[w0]


    for i in range(1,n) :
        k1 = f(v[i - 1], w[i - 1])
        l1 = g(v[i - 1], w[i - 1])
        k2 = f(v[i - 1] + h / 2 * k1, w[i - 1] + h / 2 * l1)
        l2 = g(v[i - 1] + h / 2 * k1, w[i - 1] + h / 2 * l1)
        k3 = f(v[i - 1] + h / 2 * k2, w[i - 1] + h / 2 * l2)
        l3 = g(v[i - 1] + h / 2 * k2, w[i - 1] + h / 2 * l2)
        k4 = f(v[i - 1] + h * k3, w[i - 1] + h * l3)
        l4 = g(v[i - 1] + h * k3, w[i - 1] + h * l3)

        v.append ( v[i - 1] + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4))
        w.append(w[i - 1] + h / 6 * (l1 + 2 * l2 + 2 * l3 + l4))
    return [v,w]


if __name__=='__main__':
    a = 0.7
    b = 0.5
    c = 0.4
    d = 0.7
    r=RK4(0.8,1.2,a,b,c,d)
    plt.plot(r[0], r[1])
    plt.xlabel('V - liczebność populacji ofiar')
    plt.ylabel('P - liczebność populacji drapieżników')
    plt.show()

    t=[i*0.1 for i in range(0,n)]
    plt.plot(t,r[0], color='green')
    plt.plot(t,r[1], color='red')
    plt.xlabel('t - czas ')

    plt.ylabel('liczebność populacji')
    plt.legend(['V', 'P'])
    plt.show()

'''
Writing a function to illustrate the central limit theorem graphically
'''

#Import random to create a random sample#
from random import randint
#Import statistics to use the mean() function#
from statistics import mean
#Import matplotlib to create histograms#
import matplotlib.pyplot as plt

def clt(n, a, b, reps):
    '''
    :param n: size of the sample
    :param a: lower bound for the range of values required
    :param b: upper bound for the range of values required
    :param reps: number of samples chosen
    :return: list of means of the samples chosen and plotted histogram of means
    '''
    means = []
    data = []
    for i in range(reps):
        for i in range(n):
            data.append(randint(a,b))
        m = mean(data)
        means.append(m)
    print(means)
    plt.hist(means, edgecolor = "black")
    plt.xlabel('Data')
    plt.ylabel('Frequency')
    plt.title('Histogram of Generated Random Data')
    plt.show()

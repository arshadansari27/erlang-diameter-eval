import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import pandas as pd
import itertools
import numpy as np
#data
df = pd.DataFrame.from_csv('test.csv', sep='\t', header=0) # names=['Id', 'Client', 'Count', 'Tot_Avg', 'Ok', 'Error', 'Insta_Avg'])

font_size = 14

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(nrows=2, ncols=2)
ax1.plot(df.index, df['Count'], marker='.', color='m')
start, end = ax1.get_xlim()
ax1.xaxis.set_ticks(np.arange(start, end, 1000000))
ax1.xaxis.set_major_formatter(ticker.FormatStrFormatter('%0.1f'))
ax1.set_title('Number of Request', fontsize=font_size)

ax2.plot(df.index, df['Insta_Avg'], marker='.', color='r')
start, end = ax2.get_xlim()
ax2.xaxis.set_ticks(np.arange(start, end, 1000000))
ax2.xaxis.set_major_formatter(ticker.FormatStrFormatter('%0.1f'))
ax2.set_title('Instanteneous Average Requests per Second', fontsize=font_size)

ax3.plot(df.index, df['Tot_Avg'], marker='.', color='b')
start, end = ax3.get_xlim()
ax3.xaxis.set_ticks(np.arange(start, end, 1000000))
ax3.xaxis.set_major_formatter(ticker.FormatStrFormatter('%0.1f'))
ax3.set_title('Cummulative Average Requests per Second', fontsize=font_size)

ax4.plot(df.index, df['Client'], marker='.', color='g')
start, end = ax4.get_xlim()
ax4.xaxis.set_ticks(np.arange(start, end, 500))
ax4.xaxis.set_major_formatter(ticker.FormatStrFormatter('%0.1f'))
ax4.set_title('Number of Clients', fontsize=font_size)
plt.tight_layout()
#plt.show()
fig.savefig("example.pdf")

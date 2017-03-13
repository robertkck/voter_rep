import pandas as pd
import plotly.plotly as py
import numpy as np
import matplotlib.pyplot as plt
import powerindex as px
import seaborn as sns

path = 'C:/Onedrive/Arbeit/Bruegel/Brexit/Voter Representation/'

def gini(population, representatives):
    df = pd.DataFrame([population, representatives])
    df=df.T
    pop, rep = list(df.columns.values)
    df['pop_rep']=df[rep]*10000/df[pop]
    df = df.sort_values('pop_rep')
    df['height']= 0
    df['area'] = 0
    df['pop_cum']=0
    df['height'] = df[rep].cumsum()
    df['pop_cum'] = df[pop].cumsum()
    df['area'] = (df['height'] - df[rep] / 2.)*df[pop]
    df['area_cum']=df['area'].cumsum()
    fair_area = df[rep].sum() * df[pop].sum() / 2.
    return (fair_area - df['area'].sum()) / fair_area


# Scenarios
# 1) No reduc - no redist
# 5) No reduc - proportional
# 7) No reduc - equally (remainder 21)
# 4) No reduc - Duff
# 2) No reduc - reduce MAL
# 9) No reduc - reduce MAL no 96
# 3) Cambridge 751
# 6) Camebridge 736
# 8) Benchmark


#########################################################################################################
### EU Parliament
# Load data
demo = pd.read_csv(path + 'data/Eurostat_demo.csv', index_col='GEO', usecols=[0,1,3], thousands=",", na_values=":")
demo.columns=['year', 'pop']

demo = demo.loc[demo['year']==2016,:]
reps = pd.DataFrame.from_csv(path+ 'data/EUR_EUParl_reps.csv', index_col='country')
eu = demo.join(reps)

# Calculate voters per MEP and Advantage Ratio
eu['rep_share']=eu['rep']/eu['rep'].sum()
eu['pop_share']=eu['pop']/eu['pop'].sum()
eu['advantage']=eu['rep_share']/eu['pop_share']
eu['pop_rep']=eu['pop']/eu['rep']
eu['share_diff']=abs(eu['rep_share']-eu['pop_share'])
mal_eu=0.5*eu['share_diff'].sum()
eu=eu.sort_values('pop_rep')
eu['rep_cum']=eu['rep'].cumsum().shift()
eu['rep_cum'].iloc[0]=0

# Calulate proportional distribution
eu['rep_equal']= (eu['rep'].sum()*eu['pop_share']).round(0)
eu['rep_share_equal']=eu['rep_equal']/eu['rep_equal'].sum()
eu['share_diff_equal']=abs(eu['rep_share_equal']-eu['pop_share'])
mal_eu_equal=0.5*eu['share_diff_equal'].sum()

# Minimise malapportionment while sticking to rulez
total_meps = np.arange(751-28*6)
eu['rep_minmal'] = 6
temp = eu.loc[:, ['rep_minmal', 'pop_share', 'pop']]
for i in total_meps-1:
    temp['rep_share']=temp['rep_minmal']/temp['rep_minmal'].sum()
    temp['share_diff']=temp['rep_share']-temp['pop_share']
    temp.loc[temp.share_diff==temp.loc[temp.rep_minmal!=96].share_diff.min(),'rep_minmal']+=1

eu.rep_minmal=temp.rep_minmal
eu['rep_minmal_share']=eu['rep_minmal']/eu['rep_minmal'].sum()
eu['advantage_minmal']=eu['rep_minmal_share']/eu['pop_share']
eu['pop_rep_minmal']=eu['pop']/eu['rep_minmal']
eu['share_minmal_diff']=abs(eu['rep_minmal_share']-eu['pop_share'])
mal_eu_minmal=0.5*eu['share_minmal_diff'].sum()
eu['rep_minmal_cum']=eu['rep_minmal'].cumsum().shift()
eu['rep_minmal_cum'].iloc[0]=0
eu['pop_rep_minmal']=eu['pop']/eu['rep_minmal']

# Calculate power
q = np.ceil(eu['rep'].astype(float).sum()/2).astype(int)
game = px.Game(q, eu['rep'])
game.calc()
eu['banzhaf']=game.banzhaf
eu['shapley']=game.shapley_shubik

#Calculate Gini
eu['gini']=gini(eu['pop'], eu['rep'])

# Calculate cambridge method
d=1000000
inc = 500
eu['96']=96
eu['rep_cam']=5+eu['pop']/d
while (eu['rep_cam'].sum()<751):
    eu['rep_cam']=5+eu['pop']/d
    eu['rep_cam']= pd.DataFrame([eu['rep_cam'], eu['96']]).min()
    eu['rep_cam']= np.ceil(eu['rep_cam'])
    d = d - inc
    
eu['rep_share_cam']=eu['rep_cam']/eu['rep_cam'].sum()
eu['share_diff_cam']=abs(eu['rep_share_cam']-eu['pop_share'])
mal_eu_cam=0.5*eu['share_diff_cam'].sum()

### Plot values
# Plot population per representative
n = np.arange(len(eu))
plt.bar(eu['rep_cum'],eu['pop_rep'], width=eu['rep']) # add horizontal line for equality
plt.bar(eu['rep_minmal_cum'],eu['pop_rep_minmal'], width=eu['rep_minmal']) # add horizontal line for equality
plt.bar(n,eu['advantage'], width=0.5)
plt.bar(n, eu['banzhaf'], width=0.5)
plt.bar(n, eu.rep, width=0.5)

plt.pie(eu['banzhaf'])

plt.scatter(eu['banzhaf'], eu['rep'])
plt.scatter( eu['pop'], eu['rep'])
plt.scatter( eu['pop'], eu['rep_minmal'])
plt.scatter(eu['pop'], eu['banzhaf'])
plt.scatter(np.log(eu['pop']), eu['advantage'])
plt.scatter(eu['pop_rep'], eu['rep']) # add horizontal line for equality
plt.scatter(eu['pop_rep_minmal'], eu['rep_minmal']) # add horizontal line for equality

sns.barplot(x=eu.index, y=eu['rep'])
eu_long = pd.melt(eu.reset_index(), id_vars=['GEO'], value_vars=['advantage','banzhaf'], var_name='variable', value_name='val')
fig = plt.figure()
ax1 = fig.add_subplot(111)
ax2 = ax1.twinx()
bar_width=0.35
b_eu= eu.banzhaf.plot(kind='bar', color='b', width=bar_width, position=0, ax=ax1)
b_eu1= eu['pop'].plot(kind='bar', color='g', width=bar_width, position=1, ax=ax2)
plt.xlabel('Countries')
plt.ylabel('Power Index')
plt.legend()
plt.tight_layout()
plt.show()

# map

### Scenarios
eu_brexit=eu.loc[eu.index!="United Kingdom", ['pop', 'rep', 'rep_share', 'pop_rep', 'banzhaf' ]]
eu_brexit['pop_share']=eu_brexit['pop']/eu_brexit['pop'].sum()

# 1) Reduction
eu_brexit['rep1'] = eu_brexit['rep']
eu_brexit['rep_share1']=eu_brexit['rep1']/eu_brexit['rep1'].sum()
eu_brexit['advantage1']=eu_brexit['rep_share1']/eu_brexit['pop_share']
eu_brexit['pop_rep1']=eu_brexit['pop']/eu_brexit['rep1']
eu_brexit['share1_diff']=abs(eu_brexit['rep_share1']-eu_brexit['pop_share'])
mal_eu_brexit1=0.5*eu_brexit['share1_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep1')
eu_brexit['rep1_cum']=eu_brexit['rep1'].cumsum().shift()
eu_brexit['rep1_cum'].iloc[0]=0

# 2) Redistribute 73 to reduce mal
eu_brexit['rep2'] = eu_brexit['rep']
# Minimise malapportionment
uk_meps = np.arange(73)
temp = eu_brexit.loc[:, ['rep2', 'pop_share']]

for i in uk_meps-1:
    temp['rep_share']=temp['rep2']/temp['rep2'].sum()
    temp['share_diff']=temp['rep_share']-temp['pop_share']
    temp.loc[temp.share_diff==temp.loc[temp.rep2!=96].share_diff.min(),'rep2']+=1

eu_brexit.rep2=temp.rep2
eu_brexit['rep_share2']=eu_brexit['rep2']/eu_brexit['rep2'].sum()
eu_brexit['advantage2']=eu_brexit['rep_share2']/eu_brexit['pop_share']
eu_brexit['pop_rep2']=eu_brexit['pop']/eu_brexit['rep2']
eu_brexit['share2_diff']=abs(eu_brexit['rep_share2']-eu_brexit['pop_share'])
mal_eu_brexit2=0.5*eu_brexit['share2_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep2')
eu_brexit['rep2_cum']=eu_brexit['rep2'].cumsum().shift()
eu_brexit['rep2_cum'].iloc[0]=0

# 3) Camebridge Method with 751 
eu_brexit['96']=96
d=1000000
inc = 500
eu_brexit['rep3']=5+eu_brexit['pop']/d
while (eu_brexit['rep3'].sum()<751):
    eu_brexit['rep3']=5+eu_brexit['pop']/d
    eu_brexit['rep3']= pd.DataFrame([eu_brexit['rep3'], eu['96']]).min()
    eu_brexit['rep3']= np.ceil(eu_brexit['rep3'])
    d = d - inc
    
eu_brexit['rep_share3']=eu_brexit['rep3']/eu_brexit['rep3'].sum()
eu_brexit['advantage3']=eu_brexit['rep_share3']/eu_brexit['pop_share']
eu_brexit['pop_rep3']=eu_brexit['pop']/eu_brexit['rep3']
eu_brexit['share3_diff']=abs(eu_brexit['rep_share3']-eu_brexit['pop_share'])
mal_eu_brexit3=0.5*eu_brexit['share3_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep3')
eu_brexit['rep3_cum']=eu_brexit['rep3'].cumsum().shift()
eu_brexit['rep3_cum'].iloc[0]=0

# 4) Duff
eu_brexit['rep4']=eu_brexit['rep']+73*eu_brexit['pop_share']
eu_brexit['rep_share4']=eu_brexit['rep4']/eu_brexit['rep4'].sum()
eu_brexit['advantage4']=eu_brexit['rep_share4']/eu_brexit['pop_share']
eu_brexit['pop_rep4']=eu_brexit['pop']/eu_brexit['rep4']
eu_brexit['share4_diff']=abs(eu_brexit['rep_share4']-eu_brexit['pop_share'])
mal_eu_brexit4=0.5*eu_brexit['share4_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep4')
eu_brexit['rep4_cum']=eu_brexit['rep4'].cumsum().shift()
eu_brexit['rep4_cum'].iloc[0]=0

# 5) Keep propotions but truncate 
eu_brexit['rep5']=pd.DataFrame([eu_brexit['rep']+ 73*eu_brexit['rep_share1'],eu_brexit['96']]).min()
while eu_brexit['rep5'].round(0).sum()<751:
    diff = 751 - eu_brexit['rep5'].sum() 
    eu_brexit['rep5']=pd.DataFrame([eu_brexit['rep5']+ diff*eu_brexit['rep_share1'],eu_brexit['96']]).min()

eu_brexit['rep5']=eu_brexit['rep5'].round(0)
eu_brexit.loc['Malta','rep5']=6
eu_brexit['pop_rep5']=eu_brexit['pop']/eu_brexit['rep5']
eu_brexit['rep_share5']=eu_brexit['rep5']/eu_brexit['rep5'].sum()
eu_brexit['advantage5']=eu_brexit['rep_share5']/eu_brexit['pop_share']
eu_brexit['share5_diff']=abs(eu_brexit['rep_share5']-eu_brexit['pop_share'])
mal_eu_brexit5=0.5*eu_brexit['share5_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep5')
eu_brexit['rep5_cum']=eu_brexit['rep5'].cumsum().shift()
eu_brexit['rep5_cum'].iloc[0]=0

# 6) Cambridge optimal
total_mep=600
d=1000000
inc = 600
eu_brexit['rep6']=5+eu_brexit['pop']/d
## Minimization of malapportionment under CAM shows that 736 is optimal
#mal = [0]
#mals = np.arange(300)
#for i in mals:
#    d=1000000
#    inc = 500
#    eu_brexit['rep6']=5+eu_brexit['pop']/d
#    while eu_brexit['rep6'].sum()<total_mep:
#        eu_brexit['rep6']=5+eu_brexit['pop']/d
#        eu_brexit['rep6']= pd.DataFrame([eu_brexit['rep6'], eu['96']]).min()
#        eu_brexit['rep6']= np.ceil(eu_brexit['rep6'])
#        d = d - inc
#    eu_brexit['rep_share6']=eu_brexit['rep6']/eu_brexit['rep6'].sum()
#    eu_brexit['share6_diff']=abs(eu_brexit['rep_share6']-eu_brexit['pop_share'])
#    mal=mal + [0.5*eu_brexit['share6_diff'].sum()]               
#    total_mep+=1
#    print(i)
#
#plt.scatter(mals,mal[1:])
#mal.index(min(mal[1:]))
#pd.DataFrame(mal).to_csv(path + 'results/mal.csv')
#
### Minimization of gini under CAM shows that ??? is optimal
#total_mep=600
#d=1000000
#inc = 600
#eu_brexit['rep6']=5+eu_brexit['pop']/d
#ginic = [0]
#ginis = np.arange(300)
#for i in ginis:
#    d=1000000
#    inc = 500
#    eu_brexit['rep6']=5+eu_brexit['pop']/d
#    while eu_brexit['rep6'].sum()<total_mep:
#        eu_brexit['rep6']=5+eu_brexit['pop']/d
#        eu_brexit['rep6']= pd.DataFrame([eu_brexit['rep6'], eu['96']]).min()
#        eu_brexit['rep6']= np.ceil(eu_brexit['rep6'])
#        d = d - inc
#    eu_brexit['rep_share6']=eu_brexit['rep6']/eu_brexit['rep6'].sum()
#    eu_brexit['share6_diff']=abs(eu_brexit['rep_share6']-eu_brexit['pop_share'])
#    ginic= ginic + [gini(eu_brexit['pop'], eu_brexit['rep6'])]
#    total_mep+=1
#    print(i)
#
#plt.scatter(ginis,ginic[1:])
#ginic.index(min(ginic[1:]))
# pd.DataFrame(ginic).to_csv(path + 'results/gini.csv')

while eu_brexit['rep6'].sum()<736:
    eu_brexit['rep6']=5+eu_brexit['pop']/d
    eu_brexit['rep6']= pd.DataFrame([eu_brexit['rep6'], eu['96']]).min()
    eu_brexit['rep6']= np.ceil(eu_brexit['rep6'])
    d = d - inc
    
eu_brexit['rep_share6']=eu_brexit['rep6']/eu_brexit['rep6'].sum()
eu_brexit['advantage6']=eu_brexit['rep_share6']/eu_brexit['pop_share']
eu_brexit['pop_rep6']=eu_brexit['pop']/eu_brexit['rep6']
eu_brexit['share6_diff']=abs(eu_brexit['rep_share6']-eu_brexit['pop_share'])
mal_eu_brexit6=0.5*eu_brexit['share6_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep6')
eu_brexit['rep6_cum']=eu_brexit['rep6'].cumsum().shift()
eu_brexit['rep6_cum'].iloc[0]=0

# 7) Distribute spare seats equally
eu_brexit['rep7']=pd.DataFrame([eu_brexit['rep']+2,eu['96']]).min()
eu_brexit.iloc[-22:-1,-1]+=1
eu_brexit['pop_rep7']=eu_brexit['pop']/eu_brexit['rep7']
eu_brexit['rep_share7']=eu_brexit['rep7']/eu_brexit['rep7'].sum()
eu_brexit['advantage7']=eu_brexit['rep_share7']/eu_brexit['pop_share']
eu_brexit['share7_diff']=abs(eu_brexit['rep_share7']-eu_brexit['pop_share'])
mal_eu_brexit7=0.5*eu_brexit['share7_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep7')
eu_brexit['rep7_cum']=eu_brexit['rep7'].cumsum().shift()
eu_brexit['rep7_cum'].iloc[0]=0

# 8) Proportional  
eu_brexit['rep8']=751*eu_brexit['pop_share']
selector=eu_brexit['rep8']-eu_brexit['rep8'].round(0)
eu_brexit['rep8']=eu_brexit['rep8'].round(0)
eu_brexit['pop_rep8']=eu_brexit['pop']/eu_brexit['rep8']
eu_brexit['poorest'] = eu_brexit['pop_rep8'] - eu_brexit['pop_rep8'].mean()
eu_brexit.loc[eu_brexit['poorest']==eu_brexit['poorest'].max(),'rep8']+=1

eu_brexit['rep_share8']=eu_brexit['rep8']/eu_brexit['rep8'].sum()
eu_brexit['advantage8']=eu_brexit['rep_share8']/eu_brexit['pop_share']
eu_brexit['pop_rep8']=eu_brexit['pop']/eu_brexit['rep8']
eu_brexit['share8_diff']=abs(eu_brexit['rep_share8']-eu_brexit['pop_share'])
mal_eu_brexit8=0.5*eu_brexit['share8_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep8')
eu_brexit['rep8_cum']=eu_brexit['rep8'].cumsum().shift()
eu_brexit['rep8_cum'].iloc[0]=0

# 9) Maximize representativeness ignoring 96 max
eu_brexit['rep9'] = eu_brexit['rep']
uk_meps = np.arange(73)
temp = eu_brexit.loc[:, ['rep9', 'pop_share']]

for i in uk_meps-1:
    temp['rep_share']=temp['rep9']/temp['rep9'].sum()
    temp['share_diff']=temp['rep_share']-temp['pop_share']
    temp.loc[temp.share_diff==temp.share_diff.min(),'rep9']+=1

eu_brexit.rep9=temp.rep9
eu_brexit['rep_share9']=eu_brexit['rep9']/eu_brexit['rep9'].sum()
eu_brexit['advantage9']=eu_brexit['rep_share9']/eu_brexit['pop_share']
eu_brexit['pop_rep9']=eu_brexit['pop']/eu_brexit['rep9']
eu_brexit['share9_diff']=abs(eu_brexit['rep_share9']-eu_brexit['pop_share'])
mal_eu_brexit9=0.5*eu_brexit['share9_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep9')
eu_brexit['rep9_cum']=eu_brexit['rep9'].cumsum().shift()
eu_brexit['rep9_cum'].iloc[0]=0

# 10) Cambridge method for 3, none, 751
eu_brexit['200']=200
d=1000000
inc = 500
eu_brexit['rep10']=2+eu_brexit['pop']/d
while (eu_brexit['rep10'].sum()<751):
    eu_brexit['rep10']=2+eu_brexit['pop']/d
    eu_brexit['rep10']= pd.DataFrame([eu_brexit['rep10'], eu_brexit['200']]).min()
    eu_brexit['rep10']= np.ceil(eu_brexit['rep10'])
    d = d - inc
    
eu_brexit['rep_share10']=eu_brexit['rep10']/eu_brexit['rep10'].sum()
eu_brexit['advantage10']=eu_brexit['rep_share10']/eu_brexit['pop_share']
eu_brexit['pop_rep10']=eu_brexit['pop']/eu_brexit['rep10']
eu_brexit['share10_diff']=abs(eu_brexit['rep_share10']-eu_brexit['pop_share'])
mal_eu_brexit10=0.5*eu_brexit['share10_diff'].sum()
eu_brexit=eu_brexit.sort_values('pop_rep10')
eu_brexit['rep10_cum']=eu_brexit['rep10'].cumsum().shift()
eu_brexit['rep10_cum'].iloc[0]=0

# 11) Proportional with min 3, no max, total =
## Maximization of proportionality
#total_mep=600
#diff_avg = [0]
#diff_max = [0]
#resids = np.arange(300)
#total_pop = eu_brexit['pop'].sum()
#for i in resids:
#    eu_brexit['rep11']=2 + eu_brexit['pop'] * ((total_mep - 54) /total_pop )
#    eu_brexit['rep11'] = eu_brexit['rep11'].round(0)
#    eu_brexit['pop_rep1'] = eu_brexit['pop'] / eu_brexit['rep11']
#    diffs = abs(eu_brexit['pop_rep1'] - (total_pop / total_mep ))
#    diff_avg = diff_avg + [diffs.mean()]
#    diff_max = diff_max + [diffs.max()]
#    total_mep+=1
#    print(i)
#
#plt.scatter(resids,diff_avg[1:])
#plt.scatter(resids,diff_max[1:])
#diff_avg.index(min(diff_avg[1:238]))
#diff_max.index(min(diff_max[1:238]))

#total_mep = 823
#eu_brexit['rep11']=2 + eu_brexit['pop'] * ((total_mep - 54)/ eu_brexit['pop'].sum() )              
#eu_brexit['rep11'] = eu_brexit['rep11'].round(0)
#
#eu_brexit['rep_share11']=eu_brexit['rep11']/eu_brexit['rep11'].sum()
#eu_brexit['advantage11']=eu_brexit['rep_share11']/eu_brexit['pop_share']
#eu_brexit['pop_rep11']=eu_brexit['pop']/eu_brexit['rep11']
#eu_brexit['share11_diff']=abs(eu_brexit['rep_share11']-eu_brexit['pop_share'])
#mal_eu_brexit11=0.5*eu_brexit['share11_diff'].sum()
#eu_brexit=eu_brexit.sort_values('pop_rep11')
#eu_brexit['rep11_cum']=eu_brexit['rep11'].cumsum().shift()
#eu_brexit['rep11_cum'].iloc[0]=0

# Calculate power
scenarios=np.arange(10)
scenarios=scenarios+1
for scenario in scenarios:
    rep, banzhaf, shapley = "rep" + str(scenario), "banzhaf" + str(scenario), "shapley" + str(scenario) 
    q = np.ceil(eu_brexit[rep].astype(float).sum()/2).astype(int)
    game = px.Game(q, eu_brexit[rep].astype(int))
    game.calc()
    eu_brexit[banzhaf]=game.banzhaf
    eu_brexit[shapley]=game.shapley_shubik

# Calculate Gini
for scenario in scenarios:
    reps, gini_eu_brexit = "rep" + str(scenario), "gini" + str(scenario)
    eu_brexit[gini_eu_brexit]= gini(eu_brexit['pop'], eu_brexit[reps])

# Calculate differences to status quo
scenarios=np.arange(10)
scenarios=scenarios+1
vars = ["rep", "rep_share", "pop_rep", "banzhaf"]
for scenario in scenarios:
    for var in vars:
        eu_brexit.loc[:,"diffs_" + var + str(scenario)]=eu_brexit.loc[:,var + str(scenario)] - eu_brexit.loc[:,var] 

# Plot
plt.bar(eu['rep_cum'],eu['pop_rep'], width=eu['rep']) # add horizontal line for equality
plt.bar(eu_brexit['rep1_cum'],eu_brexit['pop_rep1'], width=eu_brexit['rep1']) # add horizontal line for equality
plt.bar(eu_brexit['rep2_cum'],eu_brexit['pop_rep2'], width=eu_brexit['rep2']) # add horizontal line for equality
plt.bar(eu_brexit['rep3_cum'],eu_brexit['pop_rep3'], width=eu_brexit['rep3']) # add horizontal line for equality
plt.bar(eu_brexit['rep4_cum'],eu_brexit['pop_rep4'], width=eu_brexit['rep4']) # add horizontal line for equality
plt.bar(eu_brexit['rep5_cum'],eu_brexit['pop_rep5'], width=eu_brexit['rep5']) # add horizontal line for equality
plt.bar(eu_brexit['rep6_cum'],eu_brexit['pop_rep6'], width=eu_brexit['rep6']) # add horizontal line for equality
plt.bar(eu_brexit['rep7_cum'],eu_brexit['pop_rep7'], width=eu_brexit['rep7']) # add horizontal line for equality
n = np.arange(len(eu_brexit))
plt.bar(n, eu_brexit['advantage1'], width=0.5)
plt.bar(n, eu_brexit['advantage6'], width=0.5)
plt.bar(n, eu_brexit['advantage7'], width=0.5)

plt.scatter(eu_brexit['pop_rep1'], eu_brexit['rep1'])
plt.scatter(eu_brexit['pop_rep2'], eu_brexit['rep2'])
plt.scatter(eu_brexit['pop_rep3'], eu_brexit['rep3'])
plt.scatter(eu_brexit['pop_rep4'], eu_brexit['rep4'])
plt.scatter(eu_brexit['pop_rep5'], eu_brexit['rep5'])
plt.scatter(eu_brexit['pop_rep6'], eu_brexit['rep6'])
plt.scatter(eu_brexit['pop_rep7'], eu_brexit['rep7'])


# Plot comparison
fig, ax = plt.subplots()
index = n
bar_width=0.35
opacity = 0.8
b_eu= plt.bar(index, eu_brexit['advantage'], bar_width, alpha=opacity, color='b', label='before')
b_eu1= plt.bar(index+bar_width, eu_brexit['advantage1'], bar_width, alpha=opacity, color='g', label='after')
plt.xlabel('Countries')
plt.ylabel('Advantage')
plt.legend()
plt.tight_layout()
plt.show()

fig, ax = plt.subplots()
bar_width=0.35
b_eu= eu_brexit.banzhaf.plot(kind='bar', color='b', width=bar_width, position=0)
b_eu1= eu_brexit.banzhaf1.plot(kind='bar', color='g', width=bar_width, position=1)
plt.xlabel('Countries')
plt.ylabel('Power Index')
plt.legend()
plt.tight_layout()
plt.show()

# Compare malapportionment
mal_eu*751
mal_eu_cam*751
mal_eu_brexit1*678
mal_eu_brexit2*751

mal_eu_brexit1
mal_eu_brexit2
mal_eu_brexit3
mal_eu_brexit4
mal_eu_brexit5
mal_eu_brexit6
mal_eu_brexit7
mal_eu_brexit8

# Add malapportionment to dataframe
for j in range(1, 11):
    eu_brexit["mal" + str(j)]=eval("mal_eu_brexit" +str(j)) 
eu['mal'] = mal_eu

# Export data
eu.to_csv(path + 'results/eu.csv')
eu_brexit.to_csv(path + 'results/eu_brexit.csv')
i=np.arange(9)
i=i+1
vars = ["rep", "rep_share", "pop_rep", "banzhaf", "gini"]
order = [var + str(j) for j in i for var in vars]
eu_brexit.loc[:,order].to_csv(path + 'results/eu_brexit_ordered.csv')

vars = ["diffs_rep", "diffs_rep_share", "diffs_pop_rep", "diffs_banzhaf"]
subset = [var + str(j) for j in i for var in vars]
eu_brexit.loc[:,subset].to_csv(path + 'results/eu_brexit_subset.csv')
         
#########################################################################################################
### US Congress House of Representatives
# Load data
us = pd.read_csv(path + 'data/USCensus10_114Congr_demo.csv', index_col='state', thousands=",")
us_corr = pd.read_csv(path + 'data/US_state_correspondence.csv', index_col=0, names={'state', 'code'})
us=us.join(us_corr)
# us=us.reset_index().set_index('GEO_ID')

# Calculate voters per rep and Advantage Ratio by Congressional District
us['rep_share']=us['rep']/us['rep'].sum()
us['pop_share']=us['pop']/us['pop'].sum()
us['advantage']=us['rep_share']/us['pop_share']
us['pop_rep']=us['pop']/us['rep']
us['share_diff']=abs(us['rep_share']-us['pop_share'])
mal_us=0.5*us['share_diff'].sum()
mal_us_nodc=0.5*us.loc[us['rep']>0,'share_diff'].sum()

us = us.loc[us['rep']>0,:]
us=us.sort_values('pop_rep')
us['rep_cum']=us['rep'].cumsum().shift()
us['rep_cum'].iloc[0]=0

# Calculate power
# Not interesting because all are 1

# Calculate gini
us_gini=us.reset_index()
us['gini']=gini(us_gini['pop'], us_gini['rep'])

### Plot values
# Plot population per representative
plt.bar(us['rep_cum'],us['pop_rep'], width=us['rep']) # add horizontal line for equality
n = np.arange(len(us))
plt.bar(n,us['advantage'], width=0.5)

### House of representatives by state
# Calculate voters per rep and Advantage Ratio by state
us_state=us.groupby('code')['rep','pop'].apply(sum)
us_state['rep_share']=us_state['rep']/us_state['rep'].sum()
us_state['pop_share']=us_state['pop']/us_state['pop'].sum()
us_state['advantage']=us_state['rep_share']/us_state['pop_share']
us_state['pop_rep']=us_state['pop']/us_state['rep']
us_state['share_diff']=abs(us_state['rep_share']-us_state['pop_share'])
mal_us_state=0.5*us_state['share_diff'].sum()
mal_us_state_nodc=0.5*us_state.loc[us_state['rep']>0,'share_diff'].sum()

us_state=us_state.sort_values('pop_rep')
us_state['rep_cum']=us_state['rep'].cumsum().shift()
us_state['rep_cum'].iloc[0]=0

# Calculate power
q = np.ceil(us_state['rep'].astype(float).sum()/2).astype(int)
game = px.Game(q, us_state['rep'])
game.calc()
us_state['banzhaf']=game.banzhaf
us_state['shapley']=game.shapley_shubik

### Plot values
# Plot population per representative
plt.bar(us_state['rep_cum'],us_state['pop_rep'], width=us_state['rep']) # add horizontal line for equality
n = np.arange(len(us_state))
plt.bar(n,us_state['advantage'], width=0.5)
plt.pie(us_state['banzhaf'])
plt.bar(n, us_state['banzhaf'], width=0.5)
plt.hist(us_state['banzhaf'])
plt.scatter(us_state['pop'], us_state['banzhaf'])


trace = dict(
    type = 'choropleth', 
    locations = us_state.index,
    locationmode="USA-states",
    z=us_state['pop_rep'])

layout = dict(
    title = "Population per Representative in the US Congress",
    geo = dict(
        scope='usa'))

data = [trace]
fig = dict(data=data, layout=layout)
url=py.plot(fig, filename="Malapportionment US")

# Export data
us.to_csv(path + 'results/us.csv')

#########################################################################################################
### Germany
## Bundesrat 
# Load data
de_rat = pd.read_csv(path + 'data/destatis_bundesrat.csv', index_col=0)
de_rat.rename(columns={'2015':'pop'}, inplace=True)

# Calculate voters per rep and Advantage Ratio
de_rat['rep_share']=de_rat['rep']/de_rat['rep'].sum()
de_rat['pop_share']=de_rat['pop']/de_rat['pop'].sum()
de_rat['advantage']=de_rat['rep_share']/de_rat['pop_share']
de_rat['pop_rep']=de_rat['pop']/de_rat['rep']
de_rat['share_diff']=abs(de_rat['rep_share']-de_rat['pop_share'])
mal_de_rat=0.5*de_rat['share_diff'].sum()
de_rat=de_rat.sort_values('pop_rep')
de_rat['rep_cum']=de_rat['rep'].cumsum().shift()
de_rat['rep_cum'].iloc[0]=0

# Calculate power
q = np.ceil(de_rat['rep'].astype(float).sum()/2).astype(int)
game = px.Game(q, de_rat['rep'])
game.calc()
de_rat['banzhaf']=game.banzhaf
de_rat['shapley']=game.shapley_shubik

# Calculate gini
de_rat['gini']=gini(de_rat['pop'], de_rat['rep'])

# Plot values
# Plot population per representative
plt.bar(de_rat['rep_cum'],de_rat['pop_rep'], width=de_rat['rep']) # add horizontal line for equality
n = np.arange(len(de_rat))
plt.bar(n,de_rat['advantage'], width=0.5)
plt.pie(de_rat['banzhaf'])
plt.bar(n, de_rat['banzhaf'], width=0.5)
plt.hist(de_rat['banzhaf'])
plt.scatter(de_rat['rep'], de_rat['banzhaf'])

## Bundestag 
# Load data
de_tag = pd.read_csv(path + 'data/destatis_bundestag.csv', index_col=0)
de_tag.rename(columns={'2015':'pop'}, inplace=True)

# Calculate voters per rep and Advantage Ratio
de_tag['rep_share']=de_tag['rep']/de_tag['rep'].sum()
de_tag['pop_share']=de_tag['pop']/de_tag['pop'].sum()
de_tag['advantage']=de_tag['rep_share']/de_tag['pop_share']
de_tag['pop_rep']=de_tag['pop']/de_tag['rep']
de_tag['share_diff']=abs(de_tag['rep_share']-de_tag['pop_share'])
mal_de_tag=0.5*de_tag['share_diff'].sum()
de_tag=de_tag.sort_values('pop_rep')
de_tag['rep_cum']=de_tag['rep'].cumsum().shift()
de_tag['rep_cum'].iloc[0]=0

# Calculate power
q = np.ceil(de_tag['rep'].astype(float).sum()/2).astype(int)
game = px.Game(q, de_tag['rep'])
game.calc()
de_tag['banzhaf']=game.banzhaf
de_tag['shapley']=game.shapley_shubik

# Calculate gini
de_tag['gini']=gini(de_tag['pop'], de_tag['rep'])

# Plot values
# Plot population per representative
plt.bar(de_tag['rep_cum'],de_tag['pop_rep'], width=de_tag['rep']) # add horizontal line for equality
n = np.arange(len(de_tag))
plt.bar(n,de_tag['advantage'], width=0.5)
plt.pie(de_tag['banzhaf'])
plt.bar(n, de_tag['banzhaf'], width=0.5)
plt.hist(de_tag['banzhaf'])
plt.scatter(de_tag['rep'], de_tag['banzhaf'])
plt.scatter(de_tag['pop'], de_tag['banzhaf'])

# Export data
de_tag.to_csv(path + 'results/de_tag.csv')
de_rat.to_csv(path + 'results/de_rat.csv')


#############################################################################################
# Compare Malapportionment internationally
mal_eu
mal_us_nodc
mal_de_rat
mal_de_tag

# Plot Lorenz curves
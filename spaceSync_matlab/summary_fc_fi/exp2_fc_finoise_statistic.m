%each colomun means:"meanFc"'  '"meanFi"'  '"meanFi_noise"'  '"sdFc"'  '"sdFi"'  '"sdFi_noise"
%each row mean "x", "y, "z"

s1_statistic_normal = importdata('glass_statics_normal.csv')
s1_statistic_speedup = importdata('glass_statics_speedup.csv')

s2_statistic_normal = importdata('top_statics_normal.csv')
s2_statistic_speedup = importdata('top_statics_speedup.csv')

s3_statistic_normal = importdata('leftHand_statics_normal.csv')
s3_statistic_speedup = importdata('leftHand_statics_speedup.csv')

s4_statistic_normal = importdata('rightHand_statics_normal.csv')
s4_statistic_speedup = importdata('rightHand_statics_speedup.csv')

s5_statistic_normal = importdata('leftPants_statics_normal.csv')
s5_statistic_speedup = importdata('leftPants_statics_speedup.csv')

s6_statistic_normal = importdata('rightPants_statics_normal.csv')
s6_statistic_speedup = importdata('rightPants_statics_speedup.csv')

b1 = s1_statistic_normal.data(2,1)

display(s1_statistic_normal.colheaders)
display(s1_statistic_normal.data)
display(s1_statistic_speedup.colheaders)
display(s1_statistic_speedup.data)

display(s2_statistic_normal.colheaders)
display(s2_statistic_normal.data)
display(s2_statistic_speedup.colheaders)
display(s2_statistic_speedup.data)

display(s3_statistic_normal.colheaders)
display(s3_statistic_normal.data)
display(s3_statistic_speedup.colheaders)
display(s3_statistic_speedup.data)

display(s4_statistic_normal.colheaders)
display(s4_statistic_normal.data)
display(s4_statistic_speedup.colheaders)
display(s4_statistic_speedup.data)

display(s5_statistic_normal.colheaders)
display(s5_statistic_normal.data)
display(s5_statistic_speedup.colheaders)
display(s5_statistic_speedup.data)

display(s6_statistic_normal.colheaders)
display(s6_statistic_normal.data)
display(s6_statistic_speedup.colheaders)
display(s6_statistic_speedup.data)

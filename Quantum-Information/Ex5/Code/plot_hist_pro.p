# launch on terminal:
# gnuplot -e "file_name='hermitian' plot_hist_pro.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
#set grid mxtics lw 0.5
#set grid mytics lw 0.5


# set title
set title "Distribution of ".file_name." matrices spacing"

# format axis ticks
set format x "%1.1f"
set format y "%1.1f"

# plot labels
set xlabel sprintf("Spacing s_i")
set ylabel 'Density'

# set colors palette inferno
set style line  1 lt 1 lc rgb '#000004' # black
set style line  2 lt 1 lc rgb '#1f0c48' # dark purple
set style line  3 lt 1 lc rgb '#550f6d' # dark purple
set style line  4 lt 1 lc rgb '#88226a' # purple
set style line  5 lt 1 lc rgb '#a83655' # red-magenta
set style line  6 lt 1 lc rgb '#e35933' # red
set style line  7 lt 1 lc rgb '#f9950a' # orange
set style line  8 lt 1 lc rgb '#f8c932' # yellow-orange
set style line  9 lt 1 lc rgb '#fcffa4' # light yellow


# set legend
set key top right reverse Left box
set key height +1
set key width +1

# fit
set fit quiet

f(x)  = a * (x**b) * exp( -c * (x**d) )
g1(x) = a1 * (x**b1) * exp( -c1 * (x**d1) )
g2(x) = a2 * (x**b2) * exp( -c2 * (x**d2) )
g3(x) = a3 * (x**b3) * exp( -c3 * (x**d3) )
g4(x) = a4 * (x**b4) * exp( -c4 * (x**d4) )
g5(x) = a5 * (x**b5) * exp( -c5 * (x**d5) )

set fit logfile 'hist_plot_pro/'.file_name.'-local.log'
fit f(x) "hist_data/".file_name."-global-1000.dat" u 2:4 via a,b,c,d
fit g1(x) "hist_data/".file_name."-local-10.dat" u 2:4 via a1,b1,c1,d1
fit g2(x) "hist_data/".file_name."-local-50.dat" u 2:4 via a2,b2,c2,d2
fit g3(x) "hist_data/".file_name."-local-100.dat" u 2:4 via a3,b3,c3,d3
fit g4(x) "hist_data/".file_name."-local-250.dat" u 2:4 via a4,b4,c4,d4
fit g5(x) "hist_data/".file_name."-local-1000.dat" u 2:4 via a5,b5,c5,d5

stats "hist_data/".file_name."-global-10.dat" u 3 nooutput
print STATS_sum

# plot
set output 'hist_plot_pro/hist_'.file_name.'-local.pdf'

plot "hist_data/".file_name."-global-1000.dat" u 2:4 ls 1 lw 2 pointtype 7 pointsize 0.4 title 'Global', \
     "hist_data/".file_name."-local-10.dat" u 2:4 ls 2 lw 2 pointtype 7 pointsize 0.4 title 'Level=10', \
     "hist_data/".file_name."-local-50.dat" u 2:4 ls 3 lw 2 pointtype 7 pointsize 0.4 title 'Level=50', \
     "hist_data/".file_name."-local-100.dat" u 2:4 ls 5 lw 2 pointtype 7 pointsize 0.4 title 'Level=100', \
     "hist_data/".file_name."-local-250.dat" u 2:4 ls 6 lw 2 pointtype 7 pointsize 0.4 title 'Level=250', \
     "hist_data/".file_name."-local-1000.dat" u 2:4 ls 7 lw 2 pointtype 7 pointsize 0.4 title 'Level=1000', \
          f(x) w linespoints ls 1 lw 2 pointsize 0 title sprintf("P(s)=%2.2fx^{%2.3f}exp(-%2.2fx^{%2.2f})", a,b,c,d), \
          g1(x) w linespoints ls 2 lw 2 pointsize 0 title sprintf("P(s)=%2.2fs^{%2.3f}exp(-%2.2fs^{%2.2f})", a1,b1,c1,d1), \
          g2(x) w linespoints ls 3 lw 2 pointsize 0 title sprintf("P(s)=%2.2fs^{%2.3f}exp(-%2.2fs^{%2.2f})", a2,b2,c2,d2), \
          g3(x) w linespoints ls 5 lw 2 pointsize 0 title sprintf("P(s)=%2.2fs^{%2.3f}exp(-%2.2fs^{%2.2f})", a3,b3,c3,d3), \
          g4(x) w linespoints ls 6 lw 2 pointsize 0 title sprintf("P(s)=%2.2fs^{%2.3f}exp(-%2.2fs^{%2.2f})", a4,b4,c4,d4), \
          g5(x) w linespoints ls 7 lw 2 pointsize 0 title sprintf("P(s)=%2.2fs^{%2.3f}exp(-%2.2fs^{%2.2f})", a5,b5,c5,d5)

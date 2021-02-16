# launch on terminal:
# gnuplot plot_diff.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'
set termopt enhanced
set encoding utf8

# format axes ticks
#set format x "10^{%T}"
#set format y "10^{%T}"
set format x "%.1f"
set format y "%.2f"

# set legend
set key top left reverse Left box
set key height + 1
set key width + 2
#set key title "time step"

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
#set grid mxtics lw 0.5
#set grid mytics lw 0.5


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


# plot labels
set xlabel "λ"
set ylabel "E_1-E_0"

#set logscale y
#set yrange [:1]

set title "Energy difference between E_0 and E_1 for different N"

set output 'diff.pdf'

plot for [i=3:10] 'results/eig_'.i.'.dat' u 1:($3-$2) with lp ls i-2 lw 2 pointtype 7 pointsize 0.6 title 'N='.i.''

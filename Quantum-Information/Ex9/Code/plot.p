# launch on terminal:
# gnuplot -e "k=4" -e "file_name='eig_5'" plot.p

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
set key bottom left reverse Left box
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
set ylabel "E_k"

#set logscale y
set yrange [:1]


#set title "Energy spectrum of the first ".k." levels"

set output 'plots/'.file_name.'.pdf'
plot for [i=2:(k+1)] 'results/'.file_name.'.dat' u 1:i with lp ls (i) lw 2 pointtype 7 pointsize 0.6 title "E_".(i-2).""

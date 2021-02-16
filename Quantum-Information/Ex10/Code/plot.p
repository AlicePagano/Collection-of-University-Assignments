# launch on terminal:
# gnuplot -e "file_name='prova'" plot.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'
set termopt enhanced
set encoding utf8

# format axes ticks
#set format x "10^{%T}"
#set format y "10^{%T}"
set format x "%.2f"
set format y "%.2f"

# set legend
set key top right reverse Left box
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


f(x) = x>-2 & x<2 ? -1-x**2/4 : -abs(x)

# plot labels
set xlabel "λ"
set ylabel "G_0 (λ)"

#set logscale y
#set yrange [:1]

#unset key

set title "Ground state energy density as a function of λ"

set output 'RG_ground_state.pdf' #'plots/'.file_name.'.pdf'

plot 'results/'.file_name.'.dat' u 1:2 with lp ls 5 lw 2 pointtype 7 pointsize 0.6 title 'RG method', \
f(x) with l ls 2 lw 2 title 'Mean Field'

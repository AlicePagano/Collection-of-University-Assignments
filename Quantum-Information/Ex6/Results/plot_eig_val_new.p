# launch on terminal:
# gnuplot plot_eig_val.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
set grid mxtics lw 0.5
set grid mytics lw 0.5


# set title
set title "Relative difference between numerical (λ_n) and analytical (λ_a) eigenvalues"

# format axis ticks
#set format x "%1.1f"
#set format y "%1.1f"
set format x "10^{%T}" #"%.0s×10^{%T}"
set format y "10^{%T}"

# set logscale
set logscale x
set logscale y

# plot labels
set xlabel sprintf("n")
set ylabel sprintf("|λ_n - λ_a|/λ_a")

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
set key bottom right reverse Left box
set key height +1
set key width +1

# plot
set output 'plots/diff_eig_val.pdf'

plot "results_3/eig_val.dat" u 1:(abs($2-$3)/$3) with linespoints  ls 2 lw 2 pointtype 7 pointsize 0.3 title '[-3,3]',\
     "results_5/eig_val.dat" u 1:(abs($2-$3)/$3) with linespoints  ls 5 lw 2 pointtype 7 pointsize 0.3 title '[-5,5]'

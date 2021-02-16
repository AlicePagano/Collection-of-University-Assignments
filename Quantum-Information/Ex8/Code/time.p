# launch on terminal:
# gnuplot time.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'
set termopt enhanced
set encoding utf8

# format axes ticks
set format x "10^{%T}"
set format y "10^{%T}"

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
set xlabel "N"
set ylabel "Elapsed Time [s]"

#set logscale y

set title "Elapsed time for a generic state initialization"

#set output 'time_non-sep.pdf'
#plot "results/time_F_2.dat" u 1:3 with lp ls 2 lw 2 pointtype 7 pointsize 0.6 title "D=2", \
#     "results/time_F_3.dat" u 1:3 with lp ls 4 lw 2 pointtype 7 pointsize 0.6 title "D=3", \
#     "results/time_F_4.dat" u 1:3 with lp ls 6 lw 2 pointtype 7 pointsize 0.6 title "D=4", \
#     "results/time_F_5.dat" u 1:3 with lp ls 8 lw 2 pointtype 7 pointsize 0.6 title "D=5"


set output 'time_sep.pdf'
plot "results/time_T_2.dat" u 1:3 with lp ls 2 lw 2 pointtype 7 pointsize 0.6 title "D=2", \
     "results/time_T_3.dat" u 1:3 with lp ls 4 lw 2 pointtype 7 pointsize 0.6 title "D=3", \
     "results/time_T_4.dat" u 1:3 with lp ls 6 lw 2 pointtype 7 pointsize 0.6 title "D=4", \
     "results/time_T_5.dat" u 1:3 with lp ls 8 lw 2 pointtype 7 pointsize 0.6 title "D=5"

# launch on terminal:
# gnuplot cmap.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15'
set output 'color_map.pdf'

# format x,y axes ticks
set format x  "%.1f"
set format y  "%.1f"
set format cb "%.1f"

# plot title
set title "Squared modulus of wavefunction over time |Ψ(x)|^{2}"

# plot labels
set xlabel "x [a.u.]"
set ylabel "time [a.u.]"

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

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
#set grid mxtics lw 0.5
#set grid mytics lw 0.5


set cblabel "|Ψ(x)|^{2}"

set cbtics scale 0

load 'inferno.pal'

stats  "results/evol_cmap.dat"  u 2 nooutput
x_min = STATS_min
x_max = STATS_max
stats  "results/evol_cmap.dat"  u 1 nooutput
y_min = STATS_min
y_max = STATS_max

set xr [x_min:x_max]
set yr [y_min:y_max]
plot "results/evol_cmap.dat" u 2:1:3 with image notitle

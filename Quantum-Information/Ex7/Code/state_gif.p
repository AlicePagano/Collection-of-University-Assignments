# launch on terminal:
# gnuplot -e "Nt=500" state_gif.p

# output file
set terminal gif font 'CMU Serif Bold,15' animate delay 5 #size 8,6
set termopt enhanced
set encoding utf8
set output 'state.gif'

# format axes ticks
set format x "%.1f"
set format y "%.2f"

# plot title
set title "Time evolution of wavefunction"

# plot labels
set xlabel "x [a.u.]"
set ylabel "Ψ(x)"

# set legend
set key top right reverse Left box
set key height +1
set key width +1

# set colors palette inferno
set style line  1 lt 1 lc rgb '#000004' # black
set style line  2 lt 1 lc rgb '#1f0c48' # dark purple
set style line  3 lt 1 lc rgb '#550f6d' # dark purples
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

do for [k=2:Nt] {
    set yrange [-1.50:1.50]
    plot "results/evol_real.dat" u 1:k with l ls 3 lw 2 title "Re[Ψ]: ts ".k, \
         "results/evol_imag.dat" u 1:k with l ls 6 lw 2 title "Im[Ψ]: ts ".k
}

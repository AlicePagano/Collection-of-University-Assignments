# launch on terminal:
# gnuplot state.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'
set termopt enhanced
set encoding utf8

# format axes ticks
set format x "%.1f"
set format y "%.2f"


# plot labels
set xlabel "x [a.u.]"
set ylabel "Ψ(x)"

# set legend
set key top left reverse Left box
set key height +1
set key width +1
set key title "time step"
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

set yrange [-1.50:1.50]

# plot labels
set xlabel "x [a.u.]"
set ylabel "Re[Ψ]"

set title "Time evolution of wavefunction (Re[Ψ])"
set output 'state_re.pdf'
plot "results/evol_real.dat" u 1:2 with l ls 1 lw 2 title "1", \
     "results/evol_real.dat" u 1:101 with l ls 2 lw 2 title "100", \
     "results/evol_real.dat" u 1:301 with l ls 3 lw 2 title "300", \
     "results/evol_real.dat" u 1:501 with l ls 4 lw 2 title "500", \
     "results/evol_real.dat" u 1:701 with l ls 5 lw 2 title "700", \
     "results/evol_real.dat" u 1:701 with l ls 6 lw 2 title "800", \
     "results/evol_real.dat" u 1:901 with l ls 7 lw 2 title "900"

# plot labels
set xlabel "x [a.u.]"
set ylabel "Im[Ψ]"

set title "Time evolution of wavefunction (Im[Ψ])"
set output 'state_im.pdf'
plot "results/evol_imag.dat" u 1:2 with l ls 1 lw 2 title "1", \
     "results/evol_imag.dat" u 1:101 with l ls 2 lw 2 title "100", \
     "results/evol_imag.dat" u 1:301 with l ls 3 lw 2 title "300", \
     "results/evol_imag.dat" u 1:501 with l ls 4 lw 2 title "500", \
     "results/evol_imag.dat" u 1:701 with l ls 5 lw 2 title "700", \
     "results/evol_imag.dat" u 1:801 with l ls 6 lw 2 title "800", \
     "results/evol_imag.dat" u 1:901 with l ls 7 lw 2 title "900"

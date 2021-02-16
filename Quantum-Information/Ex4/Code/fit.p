# launch on terminal:
# gnuplot -e "opt_flag='O1'" script_gnuplot.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'
set output 'plot/matmult_'.opt_flag.'.pdf'

# set logscale for x and y axes
set logscale x
set logscale y

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
set grid mxtics lw 0.5
set grid mytics lw 0.5

# format y axis ticks
#set format y "10^{%L}"

# set title
set title "Matrix-Matrix Multiplication (".opt_flag." optimization)"

# plot labels
set xlabel 'Matrix Size Log[N]'
set ylabel 'Elapsed Time Log[s]'

# set colors
set style line  1 lt 1 lc rgb '#200060ad' # dark purple
set style line  2 lt 1 lc rgb '#2088226a' # purple
set style line  3 lt 1 lc rgb '#20f9950a' # orange

# set legend
set key top left reverse Left box
set key height +1

# fit
set fit quiet
set fit logfile '/dev/null'

f(x) = a1*x + b1
g(x) = a2*x + b2
h(x) = a3*x + b3

fit [log(100):] f(x) "time/time_".opt_flag."_mat_mult_col.dat" u (log($1)):(log($3)):(log($4)) yerrors via a1,b1
fit [log(100):] g(x) "time/time_".opt_flag."_mat_mult_row.dat" u (log($1)):(log($3)):(log($4)) yerrors via a2,b2
fit [log(100):] h(x) "time/time_".opt_flag."_mat_mult_mat.dat" u (log($1)):(log($3)):(log($4)) yerrors via a3,b3

# plot
plot "time/time_".opt_flag."_mat_mult_col.dat" u 1:3:4 w yerrorbars ls 1 lw 2 pointtype 7 pointsize 0.7 title 'MatMultbyColumn', \
     "time/time_".opt_flag."_mat_mult_row.dat" u 1:3:4 w yerrorbars ls 2 lw 2 pointtype 7 pointsize 0.7 title 'MatMultbyRow', \
     "time/time_".opt_flag."_mat_mult_mat.dat" u 1:3:4 w yerrorbars ls 3 lw 2 pointtype 7 pointsize 0.7 title 'MatMul', \
     exp(f(log(x))) w linespoints ls 1 lw 2 pointsize 0 title sprintf("log(y)=%.2f*log(x) %+.2f", a1, b1), \
     exp(g(log(x))) w linespoints ls 2 lw 2 pointsize 0 title sprintf("log(y)=%.2f*log(x) %+.2f", a2, b2), \
     exp(h(log(x))) w linespoints ls 3 lw 2 pointsize 0 title sprintf("log(y)=%.2f*log(x) %+.2f", a3, b3)

#plot for [file in system("ls time/time_".opt_flag."*.dat")] file u 1:3:4 with yerrorbars

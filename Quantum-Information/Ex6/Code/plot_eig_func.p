# launch on terminal:
# gnuplot -e "col=1" plot.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
#set grid mxtics lw 0.5
#set grid mytics lw 0.5


# set title
set title "Eigenfunction: Ψ_".(col-1)

# format axis ticks
set format x "%1.2f"
set format y "%1.2f"

# plot labels
set xlabel sprintf("x")
set ylabel sprintf("Ψ_".(col-1)."(x)")

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

a = 0.75
f1(x) = a * exp(-x**2/2)
f2(x) = a * sqrt(2) * x * exp(-x**2/2)
f3(x) = -a/sqrt(2) * (2*x**2 -1) * exp(-x**2/2)

# plot
set output 'plots/eig_func_'.(col-1).'.pdf'

plot "results/eig_func.dat" u 1:col ls (col-1) lw 2 pointtype 7 pointsize 0.3 title 'Numerical'

if ((col-1)==1){ set output 'plots/eig_func_an_'.(col-1).'.pdf'; replot f1(x) w l ls 7 title 'Analytical';
}

if ((col-1)==2){ set output 'plots/eig_func_an_'.(col-1).'.pdf'; replot f2(x) w l ls 7 title 'Analytical';
}

if ((col-1)==3){ set output 'plots/eig_func_an_'.(col-1).'.pdf'; replot f3(x) w l ls 7 title 'Analytical';
}

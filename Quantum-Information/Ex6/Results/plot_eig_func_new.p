# launch on terminal:
# gnuplot plot_eig_func_new.p

# output file
set terminal pdfcairo color size 8,6 font 'CMU Serif Bold,15' #'Helvetica,15'

# set grid and border
set border linewidth 1.5
set grid xtics lw 1
set grid ytics lw 1
#set grid mxtics lw 0.5
#set grid mytics lw 0.5



# format axis ticks
set format x "%1.2f"
set format y "%1.2f"

# plot labels
set xlabel sprintf("x")
set ylabel sprintf("Ψ(x)")

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
f4(x) = -a/sqrt(3) * ( 2*x**3 - 3*x ) * exp(-x**2/2)

# plot
set output 'plots/eig_func_1.pdf'
set title "Eigenfunction Ψ_0"
set ylabel sprintf("Ψ_0(x)")

plot  "results_3/eig_func.dat" u 1:2 w l ls 2 lw 2.5 title 'Numerical: [-3,3]', \
    "results_5/eig_func.dat" u 1:2 w l ls 5 lw 2.5 title 'Numerical: [-5,5]', \
    f1(x)w l ls 7 lw 1 title 'Analytical'

set output 'plots/eig_func_2.pdf'
set title "Eigenfunction Ψ_1"
set ylabel sprintf("Ψ_1(x)")

plot  "results_3/eig_func.dat" u 1:3 w l ls 2 lw 2.5  title 'Numerical: [-3,3]', \
    "results_5/eig_func.dat" u 1:3 w l ls 5 lw 2.5 title 'Numerical: [-5,5]', \
    f2(x) w l ls 7 lw 1 title 'Analytical'

set output 'plots/eig_func_3.pdf'
set title "Eigenfunction Ψ_2"
set ylabel sprintf("Ψ_2(x)")

plot  "results_3/eig_func.dat" u 1:4 w l ls 2 lw 2.5 title 'Numerical: [-3,3]', \
    "results_5/eig_func.dat" u 1:4 w l ls 5 lw 2.5 title 'Numerical: [-5,5]', \
    f3(x) w l ls 7 lw 1 title 'Analytical'

set output 'plots/eig_func_4.pdf'
set title "Eigenfunction Ψ_3"
set ylabel sprintf("Ψ_3(x)")

plot  "results_3/eig_func.dat" u 1:5 w l ls 2 lw 2.5 title 'Numerical: [-3,3]', \
    "results_5/eig_func.dat" u 1:5 w l ls 5 lw 2.5 title 'Numerical: [-5,5]', \
    f4(x) w l ls 7 lw 1 title 'Analytical'

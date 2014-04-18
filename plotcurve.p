set xrange [-1:11]
set yrange [11:-1] reverse 
plot "curve.dat" using 1:2 with lp

onbreak {quit -force}
onerror {quit -force}

asim -t 1ps +access +r +m+ila_0 -L xil_defaultlib -L secureip -O5 xil_defaultlib.ila_0

do {wave.do}

view wave
view structure

do {ila_0.udo}

run -all

endsim

quit -force

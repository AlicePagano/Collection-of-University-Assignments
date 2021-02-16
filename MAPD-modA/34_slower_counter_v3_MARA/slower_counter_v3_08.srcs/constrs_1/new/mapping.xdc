set_property -dict { PACKAGE_PIN E3     IOSTANDARD LVCMOS33} [get_ports { clk }];
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports{ clk }];


set_property PACKAGE_PIN E1 [get_ports { y_out[0] }];
set_property IOSTANDARD LVCMOS33 [get_ports { y_out[0] }];

set_property PACKAGE_PIN G4 [get_ports { y_out[1] }];
set_property IOSTANDARD LVCMOS33 [get_ports { y_out[1] }];

set_property PACKAGE_PIN H4 [get_ports { y_out[2] }];
set_property IOSTANDARD LVCMOS33 [get_ports { y_out[2] }];

set_property PACKAGE_PIN K2 [get_ports { y_out[3] }];
set_property IOSTANDARD LVCMOS33 [get_ports { y_out[3] }];

set_property PACKAGE_PIN D9 [get_ports { rst }];
set_property IOSTANDARD LVCMOS33 [get_ports { rst }];

set_property PACKAGE_PIN C9 [get_ports { frz }];
set_property IOSTANDARD LVCMOS33 [get_ports { frz }];


set_property PACKAGE_PIN A8 [get_ports { sel_in[0] }];
set_property IOSTANDARD LVCMOS33 [get_ports { sel_in[0] }];

set_property PACKAGE_PIN C11 [get_ports { sel_in[1] }];
set_property IOSTANDARD LVCMOS33 [get_ports { sel_in[1] }];

set_property PACKAGE_PIN C10 [get_ports { sel_in[2] }];
set_property IOSTANDARD LVCMOS33 [get_ports { sel_in[2] }];

set_property PACKAGE_PIN A10 [get_ports { sel_in[3] }];
set_property IOSTANDARD LVCMOS33 [get_ports { sel_in[3] }];
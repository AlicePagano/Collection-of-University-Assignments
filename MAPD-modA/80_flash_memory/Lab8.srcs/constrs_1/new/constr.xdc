set_property PACKAGE_PIN E3 [get_ports {clk_base_xc7a_i}]
set_property IOSTANDARD LVCMOS33 [get_ports {clk_base_xc7a_i}]
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports {clk_base_xc7a_i}];

## Quad SPI Flash
set_property -dict { PACKAGE_PIN L13   IOSTANDARD LVCMOS33 } [get_ports { flash_cs_o }]; #IO_L6P_T0_FCS_B_14 Sch=qspi_cs
set_property -dict { PACKAGE_PIN K17   IOSTANDARD LVCMOS33 } [get_ports { flash_mosi_o }]; #IO_L1P_T0_D00_MOSI_14 Sch=qspi_dq[0]
set_property -dict { PACKAGE_PIN K18   IOSTANDARD LVCMOS33 } [get_ports { flash_miso_i }]; #IO_L1N_T0_D01_DIN_14 Sch=qspi_dq[1]
set_property -dict { PACKAGE_PIN L16   IOSTANDARD LVCMOS33 } [get_ports { flash_clk_o }]; #IO_L1N_T0_D01_DIN_14 Sch=qspi_dq[1]
set_property -dict { PACKAGE_PIN L14   IOSTANDARD LVCMOS33 } [get_ports { flash_w_o }]; #IO_L2P_T0_D02_14 Sch=qspi_dq[2]
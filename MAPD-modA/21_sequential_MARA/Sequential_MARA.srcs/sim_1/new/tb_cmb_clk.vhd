----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/25/2019 05:12:40 PM
-- Design Name: 
-- Module Name: tb_cmb_clk - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity tb_cmb_clk is
--  Port ( );
end tb_cmb_clk;

architecture Behavioral of tb_cmb_clk is

component cmb_clk is
    Port (  clk   :   in  std_logic;
            rst   :   in  std_logic;
            a_in  :   in  std_logic;
            ab_in :   in  std_logic;
            cmb_out:  out std_logic;
            ltch_out: out std_logic;
            ff_out:   out std_logic);
end component;

signal clk, rst : std_logic;
signal a, ab    : std_logic;
signal cmb, ltch, ff : std_logic;

begin

uut : cmb_clk port map (clk=>clk, rst=>rst, a_in=>a, ab_in=>ab, cmb_out=>cmb, ltch_out=>ltch, ff_out=>ff);

p_clk : process
begin
    clk <= '0'; wait for 5 ns; clk <= '1'; wait for 5 ns;
end process;

p_rst : process
begin
    rst <= '1'; wait for 15 ns; rst <= '0'; wait;
end process;

p_cmb : process
begin
    a <= '0'; wait for 0.2 ns; ab <= '1'; wait for 67 ns;
    a <= '1'; wait for 0.2 ns; ab <= '0'; wait for 67 ns;
end process;

end Behavioral;

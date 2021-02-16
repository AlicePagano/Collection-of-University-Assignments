----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 12/10/2019 08:53:28 AM
-- Design Name: 
-- Module Name: tb_fsm - Behavioral
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

entity tb_fsm is
--  Port ( );
end tb_fsm;

architecture Behavioral of tb_fsm is

component fsm_1 is
    Port (  clk : in std_logic;
            rst : in std_logic;
            sel_in : in std_logic_vector(3 downto 0);            
            y_out : out std_logic_vector(3 downto 0);
            y_out_r : out std_logic_vector(3 downto 0));
end component;

signal clk, rst : std_logic;
signal y_out : std_logic_vector (3 downto 0);
signal y_out_r : std_logic_vector(3 downto 0);
signal sel :  std_logic_vector (3 downto 0);
    
--type state is (s_idle, s_0xxx, s_01xx, s_010x, s_0101);
--signal state_curr, state_next : state;

begin

uut : fsm_1 port map (clk => clk, rst => rst, sel_in => sel, y_out => y_out, y_out_r => y_out_r);

p_clk : process
    begin
        clk <= '0'; wait for 5 ns; clk <= '1'; wait for 5 ns;   
 end process;
 
p_rst : process
    begin
       rst <= '1'; wait for 15 ns; rst <= '0'; wait; 
    end process;
    
p_cmb_0 : process
    begin
        sel <= "0101"; wait for 100 ns;
        sel <= "1010"; wait for 100 ns;
        sel <= "0001"; wait for 100 ns;
    end process;  
       
    
--p_cmb_0 : process
--    begin
--        sel(0) <= '1'; wait for 100 ns; sel(0) <= '0'; wait for 100 ns; 
--    end process;  
    
--p_cmb_1 : process
--    begin
--        sel(1) <= '1'; wait for 200 ns; sel(1) <= '0'; wait for 200 ns; 
--    end process;    
    
--p_cmb_2 : process
--    begin
--        sel(2) <= '1'; wait for 400 ns; sel(2) <= '0'; wait for 400 ns;
--    end process;  
       
--p_cmb_3 : process
--    begin
--        sel(3) <= '1'; wait for 800 ns; sel(3) <= '0'; wait for 800 ns;
--    end process;     
end Behavioral;

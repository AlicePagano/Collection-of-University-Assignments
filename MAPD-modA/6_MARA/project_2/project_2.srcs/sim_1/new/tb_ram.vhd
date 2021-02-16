----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01/15/2020 03:35:43 PM
-- Design Name: 
-- Module Name: tb_ram - Behavioral
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
use ieee.numeric_std.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;


entity tb_ram is 

end tb_ram;

architecture IMP of tb_ram is

component bram_asym is port(
  clk   : in  std_logic;
  enB   : in  std_logic;
  weA   : in  std_logic;
  addrA : in  unsigned(10 downto 0);
  addrB : in  unsigned(10 downto 0);
  diA   : in  std_logic_vector(7 downto 0);
  doA   : out std_logic_vector(7 downto 0);
  doB   : out std_logic_vector(7 downto 0)
  );
end component;

signal  clk : std_logic;
signal  enB   : std_logic;
signal  weA   : std_logic;
signal  addrA : unsigned(10 downto 0);
signal  addrB : unsigned(10 downto 0);
signal  diA   : std_logic_vector(7 downto 0);
signal  doA   : std_logic_vector(7 downto 0);
signal  doB   : std_logic_vector(7 downto 0);

begin

uut : bram_asym port map (clk => clk,
                          enB =>  enB,   
                          weA =>  weA,  
                          addrA => addrA, 
                          addrB => addrB, 
                          diA  => diA,   
                          doA  => doA,   
                          doB  =>  doB); 
                          
p_clk : process
begin
    clk <= '0'; wait for 5 ns;
    clk <= '1'; wait for 5 ns;
end process;    
    
write : process
begin
    weA <= '1'; 
    addrA <= "00000000001"; 
    diA <= "10101010";
    wait;
end process;





end;

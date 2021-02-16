----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/25/2019 05:38:00 PM
-- Design Name: 
-- Module Name: blink - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cnt is
    Port (clk : in std_logic;
            rst : in std_logic;
            frz : in std_logic;
            sel_in : in std_logic_vector(3 downto 0);            
            y_out : out std_logic_vector(3 downto 0));
end cnt;

architecture rtl of cnt is
    signal slow_clk, slow_clk_p : std_logic; signal counter : unsigned (27 downto 0);
    signal slow_counter : unsigned (3 downto 0);
    signal sel :  std_logic_vector (2 downto 0);

begin

sel <= sel_in(3 downto 1);

p_cnt : process(clk,rst,sel_in) is
    begin
    
    if rst = '1' then 
        counter <= (others => '0');
    end if; 

    if rising_edge(clk) then
        counter <= counter +1;
    end if;
    
end process;
 
p_slw_cnt : process(clk,rst,frz,slow_clk,sel_in) is
    begin

    if rst = '1' then 
        slow_counter <= (others => '0');    
    end if;

        
    if rising_edge(clk) then
        slow_clk_p <= slow_clk;
        
        if frz = '0' then
            if slow_clk = '1' and slow_clk_p = '0' then
                if sel_in(0) = '0' then
                    slow_counter <= slow_counter + 1;
                elsif sel_in(0) = '1' then
                    slow_counter <= slow_counter - 1;
                end if;
            end if;
        end if;
    end if;
     
end process;


speed : process(clk,rst,slow_clk,sel) is
    begin
   
    case sel is 
        when "000" => slow_clk <= counter(27);
        when "001" => slow_clk <= counter(26);
        when "010" => slow_clk <= counter(25);
        when "011" => slow_clk <= counter(24);
        when "100" => slow_clk <= counter(23);
        when "101" => slow_clk <= counter(22);  
        when "110" => slow_clk <= counter(21);
        when "111" => slow_clk <= counter(20);
    end case;      
       
end process;
 
y_out <= std_logic_vector(slow_counter);

end rtl;
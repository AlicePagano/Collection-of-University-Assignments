library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bram_asym is port(
  clk   : in  std_logic;
  enB   : in  std_logic;
  weA   : in  std_logic;
  addrA : in  unsigned(10 downto 0);
  addrB : in  unsigned(10 downto 0);
  diA   : in  std_logic_vector(7 downto 0);
  doA   : out std_logic_vector(7 downto 0);
  doB   : out std_logic_vector(7 downto 0)
  );
end;

architecture IMP of bram_asym is

  type ramType is array (0 to 2047) of std_logic_vector(7 downto 0);
  shared variable ram : ramType               := (others => (others => '0'));

begin
  -- Asymmetric BRAM inference
  process(clk)
  begin
    if rising_edge(clk) then
      if weA = '1' then
        ram(to_integer(addrA)) := diA;
      end if;
    end if;
   -- doA <= ram(to_integer(addrA));
  end process;
  process(clk)
  begin
    if rising_edge(clk) then
          doB <= ram(to_integer(addrB));
          doA <= ram(to_integer(addrA));

    end if;
  end process;

end;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fir_filter_4 is
  port (
    i_clk     : in  std_logic;
    i_rstb    : in  std_logic;
    -- coefficient
    i_coeff_0 : in  std_logic_vector(7 downto 0);
    i_coeff_1 : in  std_logic_vector(7 downto 0);
    i_coeff_2 : in  std_logic_vector(7 downto 0);
    i_coeff_3 : in  std_logic_vector(7 downto 0);
    -- data input
    i_data    : in  std_logic_vector(7 downto 0);
    -- filtered data 
    o_data    : out std_logic_vector(9 downto 0));
end fir_filter_4;

architecture rtl of fir_filter_4 is

  type t_data_pipe is array (0 to 3) of signed(7 downto 0);
  type t_coeff is array (0 to 3) of signed(7 downto 0);

  type t_mult is array (0 to 3) of signed(15 downto 0);
  type t_add_st0 is array (0 to 1) of signed(15+1 downto 0);

  signal r_coeff   : t_coeff;
  signal p_data    : t_data_pipe;
  signal r_mult    : t_mult;
  signal r_add_st0 : t_add_st0;
  signal r_add_st1 : signed(15+2 downto 0);

begin

  p_input : process (i_rstb, i_clk)
  begin
    if(i_rstb = '0') then
      p_data  <= (others => (others => '0'));
      r_coeff <= (others => (others => '0'));
    elsif(rising_edge(i_clk)) then
      p_data     <= signed(i_data)&p_data(0 to p_data'length-2);
      r_coeff(0) <= signed(i_coeff_0);
      r_coeff(1) <= signed(i_coeff_1);
      r_coeff(2) <= signed(i_coeff_2);
      r_coeff(3) <= signed(i_coeff_3);
    end if;
  end process p_input;

  p_mult : process (i_rstb, i_clk)
  begin
    if(i_rstb = '0') then
      r_mult <= (others => (others => '0'));
    elsif(rising_edge(i_clk)) then
      for k in 0 to 3 loop
        r_mult(k) <= p_data(k) * r_coeff(k);
      end loop;
    end if;
  end process p_mult;

  p_add_st0 : process (i_rstb, i_clk)
  begin
    if(i_rstb = '0') then
      r_add_st0 <= (others => (others => '0'));
    elsif(rising_edge(i_clk)) then
      for k in 0 to 1 loop
        r_add_st0(k) <= resize(r_mult(2*k), 17) + resize(r_mult(2*k+1), 17);
      end loop;
    end if;
  end process p_add_st0;

  p_add_st1 : process (i_rstb, i_clk)
  begin
    if(i_rstb = '0') then
      r_add_st1 <= (others => '0');
    elsif(rising_edge(i_clk)) then
      r_add_st1 <= resize(r_add_st0(0), 18) + resize(r_add_st0(1), 18);
    end if;
  end process p_add_st1;

  p_output : process (i_rstb, i_clk)
  begin
    if(i_rstb = '0') then
      o_data <= (others => '0');
    elsif(rising_edge(i_clk)) then
      o_data <= std_logic_vector(r_add_st1(9 downto 0));
    end if;
  end process p_output;
end rtl;

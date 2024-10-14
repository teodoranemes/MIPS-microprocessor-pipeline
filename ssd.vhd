library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_arith.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;


entity SSD is
    Port ( cnt : in STD_LOGIC_VECTOR (31 downto 0);
           clk : in STD_LOGIC;
           an : out STD_LOGIC_VECTOR (7 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end SSD;

architecture Behavioral of SSD is

signal counter : std_logic_vector(16 downto 0):= (others => '0');
signal iesire_mux : std_logic_vector(3 downto 0) :=(others =>'0');

begin

with iesire_mux SELect
   cat<= "1111001" when "0001",   --1
         "0100100" when "0010",   --2
         "0110000" when "0011",   --3
         "0011001" when "0100",   --4
         "0010010" when "0101",   --5
         "0000010" when "0110",   --6
         "1111000" when "0111",   --7
         "0000000" when "1000",   --8
         "0010000" when "1001",   --9
         "0001000" when "1010",   --A
         "0000011" when "1011",   --b
         "1000110" when "1100",   --C
         "0100001" when "1101",   --d
         "0000110" when "1110",   --E
         "0001110" when "1111",   --F
         "1000000" when others;   --0


process (counter(16 downto 14), cnt)
begin
   case counter(16 downto 14) is
      when "000" => iesire_mux <= cnt(3 downto 0);
      when "001" => iesire_mux <= cnt(7 downto 4);
      when "010" => iesire_mux <= cnt(11 downto 8);
      when "011" => iesire_mux <= cnt(15 downto 12);
      when "100" => iesire_mux <= cnt(19 downto 16);
      when "101" => iesire_mux <= cnt(23 downto 20);
      when "110" => iesire_mux <= cnt(27 downto 24);
      when "111" => iesire_mux <= cnt(31 downto 28);
     
   end case;
end process;

process(counter(16 downto 14))
begin
    case counter(16 downto 14) is 
        when "000" => an <= "11111110";
        when "001" => an <= "11111101";
        when "010" => an <= "11111011";
        when "011" => an <= "11110111";
        when "100" => an <= "11101111";
        when "101" => an <= "11011111";
        when "110" => an <= "10111111";
        when "111" => an <= "01111111";
    end case;
end process;

process (clk)
begin
   if clk='1' and clk'event then
      counter <= counter + 1;
   end if;
end process;
end Behavioral;
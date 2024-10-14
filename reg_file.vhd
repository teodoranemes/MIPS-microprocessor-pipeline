
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_arith.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;


entity reg_file is
    Port (
    clk: in std_logic;
    en: in std_logic;
    ra1: in std_logic_vector(4 downto 0);
    ra2: in std_logic_vector(4 downto 0);
    regWR: in std_logic;
    wd: in std_logic_vector(31 downto 0);
    wa: in std_logic_vector(4 downto 0);
    rd1: out std_logic_vector(31 downto 0);
    rd2: out std_logic_vector(31 downto 0)
    );
end reg_file;

architecture Behavioral of reg_file is

type reg is array(0 to 31) of std_logic_vector(31 downto 0);
signal r: reg := (
x"00000001",
x"00000201",
x"00002402",
x"00001003",
x"00001234",
x"00000321",
x"00000041",
x"00009907",
others => x"00000000");

begin

rd1 <= r(conv_integer(ra1));
rd2 <= r(conv_integer(ra2));


process(clk)
begin
if rising_edge(clk) then
    if regWR = '1' and en = '1' then
        r(conv_integer(wa)) <= wd;
    end if;
end if;
end process;


end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ID is
     Port (
     clk: in std_logic;
     RegWrite: in std_logic;
     Instr: in std_logic_vector(31 downto 0);
     ExtOp: in std_logic;
     RegDst: in std_logic_vector(4 downto 0);
     En : in STD_LOGIC;
     WD: in std_logic_vector(31 downto 0);
     RD1: out std_logic_vector(31 downto 0);
     RD2: out std_logic_vector(31 downto 0);
     Ext_imm: out std_logic_vector(31 downto 0);
     funct: out std_logic_vector(5 downto 0);
     sa: out std_logic_vector(4 downto 0);
     rt : out STD_LOGIC_VECTOR(4 downto 0);
     rd : out STD_LOGIC_VECTOR(4 downto 0)
     ); 
end ID;

architecture Behavioral of ID is 

component reg_file is
    Port ( 
    clk: in std_logic;
    en: in std_logic;
    ra1: in std_logic_vector(4 downto 0);
    ra2: in std_logic_vector(4 downto 0);
    regWR: in std_logic;
    wd: in std_logic_vector(31 downto 0);
    wa: in std_logic_vector(4 downto 0);
    rd1: out std_logic_vector(31 downto 0);
    rd2: out std_logic_vector(31 downto 0));
end component;

begin

register_file : reg_file port map(clk=>clk, en => En, ra1=>Instr(25 downto 21), ra2=>Instr(20 downto 16), regWr =>RegWrite, wd => WD, wa => RegDst, rd1 => RD1, rd2 => RD2);

sa <= Instr(10 downto 6);
funct <= Instr(5 downto 0);

rt <= Instr(20 downto 16);
rd <= Instr(15 downto 11);

Ext_Imm(15 downto 0) <= Instr(15 downto 0); 
Ext_Imm(31 downto 16) <= (others => Instr(15)) when ExtOp = '1' else (others => '0');

end Behavioral;
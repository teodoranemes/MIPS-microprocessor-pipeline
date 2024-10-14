
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity IFetch is
    Port (clk : in STD_LOGIC;
          rst : in STD_LOGIC;
          en : in STD_LOGIC;
          BranchAddress : in STD_LOGIC_VECTOR(31 downto 0);
          JumpAddress : in STD_LOGIC_VECTOR(31 downto 0);
          Jump : in STD_LOGIC;
          PCSrc : in STD_LOGIC;
          Instruction : out STD_LOGIC_VECTOR(31 downto 0);
          PCp4 : out STD_LOGIC_VECTOR(31 downto 0));
end IFetch;

architecture Behavioral of IFetch is

type tROM is array (0 to 63) of STD_LOGIC_VECTOR(31 downto 0);
signal ROM : tROM := (

b"001000_00010_00001_0000000000000011", -- addi $1 $2 3
b"001000_00100_00011_0000000000000010", -- addi $3 $4 2
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00001_00011_00010_00000_100000", -- add $2 $1 $3
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00010_00011_00100_00000_100010", -- sub $4 $2 $3
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00010_00000_00101_00001_000001", -- sll $5 $2 1
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00001_00000_00100_00001_000010", -- srl $4 $1 1
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00010_00011_00001_00000_100100", -- and $1 $2 $3
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00001_00100_00010_00000_100101", -- or $2 $1 $4
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00010_00100_00011_00000_100111", -- xor $3 $2 $4
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00010_00001_00011_00010_101000", -- sllv $3 $2 $1
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"100011_00010_00001_0000000000000011", -- lw $1 3($2)
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"101011_00011_00101_0000000000000010", -- sw $5 2($3)
b"000100_00011_00010_0000000000000110", -- beq $2 $3 6
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"000111_00000_00000_0000000000000001", -- j 1
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"001011_00010_00001_0000000000000100", -- ori $1 $2 4
b"000000_00000_00000_00000_00000_000000", -- NoOp
b"001001_00011_00101_0000000000000010", -- xori $5 $3 2
others => x"00000000");


signal PCAux, NextAddr, AuxSgn: STD_LOGIC_VECTOR(31 downto 0);
signal PC : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');

begin

PCAux <= PC + 4;
PCp4 <= PCAux;
Instruction <= ROM(conv_integer(PC(7 downto 2)));

AuxSgn <= BranchAddress when PCSrc = '1' else PCAux;  
    
NextAddr <= JumpAddress when Jump = '1' else AuxSgn;

process(clk, rst)
    begin
        if rst = '1' then
            PC <= (others => '0');
        elsif rising_edge(clk) then
            if en = '1' then
                PC <= NextAddr;
            end if;
        end if;
    end process;

end Behavioral;
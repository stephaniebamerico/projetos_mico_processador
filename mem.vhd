-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2016-2 trabalho semestral, autor: Roberto Hexsel, 07out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

use work.p_wires.all;

entity mem_prog is
  port (ender : in  reg6;
        instr : out reg32);

  type t_prog_mem is array (0 to 63) of reg32;

  -- memoria de programa contem somente 64 palavras
  constant program : t_prog_mem := (
    x"b0010001",                        -- addi R1,R0,1  / resp = 1
    x"b002000C",                        -- addi R2,R0,N  / num = N = 12
    x"c2000000",
    x"e2000007",                        -- bran R2,R0,6  / (num == 0) ?
    x"31210000",                        -- mul R1,R1,R2  / resp *= num
    x"b202FFFF",                        -- addi R2,R2,-1 / num--
    x"d0000003",                        -- jump 2        / ? :
    x"c1000000",                        -- display R1    / printf resp
    x"f0000000",                        -- halt          / return 0
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000");

  function BV2INT6(S: reg6) return integer is
    variable result: integer;
  begin
    --if S(5) = '1' then result := -63; else result := 0; end if;
    for i in S'range loop
      result := result * 2;
      if S(i) = '1' then
        result := result + 1;
      end if;
    end loop;
    return result;
  end BV2INT6;
  
end mem_prog;

-- nao altere esta arquitetura
architecture tabela of mem_prog is
begin  -- tabela

  instr <= program( BV2INT6(ender) );

end tabela;


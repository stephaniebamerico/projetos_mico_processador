-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2016-2 trabalho semestral, autor: Roberto Hexsel, 07out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- display: exibe inteiro na saida padrao do simulador
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use std.textio.all;
use work.p_wires.all;
entity display is
  port (rst,clk : in bit;
        enable  : in bit;
        data    : in reg32);
end display;
architecture functional of display is
  file output : text open write_mode is "STD_OUTPUT";
begin  -- functional
  U_WRITE_OUT: process(clk)
    variable msg : line;
  begin
    if falling_edge(clk) and enable = '1' then
      write ( msg, string'(BV32HEX(data)) );
      writeline( output, msg );
    end if;
  end process U_WRITE_OUT;
end functional;
-- ++ display ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- MICO X
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_wires.all;
entity mico is
  port (rst,clk : in    bit);
end mico;
architecture behaviour of mico is
  component display is                  -- neste arquivo
    port (rst,clk : in bit;
          enable  : in bit;
          data    : in reg32);
  end component display;
  
  component Extend16to32 is             -- neste arquivo
  port (A  : in reg16;
		S  : in bit;
		Z  : out reg32);
  end component Extend16to32;

  component mem_prog is                 -- no arquivo mem.vhd
    port (ender : in  reg6;
          instr : out reg32);
  end component mem_prog;
  component ULA is                      -- neste arquivo
    port (fun : in reg4;
        alfa,beta : in  reg32;
        gama      : out reg32;
        zero		: out bit);
  end component ULA;
 
  component R is                        -- neste arquivo
    port (clk         : in  bit;
          wr_en       : in  bit;
          r_a,r_b,r_c : in  reg4;
          A,B         : out reg32;
          C           : in  reg32);
  end component R;
  
  component mux2Vet32 is                -- no arquivo aux.vhd
  port(A,B : in  reg32;
       S   : in  bit;
       Z   : out reg32);
  end component mux2Vet32;
  
  component IP_code is
  port (A, B : in reg16;
		S : in reg3;
		zero : in bit;
		Z : out reg16);
  end component IP_code;
  
  component registerN is
  generic (NUM_BITS: integer := 16;
           INIT_VAL: bit_vector);
  port(clk, rst, ld: in  bit;
       D:            in  bit_vector(NUM_BITS-1 downto 0);
       Q:            out bit_vector(NUM_BITS-1 downto 0));
	end component registerN;

  type t_control_type is record
    extZero  : bit;       -- estende com zero=1, com sinal=0
    selBeta  : bit;       -- seleciona fonte para entrada B da ULA
    wr_display: bit;      -- atualiza display=1
    selNxtIP : reg3;       -- seleciona fonte do incremento do IP
    wr_reg   : bit;       -- atualiza registrador: R(c) <= C
  end record;
  type t_control_mem is array (0 to 15) of t_control_type;
  -- preencha esta tabela com os sinais de controle adequados
  -- a tabela eh indexada com o opcode da instrucao
  constant ctrl_table : t_control_mem := (
  --extZ sBeta wrD sIP wrR
    ('0','0', '0', "000",'0'),            -- NOP
    ('0','0', '0', "000",'1'),            -- ADD
    ('0','0', '0', "000",'1'),            -- SUB
    ('0','0', '0', "000",'1'),            -- MUL
    ('0','0', '0', "000",'1'),            -- AND
    ('0','0', '0', "000",'1'),            -- OR
    ('0','0', '0', "000",'1'),            -- XOR
    ('0','0', '0', "000",'1'),            -- NOT
    ('0','0', '0', "000",'1'),            -- SLL
    ('0','0', '0', "000",'1'),            -- SRL
    ('1','1', '0', "000",'1'),            -- ORI
    ('0','1', '0', "000",'1'),            -- ADDI
    ('0','0', '1', "000",'0'),            -- SHOW
    ('0','0', '0', "010",'0'),            -- JUMP
    ('0','0', '0', "001",'0'),            -- BRANCH
    ('0','0', '0', "100",'0'));           -- HALT
  signal extZero, selBeta, wr_display, wr_reg : bit;
  signal selNxtIP : reg3;
  signal instr, A, B, C, beta, extended : reg32;
  signal this  : t_control_type;
  signal i_opcode : natural range 0 to 15;
  signal const, ip, ip_out : reg16;
  signal opcode : reg4;
  signal produto: reg64;
  signal zero : bit;
  
begin 
  -- memoria de programa contem somente 64 palavras
  U_mem_prog: mem_prog port map(ip(5 downto 0), instr);

  opcode <= instr(31 downto 28);
  i_opcode <= BV2INT4(opcode);          -- indice do vetor DEVE ser inteiro
  
  this <= ctrl_table(i_opcode);         -- sinais de controle
  extZero    <= this.extZero;
  selBeta    <= this.selBeta;
  wr_display <= this.wr_display;
  selNxtIP   <= this.selNxtIP;
  wr_reg     <= this.wr_reg;
  
  U_regs: R port map (clk,  wr_reg,instr(27 downto 24) , instr(23 downto 20),instr(19 downto 16) ,A,B,C);
  
  U_ext: Extend16to32 port map (instr(15 downto 0),extZero,extended);
  U_mux32: mux2Vet32  port map (B,extended,selBeta,beta);
  U_ULA: ULA port map (opcode,A,beta,C,zero);
    
  Uip: IP_code port map (ip, instr(15 downto 0),selNxtIP,zero,ip_out);
  Uff: registerN generic map(16, x"0000") port map(clk, rst, '0', ip_out, ip); --ip out é a entrada do registrador
  
  -- nao altere esta linha
  U_display: display port map (rst, clk, wr_display, A);
end behaviour;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;
entity Extend16to32 is
  port (A  : in reg16;
		S : in bit;
		Z : out reg32);
end Extend16to32;
architecture extende of Extend16to32 is
	constant zero: integer := 0;
	
	component mux2Vet16 is
	port(A,B : in  reg16;
		 S   : in  bit;
		 Z   : out reg16);
	end component mux2Vet16;
  -- signal 
  signal sinal : reg32;
begin
	Z (15 downto 0) <= A;
	sinal(16) <= A(15);
	sinal(17) <= A(15);
	sinal(18) <= A(15);
	sinal(19) <= A(15);
	sinal(20) <= A(15);
  	sinal(21) <= A(15);
  	sinal(22) <= A(15);
  	sinal(23) <= A(15);
	sinal(24) <= A(15);  	
	sinal(25) <= A(15);
	sinal(26) <= A(15);  	
	sinal(27) <= A(15); 
	sinal(28) <= A(15); 
	sinal(29) <= A(15); 
	sinal(30) <= A(15); 
	sinal(31) <= A(15);
	ua0: mux2Vet16 port map(sinal(31 downto 16), x"0000", S,Z(31 downto 16));
	
end extende;
-- -----------------------------------------------------------------------
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;
entity ULA is
  port (fun : in reg4;
        alfa,beta : in  reg32;
        gama      : out reg32;
        zero		: out bit);
end ULA;
architecture behaviour of ULA is

component inv32 is
  generic (prop : time := t_inv);
  port(A : in reg32;
       S : out reg32);
	end component inv32;

component mux2 is
	port(A,B : in  bit;
       S   : in  bit;
       Z   : out bit);
    end component mux2;

component  mux2Vet32 is
  port(A,B : in  reg32;
       S   : in  bit;
       Z   : out reg32);
  end component mux2Vet32;
  
 component mux16Vet32 is
  port(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P: in  reg32;
       S   : in  reg4;
       Z   : out reg32);
  end component mux16Vet32;

component somador32bits is
	port (A,B : in reg32;
			vem: in bit;
			vai: out bit;
			C: out reg32);
  end component somador32bits;

component  mult32x32 is
  port(A, B : in  reg32;   -- entradas A,B
       prod : out reg64);  -- produto
  end component mult32x32;
  
component desl is
  port (A: in reg32;
		S: in reg32;
		B: out reg32);
	end component desl;
	
component desr is
  port (A: in reg32;
		S: in reg32;
		B: out reg32);
	end component desr;

signal r_invB, B, r_somador, r_and, r_or, r_xor, r_not, r_right, r_left : reg32;
signal r_mul : reg64;
signal vem : bit;

begin  -- behaviour
	-- add ou sub
	Umux : mux2 port map ('1', '0', fun(0), vem); -- vem: 0 - add ou 1 - sub
	Uinv : inv32 port map (beta, r_invB);
	Umux32 : mux2Vet32 port map (r_invB, beta, fun(0), B); -- beta - add ou r_invB - sub	
	Uadd : somador32bits port map (alfa, B, vem, open, r_somador);  
	-- mul
	Umul : mult32x32 port map (alfa, beta, r_mul);
	-- and
	r_and(31) <= alfa(31) and beta(31); r_and(30) <= alfa(30) and beta(30);
	r_and(29) <= alfa(29) and beta(29); r_and(28) <= alfa(28) and beta(28);
	r_and(27) <= alfa(27) and beta(27); r_and(26) <= alfa(26) and beta(26);
	r_and(25) <= alfa(25) and beta(25); r_and(24) <= alfa(24) and beta(24);
	r_and(23) <= alfa(23) and beta(23); r_and(22) <= alfa(22) and beta(22);
	r_and(21) <= alfa(21) and beta(21); r_and(20) <= alfa(20) and beta(20);
	r_and(19) <= alfa(19) and beta(19); r_and(18) <= alfa(18) and beta(18);
	r_and(17) <= alfa(17) and beta(17); r_and(16) <= alfa(16) and beta(16);
	r_and(15) <= alfa(15) and beta(15); r_and(14) <= alfa(14) and beta(14);
	r_and(13) <= alfa(13) and beta(13); r_and(12) <= alfa(12) and beta(12);
	r_and(11) <= alfa(11) and beta(11); r_and(10) <= alfa(10) and beta(10);
	r_and(9) <= alfa(9) and beta(9); r_and(8) <= alfa(8) and beta(8);
	r_and(7) <= alfa(7) and beta(7); r_and(6) <= alfa(6) and beta(6);
	r_and(5) <= alfa(5) and beta(5); r_and(4) <= alfa(4) and beta(4);
	r_and(3) <= alfa(3) and beta(3); r_and(2) <= alfa(2) and beta(2);
	r_and(1) <= alfa(1) and beta(1); r_and(0) <= alfa(0) and beta(0);
	-- or
	r_or(31) <= alfa(31) or beta(31); r_or(30) <= alfa(30) or beta(30);
	r_or(29) <= alfa(29) or beta(29); r_or(28) <= alfa(28) or beta(28);
	r_or(27) <= alfa(27) or beta(27); r_or(26) <= alfa(26) or beta(26);
	r_or(25) <= alfa(25) or beta(25); r_or(24) <= alfa(24) or beta(24);
	r_or(23) <= alfa(23) or beta(23); r_or(22) <= alfa(22) or beta(22);
	r_or(21) <= alfa(21) or beta(21); r_or(20) <= alfa(20) or beta(20);
	r_or(19) <= alfa(19) or beta(19); r_or(18) <= alfa(18) or beta(18);
	r_or(17) <= alfa(17) or beta(17); r_or(16) <= alfa(16) or beta(16);
	r_or(15) <= alfa(15) or beta(15); r_or(14) <= alfa(14) or beta(14);
	r_or(13) <= alfa(13) or beta(13); r_or(12) <= alfa(12) or beta(12);
	r_or(11) <= alfa(11) or beta(11); r_or(10) <= alfa(10) or beta(10);
	r_or(9) <= alfa(9) or beta(9); r_or(8) <= alfa(8) or beta(8);
	r_or(7) <= alfa(7) or beta(7); r_or(6) <= alfa(6) or beta(6);
	r_or(5) <= alfa(5) or beta(5); r_or(4) <= alfa(4) or beta(4);
	r_or(3) <= alfa(3) or beta(3); r_or(2) <= alfa(2) or beta(2);
	r_or(1) <= alfa(1) or beta(1); r_or(0) <= alfa(0) or beta(0);
	-- xor
	r_xor(31) <= alfa(31) xor beta(31); r_xor(30) <= alfa(30) xor beta(30);
	r_xor(29) <= alfa(29) xor beta(29); r_xor(28) <= alfa(28) xor beta(28);
	r_xor(27) <= alfa(27) xor beta(27); r_xor(26) <= alfa(26) xor beta(26);
	r_xor(25) <= alfa(25) xor beta(25); r_xor(24) <= alfa(24) xor beta(24);
	r_xor(23) <= alfa(23) xor beta(23); r_xor(22) <= alfa(22) xor beta(22);
	r_xor(21) <= alfa(21) xor beta(21); r_xor(20) <= alfa(20) xor beta(20);
	r_xor(19) <= alfa(19) xor beta(19); r_xor(18) <= alfa(18) xor beta(18);
	r_xor(17) <= alfa(17) xor beta(17); r_xor(16) <= alfa(16) xor beta(16);
	r_xor(15) <= alfa(15) xor beta(15); r_xor(14) <= alfa(14) xor beta(14);
	r_xor(13) <= alfa(13) xor beta(13); r_xor(12) <= alfa(12) xor beta(12);
	r_xor(11) <= alfa(11) xor beta(11); r_xor(10) <= alfa(10) xor beta(10);
	r_xor(9) <= alfa(9) xor beta(9); r_xor(8) <= alfa(8) xor beta(8);
	r_xor(7) <= alfa(7) xor beta(7); r_xor(6) <= alfa(6) xor beta(6);
	r_xor(5) <= alfa(5) xor beta(5); r_xor(4) <= alfa(4) xor beta(4);
	r_xor(3) <= alfa(3) xor beta(3); r_xor(2) <= alfa(2) xor beta(2);
	r_xor(1) <= alfa(1) xor beta(1); r_xor(0) <= alfa(0) xor beta(0);
	-- not
	Unot : inv32 port map (alfa, r_not);
	-- deslocador
	Udesl : desl port map (alfa, beta, r_left);
	Udesr : desr port map (alfa, beta, r_right);
	-- zero (para o branch)
	zero <= not (r_somador(31) or r_somador(30) or r_somador(29) or r_somador(28)
				 or r_somador(27) or r_somador(26) or r_somador(25) or r_somador(24)
				 or r_somador(23) or r_somador(22) or r_somador(21) or r_somador(20)
				 or r_somador(19) or r_somador(18) or r_somador(17) or r_somador(16)
				 or r_somador(15) or r_somador(14) or r_somador(13) or r_somador(12)
				 or r_somador(11) or r_somador(10) or r_somador(9) or r_somador(8)
				 or r_somador(7) or r_somador(6) or r_somador(5) or r_somador(4)
				 or r_somador(3) or r_somador(2) or r_somador(1) or r_somador(0));
				 
	-- resultado final
	Ures: mux16Vet32 port map (x"00000000", r_somador, r_somador, r_mul(31 downto 0), r_and, r_or, r_xor,
									r_not, r_left, r_right, r_or, r_somador, x"00000000", x"00000000",
									r_somador, x"00000000", fun, gama);
end behaviour;
-- -----------------------------------------------------------------------
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;
entity  IP_code is
  port (A, B : in reg16;
		S : in reg3;
		zero : in bit;
		Z : out reg16);
end IP_code;
architecture behaviour of IP_code is
  component  mux2Vet16 is
  port(A,B : in  reg16;
     S   : in  bit;
     Z   : out reg16);
  end component  mux2Vet16;
  
  component somador16bits is
  port(A, B : in reg16;
       C : out reg16;
       vem  : in bit;
       vai  : out bit
       );
  end component somador16bits;

  component and2 is
    port(A,B : in bit; S : out bit);
  end component and2;
  
  component or2 is
    port(A,B : in bit; S : out bit);
  end component or2;
  
  signal sinal :  reg16; -- sinal = novo IP
  signal r_and, r_or : bit;
  signal inc : reg16;
begin
	-- verifica jump ou branch^zero
	Uand : and2 port map (S(0), zero, r_and);
	Uor : or2 port map (S(1), r_and, r_or);
		
	-- calcula IP+incremento
	Umux1 : mux2Vet16 port map (x"0001", x"0000", S(2), inc);
	Ua0: somador16bits port map (inc,A,sinal,'0',open);
	
	-- seleciona IP+incremento ou constante
	Ub0: mux2Vet16 port map (sinal,B,r_or,Z);
end behaviour;
-- -----------------------------------------------------------------------
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;
entity R is
  port (clk         : in  bit;
        wr_en       : in  bit;          -- ativo em 1
        r_a,r_b,r_c : in  reg4;
        A,B         : out reg32;
        C           : in  reg32);
end R;

architecture rtl of R is
 component registrador32 is
  port(rel, rst, ld: in  bit;
        D:           in  reg32;
        Q:           out reg32);
  end component registrador32;
  
 component demux16 is
   port(a : in bit;
        s : in reg4;
        z : out reg16);
  end component demux16;
  
 component mux16Vet32 is
  port(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P: in  reg32;
       S   : in  reg4;
       Z   : out reg32);
  end component mux16Vet32;

	signal r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15 : reg32; --saidas registradores
	signal rin : reg16; -- entradas registradores
begin
	-- escrever
	Udm: demux16 port map (wr_en, r_c, rin);
	
	Ur0: registrador32 port map (clk, '0', '0', C, r0);
	Ur1: registrador32 port map (clk, '1', rin(14),C, r1);
	Ur2: registrador32 port map (clk, '1', rin(13),C, r2);
	Ur3: registrador32 port map (clk, '1', rin(12),C, r3);
	Ur4: registrador32 port map (clk, '1', rin(11),C, r4);
	Ur5: registrador32 port map (clk, '1', rin(10),C, r5);
	Ur6: registrador32 port map (clk, '1', rin(9),C, r6);
	Ur7: registrador32 port map (clk, '1', rin(8),C, r7);
	Ur8: registrador32 port map (clk, '1', rin(7),C, r8);
	Ur9: registrador32 port map (clk, '1', rin(6),C, r9);
	Ur10: registrador32 port map (clk, '1', rin(5),C, r10);
	Ur11: registrador32 port map (clk, '1', rin(4),C, r11);
	Ur12: registrador32 port map (clk, '1', rin(3),C, r12);
	Ur13: registrador32 port map (clk, '1', rin(2),C, r13);
	Ur14: registrador32 port map (clk, '1', rin(1),C, r14);
	Ur15: registrador32 port map (clk, '1', rin(0),C, r15);
	
	Um1: mux16Vet32 port map (r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15, r_a,A);
	Um2: mux16Vet32 port map (r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15, r_b,B);
end rtl;
-- -----------------------------------------------------------------------

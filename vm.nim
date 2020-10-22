import os
import streams
import bitops
from endians import swapEndian16
import termios
from posix import SIGINT, onSignal

type
  Reg = enum
    R_R0, R_R1, R_R2, R_R3, R_R4, R_R5, R_R6, R_R7, R_PC, R_COND
  Opcode = enum
    OP_BR, OP_ADD, OP_LD, OP_ST, OP_JSR, OP_AND, OP_LDR, OP_STR,
    OP_RTI, OP_NOT, OP_LDI, OP_STI, OP_JMP, OP_RES, OP_LEA, OP_TRAP
  Condflag = enum
    FL_POS = 1 shl 0,
    FL_ZRO = 1 shl 1,
    FL_NEG = 1 shl 2
  Trapcode = enum
    T_GETC = 0x20, T_OUT = 0x21, T_PUTS = 0x22, T_IN = 0x23, T_PUTSP = 0x24, T_HALT = 0x25

const MEM_SIZE = high(uint16)

var
  memory: array[MEM_SIZE, uint16]
  reg: array[R_R0..R_COND, uint16]
  running = false
  original_tio: Termios

proc vm_sext(x: uint16, width: uint16): uint16 =
  if not x.testBit(width - 1):
    return x
  x or (uint16(0xFFFF) shl width)

proc vm_loadfile(filename: string) =
  var
    s = newFileStream(filename, fmRead)
    idx: uint16
    data: uint16
  if not s.atEnd:
    data = s.readUint16.uint16
    swapEndian16(addr idx, addr data)

  while not s.atEnd and idx < MEM_SIZE:
    data = s.readUint16.uint16
    swapEndian16(addr memory[idx], addr data)
    inc idx
  s.close()

proc vm_update_flags(r: uint16) =
  if reg[r.Reg] == 0:
    reg[R_COND] = FL_ZRO.uint16
  elif reg[r.Reg].testBit(15):
    reg[R_COND] = FL_NEG.uint16
  else:
    reg[R_COND] = FL_POS.uint16

proc vm_loop() =
  reg[R_PC] = 0x3000
  running = true

  var
    instr: uint16
    op: Opcode

  while running:
    instr = memory[reg[R_PC]]
    op = Opcode(instr.bitsliced(12..15))
    inc reg[R_PC]
    case op:
      of OP_BR:
        let
          n = instr.testBit(11)
          z = instr.testBit(10)
          p = instr.testBit(9)
          offset = instr.bitsliced(0..8)
        if (n and reg[R_COND] == FL_NEG.uint16) or
           (z and reg[R_COND] == FL_ZRO.uint16) or
           (p and reg[R_COND] == FL_POS.uint16):
          reg[R_PC] += vm_sext(offset, 9)

      of OP_ADD:
        let
          imm = instr.testBit(5)
          dr = instr.bitsliced(9..11)
          sr1 = instr.bitsliced(6..8)
        if imm:
          let imm_value = instr.bitsliced(0..4)
          reg[dr.Reg] = reg[sr1.Reg] + vm_sext(imm_value, 5)
        else:
          let sr2 = instr.bitsliced(0..2)
          reg[dr.Reg] = reg[sr1.Reg] + reg[sr2.Reg]
        vm_update_flags(dr)

      of OP_LD:
        let
          dr = instr.bitsliced(9..11)
          offset = instr.bitsliced(0..8)
        reg[dr.Reg] = memory[reg[R_PC] + vm_sext(offset, 9)]
        vm_update_flags(dr)

      of OP_ST:
        let
          sr = instr.bitsliced(9..11)
          offset = instr.bitsliced(0..8)
        memory[reg[R_PC] + vm_sext(offset, 9)] = reg[sr.Reg]

      of OP_JSR:
        let cond = instr.testBit(11)
        reg[R_R7] = reg[R_PC]
        if cond:
          let offset = instr.bitsliced(0..10)
          reg[R_PC] += vm_sext(offset, 11)
        else:
          let base_r = instr.bitsliced(6..8)
          reg[R_PC] = reg[base_r.Reg]

      of OP_AND:
        let
          imm = instr.testBit(5)
          dr = instr.bitsliced(9..11)
          sr1 = instr.bitsliced(6..8)
        if imm:
          let imm_value = instr.bitsliced(0..4)
          reg[dr.Reg] = reg[sr1.Reg] and vm_sext(imm_value, 5)
        else:
          let sr2 = instr.bitsliced(0..2)
          reg[dr.Reg] = reg[sr1.Reg] and reg[sr2.Reg]
        vm_update_flags(dr)

      of OP_LDR:
        let
          dr = instr.bitsliced(9..11)
          base_r = instr.bitsliced(6..8)
          offset = instr.bitsliced(0..5)
        reg[dr.Reg] = memory[reg[base_r.Reg] + vm_sext(offset, 6)]
        vm_update_flags(dr)

      of OP_STR:
        let
          sr = instr.bitsliced(9..11)
          base_r = instr.bitsliced(6..8)
          offset = instr.bitsliced(0..5)
        memory[reg[base_r.Reg] + vm_sext(offset, 6)] = reg[sr.Reg]

      of OP_NOT:
        let
          dr = instr.bitsliced(9..11)
          sr = instr.bitsliced(6..8)
        reg[dr.Reg] = not reg[sr.Reg]
        vm_update_flags(dr)

      of OP_LDI:
        let
          dr = instr.bitsliced(9..11)
          offset = instr.bitsliced(0..8)
        reg[dr.Reg] = memory[memory[reg[R_PC] + vm_sext(offset, 9)]]
        vm_update_flags(dr)

      of OP_STI:
        let
          sr = instr.bitsliced(9..11)
          offset = instr.bitsliced(0..8)
        memory[memory[reg[R_PC] + vm_sext(offset, 9)]] = reg[sr.Reg]

      of OP_JMP:
        let base_r = instr.bitsliced(6..8)
        reg[R_PC] = reg[base_r.Reg]

      of OP_LEA:
        let
          dr = instr.bitsliced(9..11)
          offset = instr.bitsliced(0..8)
        reg[dr.Reg] = reg[R_PC] + vm_sext(offset, 9)
        vm_update_flags(dr)

      of OP_TRAP:
        let tcode = Trapcode(instr.bitsliced(0..7))
        case tcode:
          of T_GETC:
            reg[R_R0] = uint16(stdin.readChar())
          of T_OUT:
            stdout.write(char(reg[R_R0]))
            flushFile(stdout)
          of T_PUTS:
            var start = reg[R_R0]
            while start < MEM_SIZE and memory[start] != 0:
              stdout.write(char(memory[start].bitsliced(0..7)))
              inc start
            flushFile(stdout)
          of T_IN:
            stdout.write("Enter a character: ")
            flushFile(stdout)
            let ch = stdin.readChar()
            stdout.write(ch)
            flushFile(stdout)
            reg[R_R0] = uint16(ch)
          of T_PUTSP:
            var start = reg[R_R0]
            while start < MEM_SIZE:
              let second = uint8(memory[start].bitsliced(0..7))
              let first = uint8(memory[start].bitsliced(8..15))
              stdout.write(char(first))
              if memory[second] == 0x00:
                break
              stdout.write(char(second))
              inc start
            flushFile(stdout)
          of T_HALT:
            stdout.write("HALT")
            flushFile(stdout)
            running = false

      else:
        stderr.writeLine "Invalid opcode"
        flushFile(stderr)
        quit(1)

proc vm_disable_input_buffering() =
  let fd = getFileHandle(stdin)
  discard fd.tcGetAttr(addr original_tio)
  var new_tio = original_tio
  new_tio.c_lflag = new_tio.c_lflag and not Cflag(ICANON or ECHO)
  discard fd.tcSetAttr(TCSANOW, addr new_tio)

proc vm_restore_input_buffering() =
  let fd = getFileHandle(stdin)
  discard fd.tcSetAttr(TCSANOW, addr original_tio)

proc main() =
  if paramCount() < 1:
    stderr.writeLine "Usage: vm <LC-3 binary file>"
    flushFile(stderr)
    quit(1)
  let filename = paramStr(1)
  if not fileExists(filename):
    stderr.writeLine "File does not exist"
    flushFile(stderr)
    quit(1)
  vm_loadfile(filename)
  vm_disable_input_buffering()
  onSignal(SIGINT):
    vm_restore_input_buffering()
    echo ""
    quit(1)
  vm_loop()
  vm_restore_input_buffering()

main()

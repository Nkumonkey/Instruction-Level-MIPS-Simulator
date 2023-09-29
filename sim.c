#include <stdio.h>
#include "shell.h"

#define OPCODE(inst) ((inst) >> 26)
#define RS(inst) (((inst) >> 21) & 0x1F)
#define RT(inst) (((inst) >> 16) & 0x1F)
#define RD(inst) (((inst) >> 11) & 0x1F)
#define TARGET(inst) ((inst) & 0x3FFFFFF)
#define IMMEDIATE(inst) ((inst) & 0xFFFF)
#define SHAMT(inst) (((inst) >> 6) & 0x1F)
#define FUNCT(inst) ((inst) & 0x3F)

#define SIGN_EXTEND(imm, bits) (((int32_t)(imm) << (32 - (bits))) >> (32 - (bits)))

#define MEM_READ(addr) mem_read_32(addr)
#define MEM_WRITE(addr, val) mem_write_32(addr, val)

void execute_R_type(uint32_t inst) {
    uint32_t funct = FUNCT(inst);
    uint32_t rs = RS(inst);
    uint32_t rt = RT(inst);
    uint32_t rd = RD(inst);
    uint32_t shamt = SHAMT(inst);

    switch (funct) {
        case 0x0: // SLL
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
            break;
        case 0x2: // SRL
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
            break;
        case 0x3: // SRA
            NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rt] >> shamt;
            break;
        case 0x4: // SLLV
            shamt = CURRENT_STATE.REGS[rs] & 0x1F;
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
            break;
        case 0x6: // SRLV
            shamt = CURRENT_STATE.REGS[rs] & 0x1F;
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
            break;
        case 0x7: // SRAV
            shamt = CURRENT_STATE.REGS[rs] & 0x1F;
            NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rt] >> shamt;
            break;
        case 0x8: // JR
            NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
            break;
        case 0x9: // JALR
            NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
            break;
        case 0x10: // MFHI
            NEXT_STATE.REGS[rd] = CURRENT_STATE.HI;
            break;
        case 0x11: // MTHI
            NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
            break;
        case 0x12: // MFLO
            NEXT_STATE.REGS[rd] = CURRENT_STATE.LO;
            break;
        case 0x13: // MTLO
            NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
            break;
        case 0x18: // MULT
            {
                int64_t lhs = (int32_t)CURRENT_STATE.REGS[rs];
                int64_t rhs = (int32_t)CURRENT_STATE.REGS[rt];
                int64_t product = lhs * rhs;
                NEXT_STATE.HI = (uint32_t)((product >> 32) & 0xFFFFFFFF);
                NEXT_STATE.LO = (uint32_t)(product & 0xFFFFFFFF);
            }
            break;
        case 0x19: // MULTU
            {
                uint64_t lhs = CURRENT_STATE.REGS[rs];
                uint64_t rhs = CURRENT_STATE.REGS[rt];
                uint64_t product = lhs * rhs;
                NEXT_STATE.HI = (uint32_t)((product >> 32) & 0xFFFFFFFF);
                NEXT_STATE.LO = (uint32_t)(product & 0xFFFFFFFF);
            }
            break;
        case 0x1A: // DIV
            {
                int32_t lhs = (int32_t)CURRENT_STATE.REGS[rs];
                int32_t rhs = (int32_t)CURRENT_STATE.REGS[rt];
                NEXT_STATE.LO = (uint32_t)(lhs / rhs);
                NEXT_STATE.HI = (uint32_t)(lhs % rhs);
            }
            break;
        case 0x1B: // DIVU
            {
                uint32_t lhs = CURRENT_STATE.REGS[rs];
                uint32_t rhs = CURRENT_STATE.REGS[rt];
                NEXT_STATE.LO = lhs / rhs;
                NEXT_STATE.HI = lhs % rhs;
            }
            break;
        case 0x20: // ADD
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
            break;
        case 0x21: // ADDU
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
            break;
        case 0x22: // SUB
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
            break;
        case 0x23: // SUBU
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
            break;
        case 0x24: // AND
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
            break;
        case 0x25: // OR
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt];
            break;
        case 0x26: // XOR
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
            break;
        case 0x27: // NOR
            NEXT_STATE.REGS[rd] = ~(CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]);
            break;
        case 0x2A: // SLT
            NEXT_STATE.REGS[rd] = ((int32_t)CURRENT_STATE.REGS[rs] < (int32_t)CURRENT_STATE.REGS[rt]) ? 1 : 0;
            break;
        case 0x2B: // SLTU
            NEXT_STATE.REGS[rd] = (CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt]) ? 1 : 0;
            break;
        default:
            printf("Unknown R-type instruction: 0x%08x\n", inst);
    }

    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}


void execute_I_type(uint32_t inst) {
    uint32_t opcode = OPCODE(inst);
    uint32_t rs = RS(inst);
    uint32_t rt = RT(inst);
 int32_t imm = SIGN_EXTEND((inst & 0xFFFF), 16);
    switch (opcode) {
        case 0x8: // ADDI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + imm;
            break;
        case 0x9: // ADDIU
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + imm;
            break;
        case 0xC: // ANDI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & (uint32_t)imm;
            break;
        case 0xD: // ORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | (uint32_t)imm;
            break;
        case 0xE: // XORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ (uint32_t)imm;
            break;
        case 0x4: // BEQ
            if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (imm << 2) + 4;
            }
            break;
        case 0x5: // BNE
            if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (imm << 2) + 4;
            }
            break;
        case 0x6: // BLEZ
            if (CURRENT_STATE.REGS[rs] <= 0) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (imm << 2) + 4;
            }
            break;
        case 0x7: // BGTZ
            if (CURRENT_STATE.REGS[rs] > 0) {
                NEXT_STATE.PC = CURRENT_STATE.PC + (imm << 2) + 4;
            }
            break;
        case 0xF: // LUI
            NEXT_STATE.REGS[rt] = (uint32_t)imm << 16;
            break;
        case 0x20: // LB
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                uint8_t byte = (uint8_t)mem_read_32(addr);
                NEXT_STATE.REGS[rt] = SIGN_EXTEND(byte, 8);
            }
            break;
        case 0x24: // LBU
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                uint8_t byte = (uint8_t)mem_read_32(addr);
                NEXT_STATE.REGS[rt] = (uint32_t)byte;
            }
            break;
        case 0x21: // LH
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                uint16_t half = (uint16_t)mem_read_32(addr);
                NEXT_STATE.REGS[rt] = SIGN_EXTEND(half, 16);
            }
            break;
        case 0x25: // LHU
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                uint16_t half = (uint16_t)mem_read_32(addr);
                NEXT_STATE.REGS[rt] = (uint32_t)half;
            }
            break;
        case 0x23: // LW
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                NEXT_STATE.REGS[rt] = mem_read_32(addr);
            }
            break;
        case 0x28: // SB
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                uint32_t val = CURRENT_STATE.REGS[rt] & 0xFF;
                mem_write_32(addr, val);
            }
            break;
        case 0x29: // SH
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                uint32_t val = CURRENT_STATE.REGS[rt] & 0xFFFF;
                mem_write_32(addr, val);
            }
            break;
        case 0x2B: // SW
            {
                int32_t addr = CURRENT_STATE.REGS[rs] + imm;
                mem_write_32(addr, CURRENT_STATE.REGS[rt]);
            }
            break;
        default:
            printf("Unknown I-type instruction: 0x%08x\n", inst);
    }

    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void execute_J_type(uint32_t inst) {
    uint32_t opcode = OPCODE(inst);
    uint32_t target = TARGET(inst);

    switch (opcode) {
        case 0x2: // J
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xF0000000) | (target << 2);
            break;
        case 0x3: // JAL
            NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xF0000000) | (target << 2);
            break;
        default:
            printf("Unknown J-type instruction: 0x%08x\n", inst);
    }
}
void execute_memory_operations(uint32_t inst) {
    uint32_t opcode = OPCODE(inst);
    uint32_t rs = RS(inst);
    uint32_t rt = RT(inst);
    int32_t imm = SIGN_EXTEND(IMMEDIATE(inst), 16);

    switch (opcode) {
        case 0x20: // LB
            NEXT_STATE.REGS[rt] = SIGN_EXTEND(MEM_READ(CURRENT_STATE.REGS[rs] + imm), 8);
            break;
        case 0x24: // LBU
            NEXT_STATE.REGS[rt] = MEM_READ(CURRENT_STATE.REGS[rs] + imm) & 0xFF;
            break;
        case 0x21: // LH
            NEXT_STATE.REGS[rt] = SIGN_EXTEND(MEM_READ(CURRENT_STATE.REGS[rs] + imm), 16);
            break;
        case 0x25: // LHU
            NEXT_STATE.REGS[rt] = MEM_READ(CURRENT_STATE.REGS[rs] + imm) & 0xFFFF;
            break;
        case 0x23: // LW
            NEXT_STATE.REGS[rt] = MEM_READ(CURRENT_STATE.REGS[rs] + imm);
            break;
        case 0x28: // SB
            MEM_WRITE(CURRENT_STATE.REGS[rs] + imm, CURRENT_STATE.REGS[rt] & 0xFF);
            break;
        case 0x29: // SH
            MEM_WRITE(CURRENT_STATE.REGS[rs] + imm, CURRENT_STATE.REGS[rt] & 0xFFFF);
            break;
        case 0x2B: // SW
            MEM_WRITE(CURRENT_STATE.REGS[rs] + imm, CURRENT_STATE.REGS[rt]);
            break;
        default:
            printf("Unknown memory operation: 0x%08x\n", inst);
    }

    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void process_instruction() {
    uint32_t inst = MEM_READ(CURRENT_STATE.PC);
   
    printf("Instruction: 0x%08x\n", inst);

    uint32_t opcode = OPCODE(inst);

    switch (opcode) {
        case 0x0: // R-type
            execute_R_type(inst);
            break;
        case 0x2:
        case 0x3: // J-type
            execute_J_type(inst);
            break;
        default: // I-type and memory operations
            if ((opcode >= 0x8 && opcode <= 0xE) || (opcode >= 0x20 && opcode <= 0x2B)) {
                execute_I_type(inst);
            } else {
                printf("Unknown instruction: 0x%08x\n", inst);
            }
            break;
    }
}

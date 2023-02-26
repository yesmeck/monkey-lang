use byteorder::{BigEndian, ByteOrder, WriteBytesExt};
use std::{collections::HashMap, fmt::Display, ops::Range};

use lazy_static::lazy_static;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Opcode {
    Constant = 0,
    Add = 1,
    Pop = 2,
    Sub = 3,
    Mul = 4,
    Div = 5,
    True = 6,
    False = 7,
    Equal = 8,
    NotEqual = 9,
    GreaterThan = 10,
    Minus = 11,
    Bang = 12,
    JumpNotTruth = 13,
    Jump = 14,
    Null = 15,
    GetGlobal = 16,
    SetGlobal = 17,
    Array = 18,
    Hash = 19,
    Index = 20,
    Call = 21,
    ReturnValue = 22,
    Return = 23,
    GetLocal = 24,
    SetLocal = 25,
}

impl Opcode {
    pub fn from(code: u8) -> Self {
        match code {
            0 => Self::Constant,
            1 => Self::Add,
            2 => Self::Pop,
            3 => Self::Sub,
            4 => Self::Mul,
            5 => Self::Div,
            6 => Self::True,
            7 => Self::False,
            8 => Self::Equal,
            9 => Self::NotEqual,
            10 => Self::GreaterThan,
            11 => Self::Minus,
            12 => Self::Bang,
            13 => Self::JumpNotTruth,
            14 => Self::Jump,
            15 => Self::Null,
            16 => Self::GetGlobal,
            17 => Self::SetGlobal,
            18 => Self::Array,
            19 => Self::Hash,
            20 => Self::Index,
            21 => Self::Call,
            22 => Self::ReturnValue,
            23 => Self::Return,
            24 => Self::GetLocal,
            25 => Self::SetLocal,
            _ => unreachable!(),
        }
    }

    pub fn make(&self, operands: Vec<u16>) -> Instructions {
        if let Some(def) = DEFINITIONS.get(self) {
            let mut instruction_len = 1;
            for w in def.1.iter() {
                instruction_len += w;
            }

            let mut instruction = Instructions(vec![]);
            instruction.0.push(self.to_owned() as u8);

            for (i, operand) in operands.iter().enumerate() {
                let width = def.1[i];
                match width {
                    1 => instruction.0.write_u8(*operand as u8).unwrap(),
                    2 => instruction.0.write_u16::<BigEndian>(*operand).unwrap(),
                    _ => unreachable!(),
                }
            }

            instruction
        } else {
            Instructions::default()
        }
    }

    pub fn definition(&self) -> &Definition {
        DEFINITIONS.get(self).unwrap()
    }
}

#[derive(Debug)]
pub struct Definition(&'static str, Vec<u64>);

lazy_static! {
    static ref DEFINITIONS: HashMap<Opcode, Definition> = {
        HashMap::from([
            (Opcode::Constant, Definition("OpConstant", vec![2])),
            (Opcode::Add, Definition("OpAdd", vec![])),
            (Opcode::Sub, Definition("OpSub", vec![])),
            (Opcode::Mul, Definition("OpMul", vec![])),
            (Opcode::Div, Definition("OpDiv", vec![])),
            (Opcode::Pop, Definition("OpPop", vec![])),
            (Opcode::True, Definition("OpTrue", vec![])),
            (Opcode::False, Definition("OpFalse", vec![])),
            (Opcode::Equal, Definition("OpEqual", vec![])),
            (Opcode::NotEqual, Definition("OpNotEqual", vec![])),
            (Opcode::GreaterThan, Definition("OpGreaterThan", vec![])),
            (Opcode::Minus, Definition("OpMinus", vec![])),
            (Opcode::Bang, Definition("OpBang", vec![])),
            (Opcode::JumpNotTruth, Definition("OpJumpNotTruth", vec![2])),
            (Opcode::Jump, Definition("OpJump", vec![2])),
            (Opcode::Null, Definition("OpNull", vec![])),
            (Opcode::GetGlobal, Definition("OpGetGlobal", vec![2])),
            (Opcode::SetGlobal, Definition("OpSetGlobal", vec![2])),
            (Opcode::Array, Definition("OpArray", vec![2])),
            (Opcode::Hash, Definition("OpHash", vec![2])),
            (Opcode::Index, Definition("OpIndex", vec![])),
            (Opcode::Call, Definition("OpCall", vec![])),
            (Opcode::ReturnValue, Definition("OpReturnValue", vec![])),
            (Opcode::GetLocal, Definition("OpGetLocal", vec![1])),
            (Opcode::SetLocal, Definition("OpSetLocal", vec![1])),
        ])
    };
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn take_range(&self, range: Range<usize>) -> Self {
        Self(self.0[range].to_vec())
    }

    pub fn append(&mut self, other: &mut Instructions) {
        self.0.append(&mut other.0)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn read_u8_from(&self, pos: usize) -> u8 {
        self.0[pos]
    }

    pub fn read_u16_from(&self, pos: usize) -> u16 {
        BigEndian::read_u16(&self.0[pos..])
    }

    pub fn read_op_at(&self, pos: usize) -> Opcode {
        Opcode::from(self.0[pos])
    }

    pub fn read_operands(&self, def: &Definition) -> (Vec<u16>, usize) {
        let mut operands = vec![0; def.1.len()];

        let mut offset = 0;
        for (i, width) in def.1.iter().enumerate() {
            match width {
                1 => operands[i] = self.0[offset + 1] as u16,
                2 => operands[i] = BigEndian::read_u16(&self.0[offset + 1..]),
                _ => unreachable!(),
            }
            offset += width.to_owned() as usize;
        }

        (operands, offset)
    }

    fn fmt_instruction(&self, def: &Definition, operands: &Vec<u16>) -> String {
        let operand_count = def.1.len();
        if operands.len() != operand_count {
            format!(
                "ERROR: operand len {} does not match defined {}",
                operands.len(),
                operand_count
            )
        } else {
            match operand_count {
                0 => def.0.to_owned(),
                1 => format!("{} {}", def.0, operands[0]),
                _ => format!("ERROR: unhandled operandCount for {}", def.0),
            }
        }
    }
}

impl From<Vec<Instructions>> for Instructions {
    fn from(value: Vec<Instructions>) -> Self {
        Self(value.iter().flat_map(|i| i.0.to_owned()).collect())
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();
        let mut i = 0;
        while i < self.0.len() {
            let op = Opcode::from(self.0[i]);
            let def = op.definition();
            let (operands, read) = &self
                .take_range(Range {
                    start: i,
                    end: self.0.len(),
                })
                .read_operands(def);
            out.push_str(&format!(
                "{:04} {}\n",
                i,
                self.fmt_instruction(def, operands)
            ));
            i += 1 + read;
        }
        write!(f, "{}", out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instructions_string() {
        let instructions = [
            Opcode::Add.make(vec![]),
            Opcode::GetLocal.make(vec![1]),
            Opcode::Constant.make(vec![2]),
            Opcode::Constant.make(vec![65535]),
        ];
        let expected = "0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
";
        let concated = Instructions(instructions.iter().flat_map(|i| i.0.to_owned()).collect());
        assert_eq!(format!("{}", concated), expected);
    }

    #[test]
    fn test_make() {
        let tests = [
            (
                Opcode::Constant,                                       // op
                vec![65534],                                            // operands
                Instructions(vec![Opcode::Constant as u8, 0xFF, 0xFE]), // expected
            ),
            (Opcode::Add, vec![], Instructions(vec![Opcode::Add as u8])),
            (
                Opcode::GetLocal,
                vec![255],
                Instructions(vec![Opcode::GetLocal as u8, 0xFF]),
            ),
        ];

        for test in tests.iter() {
            let (op, operands, expected) = test;
            let instruction = op.make(operands.to_owned());
            assert_eq!(instruction, *expected);
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = [
            (Opcode::Constant, vec![65534], 2),
            (Opcode::GetLocal, vec![255], 1),
        ];

        for (op, operands, bytes_read) in tests.iter() {
            let instruction = op.make(operands.to_owned());
            let def = op.definition();
            let (operands_read, offset) = instruction.read_operands(def);

            assert_eq!(&offset, bytes_read);
            assert_eq!(&operands_read, operands);
        }
    }
}

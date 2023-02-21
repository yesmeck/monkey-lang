use byteorder::{BigEndian, ByteOrder, WriteBytesExt};
use std::{collections::HashMap, fmt::Display, ops::Range};

use lazy_static::lazy_static;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum OpCode {
    OpConstant = 0,
    OpAdd = 1,
    OpPop = 2,
    OpSub = 3,
    OpMul = 4,
    OpDiv = 5,
    OpTrue = 6,
    OpFalse = 7,
    OpEqual = 8,
    OpNotEqual = 9,
    OpGreaterThan = 10,
    OpMinus = 11,
    OpBang = 12,
    NoOp,
}

impl OpCode {
    pub fn from(code: u8) -> Self {
        match code {
            0 => Self::OpConstant,
            1 => Self::OpAdd,
            2 => Self::OpPop,
            3 => Self::OpSub,
            4 => Self::OpMul,
            5 => Self::OpDiv,
            6 => Self::OpTrue,
            7 => Self::OpFalse,
            8 => Self::OpEqual,
            9 => Self::OpNotEqual,
            10 => Self::OpGreaterThan,
            11 => Self::OpMinus,
            12 => Self::OpBang,
            _ => Self::NoOp,
        }
    }
}

#[derive(Debug)]
pub struct Definition(&'static str, Vec<u64>);

lazy_static! {
    static ref DEFINITIONS: HashMap<OpCode, Definition> = {
        HashMap::from([
            (OpCode::OpConstant, Definition("OpConstant", vec![2])),
            (OpCode::OpAdd, Definition("OpAdd", vec![])),
            (OpCode::OpSub, Definition("OpSub", vec![])),
            (OpCode::OpMul, Definition("OpMul", vec![])),
            (OpCode::OpDiv, Definition("OpDiv", vec![])),
            (OpCode::OpPop, Definition("OpPop", vec![])),
            (OpCode::OpTrue, Definition("OpTrue", vec![])),
            (OpCode::OpFalse, Definition("OpFalse", vec![])),
            (OpCode::OpEqual, Definition("OpEqual", vec![])),
            (OpCode::OpNotEqual, Definition("OpNotEqual", vec![])),
            (OpCode::OpGreaterThan, Definition("OpGreaterThan", vec![])),
            (OpCode::OpMinus, Definition("OpMinus", vec![])),
            (OpCode::OpBang, Definition("OpBang", vec![])),
        ])
    };
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    fn take_range(&self, range: Range<usize>) -> Self {
        Self(self.0[range].to_vec())
    }

    pub fn append(&mut self, other: &mut Instructions) {
        self.0.append(&mut other.0)
    }

    pub fn len(&self) -> usize {
        self.0.len()
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
                0 => format!("{}", def.0),
                1 => format!("{} {}", def.0, operands[0]),
                _ => format!("ERROR: unhandled operandCount for {}", def.0),
            }
        }
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();
        let mut i = 0;
        while i < self.0.len() {
            if let Some(def) = lookup(&OpCode::from(self.0[i])) {
                let (operands, read) = read_operands(
                    def,
                    &self.take_range(Range {
                        start: i,
                        end: self.0.len(),
                    }),
                );
                out.push_str(&format!(
                    "{:04} {}\n",
                    i,
                    self.fmt_instruction(def, &operands)
                ));
                i += 1 + read;
            }
        }
        write!(f, "{}", out)
    }
}

fn lookup(op: &OpCode) -> Option<&Definition> {
    DEFINITIONS.get(op)
}

pub fn read_operands(def: &Definition, ins: &Instructions) -> (Vec<u16>, usize) {
    let mut operands = vec![0; def.1.len()];

    let mut offset = 0;
    for (i, width) in def.1.iter().enumerate() {
        match width {
            2 => operands[i] = BigEndian::read_u16(&ins.0[offset + 1..]), // first byte is op
            _ => {}
        }
        offset += width.to_owned() as usize;
    }

    (operands, offset)
}

pub fn make(op: OpCode, operands: Vec<u16>) -> Instructions {
    if let Some(def) = DEFINITIONS.get(&op) {
        let mut instruction_len = 1;
        for w in def.1.iter() {
            instruction_len += w;
        }

        let mut instruction = Instructions(vec![]);
        instruction.0.push(op.to_owned() as u8);

        for (i, operand) in operands.iter().enumerate() {
            let width = def.1[i];
            match width {
                2 => instruction.0.write_u16::<BigEndian>(*operand).unwrap(),
                _ => {}
            }
        }

        instruction
    } else {
        Instructions::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instructions_string() {
        let instructions = [
            make(OpCode::OpAdd, vec![]),
            make(OpCode::OpConstant, vec![2]),
            make(OpCode::OpConstant, vec![65535]),
        ];
        let expected = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
";
        let concated = Instructions(instructions.iter().flat_map(|i| i.0.to_owned()).collect());
        assert_eq!(format!("{}", concated), expected);
    }

    #[test]
    fn test_make() {
        let tests = [
            (
                OpCode::OpConstant,                                       // op
                vec![65534 as u16],                                       // operands
                Instructions(vec![OpCode::OpConstant as u8, 0xFF, 0xFE]), // expected
            ),
            (
                OpCode::OpAdd,
                vec![],
                Instructions(vec![OpCode::OpAdd as u8]),
            ),
        ];

        for test in tests.iter() {
            let (op, operands, expected) = test;
            let instruction = make(op.to_owned(), operands.to_owned());
            assert_eq!(instruction, *expected);
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = [(OpCode::OpConstant, vec![65534], 2)];

        for (op, operands, bytes_read) in tests.iter() {
            let instruction = make(op.to_owned(), operands.to_owned());
            if let Some(def) = lookup(op) {
                let (operands_read, offset) = read_operands(def, &instruction);

                assert_eq!(&offset, bytes_read);
                assert_eq!(&operands_read, operands);
            } else {
                panic!("definition not found");
            }
        }
    }
}

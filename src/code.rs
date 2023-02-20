use byteorder::{BigEndian, ByteOrder, WriteBytesExt};
use std::{collections::HashMap, fmt::Display, ops::Range};

use lazy_static::lazy_static;

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
        let operand_count = def.operand_width.len();
        if operands.len() != operand_count {
            format!(
                "ERROR: operand len {} does not match defined {}",
                operands.len(),
                operand_count
            )
        } else {
            match operand_count {
                0 => format!("{}", def.name),
                1 => format!("{} {}", def.name, operands[0]),
                _ => format!("ERROR: unhandled operandCount for {}", def.name),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum OpCode {
    OpConstant = 0,
    OpAdd = 1,
    NoOp,
}

impl OpCode {
    pub fn from(code: u8) -> Self {
        match code {
            0 => Self::OpConstant,
            1 => Self::OpAdd,
            _ => Self::NoOp,
        }
    }
}

#[derive(Debug)]
pub struct Definition {
    name: &'static str,
    operand_width: Vec<u64>,
}

lazy_static! {
    static ref DEFINITIONS: HashMap<OpCode, Definition> = {
        HashMap::from([
            (
                OpCode::OpConstant,
                Definition {
                    name: "OpConstant",
                    operand_width: vec![2],
                },
            ),
            (
                OpCode::OpAdd,
                Definition {
                    name: "OpAdd",
                    operand_width: vec![],
                },
            ),
        ])
    };
}

fn lookup(op: &OpCode) -> Option<&Definition> {
    DEFINITIONS.get(op)
}

pub fn read_operands(def: &Definition, ins: &Instructions) -> (Vec<u16>, usize) {
    let mut operands = vec![0; def.operand_width.len()];

    let mut offset = 0;
    for (i, width) in def.operand_width.iter().enumerate() {
        match width {
            2 => operands[i] = BigEndian::read_u16(&ins.0[offset + 1..]), // first byte is op
            _ => {}
        }
        offset += width.to_owned() as usize;
    }

    (operands, offset)
}

pub fn make(op: &OpCode, operands: Vec<u16>) -> Instructions {
    if let Some(def) = DEFINITIONS.get(&op) {
        let mut instruction_len = 1;
        for w in def.operand_width.iter() {
            instruction_len += w;
        }

        let mut instruction = Instructions(vec![]);
        instruction.0.push(op.to_owned() as u8);

        for (i, operand) in operands.iter().enumerate() {
            let width = def.operand_width[i];
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
            make(&OpCode::OpAdd, vec![]),
            make(&OpCode::OpConstant, vec![2]),
            make(&OpCode::OpConstant, vec![65535]),
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
            let instruction = make(op, operands.to_owned());
            assert_eq!(instruction, *expected);
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = [(OpCode::OpConstant, vec![65534], 2)];

        for (op, operands, bytes_read) in tests.iter() {
            let instruction = make(op, operands.to_owned());
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

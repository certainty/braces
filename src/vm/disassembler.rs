use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::OpCode;
use super::printer;
use super::{byte_code, value::symbol::InternedSymbol};
use std::io::Write;

pub fn disassemble<W: Write>(out: &mut W, chunk: &Chunk, context: &str) {
    let mut address: usize = 0;

    out.write_fmt(format_args!("== {} ==\n", context)).unwrap();

    while address < chunk.code.len() {
        address = disassemble_instruction(out, chunk, address);
    }
    out.write("\n".as_bytes()).unwrap();
}

pub fn disassemble_instruction<W: Write>(out: &mut W, chunk: &Chunk, address: usize) -> usize {
    if let Some((begin, _, _)) = chunk.find_line(address) {
        if address > 0 && begin <= address - 1 {
            out.write_all("   | ".as_bytes()).unwrap();
        } else {
            out.write_fmt(format_args!("{:04} ", address)).unwrap();
        }
    } else {
        out.write_fmt(format_args!("{:04} ", address)).unwrap();
    }

    match &chunk.code[address] {
        &OpCode::Halt => disassemble_simple(out, "OP_HALT", address),
        &OpCode::Const(const_address) => {
            disassemble_constant(out, chunk, "OP_CONST", address, const_address)
        }
        &OpCode::Get => disassemble_simple(out, "OP_GET", address),
        &OpCode::Sym(interned) => disassemble_symbol(out, chunk, "OP_SYM", address, interned),
        &OpCode::Nop => disassemble_simple(out, "OP_NOP", address),
        &OpCode::Apply => disassemble_simple(out, "OP_APPLY", address),
    }
}

fn disassemble_simple<W: Write>(out: &mut W, name: &str, address: usize) -> usize {
    out.write_fmt(format_args!("{}\n", name)).unwrap();
    address + 1
}

fn disassemble_symbol<W: Write>(
    out: &mut W,
    chunk: &Chunk,
    name: &str,
    address: AddressType,
    interned: InternedSymbol,
) -> usize {
    out.write_fmt(format_args!(
        "{:<16} {:04}  '{}'\n",
        name,
        interned.0,
        printer::print(&interned, &chunk.symbols)
    ))
    .unwrap();

    address + 1
}

fn disassemble_constant<W: Write>(
    out: &mut W,
    chunk: &Chunk,
    name: &str,
    address: AddressType,
    constant_address: byte_code::ConstAddressType,
) -> usize {
    out.write_fmt(format_args!(
        "{:<16} {:04}  '{}'\n",
        name,
        constant_address,
        printer::print(&chunk.constants[constant_address as usize], &chunk.symbols)
    ))
    .unwrap();

    address + 1
}

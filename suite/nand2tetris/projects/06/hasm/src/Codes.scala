package hasm

trait Codes[O]:
  def fromNumber(number: Int): O
  def fromA(address: Address): O
  def fromC(instruction: Instruction.C): O

object Codes:
  inline def apply[O: Codes] = summon[Codes[O]]

  extension (str: String) 
    def padLeftTo(len: Int, elem: Char): String = elem.toString * (len - str.size) + str

import Codes.*

given Codes[String] with
  def fromNumber(number: Int): String = 
    number.toBinaryString.padLeftTo(16, '0')
  def fromA(address: Address): String =
    s"0${address.asBinaryString.padLeftTo(15, '0')}"

  def fromC(instruction: Instruction.C): String =
    // use M in comparison
    def a = if instruction.comp.contains("M") then "1" else "0"

    def comp(field: String): String =
      field match
        case "0"           => "101010"
        case "1"           => "111111"
        case "-1"          => "111010"
        case "D"           => "001100"
        case "A" | "M"     => "110000"
        case "!D"          => "001101"
        case "!A" | "!M"   => "110001"
        case "-D"          => "001111"
        case "-A" | "-M"   => "110011"
        case "D+1"         => "011111"
        case "A+1" | "M+1" => "110111"
        case "D-1"         => "001110"
        case "A-1" | "M-1" => "110010"
        case "D+A" | "D+M" => "000010"
        case "D-A" | "D-M" => "010011"
        case "A-D" | "M-D" => "000111"
        case "D&A" | "D&M" => "000000"
        case "D|A" | "D|M" => "010101"

    def dest(field: Option[String]): String =
      field.fold("000"):
        case "M"   => "001"
        case "D"   => "010"
        case "MD"  => "011"
        case "A"   => "100"
        case "AM"  => "101"
        case "AD"  => "110"
        case "AMD" => "111"

    def jump(field: Option[String]): String =
      field.fold("000"):
        case "JGT" => "001"
        case "JEQ" => "010"
        case "JGE" => "011"
        case "JLT" => "100"
        case "JNE" => "101"
        case "JLE" => "110"
        case "JMP" => "111"

    s"111${a}${comp(instruction.comp)}${dest(instruction.dest)}${jump(instruction.jump)}"

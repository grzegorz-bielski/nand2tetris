package hasm

import scala.util.chaining.*

object HASM:
  type Result[Out] = Either[Vector[Error], Vector[Out]]

  def assembleToString(source: String, dest: String): Unit =
    Parser.parseAll(os.Path(s"$source.asm")).pipe(assemble[String]) match
      case Left(errors) =>
        println(s"Could not assemble the program: $errors")
      case Right(outputLines) =>
        os.write.over(os.Path(s"$dest.hack"), outputLines.mkString("\n"))

  def assemble[Out: Codes](sourceLines: Vector[Parser.Result]): HASM.Result[Out] =
    val errors = sourceLines.collect:
      case err: Error => err

    if errors.nonEmpty then Left(errors)
    else
      val instructions = sourceLines.collect:
        case inst: Instruction => inst

      stageOne(instructions) pipe stageTwo(instructions) pipe (_.outputLines) pipe Right.apply

  private def stageOne(instructions: Vector[Instruction]) =
    instructions.foldLeft(StageOneState.initial): (state, instruction) =>
      instruction match
        case Instruction.L(symbol) => state.addLabelSymbol(symbol)
        case _                     => state.incrementLine

  private def stageTwo[Out: Codes](instructions: Vector[Instruction])(stageOne: StageOneState) =
    instructions.foldLeft(StageTwoState.fromStageOne[Out](stageOne)): (state, instruction) =>
      instruction match
        case Instruction.A(symbol) =>
          symbol.toIntOption match
            case Some(value) => state.appendOutputLine(Codes[Out].fromNumber(value))
            case None =>
              state.symbolTable.get(symbol) match
                case None =>
                  state
                    .addVariableSymbol(symbol)
                    .appendOutputLine(Codes[Out].fromA(state.lastVariableAddress))
                case Some(value) => state.appendOutputLine(Codes[Out].fromA(value))
        case i: Instruction.C => state.appendOutputLine(Codes[Out].fromC(i))
        case _                => state

  private final case class StageOneState(
      symbolTable: Map[String, Address],
      labelAddress: Address
  ):
    def incrementLine = copy(labelAddress = labelAddress.next)
    def addLabelSymbol(symbol: String) =
      copy(symbolTable = symbolTable + (symbol -> labelAddress))

  private object StageOneState:
    def initial: StageOneState =
      val defaultSymbols = (0 to 15).map(i => s"R$i" -> Address(i)).toMap ++
        Map(
          "SP" -> Address(0),
          "LCL" -> Address(1),
          "ARG" -> Address(2),
          "THIS" -> Address(3),
          "THAT" -> Address(4),
          "SCREEN" -> Address(16384),
          "KBD" -> Address(24576)
        )

      StageOneState(defaultSymbols, Address.initial)

  private final case class StageTwoState[O](
      symbolTable: Map[String, Address],
      outputLines: Vector[O],
      lastVariableAddress: Address
  ):
    def appendOutputLine(line: O) = copy(outputLines = outputLines :+ line)
    def addVariableSymbol(symbol: String) =
      copy(
        symbolTable = symbolTable + (symbol -> lastVariableAddress),
        lastVariableAddress = lastVariableAddress.next
      )

  private object StageTwoState:
    def fromStageOne[O](stageOne: StageOneState): StageTwoState[O] =
      StageTwoState(stageOne.symbolTable, Vector.empty, Address(16))

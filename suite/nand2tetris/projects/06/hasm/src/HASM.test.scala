package hasm

class HASMSpec extends munit.FunSuite:
  val root = os.pwd / os.up / os.up

  test("should assemble programs without symbols correctly"):
    assertEquals(
      assembleAt(root / "add" / "Add.asm"),
      expectedAt(root / "add" / "Add.hack")
    )

    assertEquals(
      assembleAt(root / "max" / "MaxL.asm"),
      expectedAt(root / "max" / "MaxL.hack")
    )

    assertEquals(
      assembleAt(root / "rect" / "RectL.asm"),
      expectedAt(root / "rect" / "RectL.hack")
    )

    assertEquals(
      assembleAt(root / "pong" / "PongL.asm"),
      expectedAt(root / "pong" / "PongL.hack")
    )

  test("should assemble programs with symbols correctly"):
    assertEquals(
      assembleAt(root / "max" / "Max.asm"),
      expectedAt(root / "max" / "Max.hack")
    )

    assertEquals(
      assembleAt(root / "rect" / "Rect.asm"),
      expectedAt(root / "rect" / "Rect.hack")
    )

    assertEquals(
      assembleAt(root / "pong" / "Pong.asm"),
      expectedAt(root / "pong" / "Pong.hack")
    )

  def assembleAt(path: os.Path) =
    HASM.assemble[String](Parser.parseAll(path))

  def expectedAt(path: os.Path) = 
    Right(os.read.lines(path).toVector)

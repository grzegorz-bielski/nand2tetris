package hvm

class HVMSpec extends munit.FunSuite:
  test("should translate programs to the Hack assembly correctly"):
    val root = os.pwd / os.up / os.up

    assertEquals(
      translateAt(root / "MemoryAccess" / "BasicTest" / "BasicTest.vm"),
      expectedAt(root / "MemoryAccess" / "BasicTest" / "BasicTest.asm")
    )

    assertEquals(
      translateAt(root / "MemoryAccess" / "PointerTest" / "PointerTest.vm"),
      expectedAt(root / "MemoryAccess" / "PointerTest" / "PointerTest.asm")
    )

    assertEquals(
      translateAt(root / "MemoryAccess" / "StaticTest" / "StaticTest.vm"),
      expectedAt(root / "MemoryAccess" / "StaticTest" / "StaticTest.asm")
    )

    assertEquals(
      translateAt(root / "StackArithmetic" / "SimpleAdd" / "SimpleAdd.vm"),
      expectedAt(root / "StackArithmetic" / "SimpleAdd" / "SimpleAdd.asm")
    )

    assertEquals(
      translateAt(root / "StackArithmetic" / "StackTest" / "StackTest.vm"),
      expectedAt(root / "StackArithmetic" / "StackTest" / "StackTest.asm")
    )

  def translateAt(path: os.Path) =
    HVM.translate(path.last, Parser.parseAll(path)).map(_.mkString("\n"))

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).toVector.mkString("\n"))

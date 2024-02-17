package jackc

trait JackcSpec extends munit.FunSuite:
    // project 10 root
    val `10` = os.pwd / os.up / os.up

    // project 11 root
    val `11` = `10` / os.up / "11"
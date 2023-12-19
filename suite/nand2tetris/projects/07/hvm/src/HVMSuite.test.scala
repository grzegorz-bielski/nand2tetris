package hvm

trait HVMSuite extends munit.FunSuite:
    // project 7 root
    val `07` = os.pwd / os.up / os.up
    
    // project 8 root
    val `08` = os.pwd / os.up / os.up / os.up / "08"
import Test.HUnit
import Args

main = runTestTT tests


tests = TestList [TestLabel "Args processor will process two args" testCaseWillProcessTwoArgs]


testCaseWillProcessTwoArgs =
    let args = ["a", "b"]
        flags = Args.processArgs args
    in assertBool $ Args.isValidConfig flags



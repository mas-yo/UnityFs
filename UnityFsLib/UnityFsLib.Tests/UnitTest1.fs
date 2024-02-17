module UnityFsLib.Tests

open NUnit.Framework
open UnityFsLib.GameLogic

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestCalcWaitingTime () =
    let (WaitingTime result) = calcWaitingTime (WaitingTime 10) (Quickness 10)
    Assert.AreEqual(9, result)
    
    let (WaitingTime result) = calcWaitingTime (WaitingTime 0) (Quickness 10)
    Assert.AreEqual(10, result)


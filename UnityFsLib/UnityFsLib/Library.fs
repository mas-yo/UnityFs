namespace UnityFsLib

module Say =
    let hello name =
        let namePlus = name + "world"
        printfn "Hello %s" namePlus
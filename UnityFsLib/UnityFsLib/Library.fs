namespace UnityFsLib

open System;
open UnityEngine

module Say =
    let hello name =
        let namePlus = name + "world"
        printfn "Hello %s" namePlus
        
        
        
module Move =
    let updateRadian (deltaTime: float32, currentRadian: float32) =
        currentRadian + deltaTime
        
    let getX (radian: float32, radius: float32) =
        float32 (float radius * Math.Sin(float radian))

    let getY (radian: float32, radius: float32) =
        float32 (float radius * Math.Cos(float radian))
        
    let move (position:Vector3, radian:float32) =
        let x = getX(radian, 5.0f);
        let y = getY(radian, 5.0f);
        Vector3(x, y, position.z)
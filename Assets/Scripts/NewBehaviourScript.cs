using System.Collections;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityFsLib;
using Vector2 = System.Numerics.Vector2;

public class NewBehaviourScript : MonoBehaviour
{
    private Transform _transform;
    public ECS.World World = new ECS.World();

    private float _currentRadian = 0;
    // Start is called before the first frame update
    void Start()
    {

        _transform = GetComponent<Transform>();
        
        World.Positions = new FSharpList<ECS.Component<Vector2>>()
    }

    // Update is called once per frame
    void Update()
    {
        // _currentRadian = Move.updateRadian(Time.deltaTime, _currentRadian);
        // _transform.position = Move.move(_transform.position, _currentRadian);
        var a = Move.move(Time.deltaTime, _currentRadian);
        _transform.position = new Vector3(a.x, a.y, _transform.position.z);
        _currentRadian = a.radian;
        Physics.Raycast()

    }
}

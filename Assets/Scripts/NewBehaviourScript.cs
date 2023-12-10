using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityFsLib;

public class NewBehaviourScript : MonoBehaviour
{
    private Transform _transform;

    private float _currentRadian = 0;
    // Start is called before the first frame update
    void Start()
    {

        _transform = GetComponent<Transform>();
    }

    // Update is called once per frame
    void Update()
    {
        _currentRadian = Move.updateRadian(Time.deltaTime, _currentRadian);
        _transform.position = Move.move(_transform.position, _currentRadian);
    }
}

using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityFsLib;
using Vector2 = System.Numerics.Vector2;

public class NewBehaviourScript : MonoBehaviour
{
    
    [SerializeField]
    public GameObject _boxPrefab;

    private int _nextEntityId = 4;
    private GameLogic.World _world;
    private Dictionary<EntityComponent.EntityId, GameObject> _gameObjects = new Dictionary<EntityComponent.EntityId, GameObject>();

    void Start()
    {
        _world = GameLogic.NewWorld;
        _gameObjects.Add(EntityComponent.EntityId.NewEntityId(1), GameObject.Find("1"));
        _gameObjects.Add(EntityComponent.EntityId.NewEntityId(2), GameObject.Find("2"));
        _gameObjects.Add(EntityComponent.EntityId.NewEntityId(3), GameObject.Find("3"));
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetMouseButtonUp(0))
        {
            var plane = new Plane(Vector3.up, 0);
            var mousePos = Input.mousePosition;
            var ray = Camera.main.ScreenPointToRay(mousePos);
            if (plane.Raycast(ray, out float enter))
            {
                var p = ray.GetPoint(enter);
                _world = GameLogic.AddEntity(
                    EntityComponent.EntityId.NewEntityId(_nextEntityId), 
                    GameLogic.Position.NewPosition(new Vector2(p.x, p.z)), 
                    Random.Range(0.001f, 0.02f),
                    Random.Range(10, 300),
                    _world);

                var newObject = Instantiate(_boxPrefab, p, Quaternion.identity);
                _gameObjects.Add(EntityComponent.EntityId.NewEntityId(_nextEntityId), newObject);

                _nextEntityId++;
            }
        }
        
        _world = GameLogic.Update(_world);

        foreach (var pos in _world.CurrentPositions)
        {
            var obj = _gameObjects[pos.EntityId];
            obj.transform.position = new Vector3()
            {
                x = pos.Value.Item.X,
                y = obj.transform.position.y,
                z = pos.Value.Item.Y,
            };
        }
    }
}

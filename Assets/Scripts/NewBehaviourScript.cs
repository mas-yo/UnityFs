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
    private GameLogic.World _world;
    private Dictionary<EntityComponent.EntityId, GameObject> _gameObjects = new Dictionary<EntityComponent.EntityId, GameObject>();

    void Start()
    {
        _world = GameLogic.NewWorld;
        _gameObjects.Add(EntityComponent.EntityId.NewEntityId(1), GameObject.Find("1"));
        _gameObjects.Add(EntityComponent.EntityId.NewEntityId(2), GameObject.Find("2"));
    }

    // Update is called once per frame
    void Update()
    {
        _world = GameLogic.Update(_world);

        foreach (var (eid, go) in _gameObjects)
        {
            var pos = _world.CurrentPositions.First(x => x.EntityId.Equals(eid));
            go.transform.position = new Vector3()
            {
                x = pos.Value.Item.X,
                y = go.transform.position.y,
                z = pos.Value.Item.Y,
            };
        }
    }
}

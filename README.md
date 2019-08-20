# Blender to Webots exporter add-on

Blender add-on to export your Blender project to Webots.

![Demo](demo.gif)

## Features

- Export your Blender project to Webots
- Define conversion rules in a JSON file
- Support collision export (either generated from the AABB boxes or manually defined)
- Support joint export (linear or rotational with custom parameters including axis, limits, etc.)
- Support the export of any [Webots Solid (or derived) node](https://www.cyberbotics.com/doc/reference/jointparameters) with custom fields (`Camera.width`, `name`, etc.).
- Basic support of materials (colors and the base color map) to Webots PBR appearance.

## Requirements

- [Blender 2.79b](https://www.blender.org)
- [Webots R2019a.rev1](https://www.cyberbotics.com) or later

## Installation and Update

Let say `$BLENDER_ADD_ON_PATH` is the Blender add-on user directory:

- on macOS: `export BLENDER_ADD_ON_PATH=$HOME/Library/Application\ Support/Blender/2.79/scripts/addons`
- on linux: `export BLENDER_ADD_ON_PATH=$HOME/.config/blender/2.79/scripts/addons`

Install the add-on by applying the following commands:

```
mkdir -p $BLENDER_ADD_ON_PATH/export_webots
cp __init__.py "$BLENDER_ADD_ON_PATH/export_webots"
cp export_webots.py "$BLENDER_ADD_ON_PATH/export_webots"
```

Enable the "Webots exporter" add-on in `Blender / Preferences / Add-ons / Community`.

## JSON Conversion File Specifications

The [JSON](https://en.wikipedia.org/wiki/JSON) conversion file specifies how to convert the Blender nodes to Webots ones.

If not specified in the parameters, the JSON conversion file is searched in the same path as the `.blend` file.

It is basically a JSON Object (i.e. key-value associative array) containing a conversion rule for each node.
During the exportation, when a key of this JSON Object matches with the slugified Blender node name, then its value (a JSON Object) is used for the conversion.

The conversion JSON Object could contain the following conversion rules:

| Key | Flags | Value |
| --- | --- | --- |
| `target node` | _required_ | JSON String defining to which Webots node the Blender node should be converted. For example, it could be `Solid`, `Robot`, `Camera`, `HingeJoint`, `SliderJoint` or a custom Webots appearance node (like `BrushedAluminium`). |
| `fields` | _optional_ | JSON Object which can contain any Webots node fields. The conversion tool will add these fields as-is. It is convenient to add device specific fields, like `Camera.width`, `Emitter.name`, etc. |

The following conversion rules are only available in the case of a `Solid` node (or derived):

| Key | Flags | Value |
| --- | --- | --- |
| `physics` | _optional_ | JSON Object which can contain the [Webots Physics node](https://www.cyberbotics.com/doc/reference/physics) fields (like `mass`, `density` and `centerOfMass`). If this key is not defined, then no `Physics` node is generated (the Solid can be pinned to the static environment, or controlled using kinematics rules.) |
| `boundingObject` | _optional_ | JSON Object which can contain either a JSON String called `custom` to define the content of the [Webots Solid.boundingObject field](https://www.cyberbotics.com/doc/reference/solid) fields. If `custom` is not defined, then the AABB box of the Blender object is used to create the boundingObject. If this key is not defined, then no collision object is generated. |

The following conversion rules are only available in the case of a `Joint` node:

| Key | Flags | Value |
| --- | --- | --- |
| `jointParameters` | _optional_ | JSON Object which can contain the [Webots (Hinge)JointParameters node](https://www.cyberbotics.com/doc/reference/jointparameters) fields (like `axis`, `spring/dampingConstant` and `suspension*`). |
| `motor` | _optional_ | JSON Object which can contain the [Webots LinearMotor or RotationalMotor node](https://www.cyberbotics.com/doc/reference/rotationalmotor) fields (like `name`, `maxTorque`, `maxPosition` or `maxVelocity`) |
| `positionSensor` | _optional_ | JSON Object which can contain the [Webots PositionSensor node](https://www.cyberbotics.com/doc/reference/positionsensor) fields (like `name`) |

Examples:

https://github.com/cyberbotics/blender-webots-exporter/tree/master/examples


## References

This script has been inspired by the [Blender X3D exporter](https://github.com/sobotka/blender-addons/blob/master/io_scene_x3d/export_x3d.py) of Campbell Barton, Bart, Bastien Montagne, Seva Alekseyev.

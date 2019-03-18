# Blender to Webots exporter plugin

Blender plugin to export to Webots files.

## Features

- Export your Blender project to Webots.
- Define Webots-specific data in a JSON file.
- Supports collisions (AABB-based boxes or custom ones)
- Supports joints (linear or rotational with custom axis)
- Supports any Webots Solid nodes and extra-parameters.

## Requirements

Tested on macOS and Linux for Blender 2.79.

## Installation and Update

Let say `$BLENDER_ADD_ON_PATH` is the Blender add-on user directory:
- on macOS: `export BLENDER_ADD_ON_PATH=$HOME/Library/Application\ Support/Blender/2.79/scripts/addons`
- on linux: `export BLENDER_ADD_ON_PATH=$HOME/.config/blender/2.76/scripts/addons`

Install the plugin by applying the following commands:

```
mkdir -p $BLENDER_ADD_ON_PATH/export_webots
cp __init__.py "$BLENDER_ADD_ON_PATH/export_webots"
cp export_webots.py "$BLENDER_ADD_ON_PATH/export_webots"
```

Enable the "Webots exporter" add-on in `Blender / Preferences / Add-ons / Testing`.

## JSON Data File Specifications

[The data JSON file](https://en.wikipedia.org/wiki/JSON) specifies how to convert the Blender nodes to Webots ones.

It is basically a JSON Object (i.e. key-value associative array) containing the conversion rules for each node.
During the exportation, if a key of this JSON Object match with the slugified Blender node name, then its value (the conversion JSON Object) is used for the conversion.

A conversion JSON Object could contain the following conversion rules:

- `physics`: Object which can contain the [Webots Physics node](https://www.cyberbotics.com/doc/reference/physics) fields (like `mass`, `density` and `centerOfMass`)
- `boundingObject`: Object which can contain either a JSON String called `custom` to define the content of the [Webots Solid.boundingObject field](https://www.cyberbotics.com/doc/reference/solid) fields. If `custom` is not defined, then the AABB box of the Blender object is used to create the boundingObject.

Examples:

https://github.com/omichel/blender-webots-exporter/tree/master/examples


## References

This script has been inspired by the [Blender X3D exporter](https://github.com/sobotka/blender-addons/blob/master/io_scene_x3d/export_x3d.py) of Campbell Barton, Bart, Bastien Montagne, Seva Alekseyev.

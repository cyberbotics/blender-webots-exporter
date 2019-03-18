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

## References

This script has been inspired by the [Blender X3D exporter](https://github.com/sobotka/blender-addons/blob/master/io_scene_x3d/export_x3d.py) of Campbell Barton, Bart, Bastien Montagne, Seva Alekseyev.

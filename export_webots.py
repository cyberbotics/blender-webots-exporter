# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# <pep8 compliant>

# Inspired by the x3d exporter of bart:neeneenee*de, http://www.neeneenee.de/vrml, Campbell Barton
# refrence: https://github.com/sobotka/blender-addons/blob/master/io_scene_x3d/export_x3d.py

"""
This script exports to Webots format.
Usage:
Run this script from "File->Export" menu.  A pop-up will ask whether you
want to export only selected or all relevant objects.
Known issues:
    Doesn't handle multiple materials (don't use material indices);<br>
    Doesn't handle multiple UV textures on a single mesh (create a mesh for each texture);<br>
    Can't get the texture array associated with material * not the UV ones;
"""

import math
import os

import bpy
import mathutils

from bpy_extras.io_utils import create_derived_objects, free_derived_objects


def clight_color(col):
    return tuple([max(min(c, 1.0), 0.0) for c in col])


def matrix_direction_neg_z(matrix):
    return (matrix.to_3x3() * mathutils.Vector((0.0, 0.0, -1.0))).normalized()[:]


def prefix_quoted_str(value, prefix):
    return value[0] + prefix + value[1:]


def suffix_quoted_str(value, suffix):
    return value[:-1] + suffix + value[-1:]


def bool_as_str(value):
    return ('false', 'true')[bool(value)]


def clean_def(txt):
    # see report [#28256]
    if not txt:
        txt = "None"
    # no digit start
    if txt[0] in "1234567890+-":
        txt = "_" + txt
    return txt.translate({
        # control characters 0x0-0x1f
        # 0x00: "_",
        0x01: "_",
        0x02: "_",
        0x03: "_",
        0x04: "_",
        0x05: "_",
        0x06: "_",
        0x07: "_",
        0x08: "_",
        0x09: "_",
        0x0a: "_",
        0x0b: "_",
        0x0c: "_",
        0x0d: "_",
        0x0e: "_",
        0x0f: "_",
        0x10: "_",
        0x11: "_",
        0x12: "_",
        0x13: "_",
        0x14: "_",
        0x15: "_",
        0x16: "_",
        0x17: "_",
        0x18: "_",
        0x19: "_",
        0x1a: "_",
        0x1b: "_",
        0x1c: "_",
        0x1d: "_",
        0x1e: "_",
        0x1f: "_",

        0x7f: "_",  # 127

        0x20: "_",  # space
        0x22: "_",  # "
        0x27: "_",  # '
        0x23: "_",  # #
        0x2c: "_",  # ,
        0x2e: "_",  # .
        0x5b: "_",  # [
        0x5d: "_",  # ]
        0x5c: "_",  # \
        0x7b: "_",  # {
        0x7d: "_"  # }
    })


def build_hierarchy(objects):
    """ returns parent child relationships, skipping
    """
    objects_set = set(objects)
    par_lookup = {}

    def test_parent(parent):
        while (parent is not None) and (parent not in objects_set):
            parent = parent.parent
        return parent

    for obj in objects:
        par_lookup.setdefault(test_parent(obj.parent), []).append((obj, []))

    for parent, children in par_lookup.items():
        for obj, subchildren in children:
            subchildren[:] = par_lookup.get(obj, [])

    return par_lookup.get(None, [])


# -----------------------------------------------------------------------------
# Functions for writing output file
# -----------------------------------------------------------------------------

def export(file,
           global_matrix,
           scene,
           use_mesh_modifiers=False,
           use_selection=True,
           use_triangulate=False,
           use_normals=False,
           use_hierarchy=True,
           path_mode='AUTO',
           name_decorations=True,
           ):

    # -------------------------------------------------------------------------
    # Global Setup
    # -------------------------------------------------------------------------
    import bpy_extras
    from bpy_extras.io_utils import unique_name
    from xml.sax.saxutils import quoteattr, escape

    if name_decorations:
        # If names are decorated, the uuid map can be split up
        # by type for efficiency of collision testing
        # since objects of different types will always have
        # different decorated names.
        uuid_cache_object = {}    # object
        uuid_cache_light = {}      # 'LA_' + object.name
        uuid_cache_view = {}      # object, different namespace
        uuid_cache_mesh = {}      # mesh
        uuid_cache_material = {}  # material
        uuid_cache_image = {}     # image
        uuid_cache_world = {}     # world
        CA_ = 'CA_'
        OB_ = 'OB_'
        ME_ = 'ME_'
        IM_ = 'IM_'
        WO_ = 'WO_'
        MA_ = 'MA_'
        LA_ = 'LA_'
        group_ = 'group_'
    else:
        # If names are not decorated, it may be possible for two objects to
        # have the same name, so there has to be a unified dictionary to
        # prevent uuid collisions.
        uuid_cache = {}
        uuid_cache_object = uuid_cache           # object
        uuid_cache_light = uuid_cache             # 'LA_' + object.name
        uuid_cache_view = uuid_cache             # object, different namespace
        uuid_cache_mesh = uuid_cache             # mesh
        uuid_cache_material = uuid_cache         # material
        uuid_cache_image = uuid_cache            # image
        uuid_cache_world = uuid_cache            # world
        del uuid_cache
        CA_ = ''
        OB_ = ''
        ME_ = ''
        IM_ = ''
        WO_ = ''
        MA_ = ''
        LA_ = ''
        group_ = ''

    _TRANSFORM = '_TRANSFORM'

    # store files to copy
    copy_set = set()

    # store names of newly cerated meshes, so we dont overlap
    mesh_name_set = set()

    fw = file.write
    base_src = os.path.dirname(bpy.data.filepath)
    base_dst = os.path.dirname(file.name)

    # -------------------------------------------------------------------------
    # File Writing Functions
    # -------------------------------------------------------------------------

    def writeHeader(ident):
        filepath_quoted = quoteattr(os.path.basename(file.name))
        blender_ver_quoted = quoteattr('Blender %s' % bpy.app.version_string)

        fw('%s<?xml version="1.0" encoding="UTF-8"?>\n' % ident)
        fw('%s<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.0//EN" "http://www.web3d.org/specifications/x3d-3.0.dtd">\n' % ident)
        fw('%s<X3D version="3.0" profile="Immersive" xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance" xsd:noNamespaceSchemaLocation="http://www.web3d.org/specifications/x3d-3.0.xsd">\n' % ident)

        ident += '\t'
        fw('%s<head>\n' % ident)
        ident += '\t'
        fw('%s<meta name="filename" content=%s />\n' % (ident, filepath_quoted))
        fw('%s<meta name="generator" content=%s />\n' % (ident, blender_ver_quoted))
        # this info was never updated, so blender version should be enough
        # fw('%s<meta name="translator" content="X3D exporter v1.55 (2006/01/17)" />\n' % ident)
        ident = ident[:-1]
        fw('%s</head>\n' % ident)
        fw('%s<Scene>\n' % ident)
        ident += '\t'
        return ident

    def writeFooter(ident):
        ident = ident[:-1]
        fw('%s</Scene>\n' % ident)
        ident = ident[:-1]
        fw('%s</X3D>' % ident)
        return ident

    def writeViewpoint(ident, obj, matrix, scene):
        view_id = quoteattr(unique_name(obj, CA_ + obj.name, uuid_cache_view, clean_func=clean_def, sep="_"))

        loc, rot, scale = matrix.decompose()
        rot = rot.to_axis_angle()
        rot = (*rot[0].normalized(), rot[1])

        ident_step = ident + (' ' * (-len(ident) + fw('%s<Viewpoint ' % ident)))
        fw('DEF=%s\n' % view_id)
        fw(ident_step + 'centerOfRotation="0 0 0"\n')
        fw(ident_step + 'position="%3.2f %3.2f %3.2f"\n' % loc[:])
        fw(ident_step + 'orientation="%3.2f %3.2f %3.2f %3.2f"\n' % rot)
        fw(ident_step + 'fieldOfView="%.3f"\n' % obj.data.angle)
        fw(ident_step + '/>\n')

    def writeFog(ident, world):
        if world:
            mtype = world.mist_settings.falloff
            mparam = world.mist_settings
        else:
            return

        if mparam.use_mist:
            ident_step = ident + (' ' * (-len(ident) + fw('%s<Fog ' % ident)))
            fw('fogType="%s"\n' % ('LINEAR' if (mtype == 'LINEAR') else 'EXPONENTIAL'))
            fw(ident_step + 'color="%.3f %.3f %.3f"\n' % clight_color(world.horizon_color))
            fw(ident_step + 'visibilityRange="%.3f"\n' % mparam.depth)
            fw(ident_step + '/>\n')
        else:
            return

    def writeNavigationInfo(ident, scene, has_light):
        ident_step = ident + (' ' * (-len(ident) + fw('%s<NavigationInfo ' % ident)))
        fw('headlight="%s"\n' % bool_as_str(not has_light))
        fw(ident_step + 'visibilityLimit="0.0"\n')
        fw(ident_step + 'type=\'"EXAMINE", "ANY"\'\n')
        fw(ident_step + 'avatarSize="0.25, 1.75, 0.75"\n')
        fw(ident_step + '/>\n')

    def writeTransform_begin(ident, matrix, def_id):
        ident_step = ident + (' ' * (-len(ident) + fw('%s<Transform ' % ident)))
        if def_id is not None:
            fw('DEF=%s\n' % def_id)
        else:
            fw('\n')

        loc, rot, sca = matrix.decompose()
        rot = rot.to_axis_angle()
        rot = (*rot[0], rot[1])

        fw(ident_step + 'translation="%.6f %.6f %.6f"\n' % loc[:])
        # fw(ident_step + 'center="%.6f %.6f %.6f"\n' % (0, 0, 0))
        fw(ident_step + 'scale="%.6f %.6f %.6f"\n' % sca[:])
        fw(ident_step + 'rotation="%.6f %.6f %.6f %.6f"\n' % rot)
        fw(ident_step + '>\n')
        ident += '\t'
        return ident

    def writeTransform_end(ident):
        ident = ident[:-1]
        fw('%s</Transform>\n' % ident)
        return ident

    def writeSpotLight(ident, obj, matrix, lamp, world):
        # note, light_id is not re-used
        light_id = quoteattr(unique_name(obj, LA_ + obj.name, uuid_cache_light, clean_func=clean_def, sep="_"))

        if world:
            ambi = world.ambient_color
            amb_intensity = ((ambi[0] + ambi[1] + ambi[2]) / 3.0) / 2.5
            del ambi
        else:
            amb_intensity = 0.0

        # compute cutoff and beamwidth
        intensity = min(lamp.energy / 1.75, 1.0)
        beamWidth = lamp.spot_size * 0.37
        # beamWidth=((lamp.spotSize*math.pi)/180.0)*.37
        cutOffAngle = beamWidth * 1.3

        orientation = matrix_direction_neg_z(matrix)

        location = matrix.to_translation()[:]

        radius = lamp.distance * math.cos(beamWidth)
        # radius = lamp.dist*math.cos(beamWidth)
        ident_step = ident + (' ' * (-len(ident) + fw('%s<SpotLight ' % ident)))
        fw('DEF=%s\n' % light_id)
        fw(ident_step + 'radius="%.4f"\n' % radius)
        fw(ident_step + 'ambientIntensity="%.4f"\n' % amb_intensity)
        fw(ident_step + 'intensity="%.4f"\n' % intensity)
        fw(ident_step + 'color="%.4f %.4f %.4f"\n' % clight_color(lamp.color))
        fw(ident_step + 'beamWidth="%.4f"\n' % beamWidth)
        fw(ident_step + 'cutOffAngle="%.4f"\n' % cutOffAngle)
        fw(ident_step + 'direction="%.4f %.4f %.4f"\n' % orientation)
        fw(ident_step + 'location="%.4f %.4f %.4f"\n' % location)
        fw(ident_step + '/>\n')

    def writeDirectionalLight(ident, obj, matrix, lamp, world):
        # note, light_id is not re-used
        light_id = quoteattr(unique_name(obj, LA_ + obj.name, uuid_cache_light, clean_func=clean_def, sep="_"))

        if world:
            ambi = world.ambient_color
            # ambi = world.amb
            amb_intensity = ((float(ambi[0] + ambi[1] + ambi[2])) / 3.0) / 2.5
        else:
            ambi = 0
            amb_intensity = 0.0

        intensity = min(lamp.energy / 1.75, 1.0)

        orientation = matrix_direction_neg_z(matrix)

        ident_step = ident + (' ' * (-len(ident) + fw('%s<DirectionalLight ' % ident)))
        fw('DEF=%s\n' % light_id)
        fw(ident_step + 'ambientIntensity="%.4f"\n' % amb_intensity)
        fw(ident_step + 'color="%.4f %.4f %.4f"\n' % clight_color(lamp.color))
        fw(ident_step + 'intensity="%.4f"\n' % intensity)
        fw(ident_step + 'direction="%.4f %.4f %.4f"\n' % orientation)
        fw(ident_step + '/>\n')

    def writePointLight(ident, obj, matrix, lamp, world):
        # note, light_id is not re-used
        light_id = quoteattr(unique_name(obj, LA_ + obj.name, uuid_cache_light, clean_func=clean_def, sep="_"))

        if world:
            ambi = world.ambient_color
            # ambi = world.amb
            amb_intensity = ((float(ambi[0] + ambi[1] + ambi[2])) / 3.0) / 2.5
        else:
            ambi = 0.0
            amb_intensity = 0.0

        intensity = min(lamp.energy / 1.75, 1.0)
        location = matrix.to_translation()[:]

        ident_step = ident + (' ' * (-len(ident) + fw('%s<PointLight ' % ident)))
        fw('DEF=%s\n' % light_id)
        fw(ident_step + 'ambientIntensity="%.4f"\n' % amb_intensity)
        fw(ident_step + 'color="%.4f %.4f %.4f"\n' % clight_color(lamp.color))

        fw(ident_step + 'intensity="%.4f"\n' % intensity)
        fw(ident_step + 'radius="%.4f" \n' % lamp.distance)
        fw(ident_step + 'location="%.4f %.4f %.4f"\n' % location)
        fw(ident_step + '/>\n')

    def writeIndexedFaceSet(ident, obj, mesh, matrix, world):
        obj_id = quoteattr(unique_name(obj, OB_ + obj.name, uuid_cache_object, clean_func=clean_def, sep="_"))
        mesh_id = quoteattr(unique_name(mesh, ME_ + mesh.name, uuid_cache_mesh, clean_func=clean_def, sep="_"))
        mesh_id_group = prefix_quoted_str(mesh_id, group_)
        mesh_id_coords = prefix_quoted_str(mesh_id, 'coords_')
        mesh_id_normals = prefix_quoted_str(mesh_id, 'normals_')

        # tessellation faces may not exist
        if not mesh.tessfaces and mesh.polygons:
            mesh.update(calc_tessface=True)

        if not mesh.tessfaces:
            return

        use_collnode = bool([mod for mod in obj.modifiers
                             if mod.type == 'COLLISION'
                             if mod.show_viewport])

        if use_collnode:
            fw('%s<Collision enabled="true">\n' % ident)
            ident += '\t'

        # use _ifs_TRANSFORM suffix so we dont collide with transform node when
        # hierarchys are used.
        ident = writeTransform_begin(ident, matrix, suffix_quoted_str(obj_id, "_ifs" + _TRANSFORM))

        if mesh.tag:
            fw('%s<Group USE=%s />\n' % (ident, mesh_id_group))
        else:
            mesh.tag = True

            fw('%s<Group DEF=%s>\n' % (ident, mesh_id_group))
            ident += '\t'

            is_uv = bool(mesh.tessface_uv_textures.active)
            # is_col, defined for each material

            is_coords_written = False

            mesh_materials = mesh.materials[:]
            if not mesh_materials:
                mesh_materials = [None]

            mesh_material_tex = [None] * len(mesh_materials)
            mesh_material_mtex = [None] * len(mesh_materials)
            mesh_material_images = [None] * len(mesh_materials)

            for i, material in enumerate(mesh_materials):
                if material:
                    for mtex in material.texture_slots:
                        if mtex:
                            tex = mtex.texture
                            if tex and tex.type == 'IMAGE':
                                image = tex.image
                                if image:
                                    mesh_material_tex[i] = tex
                                    mesh_material_mtex[i] = mtex
                                    mesh_material_images[i] = image
                                    break

            mesh_materials_use_face_texture = [getattr(material, 'use_face_texture', True) for material in mesh_materials]

            # fast access!
            mesh_vertices = mesh.vertices[:]
            mesh_faces = mesh.tessfaces[:]
            mesh_faces_materials = [f.material_index for f in mesh_faces]
            mesh_faces_vertices = [f.vertices[:] for f in mesh_faces]

            if is_uv and True in mesh_materials_use_face_texture:
                mesh_faces_image = [(fuv.image
                                     if mesh_materials_use_face_texture[mesh_faces_materials[i]]
                                     else mesh_material_images[mesh_faces_materials[i]])
                                     for i, fuv in enumerate(mesh.tessface_uv_textures.active.data)]

                mesh_faces_image_unique = set(mesh_faces_image)
            elif len(set(mesh_material_images) | {None}) > 1:  # make sure there is at least one image
                mesh_faces_image = [mesh_material_images[material_index] for material_index in mesh_faces_materials]
                mesh_faces_image_unique = set(mesh_faces_image)
            else:
                mesh_faces_image = [None] * len(mesh_faces)
                mesh_faces_image_unique = {None}

            # group faces
            face_groups = {}
            for material_index in range(len(mesh_materials)):
                for image in mesh_faces_image_unique:
                    face_groups[material_index, image] = []
            del mesh_faces_image_unique

            for i, (material_index, image) in enumerate(zip(mesh_faces_materials, mesh_faces_image)):
                face_groups[material_index, image].append(i)

            # same as face_groups.items() but sorted so we can get predictable output.
            face_groups_items = list(face_groups.items())
            face_groups_items.sort(key=lambda m: (m[0][0], getattr(m[0][1], 'name', '')))

            is_col = (mesh.tessface_vertex_colors.active and (material is None or material.use_vertex_color_paint))
            mesh_faces_col = mesh.tessface_vertex_colors.active.data if is_col else None

            # Check if vertex colors can be exported in per-vertex mode.
            # Do we have just one color per vertex in every face that uses the vertex?
            if is_col:
                def calc_vertex_color():
                    vert_color = [None] * len(mesh.vertices)

                    for i, face in enumerate(mesh_faces):
                        fcol = mesh_faces_col[i]
                        face_colors = (fcol.color1, fcol.color2, fcol.color3, fcol.color4)
                        for j, vert_index in enumerate(face.vertices):
                            if vert_color[vert_index] is None:
                                vert_color[vert_index] = face_colors[j][:]
                            elif vert_color[vert_index] != face_colors[j][:]:
                                return False, ()

                    return True, vert_color

                is_col_per_vertex, vert_color = calc_vertex_color()
                del calc_vertex_color

            for (material_index, image), face_group in face_groups_items:  # face_groups.items()
                if face_group:
                    material = mesh_materials[material_index]

                    fw('%s<Shape>\n' % ident)
                    ident += '\t'

                    is_smooth = False

                    # kludge but as good as it gets!
                    for i in face_group:
                        if mesh_faces[i].use_smooth:
                            is_smooth = True
                            break

                    # UV's and VCols split verts off which effects smoothing
                    # force writing normals in this case.
                    # Also, creaseAngle is not supported for IndexedTriangleSet,
                    # so write normals when is_smooth (otherwise
                    # IndexedTriangleSet can have only all smooth/all flat shading).
                    is_force_normals = use_triangulate and (is_smooth or is_uv or is_col)
                    fw('%s<Appearance>\n' % ident)
                    ident += '\t'

                    if image:
                        writeImageTexture(ident, image)

                        if mesh_materials_use_face_texture[material_index]:
                            if image.use_tiles:
                                fw('%s<TextureTransform scale="%s %s" />\n' % (ident, image.tiles_x, image.tiles_y))
                        else:
                            # transform by mtex
                            loc = mesh_material_mtex[material_index].offset[:2]

                            # mtex_scale * tex_repeat
                            sca_x, sca_y = mesh_material_mtex[material_index].scale[:2]

                            sca_x *= mesh_material_tex[material_index].repeat_x
                            sca_y *= mesh_material_tex[material_index].repeat_y

                            # flip x/y is a sampling feature, convert to transform
                            if mesh_material_tex[material_index].use_flip_axis:
                                rot = math.pi / -2.0
                                sca_x, sca_y = sca_y, -sca_x
                            else:
                                rot = 0.0

                            ident_step = ident + (' ' * (-len(ident) + fw('%s<TextureTransform ' % ident)))
                            fw('\n')
                            # fw('center="%.6f %.6f" ' % (0.0, 0.0))
                            fw(ident_step + 'translation="%.6f %.6f"\n' % loc)
                            fw(ident_step + 'scale="%.6f %.6f"\n' % (sca_x, sca_y))
                            fw(ident_step + 'rotation="%.6f"\n' % rot)
                            fw(ident_step + '/>\n')

                    if material:
                        writeMaterial(ident, material, world)

                    ident = ident[:-1]
                    fw('%s</Appearance>\n' % ident)

                    mesh_faces_uv = mesh.tessface_uv_textures.active.data if is_uv else None

                    # --- IndexedFaceSet or IndexedLineSet
                    if use_triangulate:
                        ident_step = ident + (' ' * (-len(ident) + fw('%s<IndexedTriangleSet ' % ident)))

                        # --- Write IndexedTriangleSet Attributes (same as IndexedFaceSet)
                        fw('solid="%s"\n' % bool_as_str(material and material.game_settings.use_backface_culling))

                        if use_normals or is_force_normals:
                            fw(ident_step + 'normalPerVertex="true"\n')
                        else:
                            # Tell X3D browser to generate flat (per-face) normals
                            fw(ident_step + 'normalPerVertex="false"\n')

                        slot_uv = None
                        slot_col = None

                        if is_uv and is_col:
                            slot_uv = 0
                            slot_col = 1

                            def vertex_key(fidx, f_cnr_idx):
                                return (
                                    mesh_faces_uv[fidx].uv[f_cnr_idx][:],
                                    getattr(mesh_faces_col[fidx], "color%d" % (f_cnr_idx + 1))[:],
                                )
                        elif is_uv:
                            slot_uv = 0

                            def vertex_key(fidx, f_cnr_idx):
                                return (
                                    mesh_faces_uv[fidx].uv[f_cnr_idx][:],
                                )
                        elif is_col:
                            slot_col = 0

                            def vertex_key(fidx, f_cnr_idx):
                                return (
                                    getattr(mesh_faces_col[fidx], "color%d" % (f_cnr_idx + 1))[:],
                                )
                        else:
                            # ack, not especially efficient in this case
                            def vertex_key(fidx, f_cnr_idx):
                                return None

                        # build a mesh mapping dict
                        vertex_hash = [{} for i in range(len(mesh.vertices))]
                        # worst case every face is a quad
                        face_tri_list = [[None, None, None] for i in range(len(mesh.tessfaces) * 2)]
                        vert_tri_list = []
                        totvert = 0
                        totface = 0
                        temp_face = [None] * 4
                        for i in face_group:
                            fv = mesh_faces_vertices[i]
                            for j, v_idx in enumerate(fv):
                                key = vertex_key(i, j)
                                vh = vertex_hash[v_idx]
                                x3d_v = vh.get(key)
                                if x3d_v is None:
                                    x3d_v = key, v_idx, totvert
                                    vh[key] = x3d_v
                                    # key / original_vertex / new_vertex
                                    vert_tri_list.append(x3d_v)
                                    totvert += 1
                                temp_face[j] = x3d_v

                            if len(fv) == 4:
                                f_iter = ((0, 1, 2), (0, 2, 3))
                            else:
                                f_iter = ((0, 1, 2), )

                            for f_it in f_iter:
                                # loop over a quad as 2 tris
                                f_tri = face_tri_list[totface]
                                for ji, j in enumerate(f_it):
                                    f_tri[ji] = temp_face[j]
                                # quads run this twice
                                totface += 1

                        # clear unused faces
                        face_tri_list[totface:] = []

                        fw(ident_step + 'index="')
                        for x3d_f in face_tri_list:
                            fw('%i %i %i ' % (x3d_f[0][2], x3d_f[1][2], x3d_f[2][2]))
                        fw('"\n')

                        # close IndexedTriangleSet
                        fw(ident_step + '>\n')
                        ident += '\t'

                        fw('%s<Coordinate ' % ident)
                        fw('point="')
                        for x3d_v in vert_tri_list:
                            fw('%.6f %.6f %.6f ' % mesh_vertices[x3d_v[1]].co[:])
                        fw('" />\n')

                        if use_normals or is_force_normals:
                            fw('%s<Normal ' % ident)
                            fw('vector="')
                            for x3d_v in vert_tri_list:
                                fw('%.6f %.6f %.6f ' % mesh_vertices[x3d_v[1]].normal[:])
                            fw('" />\n')

                        if is_uv:
                            fw('%s<TextureCoordinate point="' % ident)
                            for x3d_v in vert_tri_list:
                                fw('%.4f %.4f ' % x3d_v[0][slot_uv])
                            fw('" />\n')

                        if is_col:
                            fw('%s<Color color="' % ident)
                            for x3d_v in vert_tri_list:
                                fw('%.3f %.3f %.3f ' % x3d_v[0][slot_col])
                            fw('" />\n')

                        ident = ident[:-1]

                        fw('%s</IndexedTriangleSet>\n' % ident)

                    else:
                        ident_step = ident + (' ' * (-len(ident) + fw('%s<IndexedFaceSet ' % ident)))

                        # --- Write IndexedFaceSet Attributes (same as IndexedTriangleSet)
                        fw('solid="%s"\n' % bool_as_str(material and material.game_settings.use_backface_culling))
                        if is_smooth:
                            # use Auto-Smooth angle, if enabled. Otherwise make
                            # the mesh perfectly smooth by creaseAngle > pi.
                            fw(ident_step + 'creaseAngle="%.4f"\n' % (mesh.auto_smooth_angle if mesh.use_auto_smooth else 4.0))

                        if use_normals:
                            # currently not optional, could be made so:
                            fw(ident_step + 'normalPerVertex="true"\n')

                        # IndexedTriangleSet assumes true
                        if is_col and not is_col_per_vertex:
                            fw(ident_step + 'colorPerVertex="false"\n')

                        # for IndexedTriangleSet we use a uv per vertex so this isnt needed.
                        if is_uv:
                            fw(ident_step + 'texCoordIndex="')

                            j = 0
                            for i in face_group:
                                if len(mesh_faces_vertices[i]) == 4:
                                    fw('%d %d %d %d -1 ' % (j, j + 1, j + 2, j + 3))
                                    j += 4
                                else:
                                    fw('%d %d %d -1 ' % (j, j + 1, j + 2))
                                    j += 3
                            fw('"\n')
                            # --- end texCoordIndex

                        if True:
                            fw(ident_step + 'coordIndex="')
                            for i in face_group:
                                fv = mesh_faces_vertices[i]
                                if len(fv) == 3:
                                    fw('%i %i %i -1 ' % fv)
                                else:
                                    fw('%i %i %i %i -1 ' % fv)

                            fw('"\n')
                            # --- end coordIndex

                        # close IndexedFaceSet
                        fw(ident_step + '>\n')
                        ident += '\t'

                        # --- Write IndexedFaceSet Elements
                        if True:
                            if is_coords_written:
                                fw('%s<Coordinate USE=%s />\n' % (ident, mesh_id_coords))
                                if use_normals:
                                    fw('%s<Normal USE=%s />\n' % (ident, mesh_id_normals))
                            else:
                                ident_step = ident + (' ' * (-len(ident) + fw('%s<Coordinate ' % ident)))
                                fw('DEF=%s\n' % mesh_id_coords)
                                fw(ident_step + 'point="')
                                for v in mesh.vertices:
                                    fw('%.6f %.6f %.6f ' % v.co[:])
                                fw('"\n')
                                fw(ident_step + '/>\n')

                                is_coords_written = True

                                if use_normals:
                                    ident_step = ident + (' ' * (-len(ident) + fw('%s<Normal ' % ident)))
                                    fw('DEF=%s\n' % mesh_id_normals)
                                    fw(ident_step + 'vector="')
                                    for v in mesh.vertices:
                                        fw('%.6f %.6f %.6f ' % v.normal[:])
                                    fw('"\n')
                                    fw(ident_step + '/>\n')

                        if is_uv:
                            fw('%s<TextureCoordinate point="' % ident)
                            for i in face_group:
                                for uv in mesh_faces_uv[i].uv:
                                    fw('%.4f %.4f ' % uv[:])
                            del mesh_faces_uv
                            fw('" />\n')

                        if is_col:
                            # Need better logic here, dynamic determination
                            # which of the X3D coloring models fits better this mesh - per face
                            # or per vertex. Probably with an explicit fallback mode parameter.
                            fw('%s<Color color="' % ident)
                            if is_col_per_vertex:
                                for i in range(len(mesh.vertices)):
                                    # may be None,
                                    fw('%.3f %.3f %.3f ' % (vert_color[i] or (0.0, 0.0, 0.0)))
                            else:  # Export as colors per face.
                                # TODO: average them rather than using the first one!
                                for i in face_group:
                                    fw('%.3f %.3f %.3f ' % mesh_faces_col[i].color1[:])
                            fw('" />\n')

                        # --- output vertexColors

                        # --- output closing braces
                        ident = ident[:-1]

                        fw('%s</IndexedFaceSet>\n' % ident)

                    ident = ident[:-1]
                    fw('%s</Shape>\n' % ident)

                    # XXX

            # fw('%s<PythonScript DEF="PS" url="object.py" >\n' % ident)
            # fw('%s    <ShaderProgram USE="MA_Material.005" containerField="references"/>\n' % ident)
            # fw('%s</PythonScript>\n' % ident)

            ident = ident[:-1]
            fw('%s</Group>\n' % ident)

        ident = writeTransform_end(ident)

        if use_collnode:
            ident = ident[:-1]
            fw('%s</Collision>\n' % ident)

    def writeMaterial(ident, material, world):
        material_id = quoteattr(unique_name(material, MA_ + material.name, uuid_cache_material, clean_func=clean_def, sep="_"))

        # look up material name, use it if available
        if material.tag:
            fw('%s<Material USE=%s />\n' % (ident, material_id))
        else:
            material.tag = True

            emit = material.emit
            ambient = material.ambient / 3.0
            diffuseColor = material.diffuse_color[:]
            if world:
                ambiColor = ((material.ambient * 2.0) * world.ambient_color)[:]
            else:
                ambiColor = 0.0, 0.0, 0.0

            emitColor = tuple(((c * emit) + ambiColor[i]) / 2.0 for i, c in enumerate(diffuseColor))
            shininess = material.specular_hardness / 512.0
            specColor = tuple((c + 0.001) / (1.25 / (material.specular_intensity + 0.001)) for c in material.specular_color)
            transp = 1.0 - material.alpha

            if material.use_shadeless:
                ambient = 1.0
                shininess = 0.0
                specColor = emitColor = diffuseColor

            ident_step = ident + (' ' * (-len(ident) + fw('%s<Material ' % ident)))
            fw('DEF=%s\n' % material_id)
            fw(ident_step + 'diffuseColor="%.3f %.3f %.3f"\n' % clight_color(diffuseColor))
            fw(ident_step + 'specularColor="%.3f %.3f %.3f"\n' % clight_color(specColor))
            fw(ident_step + 'emissiveColor="%.3f %.3f %.3f"\n' % clight_color(emitColor))
            fw(ident_step + 'ambientIntensity="%.3f"\n' % ambient)
            fw(ident_step + 'shininess="%.3f"\n' % shininess)
            fw(ident_step + 'transparency="%s"\n' % transp)
            fw(ident_step + '/>\n')

    def writeImageTexture(ident, image):
        image_id = quoteattr(unique_name(image, IM_ + image.name, uuid_cache_image, clean_func=clean_def, sep="_"))

        if image.tag:
            fw('%s<ImageTexture USE=%s />\n' % (ident, image_id))
        else:
            image.tag = True

            ident_step = ident + (' ' * (-len(ident) + fw('%s<ImageTexture ' % ident)))
            fw('DEF=%s\n' % image_id)

            # collect image paths, can load multiple
            # [relative, name-only, absolute]
            filepath = image.filepath
            filepath_full = bpy.path.abspath(filepath, library=image.library)
            filepath_ref = bpy_extras.io_utils.path_reference(filepath_full, base_src, base_dst, path_mode, "textures", copy_set, image.library)
            filepath_base = os.path.basename(filepath_full)

            images = [
                filepath_ref,
                filepath_base,
            ]
            if path_mode != 'RELATIVE':
                images.append(filepath_full)

            images = [f.replace('\\', '/') for f in images]
            images = [f for i, f in enumerate(images) if f not in images[:i]]

            fw(ident_step + "url='%s'\n" % ' '.join(['"%s"' % escape(f) for f in images]))
            fw(ident_step + '/>\n')

    def writeBackground(ident, world):

        if world is None:
            return

        # note, not re-used
        world_id = quoteattr(unique_name(world, WO_ + world.name, uuid_cache_world, clean_func=clean_def, sep="_"))

        blending = world.use_sky_blend, world.use_sky_paper, world.use_sky_real

        grd_triple = clight_color(world.horizon_color)
        sky_triple = clight_color(world.zenith_color)
        mix_triple = clight_color((grd_triple[i] + sky_triple[i]) / 2.0 for i in range(3))

        ident_step = ident + (' ' * (-len(ident) + fw('%s<Background ' % ident)))
        fw('DEF=%s\n' % world_id)
        # No Skytype - just Hor color
        if blending == (False, False, False):
            fw(ident_step + 'groundColor="%.3f %.3f %.3f"\n' % grd_triple)
            fw(ident_step + 'skyColor="%.3f %.3f %.3f"\n' % grd_triple)
        # Blend Gradient
        elif blending == (True, False, False):
            fw(ident_step + 'groundColor="%.3f %.3f %.3f, %.3f %.3f %.3f"\n' % (grd_triple + mix_triple))
            fw(ident_step + 'groundAngle="1.57"\n')
            fw(ident_step + 'skyColor="%.3f %.3f %.3f, %.3f %.3f %.3f"\n' % (sky_triple + mix_triple))
            fw(ident_step + 'skyAngle="1.57"\n')
        # Blend+Real Gradient Inverse
        elif blending == (True, False, True):
            fw(ident_step + 'groundColor="%.3f %.3f %.3f, %.3f %.3f %.3f"\n' % (sky_triple + grd_triple))
            fw(ident_step + 'groundAngle="1.57"\n')
            fw(ident_step + 'skyColor="%.3f %.3f %.3f, %.3f %.3f %.3f, %.3f %.3f %.3f"\n' % (sky_triple + grd_triple + sky_triple))
            fw(ident_step + 'skyAngle="1.57, 3.14159"\n')
        # Paper - just Zen Color
        elif blending == (False, False, True):
            fw(ident_step + 'groundColor="%.3f %.3f %.3f"\n' % sky_triple)
            fw(ident_step + 'skyColor="%.3f %.3f %.3f"\n' % sky_triple)
        # Blend+Real+Paper - komplex gradient
        elif blending == (True, True, True):
            fw(ident_step + 'groundColor="%.3f %.3f %.3f, %.3f %.3f %.3f"\n' % (sky_triple + grd_triple))
            fw(ident_step + 'groundAngle="1.57"\n')
            fw(ident_step + 'skyColor="%.3f %.3f %.3f, %.3f %.3f %.3f"\n' % (sky_triple + grd_triple))
            fw(ident_step + 'skyAngle="1.57"\n')
        # Any Other two colors
        else:
            fw(ident_step + 'groundColor="%.3f %.3f %.3f"\n' % grd_triple)
            fw(ident_step + 'skyColor="%.3f %.3f %.3f"\n' % sky_triple)

        for tex in bpy.data.textures:
            if tex.type == 'IMAGE' and tex.image:
                namemat = tex.name
                pic = tex.image
                basename = quoteattr(bpy.path.basename(pic.filepath))

                if namemat == 'back':
                    fw(ident_step + 'backUrl=%s\n' % basename)
                elif namemat == 'bottom':
                    fw(ident_step + 'bottomUrl=%s\n' % basename)
                elif namemat == 'front':
                    fw(ident_step + 'frontUrl=%s\n' % basename)
                elif namemat == 'left':
                    fw(ident_step + 'leftUrl=%s\n' % basename)
                elif namemat == 'right':
                    fw(ident_step + 'rightUrl=%s\n' % basename)
                elif namemat == 'top':
                    fw(ident_step + 'topUrl=%s\n' % basename)

        fw(ident_step + '/>\n')

    # -------------------------------------------------------------------------
    # Export Object Hierarchy (recursively called)
    # -------------------------------------------------------------------------
    def export_object(ident, obj_main_parent, obj_main, obj_children):
        matrix_fallback = mathutils.Matrix()
        world = scene.world
        free, derived = create_derived_objects(scene, obj_main)

        if use_hierarchy:
            obj_main_matrix_world = obj_main.matrix_world
            if obj_main_parent:
                obj_main_matrix = obj_main_parent.matrix_world.inverted(matrix_fallback) * obj_main_matrix_world
            else:
                obj_main_matrix = obj_main_matrix_world
            obj_main_matrix_world_invert = obj_main_matrix_world.inverted(matrix_fallback)

            obj_main_id = quoteattr(unique_name(obj_main, obj_main.name, uuid_cache_object, clean_func=clean_def, sep="_"))

            ident = writeTransform_begin(ident, obj_main_matrix if obj_main_parent else global_matrix * obj_main_matrix, suffix_quoted_str(obj_main_id, _TRANSFORM))

        for obj, obj_matrix in (() if derived is None else derived):
            obj_type = obj.type

            if use_hierarchy:
                # make transform node relative
                obj_matrix = obj_main_matrix_world_invert * obj_matrix
            else:
                obj_matrix = global_matrix * obj_matrix

            if obj_type == 'CAMERA':
                writeViewpoint(ident, obj, obj_matrix, scene)
            elif obj_type in {'MESH', 'CURVE', 'SURFACE', 'FONT'}:
                if (obj_type != 'MESH') or (use_mesh_modifiers and obj.is_modified(scene, 'PREVIEW')):
                    try:
                        me = obj.to_mesh(scene, use_mesh_modifiers, 'PREVIEW')
                    except:
                        me = None
                    do_remove = True
                else:
                    me = obj.data
                    do_remove = False

                if me is not None:
                    # ensure unique name, we could also do this by
                    # postponing mesh removal, but clearing data - TODO
                    if do_remove:
                        me.name = obj.name.rstrip("1234567890").rstrip(".")
                        me_name_new = me_name_org = me.name
                        count = 0
                        while me_name_new in mesh_name_set:
                            me.name = "%.17s.%03d" % (me_name_org, count)
                            me_name_new = me.name
                            count += 1
                        mesh_name_set.add(me_name_new)
                        del me_name_new, me_name_org, count
                    # done

                    writeIndexedFaceSet(ident, obj, me, obj_matrix, world)

                    # free mesh created with create_mesh()
                    if do_remove:
                        bpy.data.meshes.remove(me)

            elif obj_type == 'LIGHT':
                data = obj.data
                datatype = data.type
                if datatype == 'POINT':
                    writePointLight(ident, obj, obj_matrix, data, world)
                elif datatype == 'SPOT':
                    writeSpotLight(ident, obj, obj_matrix, data, world)
                elif datatype == 'SUN':
                    writeDirectionalLight(ident, obj, obj_matrix, data, world)
                else:
                    writeDirectionalLight(ident, obj, obj_matrix, data, world)
            else:
                # print('Info: Ignoring [%s], object type [%s] not handle yet' % (object.name,object.getType))
                pass

        if free:
            free_derived_objects(obj_main)

        # ---------------------------------------------------------------------
        # write out children recursively
        # ---------------------------------------------------------------------
        for obj_child, obj_child_children in obj_children:
            export_object(ident, obj_main, obj_child, obj_child_children)

        if use_hierarchy:
            ident = writeTransform_end(ident)

    # -------------------------------------------------------------------------
    # Main Export Function
    # -------------------------------------------------------------------------
    def export_main():
        world = scene.world

        # tag un-exported IDs
        bpy.data.meshes.tag(False)
        bpy.data.materials.tag(False)
        bpy.data.images.tag(False)

        if use_selection:
            objects = [obj for obj in scene.objects if obj.is_visible(scene) and obj.select]
        else:
            objects = [obj for obj in scene.objects if obj.is_visible(scene)]

        print('Info: starting Webots export to %r...' % file.name)
        ident = ''
        ident = writeHeader(ident)

        writeNavigationInfo(ident, scene, any(obj.type == 'LIGHT' for obj in objects))
        writeBackground(ident, world)
        writeFog(ident, world)

        ident = '\t\t'

        if use_hierarchy:
            objects_hierarchy = build_hierarchy(objects)
        else:
            objects_hierarchy = ((obj, []) for obj in objects)

        for obj_main, obj_main_children in objects_hierarchy:
            export_object(ident, None, obj_main, obj_main_children)

        ident = writeFooter(ident)

    export_main()

    # -------------------------------------------------------------------------
    # global cleanup
    # -------------------------------------------------------------------------
    file.close()

    # copy all collected files.
    # print(copy_set)
    bpy_extras.io_utils.path_reference_copy(copy_set)

    print('Info: finished Webots export to %r' % file.name)


##########################################################
# Callbacks, needed before Main
##########################################################

def save(context,
         filepath,
         *,
         use_selection=True,
         use_mesh_modifiers=False,
         use_triangulate=False,
         use_normals=False,
         use_hierarchy=True,
         global_matrix=None,
         path_mode='AUTO',
         name_decorations=True
         ):

    bpy.path.ensure_ext(filepath, '.wbt')

    if bpy.ops.object.mode_set.poll():
        bpy.ops.object.mode_set(mode='OBJECT')

    file = open(filepath, 'w', encoding='utf-8')

    if global_matrix is None:
        global_matrix = mathutils.Matrix()

    export(file,
           global_matrix,
           context.scene,
           use_mesh_modifiers=use_mesh_modifiers,
           use_selection=use_selection,
           use_triangulate=use_triangulate,
           use_normals=use_normals,
           use_hierarchy=use_hierarchy,
           path_mode=path_mode,
           name_decorations=name_decorations,
           )

    return {'FINISHED'}

///
/// glTF™ 2.0 Specification is available here:
/// https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html
///
const Self = @This();

const std = @import("std");
const helpers = @import("./helpers.zig");
const types = @import("./types.zig");

const mem = std.mem;
const json = std.json;
const fmt = std.fmt;
const panic = std.debug.panic;
const print = std.debug.print;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Mat4 = helpers.Mat4;
const Vec3 = helpers.Vec3;
const Quat = helpers.Quat;

pub const Scene = types.Scene;
pub const Node = types.Node;
pub const Index = types.Index;
pub const Mesh = types.Mesh;
pub const Material = types.Material;
pub const Skin = types.Skin;
pub const TextureSampler = types.TextureSampler;
pub const Image = types.Image;
pub const Camera = types.Camera;
pub const Animation = types.Animation;
pub const Texture = types.Texture;
pub const Accessor = types.Accessor;
pub const BufferView = types.BufferView;
pub const Buffer = types.Buffer;
pub const Primitive = types.Primitive;
pub const Attribute = types.Attribute;
pub const Mode = types.Mode;
pub const ComponentType = types.ComponentType;
pub const Target = types.Target;
pub const MetallicRoughness = types.MetallicRoughness;
pub const AnimationSampler = types.AnimationSampler;
pub const Channel = types.Channel;
pub const MagFilter = types.MagFilter;
pub const MinFilter = types.MinFilter;
pub const WrapMode = types.WrapMode;
pub const TargetProperty = types.TargetProperty;
pub const Asset = types.Asset;
pub const LightType = types.LightType;
pub const Light = types.Light;

pub const Data = struct {
    asset: Asset,
    scene: ?Index = null,
    scenes: ArrayList(Scene),
    cameras: ArrayList(Camera),
    nodes: ArrayList(Node),
    meshes: ArrayList(Mesh),
    materials: ArrayList(Material),
    skins: ArrayList(Skin),
    samplers: ArrayList(TextureSampler),
    images: ArrayList(Image),
    animations: ArrayList(Animation),
    textures: ArrayList(Texture),
    accessors: ArrayList(Accessor),
    buffer_views: ArrayList(BufferView),
    buffers: ArrayList(Buffer),
    lights: ArrayList(Light),
};

arena: *ArenaAllocator,
data: Data,

glb_binary: ?[]const u8 = null,

pub fn init(allocator: Allocator) Self {
    var arena = allocator.create(ArenaAllocator) catch {
        panic("Error while allocating memory for gltf arena.", .{});
    };

    arena.* = ArenaAllocator.init(allocator);

    const alloc = arena.allocator();
    return Self{
        .arena = arena,
        .data = .{
            .asset = Asset{ .version = "Undefined" },
            .scenes = ArrayList(Scene).init(alloc),
            .nodes = ArrayList(Node).init(alloc),
            .cameras = ArrayList(Camera).init(alloc),
            .meshes = ArrayList(Mesh).init(alloc),
            .materials = ArrayList(Material).init(alloc),
            .skins = ArrayList(Skin).init(alloc),
            .samplers = ArrayList(TextureSampler).init(alloc),
            .images = ArrayList(Image).init(alloc),
            .animations = ArrayList(Animation).init(alloc),
            .textures = ArrayList(Texture).init(alloc),
            .accessors = ArrayList(Accessor).init(alloc),
            .buffer_views = ArrayList(BufferView).init(alloc),
            .buffers = ArrayList(Buffer).init(alloc),
            .lights = ArrayList(Light).init(alloc),
        },
    };
}

/// Fill data by parsing a glTF file's buffer.
pub fn parse(self: *Self, file_buffer: []const u8) !void {
    if (isGlb(file_buffer)) {
        try self.parseGlb(file_buffer);
    } else {
        try self.parseGltfJson(file_buffer);
    }
}

pub fn debugPrint(self: *const Self) void {
    const msg =
        \\
        \\  glTF file info:
        \\
        \\    Node       {}
        \\    Mesh       {}
        \\    Skin       {}
        \\    Animation  {}
        \\    Texture    {}
        \\    Material   {}
        \\
        \\
    ;

    print(msg, .{
        self.data.nodes.items.len,
        self.data.meshes.items.len,
        self.data.skins.items.len,
        self.data.animations.items.len,
        self.data.textures.items.len,
        self.data.materials.items.len,
    });

    print("  Details:\n\n", .{});

    if (self.data.skins.items.len > 0) {
        print("   Skins found:\n", .{});

        for (self.data.skins.items) |skin| {
            print("     '{s}' found with {} joint(s).\n", .{
                skin.name,
                skin.joints.items.len,
            });
        }

        print("\n", .{});
    }

    if (self.data.animations.items.len > 0) {
        print("  Animations found:\n", .{});

        for (self.data.animations.items) |anim| {
            print(
                "     '{s}' found with {} sampler(s) and {} channel(s).\n",
                .{ anim.name, anim.samplers.items.len, anim.channels.items.len },
            );
        }

        print("\n", .{});
    }
}

/// Retrieve actual data from a glTF BufferView through a given glTF Accessor.
/// Note: This library won't pull to memory the binary buffer corresponding
/// to the BufferView.
pub fn getDataFromBufferView(
    self: *const Self,
    comptime T: type,
    /// List that will be fill with data.
    list: *ArrayList(T),
    accessor: Accessor,
    binary: []const u8,
) void {
    if (switch (accessor.component_type) {
        .byte => T != i8,
        .unsigned_byte => T != u8,
        .short => T != i16,
        .unsigned_short => T != u16,
        .unsigned_integer => T != u32,
        .float => T != f32,
    }) {
        panic(
            "Mismatch between gltf component '{}' and given type '{}'.",
            .{ accessor.component_type, T },
        );
    }

    if (accessor.buffer_view == null) {
        panic("Accessors without buffer_view are not supported yet.", .{});
    }

    const buffer_view = self.data.buffer_views.items[accessor.buffer_view.?];

    const comp_size = @sizeOf(T);
    const offset = (accessor.byte_offset + buffer_view.byte_offset) / comp_size;

    const stride = blk: {
        if (buffer_view.byte_stride) |byte_stride| {
            break :blk byte_stride / comp_size;
        } else {
            break :blk accessor.stride / comp_size;
        }
    };

    const total_count = accessor.count;
    const datum_count: usize = switch (accessor.type) {
        // Scalar.
        .scalar => 1,
        // Vec2.
        .vec2 => 2,
        // Vec3.
        .vec3 => 3,
        // Vec4.
        .vec4 => 4,
        // Vec4.
        .mat4x4 => 16,
        else => {
            panic("Accessor type '{}' not implemented.", .{accessor.type});
        },
    };

    const data = @ptrCast([*]const T, @alignCast(@alignOf(T), binary.ptr));

    var current_count: usize = 0;
    while (current_count < total_count) : (current_count += 1) {
        const slice = (data + offset + current_count * stride)[0..datum_count];
        list.appendSlice(slice) catch unreachable;
    }
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.arena.child_allocator.destroy(self.arena);
}

pub fn getLocalTransform(node: Node) Mat4 {
    return blk: {
        if (node.matrix) |mat4x4| {
            break :blk .{
                mat4x4[0..4].*,
                mat4x4[4..8].*,
                mat4x4[8..12].*,
                mat4x4[12..16].*,
            };
        }

        break :blk helpers.recompose(
            node.translation,
            node.rotation,
            node.scale,
        );
    };
}

pub fn getGlobalTransform(data: *const Data, node: Node) Mat4 {
    var parent_index = node.parent;
    var node_transform: Mat4 = getLocalTransform(node);

    while (parent_index != null) {
        const parent = data.nodes.items[parent_index.?];
        const parent_transform = getLocalTransform(parent);

        node_transform = helpers.mul(parent_transform, node_transform);
        parent_index = parent.parent;
    }

    return node_transform;
}

fn isGlb(glb_buffer: []const u8) bool {
    const GLB_MAGIC_NUMBER: u32 = 0x46546C67; // 'gltf' in ASCII.
    const fields = @ptrCast([*]const u32, @alignCast(4, glb_buffer));

    return fields[0] == GLB_MAGIC_NUMBER;
}

fn parseGlb(self: *Self, glb_buffer: []const u8) !void {
    const GLB_CHUNK_TYPE_JSON: u32 = 0x4E4F534A; // 'JSON' in ASCII.
    const GLB_CHUNK_TYPE_BIN: u32 = 0x004E4942; // 'BIN' in ASCII.

    // Keep track of the moving index in the glb buffer.
    var index: usize = 0;

    // 'cause most of the interesting fields are u32s in the buffer, it's
    // easier to read them with a pointer cast.
    const fields = @ptrCast([*]const u32, @alignCast(4, glb_buffer));

    // The 12-byte header consists of three 4-byte entries:
    //  u32 magic
    //  u32 version
    //  u32 length
    const total_length = blk: {
        const header = fields[0..3];

        const version = header[1];
        const length = header[2];

        if (!isGlb(glb_buffer)) {
            panic("First 32 bits are not equal to magic number.", .{});
        }

        if (version != 2) {
            panic("Only glTF spec v2 is supported.", .{});
        }

        index = header.len * @sizeOf(u32);
        break :blk length;
    };

    // Each chunk has the following structure:
    //  u32 chunkLength
    //  u32 chunkType
    //  ubyte[] chunkData
    const json_buffer = blk: {
        const json_chunk = fields[3..6];

        if (json_chunk[1] != GLB_CHUNK_TYPE_JSON) {
            panic("First GLB chunk must be JSON data.", .{});
        }

        const json_bytes: u32 = fields[3];
        const start = index + 2 * @sizeOf(u32);
        const end = start + json_bytes;

        const json_buffer = glb_buffer[start..end];

        index = end;
        break :blk json_buffer;
    };

    const binary_buffer = blk: {
        const fields_index = index / @sizeOf(u32);

        const binary_bytes = fields[fields_index];
        const start = index + 2 * @sizeOf(u32);
        const end = start + binary_bytes;

        assert(end == total_length);

        const binary = glb_buffer[start..end];

        if (fields[fields_index + 1] != GLB_CHUNK_TYPE_BIN) {
            panic("Second GLB chunk must be binary data.", .{});
        }

        index = end;
        break :blk binary;
    };

    try self.parseGltfJson(json_buffer);
    self.glb_binary = binary_buffer;

    const buffer_views = self.data.buffer_views.items;

    for (self.data.images.items) |*image| {
        if (image.buffer_view) |buffer_view_index| {
            const buffer_view = buffer_views[buffer_view_index];
            const start = buffer_view.byte_offset;
            const end = start + buffer_view.byte_length;
            image.data = binary_buffer[start..end];
        }
    }
}

fn parseGltfJson(self: *Self, gltf_json: []const u8) !void {
    const alloc = self.arena.allocator();

    var parser = json.Parser.init(alloc, false);
    defer parser.deinit();

    var gltf = try parser.parse(gltf_json);
    defer gltf.deinit();

    if (gltf.root.Object.get("asset")) |json_value| {
        var asset = &self.data.asset;

        if (json_value.Object.get("version")) |version| {
            asset.version = try alloc.dupe(u8, version.String);
        } else {
            panic("Asset's version is missing.", .{});
        }

        if (json_value.Object.get("generator")) |generator| {
            asset.generator = try alloc.dupe(u8, generator.String);
        }

        if (json_value.Object.get("copyright")) |copyright| {
            asset.copyright = try alloc.dupe(u8, copyright.String);
        }
    }

    if (gltf.root.Object.get("nodes")) |nodes| {
        for (nodes.Array.items, 0..) |item, index| {
            const object = item.Object;

            var node = Node{
                .name = undefined,
                .children = ArrayList(Index).init(alloc),
            };

            if (object.get("name")) |name| {
                node.name = try alloc.dupe(u8, name.String);
            } else {
                node.name = try fmt.allocPrint(alloc, "Node_{}", .{index});
            }

            if (object.get("mesh")) |mesh| {
                node.mesh = parseIndex(mesh);
            }

            if (object.get("camera")) |camera_index| {
                node.camera = parseIndex(camera_index);
            }

            if (object.get("skin")) |skin| {
                node.skin = parseIndex(skin);
            }

            if (object.get("children")) |children| {
                for (children.Array.items) |value| {
                    try node.children.append(parseIndex(value));
                }
            }

            if (object.get("rotation")) |rotation| {
                for (rotation.Array.items, 0..) |component, i| {
                    node.rotation[i] = parseFloat(f32, component);
                }
            }

            if (object.get("translation")) |translation| {
                for (translation.Array.items, 0..) |component, i| {
                    node.translation[i] = parseFloat(f32, component);
                }
            }

            if (object.get("scale")) |scale| {
                for (scale.Array.items, 0..) |component, i| {
                    node.scale[i] = parseFloat(f32, component);
                }
            }

            if (object.get("matrix")) |matrix| {
                node.matrix = [16]f32{
                    1, 0, 0, 0,
                    0, 1, 0, 0,
                    0, 0, 1, 0,
                    0, 0, 0, 1,
                };

                for (matrix.Array.items, 0..) |component, i| {
                    node.matrix.?[i] = parseFloat(f32, component);
                }
            }

            if (object.get("extensions")) |extensions|
            {
                if (extensions.Object.get("KHR_lights_punctual")) |lights_punctual|
                {
                    if (lights_punctual.Object.get("light")) |light|
                    {
                        node.light = @intCast(Index, light.Integer);
                    }
                }
            }

            try self.data.nodes.append(node);
        }
    }

    if (gltf.root.Object.get("cameras")) |cameras| {
        for (cameras.Array.items, 0..) |item, index| {
            const object = item.Object;

            var camera = Camera{
                .name = undefined,
                .type = undefined,
            };

            if (object.get("name")) |name| {
                camera.name = try alloc.dupe(u8, name.String);
            } else {
                camera.name = try fmt.allocPrint(alloc, "Camera_{}", .{index});
            }

            if (object.get("type")) |name| {
                if (mem.eql(u8, name.String, "perspective")) {
                    if (object.get("perspective")) |perspective| {
                        var value = perspective.Object;

                        camera.type = .{
                            .perspective = .{
                                .aspect_ratio = parseFloat(
                                    f32,
                                    value.get("aspectRatio").?,
                                ),
                                .yfov = parseFloat(f32, value.get("yfov").?),
                                .zfar = parseFloat(f32, value.get("zfar").?),
                                .znear = parseFloat(f32, value.get("znear").?),
                            },
                        };
                    } else {
                        panic("Camera's perspective value is missing.", .{});
                    }
                } else if (mem.eql(u8, name.String, "orthographic")) {
                    if (object.get("orthographic")) |orthographic| {
                        var value = orthographic.Object;

                        camera.type = .{
                            .orthographic = .{
                                .xmag = parseFloat(f32, value.get("xmag").?),
                                .ymag = parseFloat(f32, value.get("ymag").?),
                                .zfar = parseFloat(f32, value.get("zfar").?),
                                .znear = parseFloat(f32, value.get("znear").?),
                            },
                        };
                    } else {
                        panic("Camera's orthographic value is missing.", .{});
                    }
                } else {
                    panic(
                        "Camera's type must be perspective or orthographic.",
                        .{},
                    );
                }
            }

            try self.data.cameras.append(camera);
        }
    }

    if (gltf.root.Object.get("skins")) |skins| {
        for (skins.Array.items, 0..) |item, index| {
            const object = item.Object;

            var skin = Skin{
                .name = undefined,
                .joints = ArrayList(Index).init(alloc),
            };

            if (object.get("name")) |name| {
                skin.name = try alloc.dupe(u8, name.String);
            } else {
                skin.name = try fmt.allocPrint(alloc, "Skin_{}", .{index});
            }

            if (object.get("joints")) |joints| {
                for (joints.Array.items) |join| {
                    try skin.joints.append(parseIndex(join));
                }
            }

            if (object.get("skeleton")) |skeleton| {
                skin.skeleton = parseIndex(skeleton);
            }

            if (object.get("inverseBindMatrices")) |inv_bind_mat4| {
                skin.inverse_bind_matrices = parseIndex(inv_bind_mat4);
            }

            try self.data.skins.append(skin);
        }
    }

    if (gltf.root.Object.get("meshes")) |meshes| {
        for (meshes.Array.items, 0..) |item, index| {
            const object = item.Object;

            var mesh: Mesh = .{
                .name = undefined,
                .primitives = ArrayList(Primitive).init(alloc),
            };

            if (object.get("name")) |name| {
                mesh.name = try alloc.dupe(u8, name.String);
            } else {
                mesh.name = try fmt.allocPrint(alloc, "Mesh_{}", .{index});
            }

            if (object.get("primitives")) |primitives| {
                for (primitives.Array.items) |prim_item| {
                    var primitive: Primitive = .{
                        .attributes = ArrayList(Attribute).init(alloc),
                    };

                    if (prim_item.Object.get("mode")) |mode| {
                        primitive.mode = @intToEnum(Mode, mode.Integer);
                    }

                    if (prim_item.Object.get("indices")) |indices| {
                        primitive.indices = parseIndex(indices);
                    }

                    if (prim_item.Object.get("material")) |material| {
                        primitive.material = parseIndex(material);
                    }

                    if (prim_item.Object.get("attributes")) |attributes| {
                        if (attributes.Object.get("POSITION")) |position| {
                            try primitive.attributes.append(
                                .{
                                    .position = parseIndex(position),
                                },
                            );
                        }

                        if (attributes.Object.get("NORMAL")) |normal| {
                            try primitive.attributes.append(
                                .{
                                    .normal = parseIndex(normal),
                                },
                            );
                        }

                        if (attributes.Object.get("TANGENT")) |tangent| {
                            try primitive.attributes.append(
                                .{
                                    .tangent = parseIndex(tangent),
                                },
                            );
                        }

                        const texcoords = [_][]const u8{
                            "TEXCOORD_0",
                            "TEXCOORD_1",
                            "TEXCOORD_2",
                            "TEXCOORD_3",
                            "TEXCOORD_4",
                            "TEXCOORD_5",
                            "TEXCOORD_6",
                        };

                        for (texcoords) |tex_name| {
                            if (attributes.Object.get(tex_name)) |texcoord| {
                                try primitive.attributes.append(
                                    .{
                                        .texcoord = parseIndex(texcoord),
                                    },
                                );
                            }
                        }

                        const joints = [_][]const u8{
                            "JOINTS_0",
                            "JOINTS_1",
                            "JOINTS_2",
                            "JOINTS_3",
                            "JOINTS_4",
                            "JOINTS_5",
                            "JOINTS_6",
                        };

                        for (joints) |join_count| {
                            if (attributes.Object.get(join_count)) |joint| {
                                try primitive.attributes.append(
                                    .{
                                        .joints = parseIndex(joint),
                                    },
                                );
                            }
                        }

                        const weights = [_][]const u8{
                            "WEIGHTS_0",
                            "WEIGHTS_1",
                            "WEIGHTS_2",
                            "WEIGHTS_3",
                            "WEIGHTS_4",
                            "WEIGHTS_5",
                            "WEIGHTS_6",
                        };

                        for (weights) |weight_count| {
                            if (attributes.Object.get(weight_count)) |weight| {
                                try primitive.attributes.append(
                                    .{
                                        .weights = parseIndex(weight),
                                    },
                                );
                            }
                        }
                    }

                    try mesh.primitives.append(primitive);
                }
            }

            try self.data.meshes.append(mesh);
        }
    }

    if (gltf.root.Object.get("accessors")) |accessors| {
        for (accessors.Array.items) |item| {
            const object = item.Object;

            var accessor = Accessor{
                .component_type = undefined,
                .type = undefined,
                .count = undefined,
                .stride = undefined,
            };

            if (object.get("componentType")) |component_type| {
                accessor.component_type = @intToEnum(ComponentType, component_type.Integer);
            } else {
                panic("Accessor's componentType is missing.", .{});
            }

            if (object.get("count")) |count| {
                accessor.count = @intCast(i32, count.Integer);
            } else {
                panic("Accessor's count is missing.", .{});
            }

            if (object.get("type")) |accessor_type| {
                if (mem.eql(u8, accessor_type.String, "SCALAR")) {
                    accessor.type = .scalar;
                } else if (mem.eql(u8, accessor_type.String, "VEC2")) {
                    accessor.type = .vec2;
                } else if (mem.eql(u8, accessor_type.String, "VEC3")) {
                    accessor.type = .vec3;
                } else if (mem.eql(u8, accessor_type.String, "VEC4")) {
                    accessor.type = .vec4;
                } else if (mem.eql(u8, accessor_type.String, "MAT2")) {
                    accessor.type = .mat2x2;
                } else if (mem.eql(u8, accessor_type.String, "MAT3")) {
                    accessor.type = .mat3x3;
                } else if (mem.eql(u8, accessor_type.String, "MAT4")) {
                    accessor.type = .mat4x4;
                } else {
                    panic("Accessor's type '{s}' is invalid.", .{accessor_type.String});
                }
            } else {
                panic("Accessor's type is missing.", .{});
            }

            if (object.get("normalized")) |normalized| {
                accessor.normalized = normalized.Bool;
            }

            if (object.get("bufferView")) |buffer_view| {
                accessor.buffer_view = parseIndex(buffer_view);
            }

            if (object.get("byteOffset")) |byte_offset| {
                accessor.byte_offset = @intCast(usize, byte_offset.Integer);
            }

            const component_size: usize = switch (accessor.component_type) {
                .byte => @sizeOf(i8),
                .unsigned_byte => @sizeOf(u8),
                .short => @sizeOf(i16),
                .unsigned_short => @sizeOf(u16),
                .unsigned_integer => @sizeOf(u32),
                .float => @sizeOf(f32),
            };

            accessor.stride = switch (accessor.type) {
                .scalar => component_size,
                .vec2 => 2 * component_size,
                .vec3 => 3 * component_size,
                .vec4 => 4 * component_size,
                .mat2x2 => 4 * component_size,
                .mat3x3 => 9 * component_size,
                .mat4x4 => 16 * component_size,
            };

            try self.data.accessors.append(accessor);
        }
    }

    if (gltf.root.Object.get("bufferViews")) |buffer_views| {
        for (buffer_views.Array.items) |item| {
            const object = item.Object;

            var buffer_view = BufferView{
                .buffer = undefined,
                .byte_length = undefined,
            };

            if (object.get("buffer")) |buffer| {
                buffer_view.buffer = parseIndex(buffer);
            }

            if (object.get("byteLength")) |byte_length| {
                buffer_view.byte_length = @intCast(usize, byte_length.Integer);
            }

            if (object.get("byteOffset")) |byte_offset| {
                buffer_view.byte_offset = @intCast(usize, byte_offset.Integer);
            }

            if (object.get("byteStride")) |byte_stride| {
                buffer_view.byte_stride = @intCast(usize, byte_stride.Integer);
            }

            if (object.get("target")) |target| {
                buffer_view.target = @intToEnum(Target, target.Integer);
            }

            try self.data.buffer_views.append(buffer_view);
        }
    }

    if (gltf.root.Object.get("buffers")) |buffers| {
        for (buffers.Array.items) |item| {
            const object = item.Object;

            var buffer = Buffer{
                .byte_length = undefined,
            };

            if (object.get("uri")) |uri| {
                buffer.uri = uri.String;
            }

            if (object.get("byteLength")) |byte_length| {
                buffer.byte_length = @intCast(usize, byte_length.Integer);
            } else {
                panic("Buffer's byteLength is missing.", .{});
            }

            try self.data.buffers.append(buffer);
        }
    }

    if (gltf.root.Object.get("scene")) |default_scene| {
        self.data.scene = parseIndex(default_scene);
    }

    if (gltf.root.Object.get("scenes")) |scenes| {
        for (scenes.Array.items, 0..) |item, index| {
            const object = item.Object;

            var scene = Scene{
                .name = undefined,
            };

            if (object.get("name")) |name| {
                scene.name = try alloc.dupe(u8, name.String);
            } else {
                scene.name = try fmt.allocPrint(alloc, "Scene_{}", .{index});
            }

            if (object.get("nodes")) |nodes| {
                scene.nodes = ArrayList(Index).init(alloc);

                for (nodes.Array.items) |node| {
                    try scene.nodes.?.append(parseIndex(node));
                }
            }

            try self.data.scenes.append(scene);
        }
    }

    if (gltf.root.Object.get("materials")) |materials| {
        for (materials.Array.items, 0..) |item, m_index| {
            const object = item.Object;

            var material = Material{
                .name = undefined,
            };

            if (object.get("name")) |name| {
                material.name = try alloc.dupe(u8, name.String);
            } else {
                material.name = try fmt.allocPrint(alloc, "Material_{}", .{m_index});
            }

            if (object.get("pbrMetallicRoughness")) |pbrMetallicRoughness| {
                var metallic_roughness: MetallicRoughness = .{};
                if (pbrMetallicRoughness.Object.get("baseColorFactor")) |color_factor| {
                    for (color_factor.Array.items, 0..) |factor, i| {
                        metallic_roughness.base_color_factor[i] = parseFloat(f32, factor);
                    }
                }

                if (pbrMetallicRoughness.Object.get("metallicFactor")) |factor| {
                    metallic_roughness.metallic_factor = parseFloat(f32, factor);
                }

                if (pbrMetallicRoughness.Object.get("roughnessFactor")) |factor| {
                    metallic_roughness.roughness_factor = parseFloat(f32, factor);
                }

                if (pbrMetallicRoughness.Object.get("baseColorTexture")) |texture_info| {
                    metallic_roughness.base_color_texture = .{
                        .index = undefined,
                    };

                    if (texture_info.Object.get("index")) |index| {
                        metallic_roughness.base_color_texture.?.index = parseIndex(index);
                    }

                    if (texture_info.Object.get("texCoord")) |texcoord| {
                        metallic_roughness.base_color_texture.?.texcoord = @intCast(i32, texcoord.Integer);
                    }
                }

                if (pbrMetallicRoughness.Object.get("metallicRoughnessTexture")) |texture_info| {
                    metallic_roughness.metallic_roughness_texture = .{
                        .index = undefined,
                    };

                    if (texture_info.Object.get("index")) |index| {
                        metallic_roughness.metallic_roughness_texture.?.index = parseIndex(index);
                    }

                    if (texture_info.Object.get("texCoord")) |texcoord| {
                        metallic_roughness.metallic_roughness_texture.?.texcoord = @intCast(i32, texcoord.Integer);
                    }
                }

                material.metallic_roughness = metallic_roughness;
            }

            if (object.get("normalTexture")) |normal_texture| {
                material.normal_texture = .{
                    .index = undefined,
                };

                if (normal_texture.Object.get("index")) |index| {
                    material.normal_texture.?.index = parseIndex(index);
                }

                if (normal_texture.Object.get("texCoord")) |index| {
                    material.normal_texture.?.texcoord = @intCast(i32, index.Integer);
                }

                if (normal_texture.Object.get("scale")) |scale| {
                    material.normal_texture.?.scale = parseFloat(f32, scale);
                }
            }

            if (object.get("emissiveTexture")) |emissive_texture| {
                material.emissive_texture = .{
                    .index = undefined,
                };

                if (emissive_texture.Object.get("index")) |index| {
                    material.emissive_texture.?.index = parseIndex(index);
                }

                if (emissive_texture.Object.get("texCoord")) |index| {
                    material.emissive_texture.?.texcoord = @intCast(i32, index.Integer);
                }
            }

            if (object.get("occlusionTexture")) |occlusion_texture| {
                material.occlusion_texture = .{
                    .index = undefined,
                };

                if (occlusion_texture.Object.get("index")) |index| {
                    material.occlusion_texture.?.index = parseIndex(index);
                }

                if (occlusion_texture.Object.get("texCoord")) |index| {
                    material.occlusion_texture.?.texcoord = @intCast(i32, index.Integer);
                }

                if (occlusion_texture.Object.get("strength")) |strength| {
                    material.occlusion_texture.?.strength = parseFloat(f32, strength);
                }
            }

            if (object.get("alphaMode")) |alpha_mode| {
                if (mem.eql(u8, alpha_mode.String, "OPAQUE")) {
                    material.alpha_mode = .@"opaque";
                }
                if (mem.eql(u8, alpha_mode.String, "MASK")) {
                    material.alpha_mode = .mask;
                }
                if (mem.eql(u8, alpha_mode.String, "BLEND")) {
                    material.alpha_mode = .blend;
                }
            }

            if (object.get("doubleSided")) |double_sided| {
                material.is_double_sided = double_sided.Bool;
            }

            if (object.get("alphaCutoff")) |alpha_cutoff| {
                material.alpha_cutoff = parseFloat(f32, alpha_cutoff);
            }

            if (object.get("emissiveFactor")) |emissive_factor| {
                for (emissive_factor.Array.items, 0..) |factor, i| {
                    material.emissive_factor[i] = parseFloat(f32, factor);
                }
            }

            try self.data.materials.append(material);
        }
    }

    if (gltf.root.Object.get("textures")) |textures| {
        for (textures.Array.items) |item| {
            var texture = Texture{};

            if (item.Object.get("source")) |source| {
                texture.source = parseIndex(source);
            }

            if (item.Object.get("sampler")) |sampler| {
                texture.sampler = parseIndex(sampler);
            }

            try self.data.textures.append(texture);
        }
    }

    if (gltf.root.Object.get("animations")) |animations| {
        for (animations.Array.items, 0..) |item, index| {
            const object = item.Object;

            var animation = Animation{
                .samplers = ArrayList(AnimationSampler).init(alloc),
                .channels = ArrayList(Channel).init(alloc),
                .name = undefined,
            };

            if (item.Object.get("name")) |name| {
                animation.name = try alloc.dupe(u8, name.String);
            } else {
                animation.name = try fmt.allocPrint(alloc, "Animation_{}", .{index});
            }

            if (object.get("samplers")) |samplers| {
                for (samplers.Array.items) |sampler_item| {
                    var sampler: AnimationSampler = .{
                        .input = undefined,
                        .output = undefined,
                    };

                    if (sampler_item.Object.get("input")) |input| {
                        sampler.input = parseIndex(input);
                    } else {
                        panic("Animation sampler's input is missing.", .{});
                    }

                    if (sampler_item.Object.get("output")) |output| {
                        sampler.output = parseIndex(output);
                    } else {
                        panic("Animation sampler's output is missing.", .{});
                    }

                    if (sampler_item.Object.get("interpolation")) |interpolation| {
                        if (mem.eql(u8, interpolation.String, "LINEAR")) {
                            sampler.interpolation = .linear;
                        }

                        if (mem.eql(u8, interpolation.String, "STEP")) {
                            sampler.interpolation = .step;
                        }

                        if (mem.eql(u8, interpolation.String, "CUBICSPLINE")) {
                            sampler.interpolation = .cubicspline;
                        }
                    }

                    try animation.samplers.append(sampler);
                }
            }

            if (object.get("channels")) |channels| {
                for (channels.Array.items) |channel_item| {
                    var channel: Channel = .{ .sampler = undefined, .target = .{
                        .node = undefined,
                        .property = undefined,
                    } };

                    if (channel_item.Object.get("sampler")) |sampler_index| {
                        channel.sampler = parseIndex(sampler_index);
                    } else {
                        panic("Animation channel's sampler is missing.", .{});
                    }

                    if (channel_item.Object.get("target")) |target_item| {
                        if (target_item.Object.get("node")) |node_index| {
                            channel.target.node = parseIndex(node_index);
                        } else {
                            panic("Animation target's node is missing.", .{});
                        }

                        if (target_item.Object.get("path")) |path| {
                            if (mem.eql(u8, path.String, "translation")) {
                                channel.target.property = .translation;
                            } else if (mem.eql(u8, path.String, "rotation")) {
                                channel.target.property = .rotation;
                            } else if (mem.eql(u8, path.String, "scale")) {
                                channel.target.property = .scale;
                            } else if (mem.eql(u8, path.String, "weights")) {
                                channel.target.property = .weights;
                            } else {
                                panic("Animation path/property is invalid.", .{});
                            }
                        } else {
                            panic("Animation target's path/property is missing.", .{});
                        }
                    } else {
                        panic("Animation channel's target is missing.", .{});
                    }

                    try animation.channels.append(channel);
                }
            }

            try self.data.animations.append(animation);
        }
    }

    if (gltf.root.Object.get("samplers")) |samplers| {
        for (samplers.Array.items) |item| {
            const object = item.Object;
            var sampler = TextureSampler{};

            if (object.get("magFilter")) |mag_filter| {
                sampler.mag_filter = @intToEnum(MagFilter, mag_filter.Integer);
            }

            if (object.get("minFilter")) |min_filter| {
                sampler.min_filter = @intToEnum(MinFilter, min_filter.Integer);
            }

            if (object.get("wrapS")) |wrap_s| {
                sampler.wrap_s = @intToEnum(WrapMode, wrap_s.Integer);
            }

            if (object.get("wrapt")) |wrap_t| {
                sampler.wrap_t = @intToEnum(WrapMode, wrap_t.Integer);
            }

            try self.data.samplers.append(sampler);
        }
    }

    if (gltf.root.Object.get("images")) |images| {
        for (images.Array.items) |item| {
            const object = item.Object;
            var image = Image{};

            if (object.get("uri")) |uri| {
                image.uri = try alloc.dupe(u8, uri.String);
            }

            if (object.get("mimeType")) |mime_type| {
                image.mime_type = try alloc.dupe(u8, mime_type.String);
            }

            if (object.get("bufferView")) |buffer_view| {
                image.buffer_view = parseIndex(buffer_view);
            }

            try self.data.images.append(image);
        }
    }

    if (gltf.root.Object.get("extensions")) |extensions|
    {
        if (extensions.Object.get("KHR_lights_punctual")) |lights_punctual|
        {
            if (lights_punctual.Object.get("lights")) |lights|
            {
                for (lights.Array.items) |item|
                {
                    const object: std.json.ObjectMap = item.Object;

                    var light = Light {
                        .name = null,
                        .type = undefined,
                        .range = std.math.inf(f32),
                        .spot = null,
                    };

                    if (object.get("name")) |name|
                    {
                        light.name = try alloc.dupe(u8, name.String);
                    }

                    if (object.get("color")) |color|
                    {
                        for (color.Array.items, 0..) |component, i|
                        {
                            switch (component)
                            {
                                .Float => |float| {
                                    light.color[i] = @floatCast(f32, float);
                                },
                                .Integer => |integer| {
                                    light.color[i] = @intToFloat(f32, integer);
                                },
                                else => unreachable,
                            }
                        }
                    }

                    if (object.get("intensity")) |intensity|
                    {
                        switch (intensity)
                        {
                            .Float => |float| {
                                light.intensity = @floatCast(f32, float);
                            },
                            .Integer => |integer| {
                                light.intensity = @intToFloat(f32, integer);
                            },
                            else => unreachable,
                        }                        
                    }

                    if (object.get("type")) |@"type"|
                    {
                        const light_type = std.meta.stringToEnum(LightType, @"type".String) orelse unreachable;

                        light.type = light_type;
                    }

                    if (object.get("range")) |range|
                    {
                        switch (range)
                        {
                            .Float => |float| {
                                light.range = @floatCast(f32, float);
                            },
                            .Integer => |integer| {
                                light.range = @intToFloat(f32, integer);
                            },
                            else => unreachable,
                        }  
                    }

                    if (object.get("spot")) |spot|
                    {
                        light.spot = .{};

                        if (spot.Object.get("innerConeAngle")) |inner_cone_angle|
                        {
                            switch (inner_cone_angle)
                            {
                                .Float => |float| {
                                    light.spot.?.inner_cone_angle = @floatCast(f32, float);
                                },
                                .Integer => |integer| {
                                    light.spot.?.inner_cone_angle = @intToFloat(f32, integer);
                                },
                                else => undefined,
                            }
                        }

                        if (spot.Object.get("outerConeAngle")) |outer_cone_angle|
                        {
                            switch (outer_cone_angle)
                            {
                                .Float => |float| {
                                    light.spot.?.outer_cone_angle = @floatCast(f32, float);
                                },
                                .Integer => |integer| {
                                    light.spot.?.outer_cone_angle = @intToFloat(f32, integer);
                                },
                                else => undefined,
                            }
                        }
                    }

                    try self.data.lights.append(light);
                }
            }
        }
    }

    // For each node, fill parent indexes.
    for (self.data.scenes.items) |scene| {
        if (scene.nodes) |nodes| {
            for (nodes.items) |node_index| {
                var node = &self.data.nodes.items[node_index];
                fillParents(&self.data, node, node_index);
            }
        }
    }
}

// In 'gltf' files, often values are array indexes;
// this function casts Integer to 'usize'.
fn parseIndex(component: json.Value) usize {
    return switch (component) {
        .Integer => |val| @intCast(usize, val),
        else => panic(
            "The json component '{any}' is not valid number.",
            .{component},
        ),
    };
}

// Exact values could be interpreted as Integer, often we want only
// floating numbers.
fn parseFloat(comptime T: type, component: json.Value) T {
    const type_info = @typeInfo(T);
    if (type_info != .Float) {
        panic(
            "Given type '{any}' is not a floating number.",
            .{type_info},
        );
    }

    return switch (component) {
        .Float => |val| @floatCast(T, val),
        .Integer => |val| @intToFloat(T, val),
        else => panic(
            "The json component '{any}' is not a number.",
            .{component},
        ),
    };
}

fn fillParents(data: *Data, node: *Node, parent_index: Index) void {
    for (node.children.items) |child_index| {
        var child_node = &data.nodes.items[child_index];
        child_node.parent = parent_index;
        fillParents(data, child_node, child_index);
    }
}

test "gltf.parseGlb" {
    const allocator = std.testing.allocator;
    const expectEqualSlices = std.testing.expectEqualSlices;

    // This is the '.glb' file.
    const glb_buf = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/box_binary/Box.glb",
        512_000,
    );
    defer allocator.free(glb_buf);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try expectEqualSlices(u8, gltf.data.asset.version, "Undefined");

    try gltf.parseGlb(glb_buf);

    const mesh = gltf.data.meshes.items[0];
    for (mesh.primitives.items) |primitive| {
        for (primitive.attributes.items) |attribute| {
            switch (attribute) {
                .position => |accessor_index| {
                    var tmp = ArrayList(f32).init(allocator);
                    defer tmp.deinit();

                    const accessor = gltf.data.accessors.items[accessor_index];
                    gltf.getDataFromBufferView(f32, &tmp, accessor, gltf.glb_binary.?);

                    try expectEqualSlices(f32, tmp.items, &[72]f32{
                        // zig fmt: off
                        -0.50, -0.50, 0.50, 0.50, -0.50, 0.50, -0.50, 0.50, 0.50,
                        0.50, 0.50, 0.50, 0.50, -0.50, 0.50, -0.50, -0.50, 0.50, 
                        0.50, -0.50, -0.50, -0.50, -0.50, -0.50, 0.50, 0.50, 0.50, 
                        0.50, -0.50, 0.50, 0.50, 0.50, -0.50, 0.50, -0.50, -0.50, 
                        -0.50, 0.50, 0.50, 0.50, 0.50, 0.50, -0.50, 0.50, -0.50, 
                        0.50, 0.50, -0.50, -0.50, -0.50, 0.50, -0.50, 0.50, 0.50, 
                        -0.50, -0.50, -0.50, -0.50, 0.50, -0.50, -0.50, -0.50, -0.50, 
                        -0.50, 0.50, -0.50, 0.50, -0.50, -0.50, 0.50, 0.50, -0.50,
                    });
                },
                else => {},
            }
        }
    }
}

test "gltf.parseGlbTextured" {
    const allocator = std.testing.allocator;
    const expectEqualSlices = std.testing.expectEqualSlices;

    // This is the '.glb' file.
    const glb_buf = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/box_binary_textured/BoxTextured.glb",
        512_000,
    );
    defer allocator.free(glb_buf);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try gltf.parseGlb(glb_buf);

    const test_to_check = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/box_binary_textured/test.png",
        512_000
    );
    defer allocator.free(test_to_check);

    const data = gltf.data.images.items[0].data.?;
    try expectEqualSlices(u8, test_to_check, data);
}

test "gltf.parse" {
    const allocator = std.testing.allocator;
    const expectEqualSlices = std.testing.expectEqualSlices;
    const expectEqual = std.testing.expectEqual;

    // This is the '.gltf' file, a json specifying what information is in the
    // model and how to retrieve it inside binary file(s).
    const buf = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/rigged_simple/RiggedSimple.gltf",
        512_000,
    );
    defer allocator.free(buf);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try expectEqualSlices(u8, gltf.data.asset.version, "Undefined");

    try gltf.parse(buf);

    try expectEqualSlices(u8, gltf.data.asset.version, "2.0");
    try expectEqualSlices(u8, gltf.data.asset.generator.?, "COLLADA2GLTF");

    try expectEqual(gltf.data.scene, 0);

    // Nodes.
    const nodes = gltf.data.nodes.items;
    try expectEqualSlices(u8, nodes[0].name, "Z_UP");
    try expectEqualSlices(usize, nodes[0].children.items, &[_]usize{1});
    try expectEqualSlices(u8, nodes[2].name, "Cylinder");
    try expectEqual(nodes[2].skin, 0);

    try expectEqual(gltf.data.buffers.items.len > 0, true);

    // Skin
    const skin = gltf.data.skins.items[0];
    try expectEqualSlices(u8, skin.name, "Armature");
}

test "gltf.parse (cameras)" {
    const allocator = std.testing.allocator;
    const expectEqual = std.testing.expectEqual;

    const buf = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/cameras/Cameras.gltf",
        512_000,
    );
    defer allocator.free(buf);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try gltf.parse(buf);

    try expectEqual(gltf.data.nodes.items[1].camera, 0);
    try expectEqual(gltf.data.nodes.items[2].camera, 1);

    const camera_0 = gltf.data.cameras.items[0];
    try expectEqual(camera_0.type.perspective, Camera.Perspective{
        .aspect_ratio = 1.0,
        .yfov = 0.7,
        .zfar = 100,
        .znear = 0.01,
    });

    const camera_1 = gltf.data.cameras.items[1];
    try expectEqual(camera_1.type.orthographic, Camera.Orthographic{
        .xmag = 1.0,
        .ymag = 1.0,
        .zfar = 100,
        .znear = 0.01,
    });
}

test "gltf.getDataFromBufferView" {
    const allocator = std.testing.allocator;
    const expectEqualSlices = std.testing.expectEqualSlices;

    const buf = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/box/Box.gltf",
        512_000,
    );
    defer allocator.free(buf);

    // This is the '.bin' file containing all the gltf underneath data.
    const binary = try std.fs.cwd().readFileAllocOptions(
        allocator,
        "test-samples/box/Box0.bin",
        5_000_000,
        null,
        // From gltf spec, data from BufferView should be 4 bytes aligned.
        4,
        null,
    );
    defer allocator.free(binary);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try gltf.parse(buf);

    const mesh = gltf.data.meshes.items[0];
    for (mesh.primitives.items) |primitive| {
        for (primitive.attributes.items) |attribute| {
            switch (attribute) {
                .position => |accessor_index| {
                    var tmp = ArrayList(f32).init(allocator);
                    defer tmp.deinit();

                    const accessor = gltf.data.accessors.items[accessor_index];
                    gltf.getDataFromBufferView(f32, &tmp, accessor, binary);

                    try expectEqualSlices(f32, tmp.items, &[72]f32{
                        // zig fmt: off
                        -0.50, -0.50, 0.50, 0.50, -0.50, 0.50, -0.50, 0.50, 0.50,
                        0.50, 0.50, 0.50, 0.50, -0.50, 0.50, -0.50, -0.50, 0.50, 
                        0.50, -0.50, -0.50, -0.50, -0.50, -0.50, 0.50, 0.50, 0.50, 
                        0.50, -0.50, 0.50, 0.50, 0.50, -0.50, 0.50, -0.50, -0.50, 
                        -0.50, 0.50, 0.50, 0.50, 0.50, 0.50, -0.50, 0.50, -0.50, 
                        0.50, 0.50, -0.50, -0.50, -0.50, 0.50, -0.50, 0.50, 0.50, 
                        -0.50, -0.50, -0.50, -0.50, 0.50, -0.50, -0.50, -0.50, -0.50, 
                        -0.50, 0.50, -0.50, 0.50, -0.50, -0.50, 0.50, 0.50, -0.50,
                    });
                },
                else => {},
            }
        }
    }
}

test "gltf.parse (lights)"
{
    const allocator = std.testing.allocator;
    const expect = std.testing.expect;
    const expectEqual = std.testing.expectEqual;

    const buf = try std.fs.cwd().readFileAlloc(
        allocator,
        "test-samples/khr_lights_punctual/Lights.gltf",
        512_000,
    );
    defer allocator.free(buf);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try gltf.parse(buf);

    try expectEqual(@as(usize, 3), gltf.data.lights.items.len);

    try expect(gltf.data.lights.items[0].name != null);
    try expect(std.mem.eql(u8, "Light", gltf.data.lights.items[0].name.?));
    try expectEqual([3]f32 { 1, 1, 1 }, gltf.data.lights.items[0].color);
    try expectEqual(@as(f32, 1000), gltf.data.lights.items[0].intensity);
    try expectEqual(LightType.point, gltf.data.lights.items[0].type);

    try expect(gltf.data.lights.items[1].name != null);
    try expect(std.mem.eql(u8, "Light.001", gltf.data.lights.items[1].name.?));
    try expectEqual([3]f32 { 1, 1, 1 }, gltf.data.lights.items[1].color);
    try expectEqual(@as(f32, 1000), gltf.data.lights.items[1].intensity);
    try expectEqual(LightType.spot, gltf.data.lights.items[1].type);

    try expect(gltf.data.lights.items[1].spot != null);
    try expectEqual(@as(f32, 0), gltf.data.lights.items[1].spot.?.inner_cone_angle);
    try expectEqual(@as(f32, 1), gltf.data.lights.items[1].spot.?.outer_cone_angle);

    try expect(gltf.data.lights.items[2].name != null);
    try expect(std.mem.eql(u8, "Light.002", gltf.data.lights.items[2].name.?));
    try expectEqual([3]f32 { 1, 1, 1 }, gltf.data.lights.items[2].color);
    try expectEqual(@as(f32, 1000), gltf.data.lights.items[2].intensity);
    try expectEqual(LightType.directional, gltf.data.lights.items[2].type);

    try expect(gltf.data.nodes.items[0].light != null);
    try expectEqual(@as(?Index, 0), gltf.data.nodes.items[0].light);
}
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
const panic = std.debug.panic;
const print = std.debug.print;
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

pub const Data = struct {
    scenes: ArrayList(Scene),
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
};

arena: *ArenaAllocator,
data: Data,

pub fn init(allocator: Allocator) Self {
    var arena = allocator.create(ArenaAllocator) catch {
        panic("Error while allocating memory for gltf arena.", .{});
    };

    arena.* = ArenaAllocator.init(allocator);

    const alloc = arena.allocator();
    return Self{
        .arena = arena,
        .data = .{
            .scenes = ArrayList(Scene).init(alloc),
            .nodes = ArrayList(Node).init(alloc),
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
        },
    };
}

/// Fill data by parsing a glTF file's buffer.
pub fn parse(self: *Self, gltf_buffer: []const u8) !void {
    const alloc = self.arena.allocator();

    var parser = json.Parser.init(alloc, false);
    defer parser.deinit();

    var gltf = try parser.parse(gltf_buffer);
    defer gltf.deinit();

    if (gltf.root.Object.get("nodes")) |nodes| {
        for (nodes.Array.items) |item, index| {
            const object = item.Object;

            var node = Node{
                .name = undefined,
                .children = ArrayList(Index).init(alloc),
            };

            if (object.get("name")) |name| {
                node.name = try alloc.dupe(u8, name.String);
            } else {
                node.name = try std.fmt.allocPrint(alloc, "Node_{}", .{index});
            }

            if (object.get("mesh")) |mesh| {
                node.mesh = parseIndex(mesh);
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
                for (rotation.Array.items) |component, i| {
                    node.rotation[i] = parseFloat(f32, component);
                }
            }

            if (object.get("translation")) |translation| {
                for (translation.Array.items) |component, i| {
                    node.translation[i] = parseFloat(f32, component);
                }
            }

            if (object.get("scale")) |scale| {
                for (scale.Array.items) |component, i| {
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

                for (matrix.Array.items) |component, i| {
                    node.matrix.?[i] = parseFloat(f32, component);
                }
            }

            try self.data.nodes.append(node);
        }
    }

    if (gltf.root.Object.get("skins")) |skins| {
        for (skins.Array.items) |item, index| {
            const object = item.Object;

            var skin = Skin{
                .name = undefined,
                .joints = ArrayList(Index).init(alloc),
            };

            if (object.get("name")) |name| {
                skin.name = try alloc.dupe(u8, name.String);
            } else {
                skin.name = try std.fmt.allocPrint(alloc, "Skin_{}", .{index});
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
        for (meshes.Array.items) |item, index| {
            const object = item.Object;

            var mesh: Mesh = .{
                .name = undefined,
                .primitives = ArrayList(Primitive).init(alloc),
            };

            if (object.get("name")) |name| {
                mesh.name = try alloc.dupe(u8, name.String);
            } else {
                mesh.name = try std.fmt.allocPrint(alloc, "Mesh_{}", .{index});
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
                .uri = undefined,
                .byte_length = undefined,
            };

            if (object.get("uri")) |uri| {
                buffer.uri = uri.String;
            } else {
                panic("Buffer's uri is missing.", .{});
            }

            if (object.get("byteLength")) |byte_length| {
                buffer.byte_length = @intCast(usize, byte_length.Integer);
            } else {
                panic("Buffer's byteLength is missing.", .{});
            }
        }
    }

    if (gltf.root.Object.get("scenes")) |scenes| {
        for (scenes.Array.items) |item, index| {
            const object = item.Object;

            var scene = Scene{
                .name = undefined,
            };

            if (object.get("name")) |name| {
                scene.name = try alloc.dupe(u8, name.String);
            } else {
                scene.name = try std.fmt.allocPrint(alloc, "Scene_{}", .{index});
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
        for (materials.Array.items) |item, m_index| {
            const object = item.Object;

            var material = Material{
                .name = undefined,
            };

            if (object.get("name")) |name| {
                material.name = try alloc.dupe(u8, name.String);
            } else {
                material.name = try std.fmt.allocPrint(alloc, "Material_{}", .{m_index});
            }

            if (object.get("pbrMetallicRoughness")) |pbrMetallicRoughness| {
                var metallic_roughness: MetallicRoughness = .{};
                if (pbrMetallicRoughness.Object.get("baseColorFactor")) |color_factor| {
                    for (color_factor.Array.items) |factor, i| {
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
                for (emissive_factor.Array.items) |factor, i| {
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
        for (animations.Array.items) |item, index| {
            const object = item.Object;

            var animation = Animation{
                .samplers = ArrayList(AnimationSampler).init(alloc),
                .channels = ArrayList(Channel).init(alloc),
                .name = undefined,
            };

            if (item.Object.get("name")) |name| {
                animation.name = try alloc.dupe(u8, name.String);
            } else {
                animation.name = try std.fmt.allocPrint(alloc, "Animation_{}", .{index});
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

    try gltf.parse(buf);

    const nodes = gltf.data.nodes.items;
    const skin = gltf.data.skins.items[0];

    // Nodes.
    try expectEqualSlices(u8, nodes[0].name, "Z_UP");
    try expectEqualSlices(usize, nodes[0].children.items, &[_]usize{1});
    try expectEqualSlices(u8, nodes[2].name, "Cylinder");
    try expectEqual(nodes[2].skin, 0);

    // Skin
    try expectEqualSlices(u8, skin.name, "Armature");
}

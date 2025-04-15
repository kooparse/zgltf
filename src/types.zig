const std = @import("std");
const Gltf = @import("main.zig");
const pi = std.math.pi;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;

/// Index of element in data arrays.
pub const Index = usize;

/// A node in the node hierarchy.
///
/// When the node contains skin, all mesh.primitives must contain
/// JOINTS_0 and WEIGHTS_0 attributes. A node may have either a matrix
/// or any combination of translation/rotation/scale (TRS) properties.
/// TRS properties are converted to matrices and postmultiplied in
/// the T * R * S order to compose the transformation matrix.
/// If none are provided, the transform is the identity.
///
/// When a node is targeted for animation (referenced by
/// an animation.channel.target), matrix must not be present.
pub const Node = struct {
    /// The user-defined name of this object.
    /// Default to `Node_{index}`.
    name: []const u8,
    /// The index of the node's parent.
    /// A node is called a root node when it doesn’t have a parent.
    parent: ?Index = null,
    /// The index of the mesh in this node.
    mesh: ?Index = null,
    /// The index of the camera referenced by this node.
    camera: ?Index = null,
    /// The index of the skin referenced by this node.
    skin: ?Index = null,
    /// The indices of this node’s children.
    children: ArrayList(Index),
    /// A floating-point 4x4 transformation matrix stored in column-major order.
    matrix: ?[16]f32 = null,
    /// The node’s unit quaternion rotation in the order (x, y, z, w),
    /// where w is the scalar.
    rotation: [4]f32 = [_]f32{ 0, 0, 0, 1 },
    /// The node’s non-uniform scale, given as the scaling factors
    /// along the x, y, and z axes.
    scale: [3]f32 = [_]f32{ 1, 1, 1 },
    /// The node’s translation along the x, y, and z axes.
    translation: [3]f32 = [_]f32{ 0, 0, 0 },
    /// The weights of the instantiated morph target.
    /// The number of array elements must match the number of morph targets
    /// of the referenced mesh. When defined, mesh mush also be defined.
    weights: ?[]usize = null,
    ///The index of the light referenced by this node.
    light: ?Index = null,
};

/// A buffer points to binary geometry, animation, or skins.
pub const Buffer = struct {
    /// Relative paths are relative to the current glTF asset.
    /// It could contains a data:-URI instead of a path.
    /// Note: data-uri isn't implemented in this library.
    uri: ?[]const u8 = null,
    /// The length of the buffer in bytes.
    byte_length: usize,
};

/// A view into a buffer generally representing a subset of the buffer.
pub const BufferView = struct {
    /// The index of the buffer.
    buffer: Index,
    /// The length of the bufferView in bytes.
    byte_length: usize,
    /// The offset into the buffer in bytes.
    byte_offset: usize = 0,
    /// The stride, in bytes.
    byte_stride: ?usize = null,
    /// The hint representing the intended GPU buffer type
    /// to use with this buffer view.
    target: ?Target = null,
};

/// A typed view into a buffer view that contains raw binary data.
pub const Accessor = struct {
    /// The index of the bufferView.
    buffer_view: ?Index = null,
    /// The offset relative to the start of the buffer view in bytes.
    byte_offset: usize = 0,
    /// The datatype of the accessor’s components.
    component_type: ComponentType,
    /// Specifies if the accessor’s elements are scalars, vectors, or matrices.
    type: AccessorType,
    /// Computed stride: @sizeOf(component_type) * type.
    stride: usize,
    /// The number of elements referenced by this accessor.
    count: i32,
    /// Specifies whether integer data values are normalized before usage.
    normalized: bool = false,

    pub fn iterator(
        accessor: Accessor,
        comptime T: type,
        gltf: *const Gltf,
        binary: []align(4) const u8,
    ) AccessorIterator(T) {
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

        const buffer_view = gltf.data.buffer_views.items[accessor.buffer_view.?];

        const comp_size = @sizeOf(T);
        const offset = (accessor.byte_offset + buffer_view.byte_offset) / comp_size;

        const stride = blk: {
            if (buffer_view.byte_stride) |byte_stride| {
                break :blk byte_stride / comp_size;
            } else {
                break :blk accessor.stride / comp_size;
            }
        };

        const total_count: usize = @intCast(accessor.count);
        const datum_count: usize = switch (accessor.type) {
            .scalar => 1,
            .vec2 => 2,
            .vec3 => 3,
            .vec4 => 4,
            .mat4x4 => 16,
            else => {
                panic("Accessor type '{}' not implemented.", .{accessor.type});
            },
        };

        const data: [*]const T = @ptrCast(@alignCast(binary.ptr));

        return .{
            .offset = offset,
            .stride = stride,
            .total_count = total_count,
            .datum_count = datum_count,
            .data = data,
            .current = 0,
        };
    }
};

/// Iterator over accessor elements
pub fn AccessorIterator(comptime T: type) type {
    return struct {
        offset: usize,
        stride: usize,
        total_count: usize,
        datum_count: usize,
        data: [*]const T,

        current: usize,

        /// Returns the next element of the accessor, or null if iteration is done.
        pub fn next(self: *@This()) ?[]const T {
            if (self.current >= self.total_count) return null;

            const slice = (self.data + self.offset + self.current * self.stride)[0..self.datum_count];
            self.current += 1;
            return slice;
        }

        /// Returns the next element of the accessor, or null if iteration is done. Does not change self.current.
        pub fn peek(self: *const @This()) ?[]const T {
            var copy = self.*;
            return copy.next();
        }

        /// Resets the iterator to the first element
        pub fn reset(self: *@This()) void {
            self.current = 0;
        }
    };
}

/// The root nodes of a scene.
pub const Scene = struct {
    /// The user-defined name of this object.
    name: []const u8,
    /// The indices of each root node.
    nodes: ?ArrayList(Index) = null,
};

/// Joints and matrices defining a skin.
pub const Skin = struct {
    /// The user-defined name of this object.
    name: []const u8,
    /// The index of the accessor containing the floating-point
    /// 4x4 inverse-bind matrices.
    inverse_bind_matrices: ?Index = null,
    /// The index of the node used as a skeleton root.
    skeleton: ?Index = null,
    /// Indices of skeleton nodes, used as joints in this skin.
    joints: ArrayList(Index),
};

/// Reference to a texture.
const TextureInfo = struct {
    /// The index of the texture.
    index: Index,
    /// The set index of texture’s TEXCOORD attribute
    /// used for texture coordinate mapping.
    texcoord: i32 = 0,
};

/// Reference to a normal texture.
const NormalTextureInfo = struct {
    /// The index of the texture.
    index: Index,
    /// The set index of texture’s TEXCOORD attribute
    /// used for texture coordinate mapping.
    texcoord: i32 = 0,
    /// The scalar parameter applied to each normal
    /// vector of the normal texture.
    scale: f32 = 1,
};

/// Reference to an occlusion texture.
const OcclusionTextureInfo = struct {
    /// The index of the texture.
    index: Index,
    /// The set index of texture’s TEXCOORD attribute
    /// used for texture coordinate mapping.
    texcoord: i32 = 0,
    /// A scalar multiplier controlling the amount of occlusion applied.
    strength: f32 = 1,
};

/// A set of parameter values that are used to define
/// the metallic-roughness material model
/// from Physically-Based Rendering methodology.
pub const MetallicRoughness = struct {
    /// The factors for the base color of the material.
    base_color_factor: [4]f32 = [_]f32{ 1, 1, 1, 1 },
    /// The base color texture.
    base_color_texture: ?TextureInfo = null,
    /// The factor for the metalness of the material.
    metallic_factor: f32 = 1,
    /// The factor for the roughness of the material.
    roughness_factor: f32 = 1,
    /// The metallic-roughness texture.
    metallic_roughness_texture: ?TextureInfo = null,
};

/// The material appearance of a primitive.
pub const Material = struct {
    /// The user-defined name of this object.
    name: []const u8,
    /// A set of parameter values that are used to define
    /// the metallic-roughness material model
    /// from Physically Based Rendering methodology.
    metallic_roughness: MetallicRoughness = .{},
    /// The tangent space normal texture.
    normal_texture: ?NormalTextureInfo = null,
    /// The occlusion texture.
    occlusion_texture: ?OcclusionTextureInfo = null,
    /// The emissive texture.
    emissive_texture: ?TextureInfo = null,
    /// The factors for the emissive color of the material.
    emissive_factor: [3]f32 = [_]f32{ 0, 0, 0 },
    /// The alpha rendering mode of the material.
    alpha_mode: AlphaMode = .@"opaque",
    /// The alpha cutoff value of the material.
    alpha_cutoff: f32 = 0.5,
    /// Specifies whether the material is double sided.
    /// If it's false, back-face culling is enabled.
    /// If it's true, back-face culling is disabled and
    /// double sided lighting is enabled.
    is_double_sided: bool = false,
    /// Emissive strength multiplier for the emissive factor/texture.
    /// Note: from khr_materials_emissive_strength extension.
    emissive_strength: f32 = 1.0,
    /// Index of refraction of material.
    /// Note: from khr_materials_ior extension.
    ior: f32 = 1.5,
    /// The factor for the transmission of the material.
    /// Note: from khr_materials_transmission extension.
    transmission_factor: f32 = 0.0,
    /// The transmission texture.
    /// Note: from khr_materials_transmission extension.
    transmission_texture: ?TextureInfo = null,
    /// The thickness of the volume beneath the surface.
    /// Note: from khr_materials_volume extension.
    thickness_factor: f32 = 0.0,
    /// A texture that defines the thickness, stored in the G channel.
    /// Note: from khr_materials_volume extension.
    thickness_texture: ?TextureInfo = null,
    /// Density of the medium.
    /// Note: from khr_materials_volume extension.
    attenuation_distance: f32 = std.math.inf(f32),
    /// The color that white light turns into due to absorption.
    /// Note: from khr_materials_volume extension.
    attenuation_color: [3]f32 = [_]f32{ 1, 1, 1 },
    /// The strength of the dispersion effect.
    /// Note: from khr_materials_dispersion extension.
    dispersion: f32 = 0.0,
};

/// The material’s alpha rendering mode enumeration specifying
/// the interpretation of the alpha value of the base color.
const AlphaMode = enum {
    /// The alpha value is ignored, and the rendered output is fully opaque.
    @"opaque",
    /// The rendered output is either fully opaque or fully transparent
    /// depending on the alpha value and the specified alpha_cutoff value.
    /// Note: The exact appearance of the edges may be subject to
    /// implementation-specific techniques such as “Alpha-to-Coverage”.
    mask,
    /// The alpha value is used to composite the source and destination areas.
    /// The rendered output is combined with the background using
    /// the normal painting operation (i.e. the Porter and Duff over operator).
    blend,
};

/// A texture and its sampler.
pub const Texture = struct {
    /// The index of the sampler used by this texture.
    /// When undefined, a sampler with repeat wrapping and
    /// auto filtering should be used.
    sampler: ?Index = null,
    /// The index of the image used by this texture.
    /// When undefined, an extension or other mechanism should supply
    /// an alternate texture source, otherwise behavior is undefined.
    source: ?Index = null,
    /// Extension object with extension-specific objects.
    extensions: struct {
        EXT_texture_webp: ?struct {
            /// The index of the WebP image used by this texture.
            source: Index,
        } = null,
    } = .{},
};

/// Image data used to create a texture.
/// Image may be referenced by an uri or a buffer view index.
pub const Image = struct {
    /// The user-defined name of this object.
    name: ?[]const u8 = null,
    /// The URI (or IRI) of the image.
    uri: ?[]const u8 = null,
    /// The image’s media type.
    /// This field must be defined when bufferView is defined.
    mime_type: ?[]const u8 = null,
    /// The index of the bufferView that contains the image.
    /// Note: This field must not be defined when uri is defined.
    buffer_view: ?Index = null,
    /// The image's data calculated from the buffer/buffer_view.
    /// Only there if glb file is loaded.
    data: ?[]const u8 = null,
};

pub const WrapMode = enum(u32) {
    clamp_to_edge = 33071,
    mirrored_repeat = 33648,
    repeat = 10497,
};

pub const MinFilter = enum(u32) {
    nearest = 9728,
    linear = 9729,
    nearest_mipmap_nearest = 9984,
    linear_mipmap_nearest = 9985,
    nearest_mipmap_linear = 9986,
    linear_mipmap_linear = 9987,
};

pub const MagFilter = enum(u32) {
    nearest = 9728,
    linear = 9729,
};

/// Texture sampler properties for filtering and wrapping modes.
pub const TextureSampler = struct {
    /// Magnification filter.
    mag_filter: ?MagFilter = null,
    /// Minification filter.
    min_filter: ?MinFilter = null,
    /// S (U) wrapping mode.
    wrap_s: WrapMode = .repeat,
    /// T (U) wrapping mode.
    wrap_t: WrapMode = .repeat,
};

/// Values are Accessor's index.
pub const Attribute = union(enum) {
    position: Index,
    normal: Index,
    tangent: Index,
    texcoord: Index,
    color: Index,
    joints: Index,
    weights: Index,
};

pub const AccessorType = enum {
    scalar,
    vec2,
    vec3,
    vec4,
    mat2x2,
    mat3x3,
    mat4x4,
};

/// Enum values from GLTF 2.0 spec.
pub const Target = enum(u32) {
    array_buffer = 34962,
    element_array_buffer = 34963,
};

/// Enum values from GLTF 2.0 spec.
pub const ComponentType = enum(u32) {
    /// i8.
    byte = 5120,
    /// u8.
    unsigned_byte = 5121,
    /// i16.
    short = 5122,
    /// u16.
    unsigned_short = 5123,
    /// u32.
    unsigned_integer = 5125,
    /// f32.
    float = 5126,
};

/// The topology type of primitives to render.
pub const Mode = enum(u32) {
    points = 0,
    lines = 1,
    line_loop = 2,
    line_strip = 3,
    triangles = 4,
    triangle_strip = 5,
    triangle_fan = 6,
};

/// The name of the node’s TRS property to animate.
pub const TargetProperty = enum {
    /// For the "translation" property, the values that are provided by the
    /// sampler are the translation along the X, Y, and Z axes.
    translation,
    /// For the "rotation" property, the values are a quaternion
    /// in the order (x, y, z, w), where w is the scalar.
    rotation,
    /// For the "scale" property, the values are the scaling
    /// factors along the X, Y, and Z axes.
    scale,
    /// The "weights" of the Morph Targets it instantiates.
    weights,
};

/// An animation channel combines an animation sampler
/// with a target property being animated.
pub const Channel = struct {
    /// The index of a sampler in this animation used to
    /// compute the value for the target.
    sampler: Index,
    /// The descriptor of the animated property.
    target: struct {
        /// The index of the node to animate.
        /// When undefined, the animated object may be defined by an extension.
        node: Index,
        /// The name of the node’s TRS property to animate, or the "weights"
        /// of the Morph Targets it instantiates.
        property: TargetProperty,
    },
};

/// Interpolation algorithm.
pub const Interpolation = enum {
    /// The animated values are linearly interpolated between keyframes.
    /// When targeting a rotation, spherical linear interpolation (slerp)
    /// should be used to interpolate quaternions.
    linear,
    /// The animated values remain constant to the output of the first
    /// keyframe, until the next keyframe.
    step,
    /// The animation’s interpolation is computed using a cubic
    /// spline with specified tangents.
    cubicspline,
};

/// An animation sampler combines timestamps
/// with a sequence of output values and defines an interpolation algorithm.
pub const AnimationSampler = struct {
    /// The index of an accessor containing keyframe timestamps.
    input: Index,
    /// The index of an accessor, containing keyframe output values.
    output: Index,
    /// Interpolation algorithm.
    interpolation: Interpolation = .linear,
};

/// A keyframe animation.
pub const Animation = struct {
    /// The user-defined name of this object.
    name: []const u8,
    /// An array of animation channels.
    /// An animation channel combines an animation sampler with a target
    /// property being animated.
    /// Different channels of the same animation must not have the same targets.
    channels: ArrayList(Channel),
    /// An array of animation samplers.
    /// An animation sampler combines timestamps with a sequence of output
    /// values and defines an interpolation algorithm.
    samplers: ArrayList(AnimationSampler),
};

/// Geometry to be rendered with the given material.
pub const Primitive = struct {
    attributes: ArrayList(Attribute),
    /// The topology type of primitives to render.
    mode: Mode = .triangles,
    /// The index of the accessor that contains the vertex indices.
    indices: ?Index = null,
    /// The index of the material to apply to this primitive when rendering.
    material: ?Index = null,
};

/// A set of primitives to be rendered.
/// Its global transform is defined by a node that references it.
pub const Mesh = struct {
    /// The user-defined name of this object.
    name: []const u8,
    /// An array of primitives, each defining geometry to be rendered.
    primitives: ArrayList(Primitive),
};

/// Metadata about the glTF asset.
pub const Asset = struct {
    /// The glTF version that this asset targets.
    version: []const u8,
    /// Tool that generated this glTF model. Useful for debugging.
    generator: ?[]const u8 = null,
    /// A copyright message suitable for display to credit the content creator.
    copyright: ?[]const u8 = null,
};

/// A camera’s projection.
/// A node may reference a camera to apply a transform to place the camera
/// in the scene.
pub const Camera = struct {
    /// A perspective camera containing properties to create a
    /// perspective projection matrix.
    pub const Perspective = struct {
        /// The aspect ratio of the field of view.
        aspect_ratio: ?f32,
        /// The vertical field of view in radians.
        /// This value should be less than π.
        yfov: f32,
        /// The distance to the far clipping plane.
        zfar: ?f32,
        /// The distance to the near clipping plane.
        znear: f32,
    };

    /// An orthographic camera containing properties to create an
    /// orthographic projection matrix.
    pub const Orthographic = struct {
        /// The horizontal magnification of the view.
        /// This value must not be equal to zero.
        /// This value should not be negative.
        xmag: f32,
        /// The vertical magnification of the view.
        /// This value must not be equal to zero.
        /// This value should not be negative.
        ymag: f32,
        /// The distance to the far clipping plane.
        /// This value must not be equal to zero.
        /// This value must be greater than znear.
        zfar: f32,
        /// The distance to the near clipping plane.
        znear: f32,
    };

    name: []const u8,
    type: union(enum) {
        perspective: Perspective,
        orthographic: Orthographic,
    },
};

/// Specifies the light type.
pub const LightType = enum {
    /// Directional lights act as though they are infinitely far away and emit light in the direction of the local -z axis.
    /// This light type inherits the orientation of the node that it belongs to; position and scale are ignored
    /// except for their effect on the inherited node orientation. Because it is at an infinite distance,
    /// the light is not attenuated. Its intensity is defined in lumens per metre squared, or lux (lm/m^2).
    directional,
    /// Point lights emit light in all directions from their position in space; rotation and scale are ignored except
    /// for their effect on the inherited node position.
    /// The brightness of the light attenuates in a physically correct manner as distance increases from
    /// the light's position (i.e. brightness goes like the inverse square of the distance).
    /// Point light intensity is defined in candela, which is lumens per square radian (lm/sr).
    point,
    /// Spot lights emit light in a cone in the direction of the local -z axis.
    /// The angle and falloff of the cone is defined using two numbers, the innerConeAngle and outerConeAngle.
    /// As with point lights, the brightness also attenuates in a physically correct manner as distance
    /// increases from the light's position (i.e. brightness goes like the inverse square of the distance).
    /// Spot light intensity refers to the brightness inside the innerConeAngle (and at the location of the light) and
    /// is defined in candela, which is lumens per square radian (lm/sr).
    ///
    /// Engines that don't support two angles for spotlights should use outerConeAngle as the spotlight angle,
    /// leaving innerConeAngle to implicitly be 0.
    spot,
};

/// A directional, point or spot light.
pub const Light = struct {
    name: ?[]const u8,
    /// Color of the light source.
    color: [3]f32 = .{ 1, 1, 1 },
    /// Intensity of the light source. `point` and `spot` lights use luminous intensity in candela (lm/sr)
    /// while `directional` lights use illuminance in lux (lm/m^2).
    intensity: f32 = 1,
    /// Specifies the light type.
    type: LightType,
    /// When a light's type is spot, the spot property on the light is required.
    spot: ?LightSpot,
    /// A distance cutoff at which the light's intensity may be considered to have reached zero.
    range: f32,
};

pub const LightSpot = struct {
    /// Angle in radians from centre of spotlight where falloff begins.
    inner_cone_angle: f32 = 0,
    /// Angle in radians from centre of spotlight where falloff ends.
    outer_cone_angle: f32 = pi / @as(f32, 4),
};

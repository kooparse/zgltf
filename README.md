# glTF parser for Zig codebase

This project is a glTF 2.0 parser written in Zig, aiming to replace the use of some C/C++ libraries. All glTF types are fully documented, so it comes nicely with IDE autocompletion, reducing
back and forth with the [specification](https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html).

This library intends to mimic the glTF file structure in memory. Thereby it's designed around arrays and indexes instead of pointers as you may see in `cgltf` or other libraries. Also, it's the **user's responsibility** to load glTF files and their related binaries in memory.

Note: It's not as complete as the glTF specification yet, but because it's straightforward to add new parsed fields, we'll get new stuff incrementally and on-demand.

If you would like to contribute, don't hesitate! :)

Note: The main branch is for the latest Zig release (0.14.x).

## Examples

```zig
const std = @import("std");
const Gltf = @import("zgltf");

const allocator = std.heap.page_allocator;
const print = std.debug.print;

pub fn main() void {
    const buffer = try std.fs.cwd().readFileAllocOptions(
        allocator,
        "test-samples/rigged_simple/RiggedSimple.gltf",
        512_000,
        null,
        4,
        null
    );
    defer allocator.free(buf);

    var gltf = Self.init(allocator);
    defer gltf.deinit();

    try gltf.parse(buf);

    for (gltf.nodes.items) |node| {
        const message =
            \\\ Node's name: {s}
            \\\ Children count: {}
            \\\ Have skin: {}
        ;

        print(message, .{
            node.name,
            node.children.items.len,
            node.skin != null,
        });
    }

    // Or use the debufPrint method.
    gltf.debugPrint();
}
```

Also you could easily load data from an `Accessor` with `getDataFromBufferView`:

```zig
const gltf = Gltf.init(allocator);
try gltf.parse(my_gltf_buf);

const bin = try std.fs.cwd().readFileAllocOptions(
    allocator,
    "test-samples/rigged_simple/RiggedSimple0.bin",
    5_000_000,
    null,
    4,
    null
);
defer allocator.free(buf);

var vertices = ArrayList(f32).init(allocator);
defer vertices.deinit();

const mesh = gltf.data.meshes.items[0];
for (mesh.primitives.items) |primitive| {
    for (primitive.attributes.items) |attribute| {
        switch (attribute) {
            // Accessor for mesh vertices:
            .position => |accessor_index| {
                const accessor = gltf.data.accessors.items[accessor_index];
                gltf.getDataFromBufferView(f32, &vertices, accessor, bin);
            },
            else => {}
        }
    }
}

```

Also, there is an `iterator` method that helps you pull data from accessors:

```zig
// ...
for (primitive.attributes.items) |attribute| {
    switch (attribute) {
        .position => |idx| {
            const accessor = gltf.data.accessors.items[idx];
            var it = accessor.iterator(f32, &gltf, gltf.glb_binary.?);
            while (it.next()) |v| {
                try vertices.append(.{
                    .pos = .{ v[0], v[1], v[2] },
                    .normal = .{ 1, 0, 0 },
                    .color = .{ 1, 1, 1, 1 },
                    .uv_x = 0,
                    .uv_y = 0,
                });
            }
        },
        .normal => |idx| {
            const accessor = gltf.data.accessors.items[idx];
            var it = accessor.iterator(f32, &gltf, gltf.glb_binary.?);
            var i: u32 = 0;
            while (it.next()) |n| : (i += 1) {
                vertices.items[initial_vertex + i].normal = .{ n[0], n[1], n[2] };
            }
        },
        else => {},
    }
}
```

## Install

Note: **Zig 0.11.x is required.**

```zig
const zgltf = @import("path-to-zgltf/build.zig");
exe.addModule("zgltf", zgltf.module(b));
```

## Features

- [x] glTF 2.0 json file
- [x] Scenes
- [x] Nodes
- [x] Buffers/BufferViews
- [x] Meshes
- [x] Images
- [x] Materials
- [x] Animations
- [x] Skins
- [x] Cameras
- [x] Parse `glb` files
- [ ] Morth targets
- [ ] Extras data
- [ ] glTF writer

Also, we supports some glTF extensions:

- [x] khr_lights_punctual
- [x] khr_materials_emissive_strength
- [x] khr_materials_ior
- [x] khr_materials_transmission
- [x] khr_materials_volume
- [x] khr_materials_dispersion

## Contributing to the project

Don’t be shy about shooting any questions you may have. If you are a beginner/junior, don’t hesitate, I will always encourage you. It’s a safe place here. Also, I would be very happy to receive any kind of pull requests, you will have (at least) some feedback/guidance rapidly.

Behind screens, there are human beings, living any sort of story. So be always kind and respectful, because we all sheer to learn new things.

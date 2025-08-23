const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("zgltf", .{
        .root_source_file = b.path("src/main.zig"),
    });

    var tests = b.addTest(.{
        .root_module = b.addModule("main", .{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&tests.step);
}

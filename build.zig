const std = @import("std");
const Builder = std.build.Builder;
const Module = std.build.Module;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("zgltf", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    var tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&tests.step);
}

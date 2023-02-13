const std = @import("std");
const Builder = std.build.Builder;
const Module = std.build.Module;

pub const pkg = std.build.Pkg{
    .name = "zgltf",
    .source = .{ .path = thisDir() ++ "/src/main.zig" },
};

pub fn module(b: *Builder) *Module {
    return b.createModule(.{
        .source_file = .{ .path = "src/main.zig" },
    });
}

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    var tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&tests.step);
}

fn thisDir() []const u8 {
    return std.fs.path.dirname(@src().file) orelse ".";
}

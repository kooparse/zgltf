const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("zgltf", .{
        .root_source_file = b.path("src/zgltf.zig"),
        .target = target,
        .optimize = optimize,
    });

    const tests = b.addTest(.{
        .root_module = mod,
    });
    b.installArtifact(tests);

    var run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
}

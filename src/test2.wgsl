diagnostic (info, foo);

// shader must be run dvo_depth times, for each depth level.
@compute
// @workgroup_size(1)
fn cs_main(@builtin(global_invocation_id) index: vec3u) {
    A ( B < C, D > ( E ) );
    let i2 = index * 2u;
    let octants = array(
        textureLoad(voxels, i2 + vec3(0u, 0u, 0u)).r != 0u,
        textureLoad(voxels, i2 + vec3(0u, 0u, 1u)).r != 0u,
        textureLoad(voxels, i2 + vec3(0u, 1u, 0u)).r != 0u,
        textureLoad(voxels, i2 + vec3(0u, 1u, 1u)).r != 0u,
        textureLoad(voxels, i2 + vec3(1u, 0u, 0u)).r != 0u,
        textureLoad(voxels, i2 + vec3(1u, 0u, 1u)).r != 0u,
        textureLoad(voxels, i2 + vec3(1u, 1u, 0u)).r != 0u,
        textureLoad(voxels, i2 + vec3(1u, 1u, 1u)).r != 0u,
    );

    let value = pack_octants(octants);
    textureStore(dvo, index, vec4(value));
}

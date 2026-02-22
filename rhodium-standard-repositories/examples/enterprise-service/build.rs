// Build script for generating build metadata
// This creates build.rs output that can be used for SBOM and provenance tracking

fn main() {
    // Generate build information
    built::write_built_file().expect("Failed to acquire build-time information");
}

use serde::{Deserialize, Serialize};
use std::env;
use std::process::{Command, exit};

#[derive(Serialize, Deserialize, Debug)]
struct SourceInfo {
    #[serde(rename = "type")]
    source_type: Option<String>,
    path: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
struct RubyVersion {
    version: String,
    requested_version: Option<String>,
    install_path: String,
    source: Option<SourceInfo>,
    symlinked_to: Option<String>,
    installed: bool,
    active: bool,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <command> [args...]", args[0]);
        eprintln!("Example: {} ruby -v", args[0]);
        eprintln!("         {} gem --version", args[0]);
        exit(1);
    }

    // Get Ruby versions from mise
    let output = Command::new("mise")
        .args(&["ls", "ruby", "-J"])
        .output()
        .expect("Failed to execute mise ls ruby -J");

    if !output.status.success() {
        eprintln!("Error running mise ls ruby -J: {}", String::from_utf8_lossy(&output.stderr));
        exit(1);
    }

    let json_output = String::from_utf8_lossy(&output.stdout);
    let ruby_versions: Vec<RubyVersion> = match serde_json::from_str(&json_output) {
        Ok(versions) => versions,
        Err(e) => {
            eprintln!("Error parsing JSON: {}", e);
            exit(1);
        }
    };

    let installed_versions: Vec<&RubyVersion> = ruby_versions
        .iter()
        .filter(|v| v.installed)
        .collect();

    if installed_versions.is_empty() {
        eprintln!("No Ruby versions are installed");
        exit(1);
    }

    println!("Running command for {} Ruby version(s):", installed_versions.len());

    // Execute command for each Ruby version
    for ruby_version in installed_versions {
        println!("==> Running with Ruby {}", ruby_version.version);
        
        // Use mise exec to run the command with the specific Ruby version
        let mut cmd = Command::new("mise");
        cmd.arg("exec")
           .arg(&format!("ruby@{}", ruby_version.version))
           .arg("--")
           .args(&args[1..]);
        
        let status = cmd.status().expect("Failed to execute command");
        
        if !status.success() {
            eprintln!("Command failed for Ruby {}", ruby_version.version);
        }
        
        println!();
    }
}

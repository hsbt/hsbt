use std::fs;
use std::path::Path;
use chrono::Utc;
use serde::{Serialize, Deserialize};
use uuid::Uuid;

#[derive(Debug, Serialize)]
struct SBOM {
    #[serde(rename = "SPDXID")]
    spdx_id: String,
    spdx_version: String,
    creation_info: CreationInfo,
    name: String,
    data_license: String,
    document_namespace: String,
    packages: Vec<Package>,
}

#[derive(Debug, Serialize)]
struct CreationInfo {
    created: String,
    creators: Vec<String>,
    license_list_version: String,
}

#[derive(Debug, Serialize)]
struct Package {
    #[serde(rename = "SPDXID")]
    spdx_id: String,
    name: String,
    version_info: String,
    download_location: String,
    files_analyzed: bool,
    license_concluded: String,
    license_declared: String,
    supplier: String,
    external_refs: Vec<ExternalRef>,
}

#[derive(Debug, Serialize)]
struct ExternalRef {
    reference_category: String,
    reference_type: String,
    reference_locator: String,
}

#[derive(Debug, Deserialize)]
struct GemSpec {
    name: String,
    version: String,
}

fn parse_gemfile_lock(content: &str) -> Vec<GemSpec> {
    let mut specs = Vec::new();
    let mut in_specs = false;

    for line in content.lines() {
        if line.starts_with("    ") {
            if in_specs {
                let parts: Vec<&str> = line.trim().split_whitespace().collect();
                if parts.len() >= 2 {
                    specs.push(GemSpec {
                        name: parts[0].to_string(),
                        version: parts[1].trim_matches('(').trim_matches(')').to_string(),
                    });
                }
            }
        } else if line == "  specs:" {
            in_specs = true;
        } else if line.is_empty() {
            in_specs = false;
        }
    }
    specs
}

fn main() -> std::io::Result<()> {
    let content = fs::read_to_string("Gemfile.lock")?;
    let specs = parse_gemfile_lock(&content);

    let document_name = Path::new(".").file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown")
        .to_string();
    
    let spdx_id = Uuid::new_v4();

    let sbom = SBOM {
        spdx_id: "SPDXRef-DOCUMENT".to_string(),
        spdx_version: "SPDX-2.2".to_string(),
        creation_info: CreationInfo {
            created: Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string(),
            creators: vec!["Tool: gemfile_to_sbom-rs".to_string()],
            license_list_version: "3.17".to_string(),
        },
        name: document_name.clone(),
        data_license: "CC0-1.0".to_string(),
        document_namespace: format!("https://spdx.org/spdxdocs/{}-{}", document_name, spdx_id),
        packages: specs.into_iter().map(|spec| {
            let version = spec.version.clone(); // Clone here to avoid ownership issues
            Package {
                spdx_id: format!("SPDXRef-Package-{}", spec.name),
                name: spec.name.clone(),
                version_info: spec.version,
                download_location: "NOASSERTION".to_string(),
                files_analyzed: false,
                license_concluded: "NOASSERTION".to_string(),
                license_declared: "NOASSERTION".to_string(),
                supplier: "NOASSERTION".to_string(),
                external_refs: vec![ExternalRef {
                    reference_category: "PACKAGE_MANAGER".to_string(),
                    reference_type: "purl".to_string(),
                    reference_locator: format!("pkg:gem/{}@{}", spec.name, version),
                }],
            }
        }).collect(),
    };

    let json = serde_json::to_string_pretty(&sbom)?;
    fs::write("bom.json", json)?;

    Ok(())
}
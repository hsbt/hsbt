@{
    RootModule = 'PSScoopPath.psm1'
    ModuleVersion = '0.1.0'
    GUID = '1d7c9c1c-3e4b-4f70-9b71-5b98c2f9b2e2'
    Author = 'hsbt'
    CompanyName = 'Unknown'
    Copyright = '(c) 2026 hsbt. All rights reserved.'
    Description = 'Utilities for resolving Scoop paths.'
    PowerShellVersion = '5.1'
    FunctionsToExport = @('Get-ScoopPath')
    CmdletsToExport = @()
    VariablesToExport = '*'
    AliasesToExport = @()
    PrivateData = @{
        PSData = @{
            Tags = @('Scoop', 'Path')
            LicenseUri = ''
            ProjectUri = ''
        }
    }
}

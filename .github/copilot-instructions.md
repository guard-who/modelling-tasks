# Modelling Tasks
Modelling Tasks is a Haskell library and application suite for generating exercise tasks for modelling lecture contents. It covers UML Activity Diagrams, Class Diagrams, Object Diagrams, and Petri nets.

**Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.**

## 🤖 Automated Copilot Setup

This repository includes an automated setup workflow (`.github/workflows/copilot-setup-steps.yml`) that prepares the environment for Copilot operations. The workflow automatically:

- **Installs system dependencies**: graphviz, texlive-base, texlive-latex-base
- **Verifies installations**: Checks Graphviz and LaTeX are working
- **Tests network connectivity**: Validates access to hackage.haskell.org
- **Sets up Haskell Stack**: Configures the build environment
- **Installs HLint**: Enables code quality checking
- **Pre-builds dependencies**: Downloads and builds Haskell project dependencies

**When Copilot environment is provided**: System dependencies and Haskell dependencies should already be available. You can immediately proceed with builds and tests without manual setup.

**Manual setup required when**: Working outside the automated Copilot environment or troubleshooting setup issues.

## 🚨 CRITICAL WARNINGS

### ⏰ NEVER CANCEL BUILDS OR TESTS
- **Dependency builds**: 45-75 minutes (set timeout to 90+ minutes)
- **Project builds**: 30-45 minutes (set timeout to 60+ minutes)
- **Test suites**: 15-30 minutes (set timeout to 45+ minutes)
- Builds resume from cache when interrupted properly - canceling wastes progress

### 🌐 NETWORK ACCESS REQUIRED
- **Required domains**: hackage.haskell.org, raw.githubusercontent.com, github.com
- **Test connectivity**: `curl -I https://hackage.haskell.org/root.json`
- **No offline mode**: All builds require internet for dependency downloads
- **Restricted environments**: Will fail with `ConnectionTimeout` errors

## Working Effectively

### System Dependencies
**Automated in Copilot environments**: The automated setup workflow handles system dependency installation. Check if dependencies are already available before manual installation.

**Manual installation** (if needed):
- `sudo apt-get update`
- `sudo apt-get install -y graphviz texlive-base texlive-latex-base`

**Verify installations**:
- `dot -V` -- should show graphviz version 2.43.0 or later
- `pdflatex --version` -- should show pdfTeX 3.141592653 or later

### Build Tool Setup
This project uses Haskell Stack as its primary build tool. Three Stack configurations are available:
- `stack.yaml` -- main library configuration
- `stack-apps.yaml` -- applications configuration (includes app/, legacy-app/, example/)
- `stack-examples.yaml` -- examples only configuration (includes example/)

**CRITICAL NETWORK REQUIREMENT**: This project requires internet access to download dependencies from Hackage and GitHub.

**Network Issues**:
- **Symptoms**: `ConnectionTimeout` errors during `stack build --only-dependencies`
- **URLs Required**: hackage.haskell.org, raw.githubusercontent.com, github.com
- **Testing connectivity**: `ping hackage.haskell.org` and `curl -I https://hackage.haskell.org`
- **Fallback**: Builds will fail in restricted network environments (corporate firewalls, sandboxed CI)

### Building the Project
**NEVER CANCEL builds or dependency installations - they can take 60+ minutes**

**In Copilot environments**: Dependencies are pre-installed by the automated setup workflow. You can typically skip step 1 and proceed directly to building.

#### Full Build Process
1. Install dependencies: `stack --stack-yaml=stack-apps.yaml build --only-dependencies`
   - **LIKELY AUTOMATED**: In Copilot environments, this step is handled by the setup workflow
   - **NEVER CANCEL**: Takes 45-75 minutes. Set timeout to 90+ minutes.
   - Downloads from hackage.haskell.org and github.com
   - Downloads custom dependencies: output-blocks, autotool-capabilities, call-alloy, etc.
   - If this fails with `ConnectionTimeout`, you are in a restricted network environment

2. Build library and applications: `stack --stack-yaml=stack-apps.yaml build`
   - **NEVER CANCEL**: Takes 30-45 minutes. Set timeout to 60+ minutes.
   - Builds the main library plus all applications in `/app`, `/legacy-app`, and `/example`

#### Alternative Build for Library Only
- `stack build --only-dependencies` -- 30-45 minutes
- `stack build` -- 15-30 minutes

#### Alternative Build for Examples
- `stack --stack-yaml=stack-examples.yaml build --only-dependencies` -- 30-45 minutes
- `stack --stack-yaml=stack-examples.yaml build` -- 15-30 minutes

### Platform-Specific Notes
**Windows Users**: You may need to use SAT4J solver instead of MiniSat. Add this flag:
```yaml
# In stack.yaml, stack-apps.yaml, or stack-examples.yaml
flags:
  autotool-capabilities:
    alloy-use-sat4j: true
```

Or provide as argument: `stack build --flag autotool-capabilities:alloy-use-sat4j`

### Running Tests
- `stack test` -- **NEVER CANCEL**: Takes 15-30 minutes. Set timeout to 45+ minutes.
- `stack --stack-yaml=stack-apps.yaml test` -- includes all test suites
- Test-specific options:
  - `--test-arguments="--skip-needs-tuning"` -- excludes unstable/long-running tests
  - `--test-arguments="--times --maximum-generated-tests=50"` -- limits test case generation

### Running Applications
The project includes multiple command-line applications in the `/app` directory:
- `stack exec match-cd-od` -- Match class and object diagrams
- `stack exec different-names` -- Generate different name variations
- `stack exec repair-incorrect` -- Repair incorrect models
- `stack exec check-cds` -- Check class diagrams
- `stack exec concurrency` -- Concurrency analysis
- `stack exec conflicts` -- Conflict analysis

**Build applications first**: `stack --stack-yaml=stack-apps.yaml build`

### Using GHCi for Interactive Development
The repository includes a `.ghci` configuration file with pre-loaded modules and settings:
```haskell
-- .ghci automatically loads:
-- :set +s (show timing)
-- :set -XTypeApplications
-- :set -iapp/common (include path)
-- :l app/common/Common.hs (loads Common module)
-- Pre-imported: Control.OutputCapable.Blocks, Control.Monad.Trans.Except
-- Qualified imports: Data.Bimap as BM, Data.Map as M
```

For interactive task generation and testing:
```bash
stack ghci --stack-yaml=stack-examples.yaml
```

Example GHCi session for NameCdError task:
```haskell
:m + Capabilities.Alloy.IO Capabilities.Cache.IO Capabilities.Diagrams.IO Capabilities.Graphviz.IO Capabilities.PlantUml.IO
:m + Control.OutputCapable.Blocks Control.OutputCapable.Blocks.Generic
inst <- nameCdErrorGenerate defaultNameCdErrorConfig 0 0
runLangMReport (return ()) (>>) (nameCdErrorTask "/tmp/" inst) >>= \(Just (), x) -> (x English :: IO ())
```

For pretty printing configurations:
```haskell
:m + Text.Pretty.Simple
pPrint defaultNameCdErrorConfig
```

## Validation and Linting
Always run these commands before committing changes:

### Linting
**Automated in Copilot environments**: HLint is pre-installed by the automated setup workflow.

**Manual HLint installation** (if needed):
- Local HLint installation: Follow [HLint installation guide](https://github.com/ndmitchell/hlint#installation)
- **Check if available**: Run `hlint --version` to verify installation

**Running HLint**:
- Manual linting: `hlint src/ test/ app/`
- HLint configuration in `.hlint.yaml`: uses `--cpp-simple` flag and ignores "Redundant pure" warnings
- **CI validation**: HLint runs automatically in GitHub Actions on push/PR

### Spell Checking
The repository includes comprehensive spell checking via GitHub Actions:
- Uses `check-spelling/check-spelling` with multiple dictionaries
- Includes CSS, LaTeX, software terms, Haskell, German, and English dictionaries
- Checks both file content and filenames
- Runs automatically on push and pull requests
- Configuration in `.github/actions/spelling/` directory

### Code Formatting
Follow `.editorconfig` standards (automatically applied by most editors):
- 2-space indentation
- LF line endings
- Trim trailing whitespace
- 175 character line limit (160 for .als files)
- No line length limits for YAML, Markdown, or TeX files
- Special handling for test/unit/ files (formatting rules relaxed)

## Repository Structure

### Key Directories
- `src/Modelling/` -- Main library source code
  - `ActivityDiagram/` -- UML Activity Diagram tasks
  - `CdOd/` -- Class Diagram and Object Diagram tasks
  - `PetriNet/` -- Petri Net tasks
  - `Auxiliary/` -- Common utilities
- `app/` -- Command-line applications
- `example/` -- Example applications and configurations
- `legacy-app/` -- Legacy applications (includes Lexer/Parser)
- `test/` -- HSpec test suites
- `alloy/` -- Alloy specification files (.als)
  - `alloy/ad/` -- Activity Diagram specifications
  - `alloy/cd/` -- Class Diagram specifications
  - `alloy/petri/` -- Petri Net specifications

### Build Configuration Files
- `package.yaml` -- Hpack package configuration (generates .cabal file)
- `modelling-tasks.cabal` -- Generated Cabal file (DO NOT EDIT)
- `stack.yaml`, `stack-apps.yaml`, `stack-examples.yaml` -- Stack configurations
- `hie.yaml` -- Haskell IDE Engine configuration
- `.editorconfig` -- Code formatting standards for editors
- `.ghci` -- Default GHCi configuration with pre-loaded modules and imports

## CI/CD Pipeline

### GitHub Actions Workflows
- `.github/workflows/copilot-setup-steps.yml` -- Automated Copilot environment setup
- `.github/workflows/haskell.yml` -- Main CI build and test
- `.github/workflows/haskell-nightly.yml` -- Nightly builds with latest dependencies
- `.github/workflows/hlint.yml` -- Haskell linting with HLint
- `.github/workflows/linter.yml` -- Super-linter for general code quality
- `.github/workflows/spelling.yml` -- Spell checking with multiple dictionaries
- `.github/workflows/checks.yml` -- General consistency checks
- `.github/workflows/consistency.yml` -- Cabal file consistency validation
- `.github/workflows/haddock.yml` -- Generate and deploy documentation to GitHub Pages

### Build Process in CI
The CI installs system dependencies and runs comprehensive validation:
```bash
# Main build and test (haskell.yml)
stack --no-terminal test --stack-yaml=stack-apps.yaml --coverage \
  --bench --no-run-benchmarks --haddock --no-haddock-deps \
  --test-arguments="--skip-needs-tuning --times --maximum-generated-tests=50"

# Additional validations:
# - HLint checking (hlint.yml)
# - Spell checking with multiple dictionaries (spelling.yml)
# - Cabal file consistency checking (consistency.yml)
# - Super-linter for general code quality (linter.yml)
# - Nightly builds with latest dependencies (haskell-nightly.yml)
# - Haddock documentation generation (haddock.yml)
```

## Common Tasks and Troubleshooting

### Network Issues
- **Symptom**: `ConnectionTimeout` errors during `stack build --only-dependencies`
- **Root Cause**: Firewall blocking access to hackage.haskell.org, raw.githubusercontent.com, github.com
- **Testing**: `curl -I https://hackage.haskell.org/root.json` should return HTTP 200
- **Solution**: Requires unrestricted internet access or internal package mirrors
- **Workaround**: None available - build requires external dependency downloads

### Stack Configuration Issues
- **Symptom**: `Exception while reading snapshot` errors
- **Solution**: Verify internet connectivity, check stack.yaml resolver version
- **Alternative**: Try different stack yaml files (`stack-apps.yaml` vs `stack.yaml`)

### GHC Version Compatibility
- **Current supported**: GHC 9.12.2 with resolver lts-21.25
- **Stack manages GHC**: Uses system GHC when `system-ghc: true` is configured
- **Verify**: `stack ghc --version` should match expected version

### Windows-Specific Issues
- **Symptom**: Alloy solver failures
- **Solution**: Use `--flag autotool-capabilities:alloy-use-sat4j` or configure in stack.yaml:
  ```yaml
  flags:
    autotool-capabilities:
      alloy-use-sat4j: true
  ```

### Long Build Times - THIS IS NORMAL
- **Expected**: 45-75 minutes for full dependency build, 30-45 minutes for project build
- **Important**: NEVER cancel builds - they will resume from cache if interrupted properly
- **Progress indicators**: Stack shows download progress and compilation stages
- **Parallel builds**: Stack builds dependencies in parallel when possible

### Testing Specific Tasks
Different tasks can be tested by following the naming pattern in GHCi:
- Replace `NameCdError` with other task names (e.g., `MatchCdOd`, `SelectAS`)
- Change `English` to `German` for German language versions
- Tasks may require directory arguments (e.g., `"/tmp/"`) - check function signatures
- Import modules based on task type: `Modelling.CdOd.NameCdError`, `Modelling.ActivityDiagram.MatchAd`

## Validation Status

### 🤖 Automated in Copilot Environments
These components are automatically set up by the `.github/workflows/copilot-setup-steps.yml` workflow:
- System dependencies installation (graphviz, texlive-base, texlive-latex-base)
- Graphviz and LaTeX verification (`dot -V`, `pdflatex --version`)
- Network connectivity testing (`curl -I https://hackage.haskell.org/root.json`)
- Haskell Stack environment setup
- HLint installation and configuration
- Haskell project dependencies pre-installation (`stack build --only-dependencies`)

### ✅ Validated Commands
These commands have been tested and work correctly:
- `dot -V` -- verifies Graphviz installation (graphviz version 2.43.0)
- `pdflatex --version` -- verifies LaTeX installation (pdfTeX 3.141592653)
- `stack --version` -- verifies Stack installation (Version 3.7.1)
- `ghc --version` -- verifies GHC installation (GHC 9.12.2)
- `hlint --version` -- verifies HLint installation (automated in Copilot)
- Repository structure and file access work correctly

### ⚠️  Network-Dependent Commands (Automated in Copilot)
These commands require internet access but are handled by automated setup:
- `stack build --only-dependencies` -- automated by setup workflow
- `stack test` -- dependencies pre-installed in Copilot environments
- `stack exec <app-name>` -- should work after automated setup
- GHCi task generation -- dependencies available in Copilot environments

### 🔧 Workarounds for Restricted Networks
- **CI/CD**: Use unrestricted GitHub Actions environment
- **Local development**: Configure network access or use pre-built environments
- **Testing**: Use `ghc` directly for syntax checking individual modules
- **Validation**: Rely on CI workflows for full build/test validation
## Complete Validation Scenarios

### End-to-End Testing (Requires Network Access)
**In Copilot environments**: Steps 1-2 are automated by the setup workflow.

After making changes, always validate:
1. **System dependencies**: `dot -V && pdflatex --version` (automated in Copilot)
2. **Network connectivity**: `curl -I https://hackage.haskell.org/root.json` (automated in Copilot)
3. **Build succeeds**: `stack --stack-yaml=stack-apps.yaml build` (dependencies pre-installed in Copilot)
4. **Tests pass**: `stack --stack-yaml=stack-apps.yaml test` (30+ minutes)
5. **App execution**: Test at least one app with `stack exec <app-name>`
6. **GHCi interaction**: Load examples and generate task instances

### Manual Testing Workflow
1. **Start GHCi**: `stack ghci --stack-yaml=stack-examples.yaml`
2. **Generate task instance**: Follow patterns in README.md for specific tasks
3. **Export to files**: Tasks generate LaTeX and Graphviz output in specified directories
4. **Verify outputs**: Check that .tex, .svg, .pdf files are created correctly
5. **Test validation**: Try sample answers with task validation functions

### Minimal Validation (Network-Restricted)
When full builds aren't possible:
1. **Syntax check**: `ghc -Wall --make -fno-code src/Modelling/Types.hs`
2. **File structure**: Verify imports and exports align with exposed-modules
3. **Configuration**: Check stack.yaml resolver and dependencies are consistent
4. **Static analysis**: Use any available Haskell linting tools

**CRITICAL**: NEVER CANCEL builds, tests, or long-running operations. Allow 60+ minutes for builds and 30+ minutes for tests. Plan accordingly and set appropriate timeouts.

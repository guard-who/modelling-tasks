# IO Checker Script

This script checks for runaway IO usage in the library code (src/ folder).

## Purpose

After issue #242 introduced capabilities for various IO computations, this
checker ensures that:

1. No direct System.IO imports are used in src/ folder
2. No direct IO functions (readFile, writeFile, putStr, etc.) are used in
   src/ folder
3. Only Capabilities.\* modules are used for IO operations
4. The library maintains proper separation of concerns

## Usage

```bash
./scripts/check-no-runaway-io.sh
```

## What it checks

### Prohibited patterns

- `import System.IO`
- `import qualified System.IO`
- Direct IO functions: `readFile`, `writeFile`, `putStr`, `putStrLn`,
  `getLine`, etc.
- Unsafe IO: `unsafePerformIO`
- Debug functions: `Debug.Trace`

### Allowed patterns

- `import Capabilities.*` modules
- `import qualified Capabilities.*` modules
- MonadRandom functions like `shuffleM`
- Comments containing IO patterns

## Integration

This checker is integrated into the CI pipeline via
`.github/workflows/checks.yml` and runs on every push and pull request.

## Exit codes

- 0: No violations found
- 1: Violations found (fails CI)

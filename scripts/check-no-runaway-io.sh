#!/bin/bash

# CI checker for runaway IO usage in library code
# Ensures that src/ folder only uses IO through Capabilities modules

set -e

echo "Checking for runaway IO usage in src/ folder..."

# Define patterns that indicate problematic IO usage
# These patterns avoid false positives by using word boundaries
IO_PATTERNS=(
  "^[[:space:]]*import[[:space:]]+System\.IO\b"
  "^[[:space:]]*import[[:space:]]+qualified[[:space:]]+System\.IO\b"
  "^[[:space:]]*import[[:space:]]+System\.IO\."
  "^[[:space:]]*import[[:space:]]+Data\.ByteString\.Char8\b"
  "^[[:space:]]*import[[:space:]]+qualified[[:space:]]+Data\.ByteString\.Char8\b"
  "\breadFile\b"
  "\bwriteFile\b"
  "\bappendFile\b"
  "\bputStr\b"
  "\bputStrLn\b"
  "\bgetLine\b"
  "\bgetContents\b"
  "\binteract\b"
  "\bhPutStr\b"
  "\bhGetLine\b"
  "\bhPrint\b"
  "\bopenFile\b"
  "\bcloseFile\b"
  "\bwithFile\b"
  "\bhFlush\b"
  "\bunsafePerformIO\b"
  "Debug\.Trace"
)

violations_found=0

# Check each Haskell file in src/
while IFS= read -r -d '' file; do
  echo "Checking: $file"

  # Check for each problematic pattern
  for pattern in "${IO_PATTERNS[@]}"; do
    # Find lines that match the pattern
    matches=$(grep -nE "$pattern" "$file" || true)

    if [ -n "$matches" ]; then
      # Check each matching line to see if it's an allowed Capabilities import
      while IFS= read -r line; do
        if [ -n "$line" ]; then
          line_content=$(echo "$line" | cut -d: -f2-)

          # Check if this is an allowed Capabilities import
          if echo "$line_content" | grep -qE "import[[:space:]]+(qualified[[:space:]]+)?Capabilities\."; then
            # This is allowed - skip it
            continue
          fi

          # Check if this line is a comment
          if echo "$line_content" | grep -qE "^[[:space:]]*--"; then
            # This is a comment - skip it
            continue
          fi

          # This is a violation
          echo "ERROR: Found problematic IO pattern in $file:"
          echo "  $line"
          violations_found=$((violations_found + 1))
        fi
      done <<<"$matches"
    fi
  done
done < <(find src -name "*.hs" -print0)

if [ $violations_found -gt 0 ]; then
  echo ""
  echo "❌ Found $violations_found IO violation(s) in src/ folder"
  echo "Only Capabilities.* modules should be imported for IO operations"
  echo "Direct IO functions and System.IO imports are not allowed"
  exit 1
else
  echo ""
  echo "✅ No runaway IO usage found in src/ folder"
  echo "All IO operations properly go through Capabilities modules"
  exit 0
fi

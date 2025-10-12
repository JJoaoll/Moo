#!/usr/bin/env python3
import subprocess
import os
from pathlib import Path

def run_test(file_path):
    """Run a single test and return (success, output)"""
    try:
        result = subprocess.run(
            ['cabal', 'run', '-v0', 'Moo', file_path],
            capture_output=True,
            text=True,
            timeout=5
        )
        output = result.stdout + result.stderr
        return result.returncode, output
    except subprocess.TimeoutExpired:
        return -1, "TIMEOUT"

def main():
    print("=" * 60)
    print("         MOO LANGUAGE TEST SUITE REPORT")
    print("=" * 60)
    print()
    
    valid_pass = 0
    valid_fail = 0
    invalid_detected = 0
    invalid_missed = 0
    
    # Test valid examples
    print("VALID EXAMPLES (should pass):")
    print("-" * 60)
    
    valid_dir = Path('examples/valid')
    for file_path in sorted(valid_dir.glob('*.moo')):
        filename = file_path.name
        returncode, output = run_test(str(file_path))
        
        if "Program completed successfully" in output:
            print(f"✅ {filename}")
            valid_pass += 1
        else:
            print(f"❌ {filename}")
            # Show error
            for line in output.split('\n'):
                if 'error' in line.lower() or 'Error' in line:
                    print(f"   {line.strip()[:70]}")
                    break
            valid_fail += 1
    
    print()
    print("INVALID EXAMPLES (should fail with error):")
    print("-" * 60)
    
    invalid_dir = Path('examples/invalid')
    for file_path in sorted(invalid_dir.glob('*.moo')):
        filename = file_path.name
        returncode, output = run_test(str(file_path))
        
        if any(x in output for x in ["Analysis error", "Parse error", "Runtime error"]):
            print(f"✅ {filename} - correctly rejected")
            # Show what error was caught
            for line in output.split('\n'):
                if 'error' in line.lower() or 'Wrong' in line:
                    print(f"   {line.strip()[:70]}")
                    break
            invalid_detected += 1
        else:
            print(f"❌ {filename} - INCORRECTLY PASSED!")
            invalid_missed += 1
    
    print()
    print("=" * 60)
    print("SUMMARY:")
    print("=" * 60)
    total_valid = valid_pass + valid_fail
    total_invalid = invalid_detected + invalid_missed
    print(f"Valid examples:   {valid_pass}/{total_valid} passed")
    print(f"Invalid examples: {invalid_detected}/{total_invalid} caught")
    total = total_valid + total_invalid
    total_ok = valid_pass + invalid_detected
    print(f"TOTAL:            {total_ok}/{total} tests OK ({100*total_ok//total}%)")
    print("=" * 60)

if __name__ == '__main__':
    main()

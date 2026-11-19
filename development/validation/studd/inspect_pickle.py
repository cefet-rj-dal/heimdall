"""List the opcodes of a pickle without loading it (pickletools never executes it).

Usage: python3 -I inspect_pickle.py <zip> <member>
"""
import pickletools
import sys
import zipfile

with zipfile.ZipFile(sys.argv[1]) as archive:
    data = archive.read(sys.argv[2])
globals_seen, strings = set(), []
for op, arg, pos in pickletools.genops(data):
    if op.name in ("GLOBAL", "STACK_GLOBAL"):
        globals_seen.add(str(arg))
    if op.name in ("SHORT_BINUNICODE", "BINUNICODE", "UNICODE") and len(strings) < 400:
        strings.append(arg)
print("protocol opcodes ok; globals:", sorted(globals_seen))
print("strings:", strings)

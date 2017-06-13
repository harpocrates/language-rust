#!/usr/bin/env python3

import json
import sys
import subprocess
import tabulate
import argparse

def merge(dict1, dict2, def1, def2, func):
    """Merge two nested dictionaries, using default values when it makes sense"""
    assert isinstance(dict1, dict)
    assert isinstance(dict2, dict)

    toReturn = {}
    keys1 = set(dict1.keys())
    keys2 = set(dict2.keys())

    for key in keys1 | keys2:  # change this to |
        val1 = dict1.get(key, None)
        val2 = dict2.get(key, None)

        if isinstance(val1,dict) or isinstance(val2,dict):
            toReturn[key] = merge(val1 or {}, val2 or {}, def1, def2, func)
        else:
            toReturn[key] = func(val1 or def1, val2 or def2)

    return toReturn


def flattenListDict(d, indent=0):
    """Flatten a nested dictionary into a list of lists representing a table"""
    assert isinstance(d, dict)
    result = []
    for k,v in d.items():
        assert isinstance(k, str)
        if isinstance(v, list):
            first = None
            row = []
            for entry in v:
                if entry:
                    if first:
                        percentDiff = 100 * (float(entry) - first) / first
                        color = '\033[92m' if percentDiff > -1.0 else '\033[91m'
                        row.append("%s%2.1f%s" % (color, percentDiff, '%\033[0m'))
                    else:
                        first = float(entry)
                        row.append(entry)
                else:
                    row.append(entry)

            result.append([ '.' * indent + k ] + row)
        elif isinstance(v, dict):
            result.append([ '.' * indent + k ])
            result.extend(flattenListDict(v, indent + 2))
        else:
            raise "List dict can only contain lists or other list dicts"
    return result

# Currently not used...
def fmtSize(num):
    """format a number of bytes on disk into a human readable form"""
    for unit in ['','KB','MB','GB','TB','PB','EB','ZB']:
        if abs(num) < 1024.0:
            return "%3.1f%s" % (num, unit)
        num /= 1024.0
    return "%.1f%s%s" % (num, 'YB', suffix)

def revParse(commit, useAbbreviated=False):
    """get the hash for a commit"""
    abbreviated = subprocess.run(
        ["git", "rev-parse", "--abbrev-ref", commit],
        stdout=subprocess.PIPE,
        check=True
    ).stdout.decode("utf8").strip()

    other = subprocess.run(
        ["git", "rev-parse", commit],
        stdout=subprocess.PIPE,
        check=True
    ).stdout.decode("utf8").strip()

    return (useAbbreviated and abbreviated) or other

# Run benchmarks for a commit
def runBenchmarks(commit):
    """temporarily check out the given commit to run the benchmarks"""

    print("Running benchmarks for '" + commit + "'")
    commit = revParse(commit)
    print('\033[31m' + "Do not make any changes to files!" + '\033[0m')
    init = revParse("HEAD")

    localChanges = "No local changes to save\n" != subprocess.run(
        ["git", "status"],
        stdout=subprocess.PIPE
    ).stdout

    if localChanges:
        subprocess.run(["git", "stash"], stdout=subprocess.PIPE)

    subprocess.run(["git", "checkout", commit])
    subprocess.run(["stack", "bench"])
    subprocess.run(["git", "checkout", init])

    if localChanges:
        subprocess.run(["git", "stash", "pop"], stdout=subprocess.PIPE)

    print('\033[32m' + "Back to initial state" + '\033[0m')


if __name__ == "__main__":
    # Argument parser
    parser = argparse.ArgumentParser()
    parser.add_argument('--folder',           default='.', type=str, help='benchmark folder to analyze')
    parser.add_argument('--last',  nargs='?', default=5,   type=int, help='include benchmarks for the last "n" commits')
    parser.add_argument('--exact', nargs='*', default=[],  type=str, help='include benchmarks for specific commits')
    parsed = parser.parse_args(sys.argv[1:])

    # Commits
    commits = ["WIP", "HEAD"]
    if parsed.last:
        commits.extend([ "HEAD~" + str(i) for i in range(1,parsed.last) ])
    if parsed.exact:
        commits.extend([ str(commit) for commit in parsed.exact ])

    # Sanitized commits
    sanitized = ["WIP"]
    for commit in commits[1:]:
        try:
            sanitized.append(revParse(commit))
        except:
            print('Invalid commit "' + commit + '"')

    # Load the JSONs
    datas = []
    for sane in sanitized:
        try:
            with open(parsed.folder + '/' + sane + '.json') as json_data:
                datas.append(json.load(json_data))
        except:
            print('Could not read file for "' + sane + '.json"')
            datas.append({})

    # Aggregate the output
    aggregated = {}
    n = 0
    for data in datas:
        aggregated = merge(aggregated, data, n * [ None ], None, lambda xs, x: xs + [x])
        n += 1

    # Convert to a table
    print(tabulate.tabulate(flattenListDict(aggregated), [ '' ] + commits))




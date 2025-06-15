#! /usr/bin/python

import sys
import os

def safe_samefile(f1, f2):
    try:
        return os.path.samefile(f1, f2)
    except:
        return False

def strip_idx(file_name):
    assert os.path.split(file_name)[0] == ""
    parts = str.split(file_name, '-', maxsplit=1)
    if len(parts) == 1: return (None, file_name)
    idx = None
    try:
        idx = int(parts[0])
    except:
        return (None, file_name)
    return (idx, parts[1])

def find_related_files(file_name, dir):
    file_path = os.path.join(dir, file_name)
    assert os.path.splitext(file_name)[1] == ".co"
    assert os.path.exists(file_path) and os.path.isfile(file_path)
    assert os.path.split(file_name)[0] == ""
    
    res = [file_name]
    suffixes = [".expected", ".stdin"]
    for suf in suffixes:
        file = file_path + suf
        if os.path.exists(file) and os.path.isfile(file):
            res.append(file_name + suf)
    return res

def moved_file_path(old_file_path, idx, dir):
    old_dir, old_file_name = os.path.split(old_file_path)
    _, file_name = strip_idx(old_file_name)
    return os.path.join(dir, "{:03d}".format(idx) + "-" + file_name)

def find_test_files(to_move_file_path, dir):
    max_idx = 0
    res = []
    for f in os.listdir(dir):
        if safe_samefile(os.path.join(dir, f), to_move_file_path): continue
        if os.path.splitext(f)[1] != ".co": continue
        idx, _ = strip_idx(f)
        max_idx = max(max_idx, idx)
        test = find_related_files(f, dir)
        res.append((idx, test))
    res.sort(key=lambda entry: entry[0])
    return max_idx + 1, res

def rename_file_group(file_names, idx, dir):
    tgt = []
    for f in file_names:
        old_path = os.path.join("examples", f)
        new_path = moved_file_path(old_path, idx, "examples")
        tgt.append(new_path)
    return tgt


def rename_test_files(test_files, insert_idx):
    inserted_idx = None
    new_files = []
    j = 1
    for i in range(0, len(test_files)):
        idx, files = test_files[i]
        if idx >= insert_idx and inserted_idx == None:
            inserted_idx = j
            j += 1
        new_files = rename_file_group(files, j, "examples")
        j += 1
        for (old, new_path) in zip(files, new_files):
            old_path = os.path.join("examples", old)
            if not safe_samefile(old_path, new_path):
                print(f"mv {old_path} {new_path}", file=sys.stderr)
        new_files.append((j, new_files))

    if insert_idx == None:
        insert_idx = j
    return insert_idx, new_files

to_move_file_path = sys.argv[1]
to_move_dir, to_move_file_name = os.path.split(to_move_file_path)
to_move_file_names = find_related_files(to_move_file_name, to_move_dir)

max_idx, test_files = find_test_files(to_move_file_path, "examples")
insert_idx = max_idx
if len(sys.argv) == 3:
    insert_idx = int(sys.argv[2])
print("files to move:", file=sys.stderr)
insert_idx, tgt_files = rename_test_files(test_files, insert_idx)

tgt_to_move = rename_file_group(to_move_file_names, insert_idx, to_move_dir)
for (old, new) in zip(to_move_file_names, tgt_to_move):
    old_path = os.path.join(to_move_dir, old)
    new_path = os.path.join("examples", new)
    if not safe_samefile(old_path, new_path):
        print(f"mv {old_path} {new_path}", file=sys.stderr)

while True:
    action = input("Accept changes? y/N: ")
    if action == "y" or action == "Y": break
    elif action == "n" or action == "N" or action == "": sys.exit(1)


for (_, old), new in zip(test_files, tgt_files):
    for old, new in zip(old, new):
        old_path = os.path.join("examples", old)
        if not safe_samefile(old_path, new_path):
            print(f"mv {old_path} {new_path}", file=sys.stderr)
            os.rename(old_path, new_path)

for old, new_path in zip(to_move_file_names, tgt_to_move):
    old_path = os.path.join(to_move_dir, old)
    if not safe_samefile(old_path, new_path):
        print(f"mv {old_path} {new_path}", file=sys.stderr)
        os.rename(old_path, new_path)

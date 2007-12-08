import os
import fnmatch
import glob
import types

def splitall(path):
  paths = []
  old_path = path

  while True:
    path, filename = os.path.split(path)

    if path == old_path:
      if path:
        paths.append(path)
      break
    else:
      old_path = path
      paths.append(filename)
  paths.reverse()
  return paths

def relativepath(root, path):
  roots = splitall(os.path.abspath(root))
  paths = splitall(os.path.abspath(path))

  for i, (r, p) in enumerate(zip(roots, paths)):
    j = i
    if r != p:
      break
  else:
    i += 1
    j = len(roots)

  new_paths = ['..'] * (len(roots) - i) + paths[j:]

  if not new_paths:
    return '.'
  else:
    return os.path.join(*new_paths)

def find(path, name=None, include_dirs=True):
  for root, dirs, files in os.walk(path):
    if include_dirs:
      files += dirs

    for f in files:
      if name is not None and not fnmatch.fnmatch(f, name):
        continue

      yield os.path.join(root, f)

def glob_paths(paths, root='.'):
  new_paths = []
  for path in paths:
    # if the path is not a string, assume it's a list of path elements
    if not isinstance(path, types.StringTypes):
      path = os.path.join(*path)

    pattern = os.path.join(root, path)
    new_paths.extend(glob.glob(pattern))
  return new_paths

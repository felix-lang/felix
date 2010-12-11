import io
import tarfile
import zipfile
from contextlib import closing

import fbuild.builders
import fbuild.temp

# ------------------------------------------------------------------------------

def find_submodules(ctx, git):
    # Look up all the submodules
    stdout, stderr = ctx.execute(
        [git, 'submodule', 'status', '--recursive', '--cached'],
        quieter=1)

    for line in io.BytesIO(stdout):
        # Split up the line
        parts = line.decode().strip().split(' ')
        revision, path = parts[0], parts[1]

        if revision and revision[0] in ('+-'):
            revision = revision[1:]

        yield revision, path

# ------------------------------------------------------------------------------

def dist_tar(ctx, git, version):
    """Creates tar files for all of the felix submodules."""

    dst = ctx.buildroot / 'dist/felix-{}.tar'.format(version)
    dst.parent.makedirs()

    prefix = 'felix-{}/'.format(version)

    # Create the initial .tar file.
    ctx.execute([
        git, 'archive',
        '--format', 'tar',
        '--prefix', prefix,
        '-o', dst,
        'HEAD',
        ], git.name + ' archive', dst, color='yellow')

    with closing(tarfile.open(dst, 'a')) as supermodule_tar:
        for revision, submodule in find_submodules(ctx, git):
            # Create a temp file that we'll write the submodule's tar file
            # into.
            with fbuild.temp.tempfile() as f:
                ctx.execute([
                    git, 'archive',
                    '--format', 'tar',
                    '--prefix', prefix + submodule + '/',
                    '-o', f,
                    revision,
                    ], git.name + ' archive', f,
                    quieter=1,
                    cwd=submodule,
                    color='yellow')

                # Add all of the files to the supermodule's tar file.
                with closing(tarfile.open(str(f), 'r')) as submodule_tar:
                    for tarinfo in submodule_tar:
                        supermodule_tar.addfile(
                            tarinfo,
                            submodule_tar.extractfile(tarinfo))

    # Read in the tar file so we can zip it.
    with open(dst, 'rb') as f:
        tar = f.read()

    # Explicitly gzip the .tar.
    ctx.logger.check(' * gzip', dst + '.gz', color='yellow')

    import gzip
    with gzip.open(dst + '.gz', 'wb') as f:
        f.write(tar)

    # Explicitly bz2 the .tar.
    ctx.logger.check(' * bz2', dst + '.bz2', color='yellow')

    import bz2
    with bz2.BZ2File(dst + '.bz2', 'wb') as f:
        f.write(tar)

# ------------------------------------------------------------------------------

def dist_zip(ctx, git, version):
    """Creates zip files for all of the felix submodules."""

    # Find the git executable.
    git = fbuild.builders.find_program(ctx, ['git'])

    dst = ctx.buildroot / 'dist/felix-{}.zip'.format(version)
    dst.parent.makedirs()

    prefix = 'felix-{}/'.format(version)

    # Create the initial .zip file.
    ctx.execute([
        git, 'archive',
        '--format', 'zip',
        '--prefix', prefix,
        '-o', dst,
        'HEAD',
        ], git.name + ' archive', dst, color='yellow')

    with closing(zipfile.ZipFile(dst, 'a')) as supermodule_zip:
        for revision, submodule in find_submodules(ctx, git):
            # Create a temp file that we'll write the submodule's zip file
            # into.
            with fbuild.temp.tempfile() as f:
                ctx.execute([
                    git, 'archive',
                    '--format', 'zip',
                    '--prefix', prefix + submodule + '/',
                    '-o', f,
                    revision,
                    ], git.name + ' archive', f,
                    quieter=1,
                    cwd=submodule,
                    color='yellow')

                # Add all of the files to the supermodule's zip file.
                with closing(zipfile.ZipFile(str(f), 'r')) as submodule_zip:
                    for zipinfo in submodule_zip.infolist():
                        supermodule_zip.writestr(
                            zipinfo,
                            submodule_zip.read(zipinfo))

mkdir -p sandbox
#---------------------------------------------------------
echo "PROG: ack" >&2
echo "PROG: ack"
echo "Felix" >&2
echo "Felix"
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/flx-ack speed/ack/felix/test.flx && time -p sandbox/flx-ack
echo "clang" >&2
echo "clang"
clang -O2 -m64 speed/ack/c/test.c -o sandbox/clang-O2-ack && time -p sandbox/clang-O2-ack
echo "gcc" >&2
echo "gcc"
gcc -O2 -m64 speed/ack/c/test.c -o sandbox/gcc-O2-ack && time -p sandbox/gcc-O2-ack
echo "Ocaml" >&2
echo "Ocaml"
ocamlopt.opt -o sandbox/ocaml-ack unix.cmxa speed/ack/ocaml/test.ml && time -p sandbox/ocaml-ack
#---------------------------------------------------------
echo "PROG: takfp" >&2
echo "PROG: takfp"
echo "Felix" >&2
echo "Felix"
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/flx-takfp speed/takfp/felix/test.flx && time -p sandbox/flx-takfp
echo "clang" >&2
echo "clang"
clang -O2 -m64 speed/takfp/c/test.c -o sandbox/clang-O2-takfp && time -p sandbox/clang-O2-takfp
echo "gcc" >&2
echo "gcc"
gcc -O2 -m64 speed/takfp/c/test.c -o sandbox/gcc-O2-takfp && time -p sandbox/gcc-O2-takfp
echo "Ocaml" >&2
echo "Ocaml"
ocamlopt.opt -o sandbox/ocaml-takfp unix.cmxa speed/takfp/ocaml/test.ml && time -p sandbox/ocaml-takfp
#---------------------------------------------------------
echo "PROG: nbody" >&2
echo "PROG: nbody"
echo "Felix" >&2
echo "Felix"
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/flx-nbody speed/nbody/felix/test.flx && time -p sandbox/flx-nbody
echo "clang" >&2
echo "clang"
clang -O2 -m64 speed/nbody/c/test.c -o sandbox/clang-O2-nbody && time -p sandbox/clang-O2-nbody
echo "gcc" >&2
echo "gcc"
gcc -O2 -m64 speed/nbody/c/test.c -o sandbox/gcc-O2-nbody && time -p sandbox/gcc-O2-nbody
echo "Ocaml" >&2
echo "Ocaml"
ocamlopt.opt -o sandbox/ocaml-nbody unix.cmxa speed/nbody/ocaml/test.ml && time -p sandbox/ocaml-nbody
#---------------------------------------------------------
echo "PROG: mandelbrot" >&2
echo "PROG: mandelbrot"
echo "Felix" >&2
echo "Felix"
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/flx-mandelbrot speed/mandelbrot/felix/test.flx && time -p sandbox/flx-mandelbrot
echo "clang++" >&2
echo "clang++"
clang++ -O2 -m64 speed/mandelbrot/c++/test.cc -o sandbox/clang-O2-mandelbrot && time -p sandbox/clang-O2-mandelbrot
echo "g++" >&2
echo "g++"
g++ -O2 -m64 speed/mandelbrot/c++/test.cc -o sandbox/gcc-O2-mandelbrot && time -p sandbox/gcc-O2-mandelbrot
echo "Ocaml" >&2
echo "Ocaml"
ocamlopt.opt -o sandbox/ocaml-mandelbrot unix.cmxa speed/mandelbrot/ocaml/test.ml && time -p sandbox/ocaml-mandelbrot
#---------------------------------------------------------
echo "PROG: mandelbrot21" >&2
echo "PROG: mandelbrot21"
echo "Felix" >&2
echo "Felix"
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/flx-mandelbrot21 speed/mandelbrot21/felix/test.flx && time -p sandbox/flx-mandelbrot21
echo "clang++" >&2
echo "clang++"
clang++ -O2 -m64 speed/mandelbrot21/c++/test.cc -o sandbox/clang-O2-mandelbrot21 && time -p sandbox/clang-O2-mandelbrot21
echo "g++" >&2
echo "g++"
g++ -O2 -m64 speed/mandelbrot21/c++/test.cc -o sandbox/gcc-O2-mandelbrot21 && time -p sandbox/gcc-O2-mandelbrot21
echo "Ocaml"
echo "Ocaml" >&2
ocamlopt.opt -o sandbox/ocaml-mandelbrot21 unix.cmxa speed/mandelbrot21/ocaml/test.ml && time -p sandbox/ocaml-mandelbrot21
#---------------------------------------------------------
echo "PROG: spectralnorm" >&2
echo "PROG: spectralnorm"
echo "Felix" >&2
echo "Felix"
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/flx-spectralnorm speed/spectralnorm/felix/test.flx && time -p sandbox/flx-spectralnorm
echo "clang" >&2
echo "clang"
clang -O2 -m64 speed/spectralnorm/c/test.c -o sandbox/clang-O2-spectralnorm && time -p sandbox/clang-O2-spectralnorm
echo "gcc" >&2
echo "gcc"
gcc -O2 -m64 speed/spectralnorm/c/test.c -o sandbox/gcc-O2-spectralnorm && time -p sandbox/gcc-O2-spectralnorm
echo "Ocaml" >&2
echo "Ocaml"
ocamlopt.opt -o sandbox/ocaml-spectralnorm unix.cmxa speed/spectralnorm/ocaml/test.ml && time -p sandbox/ocaml-spectralnorm
#---------------------------------------------------------
rm -rf sandbox


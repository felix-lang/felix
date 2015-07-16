mkdir -p sandbox
#---------------------------------------------------------
echo "PROG: ack" >&2
echo "Felix: clang -O2" >&2
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-ack speed/ack/felix/test.build/release/host/bin/flx --felix=build.fpc --usage=hyperlight && time sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-ack
echo "C: clang 3.3 -O2" >&2
clang -O2 -m64 speed/ack/c/test.c -o sandbox/clang-O2-ack && time sandbox/clang-O2-ack
echo "C: gcc 4.2.1 -O2" >&2
gcc -O2 -m64 speed/ack/c/test.c -o sandbox/gcc-O2-ack && time sandbox/gcc-O2-ack
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-ack unix.cmxa speed/ack/ocaml/test.ml && time sandbox/ocaml-ack
#---------------------------------------------------------
echo "PROG: takfp" >&2
echo "Felix: clang -O2" >&2
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-takfp speed/takfp/felix/test.build/release/host/bin/flx --felix=build.fpc --usage=hyperlight && time sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-takfp
echo "C: clang 3.3 -O2" >&2
clang -O2 -m64 speed/takfp/c/test.c -o sandbox/clang-O2-takfp && time sandbox/clang-O2-takfp
echo "C: gcc 4.2.1 -O2" >&2
gcc -O2 -m64 speed/takfp/c/test.c -o sandbox/gcc-O2-takfp && time sandbox/gcc-O2-takfp
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-takfp unix.cmxa speed/takfp/ocaml/test.ml && time sandbox/ocaml-takfp
#---------------------------------------------------------
echo "PROG: nbody" >&2
echo "Felix: clang -O2" >&2
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-nbody speed/nbody/felix/test.build/release/host/bin/flx --felix=build.fpc --usage=hyperlight && time sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-nbody
echo "C: clang 3.3 -O2" >&2
clang -O2 -m64 speed/nbody/c/test.c -o sandbox/clang-O2-nbody && time sandbox/clang-O2-nbody
echo "C: gcc 4.2.1 -O2" >&2
gcc -O2 -m64 speed/nbody/c/test.c -o sandbox/gcc-O2-nbody && time sandbox/gcc-O2-nbody
echo "C++: clang 3.3 -O2" >&2
clang -O2 -m64 speed/nbody/c++/test.cc -o sandbox/clang-O2-nbody && time sandbox/clang-O2-nbody
echo "C++: g++ 4.2.1 -O2" >&2
g++ -O2 -m64 speed/nbody/c++/test.cc -o sandbox/gcc-O2-nbody && time sandbox/gcc-O2-nbody
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-nbody unix.cmxa speed/nbody/ocaml/test.ml && time sandbox/ocaml-nbody
#---------------------------------------------------------
echo "PROG: mandelbrot" >&2
echo "Felix: clang -O2" >&2
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-mandelbrot speed/mandelbrot/felix/test.build/release/host/bin/flx --felix=build.fpc --usage=hyperlight && time sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-mandelbrot
echo "C++: clang 3.3 -O2" >&2
clang++ -O2 -m64 speed/mandelbrot/c++/test.cc -o sandbox/clang-O2-mandelbrot && time sandbox/clang-O2-mandelbrot
echo "C++: gcc 4.2.1 -O2" >&2
g++ -O2 -m64 speed/mandelbrot/c++/test.cc -o sandbox/gcc-O2-mandelbrot && time sandbox/gcc-O2-mandelbrot
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-mandelbrot unix.cmxa speed/mandelbrot/ocaml/test.ml && time sandbox/ocaml-mandelbrot
#---------------------------------------------------------
echo "PROG: mandelbrot21" >&2
echo "Felix: clang -O2" >&2
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-mandelbrot21 speed/mandelbrot21/felix/test.build/release/host/bin/flx --felix=build.fpc --usage=hyperlight && time sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-mandelbrot21
echo "C++: clang 3.3 -O2" >&2
clang++ -O2 -m64 speed/mandelbrot21/c++/test.cc -o sandbox/clang-O2-mandelbrot21 && time sandbox/clang-O2-mandelbrot21
echo "C++: gcc 4.2.1 -O2" >&2
g++ -O2 -m64 speed/mandelbrot21/c++/test.cc -o sandbox/gcc-O2-mandelbrot21 && time sandbox/gcc-O2-mandelbrot21
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-mandelbrot21 unix.cmxa speed/mandelbrot21/ocaml/test.ml && time sandbox/ocaml-mandelbrot21
#---------------------------------------------------------
echo "PROG: spectralnorm" >&2
echo "Felix: clang -O2" >&2
build/release/host/bin/flx --felix=build.fpc --usage=hyperlight -O2 -c --static -o sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-spectralnorm speed/spectralnorm/felix/test.build/release/host/bin/flx --felix=build.fpc --usage=hyperlight && time sandbox/build/release/host/bin/flx --felix=build.fpc --usage=hyperlight-spectralnorm
echo "C: clang 3.3 -O2" >&2
clang -O2 -m64 speed/spectralnorm/c/test.c -o sandbox/clang-O2-spectralnorm && time sandbox/clang-O2-spectralnorm
echo "C: gcc 4.2.1 -O2" >&2
gcc -O2 -m64 speed/spectralnorm/c/test.c -o sandbox/gcc-O2-spectralnorm && time sandbox/gcc-O2-spectralnorm
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-spectralnorm unix.cmxa speed/spectralnorm/ocaml/test.ml && time sandbox/ocaml-spectralnorm
#---------------------------------------------------------
rm -rf sandbox


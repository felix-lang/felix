mkdir -p sandbox
echo "PROG: ack" >&2
echo "Felix: clang -O1" >&2
flx -O1 -c --static -o sandbox/flx-ack speed/ack/felix/test.flx && time sandbox/flx-ack
echo "C: clang 3.3 -O1" >&2
clang -O1 -m64 speed/ack/c/ack.c -o sandbox/clang-O1-ack && time sandbox/clang-O1-ack
echo "C: clang 3.3 -O2" >&2
clang -O2 -m64 speed/ack/c/ack.c -o sandbox/clang-O2-ack && time sandbox/clang-O2-ack
echo "C: gcc 4.2.1 -O1" >&2
gcc -O1 -m64 speed/ack/c/ack.c -o sandbox/gcc-O1-ack && time sandbox/gcc-O1-ack
echo "C: gcc 4.2.1 -O2" >&2
gcc -O2 -m64 speed/ack/c/ack.c -o sandbox/gcc-O2-ack && time sandbox/gcc-O2-ack
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-ack unix.cmxa speed/ack/ocaml/test.ml && time sandbox/ocaml-ack
echo "PROG: nbody" >&2
echo "Felix: clang -O1" >&2
flx -O1 -c --static -o sandbox/flx-nbody speed/nbody/felix/test.flx && time sandbox/flx-nbody
echo "C: clang 3.3 -O1" >&2
clang -O1 -m64 speed/nbody/c/nbody.c -o sandbox/clang-O1-nbody && time sandbox/clang-O1-nbody
echo "C: clang 3.3 -O2" >&2
clang -O2 -m64 speed/nbody/c/nbody.c -o sandbox/clang-O2-nbody && time sandbox/clang-O2-nbody
echo "C: gcc 4.2.1 -O1" >&2
gcc -O1 -m64 speed/nbody/c/nbody.c -o sandbox/gcc-O1-nbody && time sandbox/gcc-O1-nbody
echo "C: gcc 4.2.1 -O2" >&2
gcc -O2 -m64 speed/nbody/c/nbody.c -o sandbox/gcc-O2-nbody && time sandbox/gcc-O2-nbody
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-nbody unix.cmxa speed/nbody/ocaml/nbody.ml && time sandbox/ocaml-nbody
echo "PROG: mandelbrot" >&2
echo "Felix: clang -O1" >&2
flx -O1 -c --static -o sandbox/flx-mandelbrot speed/mandelbrot/felix/test.flx && time sandbox/flx-mandelbrot
echo "C++: clang 3.3 -O1" >&2
clang++ -O1 -m64 speed/mandelbrot/c++/test.cc -o sandbox/clang-O1-mandelbrot && time sandbox/clang-O1-mandelbrot
echo "C++: clang 3.3 -O2" >&2
clang++ -O2 -m64 speed/mandelbrot/c++/test.cc -o sandbox/clang-O2-mandelbrot && time sandbox/clang-O2-mandelbrot
echo "C++: gcc 4.2.1 -O1" >&2
g++ -O1 -m64 speed/mandelbrot/c++/test.cc -o sandbox/gcc-O1-mandelbrot && time sandbox/gcc-O1-mandelbrot
echo "C++: gcc 4.2.1 -O2" >&2
g++ -O2 -m64 speed/mandelbrot/c++/test.cc -o sandbox/gcc-O2-mandelbrot && time sandbox/gcc-O2-mandelbrot
echo "Ocaml 3.11.1" >&2
ocamlopt.opt -o sandbox/ocaml-mandelbrot unix.cmxa speed/mandelbrot/ocaml/test.ml && time sandbox/ocaml-mandelbrot
rm -rf sandbox

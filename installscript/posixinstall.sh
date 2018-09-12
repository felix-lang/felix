rm -rfd /usr/local/lib/felix/felix-2018.09.14
mkdir -p /usr/local/lib/felix/felix-2018.09.14/share
mkdir -p /usr/local/lib/felix/felix-2018.09.14/host
cp -r build/release/share/* /usr/local/lib/felix/felix-2018.09.14/share
cp -r build/release/host/* /usr/local/lib/felix/felix-2018.09.14/host

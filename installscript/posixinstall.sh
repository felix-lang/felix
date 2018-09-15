rm -rfd /usr/local/lib/felix/felix-2018.09.16
mkdir -p /usr/local/lib/felix/felix-2018.09.16/share
mkdir -p /usr/local/lib/felix/felix-2018.09.16/host
cp -r build/release/share/* /usr/local/lib/felix/felix-2018.09.16/share
cp -r build/release/host/* /usr/local/lib/felix/felix-2018.09.16/host

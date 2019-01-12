rm -rfd /usr/local/lib/felix/felix-2019.01.06
mkdir -p /usr/local/lib/felix/felix-2019.01.06/share
mkdir -p /usr/local/lib/felix/felix-2019.01.06/host
cp -r build/release/share/* /usr/local/lib/felix/felix-2019.01.06/share
cp -r build/release/host/* /usr/local/lib/felix/felix-2019.01.06/host

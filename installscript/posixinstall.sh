rm -rfd /usr/local/lib/felix/felix-2018.08.17-rc1
mkdir -p /usr/local/lib/felix/felix-2018.08.17-rc1/share
mkdir -p /usr/local/lib/felix/felix-2018.08.17-rc1/host
cp -r build/release/share/* /usr/local/lib/felix/felix-2018.08.17-rc1/share
cp -r build/release/host/* /usr/local/lib/felix/felix-2018.08.17-rc1/host

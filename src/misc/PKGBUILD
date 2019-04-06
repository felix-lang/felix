# Maintainer: Tony Nguyen <v35gtr@hotmail.com>
# Contributor: Alexander Rødseth <rodseth@gmail.com>

pkgname=felix
pkgver=2019.01.06.r82.gb2de456f4
pkgrel=1
pkgdesc='The Felix programming language'
arch=('x86_64')
url='https://github.com/felix-lang/felix'
license=('Felix is Free For Any Use (FFAU)/Public Domain.')
depends=('python3' 'gcc' 'gmp' 'gsl' 'sdl2' 'sdl2_ttf' 'sdl2_gfx' 'sdl2_image')
makedepends=('ocaml')
source=("${pkgname}::git+https://github.com/felix-lang/felix")
sha512sums=('SKIP')

prepare() {
	cd "$pkgname"

	# Arch Linux installs SLD2 to /usr instead of /usr/local
	sed -i 's|usr/local|usr|g' src/packages/sdl.fdoc
}

pkgver() {
	cd "$pkgname"
	git describe --long | sed 's/\([^-]*-g\)/r\1/;s/-/./g'
}

build() {
	cd "$pkgname"
	make build
	make doc
}

check() {
	cd "$pkgname"
	make test
}

package() {
	cd "$pkgname"
	make PREFIX="$pkgdir/usr/local" install
}

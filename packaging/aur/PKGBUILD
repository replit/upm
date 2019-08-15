# Maintainer: Repl.it <contact+upm@repl.it>
pkgname=upm
pkgver=1.0
pkgrel=1
pkgdesc="Universal package manager: Python, Node.js, Ruby, Emacs Lisp."
arch=('any')
url=""
license=('MIT')
source=("https://github.com/replit/upm/releases/download/v${pkgver}/upm_${pkgver}_linux_amd64.tar.gz")
md5sums=('b5447e9387ac5d9c0d7c78106941cbef')

package() {
  install -d "${pkgdir}/usr/bin"
  cp "upm" "${pkgdir}/usr/bin/"
}

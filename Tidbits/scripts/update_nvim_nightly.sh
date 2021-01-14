#!/bin/sh

tmp_file=/tmp/nvim.appimage.crdownload
install_file=~/Appimages/nvim.appimage

try_download() {
  wget \
    'https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage' \
    -T3 -t20 -cO $tmp_file
}

download() {
  success=false
  for i in $(seq 3); do
    if try_download; then
      success=true
      break
    fi
  done

  if [ $success = true ]; then
    echo 'Downloaded neovim appimage'
    return 0
  else
    echo 'Failed to download neovim appimage'
    return 1
  fi
}

install() {
  if command install -m 755 $tmp_file $install_file; then
    echo 'Installed binary'
  else
    echo 'Installation failed'
  fi
}

rm $tmp_file
if download; then
  install
fi

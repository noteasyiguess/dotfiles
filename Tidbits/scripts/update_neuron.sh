#!/bin/sh

if [ -z "$1" ]; then
  echo 'Provide the url from where to download the .tar.gz' 1>&2
  exit 1
fi

src_url=$1
tmp_file=/tmp/neuron.archive.crdownload
binary_file=/tmp/neuron #Binary extracted from the archive
install_file=~/Appimages/neuron

try_download() {
  wget \
    "$src_url" \
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
  tar xf $tmp_file -C "$(dirname $binary_file)" && \
  mv $binary_file $install_file && \
  chmod +x $install_file

  if [ $? -eq 0 ]; then
    echo 'Installed binary'
  else
    echo 'Installation failed'
  fi
}

rm $tmp_file $binary_file
if download; then
  install
fi

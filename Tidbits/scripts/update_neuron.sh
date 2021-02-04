#!/bin/sh

src_url=$(curl -sL https://api.github.com/repos/srid/neuron/releases/latest | jq -r '.assets[].browser_download_url' | grep -i linux | head -n1)
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
  command mv $binary_file $install_file && \
  chmod +x $install_file

  if [ $? -eq 0 ]; then
    echo 'Installed binary'
  else
    echo 'Installation failed'
  fi
}

echo "Using donwload link: " $src_url
rm $tmp_file $binary_file
if download; then
  install
fi

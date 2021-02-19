#!/bin/bash

[ $# -lt 1 ] && echo "Usage: $0 DIR_TO_STORE" 1>&2 && exit 1
STORE_FOLDER=$(dirname "$1/SomeFakeDir")  # To remove any trailing /

# Stats
echo "Saving links to '$STORE_FOLDER/links'..."
echo "Reading encrypted files from '$PWD'..."

# Ask for password 2 times
echo -n "Password: "; read -s -r pw
echo

# Get all fnames
all_fnames=$(ls | grep -P '[0-9]+_fname')

# Folder to store all symlinks
mkdir "$STORE_FOLDER/links"

# Foreach file decrypt and save to symlink
for fname_fn in $(echo $all_fnames); do
   # Extract the filename
   id=$(echo -n $fname_fn | sed 's/_fname//')

   # Check if all ids are numeric
   echo -n "$id" | grep -P "^[0-9]+$" 1>/dev/null || {
      echo "Warning(Skipping): "$id" is not numerical"
      continue
   }

   # Decrypt to orig filename
   orig_fn=$(cat $fname_fn | gpg -q --no-symkey-cache \
      --batch --pinentry-mode loopback --passphrase "$pw" \
      --decrypt)

   # If an error tell the user
   [ $? -ne 0 ] && {
      echo "- Failed decrypting $orig_fn"
      continue
   }

   # Prepend orig filename with id
   orig_fn=${id}_${orig_fn}

   # Encrypted data file
   dat_fn=${id}_dat  

   # Make sure [id]_dat exists
   [ ! -r "$dat_fn" ] && {
      echo "Warning(Skipping): $dat_fn doesn't exist" 1>&2
      continue
   }

   # Alas, symlink
   ln -s "$PWD/$dat_fn" "$STORE_FOLDER/links/$orig_fn"

   if [ $? -eq 0 ]; then
      echo "Made link '$orig_fn'"
   else
      echo "Failed link of '$orig_fn'" 1>&2
   fi
done

echo "Phew! Coding was hard ᛯ"

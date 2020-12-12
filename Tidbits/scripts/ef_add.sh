#!/bin/bash

# ef_add is encrypted file add
# Encrypts file and its filename and saves to [id]_dat [id]_fname in the current directory

[ $# -lt 1 ] && echo "Usage: $0 FILE_TO_ADD" 1>&2 && exit 1

# filename of the file excluding the dirname. To be used for enc
orig_fn=$(basename "$1")

# $1 must be a valid file
[ ! -r "$1" ] && echo "Error: $1 is not a file" 1>&2 && exit 2

# Get the id and check if it is a number
echo -n "Numerical id? "; read -r id 
echo -n "$id" | grep -P "^[0-9]+$" 1>/dev/null || {
   echo "Error: "$id" is not numerical. check for whitespaces"
   exit 3
}

# Got our two filenames
dat_fn=$(echo ${id}_dat)
fname_fn=$(echo ${id}_fname)

# Make sure the filenames are not already taken up
[ -f $dat_fn ] || [ -f $fname_fn ] && { 
   echo "Either $dat_fn or $fname_fn exists. Make sure to choose a unique id"
   exit 4 
}

# Ask for password 2 times
echo -n "Password: "; read -s -r pw
{
   echo -ne "\nRe-type password: "; read -s -r pw2; echo
   [ "$pw" != "$pw2" ] && echo "Error: Passwords don't match" && exit 5
   unset pw2
}

# Encrypt the file
gpg --no-symkey-cache \
   --batch --pinentry-mode loopback --passphrase "$pw" \
   -o "$dat_fn" \
   --symmetric "$1"

if [ $? -eq 0 ]; then
   echo "DONE ☻ : Saved encrypted '$orig_fn' contents -> '$dat_fn'"
else
   echo "FAILED ☹: Cannot encrypt '$orig_fn' contents -> '$dat_fn'" 1>&2
   exit 6
fi

# Encrypt the orig filename
echo $orig_fn | gpg --no-symkey-cache \
   --batch --pinentry-mode loopback --passphrase "$pw" \
   -o "$fname_fn" \
   --symmetric 

if [ $? -eq 0 ]; then
   echo "DONE ☻ : Saved encrypted '$orig_fn' filename -> '$fname_fn'"
else
   echo "FAILED ☹: Cannot encrypt '$orig_fn' filename -> '$fname_fn'" 1>&2
   exit 7
fi

# Whether to delete the orig file
echo -n "Remove the original file? "
rm -i "$1" 2>/dev/null

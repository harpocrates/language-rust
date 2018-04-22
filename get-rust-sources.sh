#!/bin/sh

# Exit if anything goes wrong
set -e

# Usage info
if ! [ $# = 1 ]
then
  echo "This script gets all of the (> 1000 LOC) source files in repositories"
  echo "under 'rust-lang' and 'rust-lang-nursery' organizations"
  echo ""
  echo "Expected usage:"
  echo "                          $0 <destination-folder>"
  echo ""
  echo "You probably want to run:"
  echo "                          $0 sample-sources"
  exit 1
else
  DEST="$1"
fi

# Work inside a temporary directory
TEMP=temp
mkdir $TEMP
cd $TEMP

# Get the JSON files
curl https://api.github.com/orgs/rust-lang/repos > rust-lang.json
curl https://api.github.com/orgs/rust-lang-nursery/repos > rust-lang-nursery.json

# Make one big JSON array of repos and extract the name and clone url
(jq -rs '.[0] + .[1] | .[] | (.name, .clone_url)' rust-lang.json rust-lang-nursery.json \
) | while read -r REPO_NAME; read -r REPO_CLONE; do
 
  # Skip 'multirust-rs-binaries' and 'rustc-timing-archive' in particular
  if [ $REPO_NAME = "multirust-rs-binaries" ] || [ $REPO_NAME = "rustc-timing-archive" ]
  then
      continue
  fi

  # Do a shallow clone of the repo
  echo "Cloning $REPO_NAME at $REPO_CLONE"
  git clone --depth=1 $REPO_CLONE

  # Find all rust files in the repo and copy each of these files to the DEST folder, provided they
  # are more than 2000 lines long. The 2000 line long stipulation serves several purposes: to
  # provide files that whose parsing time is non-trivial and also source files which are expected to
  # compile.
  echo "Finding rust files in $REPO_NAME"
  find $REPO_NAME -type f -name '*.rs' | while read -r FILE; do
    
    # Escaped file name
    DEST_FILE="../$DEST/${FILE//\//|}"

    # Check the file is longer than 2000 lines
    if (( 1000 < $(wc -l < "$FILE") ))
    then
      # copy the file over, but filter out lines which contain a reference to a 'mod <name>;'
      # since those cause some issues for the rust compiler (it will go looking for those files
      # even during parsing.
      grep -Ev "(#\[macro_use\]|mod\s+\w+;)" $FILE > $DEST_FILE
    fi

  done;

  # Delete the cloned repo
  rm -rf $REPO_NAME

done;

# Clean up
cd ..
rm -rf $TEMP

# Print disclaimer
echo "WARNING: Don't expect the 'rustc-tests' suite to necessarily"
echo "         work on all the files this produces. A failure in the"
echo "         test suite is only a bug if \`rustc\` succeeds on the"
echo "         same file."


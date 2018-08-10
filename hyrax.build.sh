set -e

# Stack build
stack build --allow-different-user

# Get a branch containg this commit
branch=$2

# Get output path
root=$(stack path | grep local-install-root | cut -d' ' -f2-)

# Copy binary to S3
aws s3 cp "$root/bin/awssy" "s3://hyrax-ci/awssy/bin/$branch/awssy"

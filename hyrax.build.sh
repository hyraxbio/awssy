set -e

# Stack build
stack build --allow-different-user

# Get a branch containg this commit
branch=$2
branch_=$(echo $branch | sed -r 's/\//_/g')

# Get output path
root=$(stack path | grep local-install-root | cut -d' ' -f2-)

# Copy binary to S3
aws s3 cp "$root/bin/awssy" "s3://hyrax-ci/awssy/bin/$branch/awssy"

cd docker
#cp "$root/bin/awssy" awssy
#docker build --tag=awssy .
#docker tag awssy localhost:5987/awssy_$branch_
#docker push localhost:5987/awssy_$branch_
./buildDocker.sh "_$branch_"
docker system prune -f

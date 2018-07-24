# awssy

Simple terminal GUI for the aws cli tool, for connecting to EC2 instances

![](awssy.png)

# Usage

## First use

For the first use specify your ssh key and the `--save` (or `-s`) option to save it as the default

  `awssy --key ~/.ssh/mykey.pem -s`

## Typical use

For all future runs you can then simply use

  `awssy`


To use a different key for a single session

  `awssy --key ~/.ssh/otherkey.pem`


To change the default

  `awssy --key ~/.ssh/otherkey.pem -s`

## Startup

When awssy starts up it will connect to EC2 to get the instance details, this can take a minute. The EC2 details are requested every 5 minutes, or when you press `F5`


## Starting a SSH session

Select a started instance and press `enter`. This will

 - Grant your current IP SSH access to the instance
 - Start a SSH session
 - When the SSH session ends, remove SSH instance for your IP


## Starting a terminal session

Select a started instance and press `s`. This will

 - Grant your current IP SSH access to the instance
 - Start a local terminal session with the following shell variables defined
   - AWS_H: The host IP
   - AWS_U: The user name (ec2-user)
   - AWS_K: Your selected ssh key
   - AWS_UH: user_name:host
   
   e.g. to copy a file `scp -i $AWS_K ./README.md $AWS_UH:/home/$AWS_U/README.md`
   
 - When the SSH session ends, remove SSH instance for your IP

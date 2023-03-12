#! /bin/sh

# Default values
KEY="${HOME}/.ssh/azure"  # Location of private key
RG=PB  # Name of resource group

#!/bin/bash
for i in "$@"
do
    case $i in
	-k=*|--key=*)
	    KEY="${i#*=}"

	    ;;
	-i=*|--ip=*)
	    IP="${i#*=}"
	    ;;
	-r=*|--resource-group=*)
	    RG="${-r#*=}"
	    ;;
	*)
            # unknown option
	    ;;
    esac
done

if [ -z "$IP" ]; then
    NEW=true
else
    NEW=false
fi

echo KEY = "${KEY}"
echo IP = "${IP}"
echo NEW = "${NEW}"

KEY_FLAG="-i $KEY"
echo KEY_FLAG = "${KEY_FLAG}"

if $NEW; then

    # Create new virtual machine. Standard_D64_v3 has 64 cores for assessing
    # Ahmdal's law. See more size at:
    # https://docs.microsoft.com/en-us/azure/virtual-machines/windows/sizes-general
    az_cmd="az vm create
       --resource-group ${RG}
       --name VM
       --image Canonical:UbuntuServer:19.04:19.04.201906280
       --size Standard_D64_v3
       --admin-username ${USER}
       --ssh-key-value ${KEY}.pub"

    # Parse the result to get the IP address
    result=$($az_cmd)
    echo "$result"
    ip_line=$(echo "$result" | grep -o '"publicIpAddress": "[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}"')
    echo "$ip_line"
    IP=$(echo "$ip_line" | grep -o '[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}')

    # Wait a bit before connecting, otherwise Azure gives "permission denied"
    sleep 20
fi

# Copy local files
ssh -o StrictHostKeyChecking=no "${KEY_FLAG}" "${USER}@${IP}" "rm -r -f PosteriorBootstrap && mkdir PosteriorBootstrap"
scp "${KEY_FLAG}" -r ./* "${USER}@${IP}:PosteriorBootstrap/"

# Install software and R packages required in case the machine doesn't have them
ssh "${KEY_FLAG}" "${USER}@${IP}" PosteriorBootstrap/azure/setup.sh
ssh "${KEY_FLAG}" "${USER}@${IP}" Rscript PosteriorBootstrap/azure/setup.R

# Run computations
ssh "${KEY_FLAG}" "${USER}@${IP}" Rscript PosteriorBootstrap/azure/build_and_compute.R

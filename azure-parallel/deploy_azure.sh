KEY="~/.ssh/azure"

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

echo KEY = ${KEY}
echo IP = ${IP}
echo NEW = ${NEW}

KEY_FLAG="-i $KEY"
echo KEYFLAG = ${KEY_FLAG}

if $new; then

    # Create new virtual machine
    az_cmd="az vm create
       --resource-group PB
       --name VM
       --image microsoft-dsvm:linux-data-science-vm-ubuntu:linuxdsvmubuntu:19.04.00
       --size Standard_F16s_v2
       --admin-username ${USER}
       --ssh-key-value ${KEY}.pub"

    # Parse the result to get the IP address
    result=$($az_cmd)
    echo $result
    ip_line=$(echo $result | grep -o '"publicIpAddress": "[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}"')
    echo $ip_line
    IP=$(echo $ip_line | grep -o '[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}')

    # Wait a bit before connecting
    sleep 10
else
    # Set the IP address manually
    IP=51.140.178.208
fi

# Copy local files
ssh -o StrictHostKeyChecking=no ${PRIVATE_KEY_FLAG} ${USER}@${IP} "rm -r -f PosteriorBootstrap && mkdir PosteriorBootstrap"
scp ${PRIVATE_KEY_FLAG} -r ./* ${USER}@${IP}:PosteriorBootstrap/

# Add compilation flags to compile Rstan
ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} "rm -r .R; mkdir .R && mv PosteriorBootstrap/azure-parallel/Makevars .R/"

# If it's a new machine, install R packages required
if $new; then
    ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} Rscript PosteriorBootstrap/azure-parallel/setup.R
fi


# This is the command that hangs the machine ----------------------------

# Run either this command to see the hang with minimal output:
ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} Rscript PosteriorBootstrap/azure-parallel/build_and_compute.R

# or this command to see the output of strace
#ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} strace -Ff -tt Rscript PosteriorBootstrap/azure-parallel/build_and_compute.R 2>&1 | tee strace-R.log


PRIVATE_KEY="~/.ssh/azure"
PRIVATE_KEY_FLAG="-i $PRIVATE_KEY"

new=true

if $new; then

    # Create new virtual machine
    az_cmd="az vm create
       --resource-group R
       --name PBVM
       --image microsoft-dsvm:linux-data-science-vm-ubuntu:linuxdsvmubuntu:19.04.00
       --size Standard_F16s_v2
       --admin-username ${USER}
       --ssh-key-value ${PRIVATE_KEY}.pub"

    # Parse the result to get the IP address
    result=$($az_cmd)
    echo $result
    ip_line=$(echo $result | grep -o '"publicIpAddress": "[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}"')
    echo $ip_line
    IP=$(echo $ip_line | grep -o '[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}')

    # Wait a bit before connecting
    sleep 60
else
    # Set the IP address manually
    IP=
fi

# Copy local files
ssh -o StrictHostKeyChecking=no ${PRIVATE_KEY_FLAG} ${USER}@${IP} "rm -r -f PosteriorBootstrap && mkdir PosteriorBootstrap"
scp ${PRIVATE_KEY_FLAG} -r ./* ${USER}@${IP}:PosteriorBootstrap/

# Add compilation flags to compile Rstan
ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} "rm -r .R; mkdir .R && mv PosteriorBootstrap/run-on-azure/Makevars .R/"


# This is the command that hangs the machine when run in parallel ----------------------------

# Run either this command to see the hang with minimal output:
ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} Rscript PosteriorBootstrap/run-on-azure/build_and_compute.R

# or this command to see the output of strace
#ssh ${PRIVATE_KEY_FLAG} ${USER}@${IP} strace -Ff -tt Rscript PosteriorBootstrap/azure-parallel/build_and_compute.R 2>&1 | tee strace-R.log


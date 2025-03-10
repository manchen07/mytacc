#!/bin/bash
#SBATCH -J SR-sim                # Job name
#SBATCH -o SRsim.o%j             # Name of stdout output file (%j expands to jobId)
#SBATCH -e SRsim.o%j             # Name of stderr output file(%j expands to jobId)
#SBATCH -p skx-dev			        # Submit to the 'normal' or 'development' queue
#SBATCH -N 2                    # Total number of nodes
#SBATCH -n 5                  	# Total number of mpi tasks requested
#SBATCH -t 2:00:00             	# Run time (hh:mm:ss)
#SBATCH --mail-user=manchen9005@gmail.com
#SBATCH --mail-type=begin
#SBATCH --mail-type=end

# load docker file
module load tacc-apptainer
echo "running the SRPB container:"
apptainer run docker://manchen07/srpb:v0

ibrun SR-simulation.R
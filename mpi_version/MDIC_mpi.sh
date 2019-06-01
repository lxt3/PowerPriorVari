#$ -N ib_pbdMPI 
#$ -cwd 
#$ -S /bin/sh 
#$ -l infiniband 
#$ -j y 
#$ -o job.out 
#$ -l h_vmem=16G 
#$ -pe orte 16

source /projects/mikem/applications/centos7/R-3.5.1/set-env.sh
source /projects/mikem/applications/centos7/openmpi/setup-env.sh

APP="Rscript runSimulationsMPI.R"

# Node list file preparation 
export SGE_NODEFILE=/tmp/pe_hostfile_$JOB_ID

# OUT_FILE=out_$JOB_ID

rm -f temp_"$JOB_ID" 
awk '{for (i=1; i<=$2; i++) print ($1); }' $PE_HOSTFILE > temp_"$JOB_ID" 
awk -F. '{ print ( $1"."$2"."$3) }' temp_"$JOB_ID" > $SGE_NODEFILE 
rm -f temp_"$JOB_ID"

# The following is for reporting only. It is not really needed 
# to run the job. It will show up in your output file. 
echo "Got $NSLOTS processors." 
echo "Machines:" 
cat $SGE_NODEFILE

# Infiniband 
export LD_LIBRARY_PATH=/projects/mikem/applications/centos7/libfabric/usr/lib64:$LD_LIBRARY_PATH
time mpirun -mca btl openib,sm,self -np $NSLOTS -v -path ~/MDIC -machinefile $SGE_NODEFILE $APP 

#!/bin/sh
# #$ -P osb-dbs               # Project name in our Sun Grid Engine set for OSB-DBS
#$ -cwd                       # Run job in current dir
#$ -S /bin/sh                 # Use specified shell
#$ -j y                       # Put errors into out-file
# -N MDIC_sim               # The name of my job:
#$ -M laura.thompson@fda.hhs.gov       # Send email to your account
#$ -N MDIC_sim # The name of my job:
# -N no_prior_sim
# Send emailon:
# b - at the beginning, e - at the end of the job.
# a - when the job is rescheduled or aborted (for example, by using the qdel command).
# s - when the job is suspended.
#$ -m beas
#$ -v RAND_INIT
#$ -o job.out 
#$ -l h_vmem=16G 

#$ -pe thread 6           # 6 thread per processor
#$ -t 1-3 # Run program on 10 processors

export RAND_INIT=`date +%s`

APP="Rscript runSimulationsMPIwAJ.R"

# Node list file preparation 
# export SGE_NODEFILE=/tmp/pe_hostfile_$SGE_TASK_ID


echo "Running array job $SGE_TASK_ID on $HOSTNAME"     # output this message to the display

START_STIME=`date +%Y%m%dT%H%M%S`
START_TIME=`date +%s`

#mkdir ~/R/wd$SGE_TASK_ID

source /projects/mikem/applications/centos7/R-3.5.1/set-env.sh
source /projects/mikem/applications/centos7/openmpi/setup-env.sh

export LD_LIBRARY_PATH=/projects/mikem/applications/centos7/libfabric/usr/lib64:$LD_LIBRARY_PATH

#time nohup R CMD BATCH --no-restore-data --no-save templateHPC.R log$SGE_TASK_ID.txt

#time nohup R CMD BATCH --no-restore-data --no-save runSimulationsMPIwAJ.R log$SGE_TASK_ID.txt
#time mpirun -mca btl openib,sm,self -np 6 -v -path ~/MDIC -machinefile $SGE_NODEFILE $APP
time mpiexec -np 6 -v $APP log$SGE_TASK_ID.txt

END_TIME=`date +%s`
ELAPSED_TIME=`expr $END_TIME - $START_TIME`

#echo "$START_STIME $HOSTNAME $SGE_TASK_ID log$SGE_TASK_ID.txt $ELAPSED_TIME" >> stats-run-pr.csv
#mv *.csv ~/R/ASANJOutput/


#rmdir ~/R/wd$SGE_TASK_ID

#
# Regular cron jobs for the dypgen package
#
0 4	* * *	root	[ -x /usr/bin/dypgen_maintenance ] && /usr/bin/dypgen_maintenance

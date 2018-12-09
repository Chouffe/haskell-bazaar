FROM postgres:11.1-alpine

# copy crontabs for root user
COPY backup.sh /home/backup.sh
COPY crontab /etc/crontabs/root

# start crond with log level 8 in foreground, output to stderr
CMD ["crond", "-f", "-d", "8"]

#!/bin/bash

DATE=$(date +%Y-%m-%d-%H-%M-%S)
DB_NAME=$(cat /run/secrets/psql-db)
DB_USER=$(cat /run/secrets/psql-user)
DB_PASSWORD=$(cat /run/secrets/psql-password)

echo "Dumping database"
PGPASSWORD="$DB_PASSWORD" pg_dump \
  -U $DB_USER \
  -d $DB_NAME \
  -h postgres \
  > /backups/$DB_NAME.$DATE.sql
echo "dumping done"

# dump the data base
"pg_dump -U ifn allometr_db > allometr_db_dump.sql"
# scp the database
"scp allometr_db_dump.sql vgranda@158.109.46.23:~/"

# go to the server and drop the database
"DROP DATABASE allometr_db;"

# create an empty database
"CREATE DATABASE allometr_db;"

# load the dump in the new database
"psql -d allometr_db -f allometr_db_dump.sql"

# giving access to guest ####
"
GRANT CONNECT ON DATABASE allometr_db TO guest;
GRANT USAGE ON SCHEMA public TO guest;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO guest;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO guest;

ALTER DEFAULT PRIVILEGES IN SCHEMA public
GRANT SELECT ON TABLES TO guest;
"

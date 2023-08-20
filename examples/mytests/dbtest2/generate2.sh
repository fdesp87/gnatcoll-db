#!/bin/bash

genprogram="gnatcoll_all2ada"

function process_db() {
dbase="-dbhost=localhost \
-dbport=5433 \
-dbname=${1} \
-dbuser=postgres \
-dbpasswd="Nosoples1" \
-dbtype=postgresql"
generate_schema="${genprogram} \
$dbase \
-text \
-omit-schema schema_${1}"

generate_orm="${genprogram} \
-dbname=${1} \
-api=${1}_database \
-orm=${1}_orm \
-dbmodel=db_${1}.orm \
-output=./generated" \
#-updatable-views \


set -x
$generate_schema > db_${1}.orm
$generate_orm

gnatpp --no-compact ./generated/${1}_database.ads
gnatpp --no-compact ./generated/${1}_database.adb
gnatpp --no-compact ./generated/${1}_database_names.ads
gnatpp --no-compact ./generated/${1}_orm.ads
gnatpp --no-compact ./generated/${1}_orm.adb
gnatpp --no-compact ./generated/${1}_orm_new.ads
gnatpp --no-compact ./generated/${1}_orm_new.adb
}

process_db test2
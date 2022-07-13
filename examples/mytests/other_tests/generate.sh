#!/bin/bash 

function process_db() {
generate_orm="../../../gnatcoll_db2ada/obj/debug/gnatcoll_all2ada \
-dbname=${1} \
-api=${1}_database \
-orm=${1}_orm \
-dbmodel=${1}.orm \
-output ./generated"

set -x
$generate_orm
gnatpp --no-compact ./generated/${1}_database.ads
gnatpp --no-compact ./generated/${1}_database.adb
gnatpp --no-compact ./generated/${1}_database_names.ads
gnatpp --no-compact ./generated/${1}_orm.ads
gnatpp --no-compact ./generated/${1}_orm.adb
gnatpp --no-compact ./generated/${1}_orm_new.ads
gnatpp --no-compact ./generated/${1}_orm_new.adb
}

process_db db1
process_db db2
process_db db3
process_db db4
process_db db5
process_db db6
process_db db7
process_db db8
process_db db9
process_db db10


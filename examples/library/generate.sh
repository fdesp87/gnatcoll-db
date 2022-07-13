cd generated

# Generate the Ada API
gnatcoll_all2ada -api=database -orm=orm -dbmodel=../dbschema.txt

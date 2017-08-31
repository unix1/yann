PROJECT = yann
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include erlang.mk

# Compile options

ERLC_OPTS += +warn_export_all +warn_missing_spec +warn_untyped_record
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

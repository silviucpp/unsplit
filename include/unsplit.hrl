-include_lib("kernel/include/logger.hrl").

% logs

-define(PRINT_MSG(Format, Args),
	io:format(Format, Args)).

-define(DEBUG_MSG(Format, Args),
	?LOG_DEBUG(Format, Args)).

-define(INFO_MSG(Format, Args),
	?LOG_INFO(Format, Args)).

-define(WARNING_MSG(Format, Args),
	?LOG_WARNING(Format, Args)).

-define(ERROR_MSG(Format, Args),
	?LOG_ERROR(Format, Args)).

% Type defs

-type merge_actions() :: [{write, any()} | {delete, any()}].
-type merge_strategy() :: same | all_keys | all_remote_keys | {atom(), atom()}.
-type merge_ret() :: stop | {ok, any()} | {ok, merge_actions(), any()} | {ok, merge_actions(), merge_strategy(), any()}.

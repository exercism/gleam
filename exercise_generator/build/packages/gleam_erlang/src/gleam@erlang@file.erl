-module(gleam@erlang@file).
-compile(no_auto_import).

-export([is_directory/1, is_file/1, make_directory/1, list_directory/1, delete_directory/1, recursive_delete/1, read/1, read_bits/1, write/2, write_bits/2, append/2, append_bits/2, delete/1]).
-export_type([reason/0]).

-type reason() :: eacces |
    eagain |
    ebadf |
    ebadmsg |
    ebusy |
    edeadlk |
    edeadlock |
    edquot |
    eexist |
    efault |
    efbig |
    eftype |
    eintr |
    einval |
    eio |
    eisdir |
    eloop |
    emfile |
    emlink |
    emultihop |
    enametoolong |
    enfile |
    enobufs |
    enodev |
    enolck |
    enolink |
    enoent |
    enomem |
    enospc |
    enosr |
    enostr |
    enosys |
    enotblk |
    enotdir |
    enotsup |
    enxio |
    eopnotsupp |
    eoverflow |
    eperm |
    epipe |
    erange |
    erofs |
    espipe |
    esrch |
    estale |
    etxtbsy |
    exdev |
    not_utf8.

-spec is_directory(binary()) -> boolean().
is_directory(Field@0) ->
    filelib:is_dir(Field@0).

-spec is_file(binary()) -> boolean().
is_file(Field@0) ->
    filelib:is_file(Field@0).

-spec make_directory(binary()) -> {ok, nil} | {error, reason()}.
make_directory(Field@0) ->
    gleam_erlang_ffi:make_directory(Field@0).

-spec list_directory(binary()) -> {ok, list(binary())} | {error, reason()}.
list_directory(Field@0) ->
    gleam_erlang_ffi:list_directory(Field@0).

-spec delete_directory(binary()) -> {ok, nil} | {error, reason()}.
delete_directory(Field@0) ->
    gleam_erlang_ffi:delete_directory(Field@0).

-spec recursive_delete(binary()) -> {ok, nil} | {error, reason()}.
recursive_delete(Field@0) ->
    gleam_erlang_ffi:recursive_delete(Field@0).

-spec read(binary()) -> {ok, binary()} | {error, reason()}.
read(Path) ->
    _pipe = Path,
    _pipe@1 = gleam_erlang_ffi:read_file(_pipe),
    gleam@result:then(
        _pipe@1,
        fun(Content) -> case gleam@bit_string:to_string(Content) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error, not_utf8}
            end end
    ).

-spec read_bits(binary()) -> {ok, bitstring()} | {error, reason()}.
read_bits(Path) ->
    gleam_erlang_ffi:read_file(Path).

-spec write(binary(), binary()) -> {ok, nil} | {error, reason()}.
write(Contents, Path) ->
    _pipe = Contents,
    _pipe@1 = gleam@bit_string:from_string(_pipe),
    gleam_erlang_ffi:write_file(_pipe@1, Path).

-spec write_bits(bitstring(), binary()) -> {ok, nil} | {error, reason()}.
write_bits(Contents, Path) ->
    gleam_erlang_ffi:write_file(Contents, Path).

-spec append(binary(), binary()) -> {ok, nil} | {error, reason()}.
append(Contents, Path) ->
    _pipe = Contents,
    _pipe@1 = gleam@bit_string:from_string(_pipe),
    gleam_erlang_ffi:append_file(_pipe@1, Path).

-spec append_bits(bitstring(), binary()) -> {ok, nil} | {error, reason()}.
append_bits(Contents, Path) ->
    gleam_erlang_ffi:append_file(Contents, Path).

-spec delete(binary()) -> {ok, nil} | {error, reason()}.
delete(Field@0) ->
    gleam_erlang_ffi:delete_file(Field@0).

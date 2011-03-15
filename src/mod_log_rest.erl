%%%----------------------------------------------------------------------
%%% File    : mod_log_rest.erl
%%% Author  : Brendon Crawford <brendon@last.vc>
%%% Purpose : Log 2 ways chat messages to a REST service
%%%----------------------------------------------------------------------

-module(mod_log_rest).
-author('brendon@last.vc').

-behaviour(gen_mod).

-export([start/2,
         init/1,
     stop/1,
     log_packet_send/3,
     log_packet_receive/4]).

%-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ?MODULE).
-define(DEFAULT_URL, "http://localhost/messages/add").

-record(config, {url=?DEFAULT_URL}).

start(Host, Opts) ->
    ?DEBUG(" ~p  ~p~n", [Host, Opts]),
    case gen_mod:get_opt(host_config, Opts, []) of
    [] ->
        start_vh(Host, Opts);
    HostConfig ->
        start_vhs(Host, HostConfig)
    end.

start_vhs(_, []) ->
    ok;
start_vhs(Host, [{Host, Opts}| Tail]) ->
    ?DEBUG("start_vhs ~p  ~p~n", [Host, [{Host, Opts}| Tail]]),
    start_vh(Host, Opts),
    start_vhs(Host, Tail);
start_vhs(Host, [{_VHost, _Opts}| Tail]) ->
    ?DEBUG("start_vhs ~p  ~p~n", [Host, [{_VHost, _Opts}| Tail]]),
    start_vhs(Host, Tail).
start_vh(Host, Opts) ->
    Url = gen_mod:get_opt(path, Opts, ?DEFAULT_URL),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_packet_send, 55),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, log_packet_receive, 55),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
         spawn(?MODULE, init, [#config{url=Url}])).

init(Config)->
    ?DEBUG("Starting ~p with config ~p~n", [?MODULE, Config]),
    loop(Config).

loop(Config) ->
    receive
    {call, Caller, get_config} ->
        Caller ! {config, Config},
        loop(Config);
    stop ->
        exit(normal)
    end.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
              ?MODULE, log_packet_send, 55),
    ejabberd_hooks:delete(user_receive_packet, Host,
              ?MODULE, log_packet_receive, 55),
    gen_mod:get_module_proc(Host, ?PROCNAME) ! stop,
    ok.

log_packet_send(From, To, Packet) ->
    log_packet(From, To, Packet, From#jid.lserver).

log_packet_receive(_JID, From, To, _Packet) when From#jid.lserver == To#jid.lserver->
    ok; % only log at send time if the message is local to the server
log_packet_receive(_JID, From, To, Packet) ->
    log_packet(From, To, Packet, To#jid.lserver).

log_packet(From, To, Packet = {xmlelement, "message", Attrs, _Els}, Host) ->
    case xml:get_attr_s("type", Attrs) of
    "groupchat" -> %% mod_muc_log already does it
        ?DEBUG("dropping groupchat: ~s", [xml:element_to_string(Packet)]),
        ok;
    "error" -> %% we don't log errors
        ?DEBUG("dropping error: ~s", [xml:element_to_string(Packet)]),
        ok;
    _ ->
        write_packet(From, To, Packet, Host)
    end;
log_packet(_From, _To, _Packet, _Host) ->
    ok.

write_packet(From, To, Packet, Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME) ! {call, self(), get_config},
    Config = receive
           {config, Result} ->
           Result
       end,
    {Subject, Body} =
    {
        case xml:get_path_s(Packet, [{elem, "subject"}, cdata]) of
            false ->
                "";
            Text ->
                Text
        end,
        xml:get_path_s(Packet, [{elem, "body"}, cdata])
    },
    case Subject ++ Body of
        "" -> %% don't log empty messages
            ?DEBUG("not logging empty message from ~s",[jlib:jid_to_string(From)]),
            ok;
        _ ->
        Url = Config#config.url,
        FromJid = From#jid.luser++"@"++From#jid.lserver,
        ToJid = To#jid.luser++"@"++To#jid.lserver,
        {FilenameTemplate, DateString, Header, MessageType} =
        case calendar:local_time() of
            {{Y, M, D}, {H, Min, S}} ->
            SortedJid = lists:sort([FromJid, ToJid]),
            Title = io_lib:format(template(Format, title), [FromJid, ToJid, Y, M, D]),
            {lists:flatten(io_lib:format("~s/~~p-~~2.2.0w-~~2.2.0w ~s - ~s~s",
                       [Path | SortedJid]++[template(Format, extension)])),
             io_lib:format(template(Format, date), [Y, M, D, H, Min, S]),

             io_lib:format(template(Format, header),
                       lists:duplicate(count(template(Format, header), "~s"),
                               Title)
                      ),
             case hd(SortedJid) of
                 FromJid ->
                 message1;
                 ToJid ->
                 message2
             end
            }
        end,
        ?DEBUG("FilenameTemplate ~p~n",[FilenameTemplate]),
        Filename = io_lib:format(FilenameTemplate, [Y, M, D]),
        ?DEBUG("logging message from ~s into ~s~n",[jlib:jid_to_string(From), Filename]),
        File = case file:read_file_info(Filename) of
               {ok, _} ->
               open_logfile(Filename);
               {error, enoent} ->
               close_previous_logfile(FilenameTemplate, Format, {Y, M, D}),
               NewFile = open_logfile(Filename),
               io:format(NewFile, Header, []),
               NewFile
           end,
        MessageText = case Subject of
               "" ->
               Body;
               _ ->
               io_lib:format(template(Format, subject), [Subject])++Body
           end,
        ?DEBUG("MessageTemplate ~s~n",[template(Format, MessageType)]),
        io:format(File, lists:flatten(template(Format, MessageType)), [DateString, FromJid, From#jid.lresource, ToJid,
                                To#jid.lresource, MessageText]),
        file:close(File)
    end.

open_logfile(Filename) ->
    case file:open(Filename, [append]) of
    {ok, File} ->
        File;
    {error, Reason} ->
        ?ERROR_MSG("Cannot write into file ~s: ~p~n", [Filename, Reason])
    end.

close_previous_logfile(FilenameTemplate, Format, Date) ->
    Yesterday = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 1),
    Filename = io_lib:format(FilenameTemplate, tuple_to_list(Yesterday)),
    case file:read_file_info(Filename) of
    {ok, _} ->
        File = open_logfile(Filename),
        io:format(File, template(Format, footer), []),
        file:close(File);
    {error, enoent} ->
        ok
    end.


% return the number of occurence of Word in String
count(String, Word) ->
    case string:str(String, Word) of
    0 ->
        0;
    N ->
        1+count(string:substr(String, N+length(Word)), Word)
    end.



template(text, extension) ->
    ".log";
template(text, title) ->
    "Messages log between ~s and ~s on ~p-~2.2.0w-~2.2.0w";
template(text, header) ->
    "~s~n-----------------------------------------------------------------------~n";
template(text, subject) ->
    "Subject: ~s~n";
template(text, message) ->
    "~~s ~~s/~~s -> ~~s/~~s~n~s~~s~n";
template(text, message1) ->
    io_lib:format(template(text, message), ["> "]);
template(text, message2) ->
    io_lib:format(template(text, message), ["< "]);
template(text, date) ->
    "~p-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w";
template(text, footer) ->
    "---- End ----~n";


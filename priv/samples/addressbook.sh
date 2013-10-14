#!/usr/bin/env escript
%% -*- mode: erlang -*-

-include("addressbook.capnp.hrl").

main(["read"|Args]) ->
    read(Args);
main(["write"|Args]) ->
    write(Args);
main(_) ->
    io:format("Usage: addressbook.sh {read [filename] | write}~n").

read([FileName]) ->
    {ok, Data} = file:read_file(FileName),
    dump_message(Data);
read([]) ->
    io:format("Reading message from stdin~n"
              "  (note, this will likely fail)~n"
              "  (if you are on windows,     )~n"
              "  (see README.                )~n~n"),
    dump_message(read_stdin()).

write([]) ->    
    {ok, Root} = addressbook(root, 'AddressBook'),
    [Alice, Bob, Steve] = addressbook(set, people, 3, Root),
    [AlicePhone] = addressbook(set, phones, 1, Alice),
    [BobPhone1, BobPhone2] = addressbook(set, phones, 2, Bob),
    [addressbook(set, Field, Value, Obj)
     || {Obj, FieldValues} <- 
            [{Alice,
              [{id, 123},
               {name, <<"Alice">>},
               {email, <<"alice@example.com">>},
               {employment, {school, <<"MIT">>}}
              ]},
             {AlicePhone,
              [{number, <<"555-1212">>},
               {type, mobile}]},
             {Bob,
              [{id, 456},
               {name, <<"Bob">>},
               {email, <<"bob@example.com">>},
               {employment, unemployed}
              ]},
             {BobPhone1,
              [{number, <<"555-4567">>},
               {type, home}]},
             {BobPhone2,
              [{number, <<"555-7654">>},
               {type, work}]},
             {Steve,
              [{id, 123456},
               {name, <<"Steve">>},
               {email, <<"steve@example.com">>},
               {employment, selfEmployed},
               {phones, 3},
               {phones, {0, {number, <<"555-1234">>}}},
               {phones, {0, {type, home}}},
               {phones, {1, {number, <<"555-4321">>}}},
               {phones, {1, {type, work}}},
               {phones, {2, {number, <<"070-5555">>}}},
               {phones, {2, {type, mobile}}}
              ]}
            ],
        {Field, Value} <- FieldValues],

    %% Get message data and pack it
    Data1 = ecapnp_serialize:pack(
              ecapnp_message:write(Root)),
    io:setopts([{encoding, unicode}]),
    io:put_chars(Data1).


dump_message(Data) ->
    %% unpack and read message data
    {ok, Message} = ecapnp_message:read(
                      ecapnp_serialize:unpack(Data)),
    {ok, Root} = addressbook(root, 'AddressBook', Message),
    People = addressbook(get, people, Root),
    [dump_person(Person) || Person <- People].

dump_person(Person) ->
    io:format("#~p ", [addressbook(get, id, Person)]),
    io:format("~s: ~s~n", [addressbook(get, name, Person),
                           addressbook(get, email, Person)]),
    Phones = addressbook(get, phones, Person),
    [io:format("  ~s phone: ~s~n", [addressbook(get, type, P),
                                    addressbook(get, number, P)]) 
     || P <- Phones],
    %% hmm... not the best looking api, this.. :/
    case addressbook(get, addressbook(get, employment, Person)) of
        unemployed -> io:format("  unemployed~n");
        {employer, Employer} ->
            io:format("  employer: ~s~n", [Employer]);
        {school, School} ->
            io:format("  student at: ~s~n", [School]);
        selfEmployed -> io:format("  self-employed~n")
    end.

read_stdin() ->
    read_stdin([]).

read_stdin(Acc)
  when is_list(Acc) ->
    read_stdin(file:read(standard_io, 1024), Acc).

read_stdin(eof, Acc) ->
    list_to_binary(
      lists:reverse(Acc));
read_stdin({ok, Data}, Acc) ->
    read_stdin([Data|Acc]).

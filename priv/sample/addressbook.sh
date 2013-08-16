#!/usr/bin/env escript
%% -*- mode: erlang -*-

-include("addressbook.capnp.hrl").

main([FileName]) ->
    {ok, Data} = file:read_file(FileName),
    process(Data);
main([]) ->
    io:format("Reading message from stdin~n"
              "  (note, this will likely fail)"
              "  (if you are on windows,     )"
              "  (see README.                )~n~n"),
    process(read_stdin()).

process(Data) ->
    {ok, Message} = ecapnp_message:read(Data),
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
    case addressbook(get, employment, Person) of
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

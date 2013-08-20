%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-export([addressbook/1, addressbook/2, addressbook/3, addressbook/4]).
addressbook(root, Type) ->
    ecapnp:set_root(Type, addressbook(schema)).

addressbook(root, Type, Message) ->
    ecapnp:get_root(Type, addressbook(schema), Message);

addressbook(get, Field, Object) ->
    ecapnp:get(Field, Object).

addressbook(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

addressbook(schema) ->
  #schema{
    name=addressbook, id=16#9eb32e19f86ee174, source= <<"addressbook.capnp">>,
    types=
      [{'Person',
        #struct{
          name='Person', id=16#98808e9832e8bc18, source= <<"addressbook.capnp:Person">>,
          dsize=1, psize=4, esize=inlineComposite, fields=
            [{id,
              #data{ type=uint32, align=0 }},
             {name,
              #ptr{ type=text, idx=0 }},
             {email,
              #ptr{ type=text, idx=1 }},
             {phones,
              #ptr{ type={list,{struct,'PhoneNumber'}}, idx=2 }},
             {employment,
              #data{ align=32, type=
                {union,
                  [{unemployed,void},
                   {employer,
                    #ptr{ type=text, idx=3 }},
                   {school,
                    #ptr{ type=text, idx=3 }},
                   {selfEmployed,void}
                  ]} }}
            ],
          types=
            [{'PhoneNumber',
              #struct{
                name='PhoneNumber', id=16#814e90b29c9e8ad0, source= <<"addressbook.capnp:Person.PhoneNumber">>,
                dsize=1, psize=1, esize=inlineComposite, fields=
                  [{number,
                    #ptr{ type=text, idx=0 }},
                   {type,
                    #data{ type={enum,'Type'}, align=0 }}
                  ],
                types=
                  [{'Type',
                    #enum{
                      name='Type', id=16#91e0bd04d585062f, source= <<"addressbook.capnp:Person.PhoneNumber.Type">>,
                      values=
                        [mobile,
                         home,
                         work
                        ]}}
                  ]}}
            ]}},
       {'AddressBook',
        #struct{
          name='AddressBook', id=16#f934d9b354a8a134, source= <<"addressbook.capnp:AddressBook">>,
          dsize=0, psize=1, esize=pointer, fields=
            [{people,
              #ptr{ type={list,{struct,'Person'}}, idx=0 }}
            ]}}
      ]}.

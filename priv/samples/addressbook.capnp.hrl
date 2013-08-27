%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-export([addressbook/1, addressbook/2, addressbook/3, addressbook/4]).

%% addressbook/4
addressbook(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% addressbook/3
addressbook(root, Type, Message) ->
    ecapnp:get_root(Type, addressbook(schema), Message);
addressbook(get, Field, Object) ->
    ecapnp:get(Field, Object).

%% addressbook/2
addressbook(root, Type) ->
    ecapnp:set_root(Type, addressbook(schema));
addressbook(get, Object) ->
    ecapnp:get(Object).

%% addressbook/1

addressbook(schema) ->
  #schema{ %% 0x9eb32e19f86ee174
    name=addressbook, id=11435534567900897652, source= <<"addressbook.capnp">>,
    types=
      [#struct{ %% 0x98808e9832e8bc18
         name='Person', id=10988939875124296728, source= <<"addressbook.capnp:Person">>,
         dsize=1, psize=4, esize=inlineComposite,
         union_field=none,
         fields=
           [{id,
             #data{ type=uint32, align=0 }},
            {name,
             #ptr{ type=text, idx=0 }},
            {email,
             #ptr{ type=text, idx=1 }},
            {phones,
             #ptr{ type={list,{struct,9317543775882349264}}, idx=2 }},
            {employment,
             #group{ id=13477914502553102653 }}
           ],
         types=
           [#struct{ %% 0x814e90b29c9e8ad0
              name='PhoneNumber', id=9317543775882349264, source= <<"addressbook.capnp:Person.PhoneNumber">>,
              dsize=1, psize=1, esize=inlineComposite,
              union_field=none,
              fields=
                [{number,
                  #ptr{ type=text, idx=0 }},
                 {type,
                  #data{ type={enum,10511609358742521391}, align=0 }}
                ],
              types=
                [#enum{ %% 0x91e0bd04d585062f
                   name='Type', id=10511609358742521391, source= <<"addressbook.capnp:Person.PhoneNumber.Type">>,
                   values=
                     [mobile,
                      home,
                      work
                     ]}
                ]},
            #struct{ %% 0xbb0b2bd4bdc3693d
              name=employment, id=13477914502553102653, source= <<"addressbook.capnp:Person.employment">>,
              dsize=1, psize=4, esize=inlineComposite,
              union_field=#data{ align=32, type=
                {union,
                  [{unemployed,void},
                   {employer,
                    #ptr{ type=text, idx=3 }},
                   {school,
                    #ptr{ type=text, idx=3 }},
                   {selfEmployed,void}
                ]} }}
           ]},
       #struct{ %% 0xf934d9b354a8a134
         name='AddressBook', id=17957216978475721012, source= <<"addressbook.capnp:AddressBook">>,
         dsize=0, psize=1, esize=pointer,
         union_field=none,
         fields=
           [{people,
             #ptr{ type={list,{struct,10988939875124296728}}, idx=0 }}
           ]}
      ]}.

create table users (
        id integer primary key autoincrement not null,
        email text not null,
        login text unique,
        inviter integer references users
        );

create table messages (
        id integer primary key autoincrement not null,
        message text not null,
        user integer not null references users(id),
        retractable boolean not null,
        parent integer references messages(id),
        vote integer not null default 0
        );

create table conversations (
        id integer primary key autoincrement not null,
        rif integer not null references messages(id)
        );

create table voting (
        message integer references messages(id),
        user integer references users(id),
        unique (message,user)
        );

create table store (
        conversation integer references conversations(id),
        user integer references users(id),
        unique (conversation,user),
        date integer not null default now()
        );
create index conv1 on conversations (rif);        
create index voting1 on voting (message,user);
create index voting2 on voting (message);
create index store1 on store (conversation,user);
create index store2 on store (user);
create index store3 on store (conversation);
